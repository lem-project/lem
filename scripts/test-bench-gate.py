#!/usr/bin/env python3
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.

"""Exercise the shell gate's failure propagation without building container images."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent.parent

FAKE = r'''#!/usr/bin/env python3
import io, os, pathlib, sys, tarfile
args=sys.argv[1:]
if pathlib.Path(sys.argv[0]).name == "git":
    if args[0] == "archive":
        with tarfile.open(fileobj=sys.stdout.buffer,mode="w|") as out:
            if os.environ["SCENARIO"] != "old-base":
                for name in ("scripts/run-yeditor-bench.lisp", "yeditor/core/bench/yeditor-bench.asd"):
                    data=b"base harness"
                    item=tarfile.TarInfo(name); item.size=len(data)
                    out.addfile(item,io.BytesIO(data))
    elif args[:2] == ["worktree","add"]:
        base=pathlib.Path(args[3]); (base/"scripts").mkdir(parents=True)
        (base/"scripts/run-yeditor-bench.lisp").write_text("base harness")
    sys.exit(0)
if args[0] == "build": sys.exit(0)
out=pathlib.Path(args[args.index("-v")+1].rsplit(":",1)[0])
command=" ".join(args)
is_base="yeditor-bench-base" in args
with (out/"calls.txt").open("a") as f: f.write(("base " if is_base else "head ")+command+"\n")
if is_base and os.environ["SCENARIO"] == "base-error":
    print("base compiler failed",file=sys.stderr); sys.exit(7)
(out/("base.json" if is_base else "head.json")).write_text("{}")
print("FAIL injected regression" if os.environ["SCENARIO"]=="regression" else "ok benchmark")
sys.exit(1 if not is_base and os.environ["SCENARIO"]=="regression" else 0)
'''


class GateTest(unittest.TestCase):
    def run_gate(self, scenario):
        tmp = tempfile.TemporaryDirectory(prefix="yeditor-bench-gate-")
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        for name in ("git", "docker"):
            script = root / name
            script.write_text(FAKE)
            script.chmod(0o755)
        out = root / "output with spaces"
        out.mkdir()
        (out / "base.json").write_text("stale baseline")
        (out / "report.txt").write_text("stale success")
        env = dict(os.environ, PATH=str(root)+os.pathsep+os.environ["PATH"], SCENARIO=scenario)
        result = subprocess.run(["bash", "scripts/bench-gate.sh", "BASE", str(out)], cwd=ROOT,
                                env=env, capture_output=True, text=True)
        return result, out

    def test_baseline_failure_stops_comparison_and_keeps_diagnostics(self):
        result, out = self.run_gate("base-error")
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("head ", (out / "calls.txt").read_text())
        self.assertIn("base compiler failed", (out / "base.log").read_text())
        self.assertIn("FAIL", (out / "report.txt").read_text())
        self.assertFalse((out / "base.json").exists())

    def test_success_uses_measure_only_for_baseline(self):
        result, out = self.run_gate("success")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("--measure-only", (out / "calls.txt").read_text())
        self.assertIn("ok benchmark", (out / "report.txt").read_text())

    def test_regression_propagates_nonzero_status(self):
        result, out = self.run_gate("regression")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("FAIL injected regression", (out / "report.txt").read_text())

    def test_old_base_removes_stale_artifacts(self):
        result, out = self.run_gate("old-base")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse((out / "base.json").exists())
        self.assertNotIn("base ", (out / "calls.txt").read_text())


if __name__ == "__main__":
    unittest.main()
