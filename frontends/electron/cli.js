#!/usr/bin/env node
const spawn = require("child_process").spawn;
const electron = require("electron");
const argv = [__dirname];
spawn(electron, argv, { stdio: "inherit" });
