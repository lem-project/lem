{
  description = "Clojure sample project for testing lem-clojure-mode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Clojure toolchain
            clojure
            clojure-lsp
            babashka

            # Java runtime
            jdk21

            # Additional tools
            rlwrap  # For better REPL experience
          ];

          shellHook = ''
            echo "Clojure development environment"
            echo ""
            echo "Available commands:"
            echo "  clj              - Start Clojure REPL"
            echo "  clj -M:nrepl     - Start nREPL server"
            echo "  clj -X:test      - Run tests"
            echo "  clojure-lsp      - Language server (started by Lem automatically)"
            echo "  bb               - Babashka (fast scripting)"
            echo ""
            echo "Versions:"
            echo "  Java: $(java -version 2>&1 | head -1)"
            echo "  Clojure: $(clj -version 2>&1 | head -1)"
            echo "  clojure-lsp: $(clojure-lsp --version 2>&1 | head -1)"
          '';
        };
      });
}
