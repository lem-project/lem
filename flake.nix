{
  description = "A flake for lem";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/release-25.11";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem =
        { pkgs, ... }:
        let
          # --- Setup & Helpers ---
          lisp = pkgs.sbcl;

          # Helper to generate the Lisp build script used by all variants
          mkBuildScript =
            {
              entryPoint ? "lem:main",
            }:
            pkgs.writeText "build-lem.lisp" ''
              (defpackage :nix-cl-user (:use :cl))
              (in-package :nix-cl-user)

              ;; Load ASDF
              (load "${lem-base.asdfFasl}/asdf.${lem-base.faslExt}")
              (asdf:initialize-output-translations '(:output-translations :disable-cache :inherit-configuration))

              ;; Load Systems
              (mapcar #'asdf:load-system (uiop:split-string (uiop:getenv "systems")))

              ;; Dump Image
              (setf uiop:*image-entry-point* #'${entryPoint})
              (uiop:dump-image "lem" :executable t :compression t)
            '';

          sources = import ./_sources/generated.nix {
            inherit (pkgs)
              fetchgit
              fetchurl
              fetchFromGitHub
              dockerTools
              ;
          };

          # --- Core Lisp Dependencies ---

          micros = lisp.buildASDFSystem {
            inherit (sources.micros) pname src version;
            systems = [ "micros" ];
          };

          jsonrpc = lisp.buildASDFSystem {
            inherit (sources.jsonrpc) pname src version;
            systems = [
              "jsonrpc"
              "jsonrpc/transport/stdio"
              "jsonrpc/transport/tcp"
              "jsonrpc/transport/websocket"
              "jsonrpc/transport/local-domain-socket"
            ];
            lispLibs = with lisp.pkgs; [
              yason
              alexandria
              bordeaux-threads
              dissect
              chanl
              vom
              usocket
              trivial-timeout
              cl_plus_ssl
              quri
              fast-io
              trivial-utf-8
              websocket-driver
              clack
              clack-handler-hunchentoot
              event-emitter
              hunchentoot
            ];
          };

          async-process =
            let
              c-lib = pkgs.stdenv.mkDerivation {
                inherit (sources.async-process) pname src version;
                nativeBuildInputs = with pkgs; [
                  libtool
                  libffi.dev
                  automake
                  autoconf
                  pkg-config
                ];
                buildPhase = "make PREFIX=$out";
              };
            in
            lisp.buildASDFSystem {
              inherit (sources.async-process) pname src version;
              systems = [ "async-process" ];
              lispLibs = [ lisp.pkgs.cffi ];
              nativeLibs = [ c-lib ];
              nativeBuildInputs = [ pkgs.pkg-config ];
            };

          lem-mailbox = lisp.buildASDFSystem {
            inherit (sources.lem-mailbox) pname src version;
            systems = [ "lem-mailbox" ];
            lispLibs = with lisp.pkgs; [
              bordeaux-threads
              bt-semaphore
              queues
              queues_dot_simple-cqueue
            ];
          };

          # --- Tree-sitter Support ---

          # C wrapper library for tree-sitter (handles by-value struct returns)
          ts-wrapper = pkgs.stdenv.mkDerivation {
            pname = "ts-wrapper";
            version = "0.1.0";
            src = "${sources.tree-sitter-cl.src}/c-wrapper";
            buildInputs = [ pkgs.tree-sitter ];
            buildPhase =
              let
                ext = if pkgs.stdenv.isDarwin then "dylib" else "so";
              in
              ''
                $CC -shared -fPIC -o libts-wrapper.${ext} ts-wrapper.c \
                  -I${pkgs.tree-sitter}/include \
                  -L${pkgs.tree-sitter}/lib \
                  -ltree-sitter
              '';
            installPhase =
              let
                ext = if pkgs.stdenv.isDarwin then "dylib" else "so";
              in
              ''
                mkdir -p $out/lib
                cp libts-wrapper.${ext} $out/lib/
              '';
          };

          # tree-sitter-cl Lisp bindings (from github.com/lem-project/tree-sitter-cl)
          tree-sitter-cl = lisp.buildASDFSystem {
            inherit (sources.tree-sitter-cl) pname version src;
            systems = [ "tree-sitter-cl" ];
            lispLibs = with lisp.pkgs; [
              cffi
              alexandria
              trivial-garbage
            ];
            nativeLibs = [
              pkgs.tree-sitter
              ts-wrapper
            ];
          };

          # Tree-sitter language grammars
          tree-sitter-grammars = {
            json = pkgs.tree-sitter-grammars.tree-sitter-json;
            markdown = pkgs.tree-sitter-grammars.tree-sitter-markdown;
            yaml = pkgs.tree-sitter-grammars.tree-sitter-yaml;
            # Add more languages here as needed:
            # javascript = pkgs.tree-sitter-grammars.tree-sitter-javascript;
            # python = pkgs.tree-sitter-grammars.tree-sitter-python;
          };

          # --- Webview Specific Dependencies ---

          c-webview = pkgs.stdenv.mkDerivation {
            pname = "c-webview";
            version = "unstable";
            src = sources.webview.src;
            nativeBuildInputs = with pkgs; [
              cmake
              ninja
              pkg-config
            ];
            buildInputs =
              if pkgs.stdenv.isLinux then
                [
                  pkgs.webkitgtk_4_1
                  pkgs.webkitgtk_6_0
                  pkgs.gtk3
                ]
              else
                [ ];

            # Use FETCHCONTENT to use pre-fetched source instead of network
            configurePhase = ''
              runHook preConfigure
              cmake -G Ninja -B build -S c \
                -DCMAKE_BUILD_TYPE=Release \
                -DFETCHCONTENT_SOURCE_DIR_WEBVIEW=${sources.webview-upstream.src}
              runHook postConfigure
            '';
            buildPhase = "cmake --build build";
            installPhase =
              let
                suffix = if pkgs.stdenv.isLinux then "so" else "dylib";
              in
              ''
                mkdir -p $out/lib
                cp build/lib/libexample.${suffix} $out/lib/libwebview.${suffix}
              '';
          };

          cl-webview = lisp.buildASDFSystem {
            inherit (sources.webview) pname src version;
            systems = [ "webview" ];
            lispLibs = with lisp.pkgs; [
              cffi
              float-features
            ];
            nativeLibs = [ c-webview ];
            postPatch = ''
              # Fix library loading path and name in the Lisp binding
              sed -i 's/(define-foreign-library (libwebview/(define-foreign-library libwebview/' webview.lisp
              sed -i '/:search-path/,/arm64"))))/d' webview.lisp
              sed -i 's/"libwebview\.so\.0\.12\.0"/"libwebview.so"/' webview.lisp
            '';
          };

          # --- Lem Core Definition ---

          # List of libraries common to all Lem frontends
          commonLispLibs = with lisp.pkgs; [
            micros
            async-process
            jsonrpc
            lem-mailbox
            tree-sitter-cl
            deploy
            iterate
            closer-mop
            trivia
            alexandria
            trivial-gray-streams
            trivial-types
            cl-ppcre
            inquisitor
            babel
            bordeaux-threads
            yason
            log4cl
            split-sequence
            str
            dexador
            cl-mustache
            esrap
            parse-number
            cl-package-locks
            trivial-utf-8
            swank
            _3bmd
            _3bmd-ext-code-blocks
            lisp-preprocessor
            trivial-ws
            trivial-open-browser
            frugal-uuid
            hunchentoot
          ];

          # The base derivation that other variants inherit from
          lem-base = lisp.buildASDFSystem {
            pname = "lem-base";
            version = "unstable";
            src = ./.;
            nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
            lispLibs = commonLispLibs;

            postPatch = ''
              sed -i '1i(pushnew :nix-build *features*)' lem.asd
            '';

            buildScript = mkBuildScript { entryPoint = "lem:main"; };

            installPhase = ''
              runHook preInstall
              mkdir -p $out/bin
              install lem $out/bin
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH" \
                --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"
              runHook postInstall
            '';
          };

          # 1. Ncurses Variant
          lem-ncurses = lem-base.overrideLispAttrs (o: {
            pname = "lem-ncurses";
            meta.mainProgram = "lem";
            systems = [ "lem-ncurses" "tree-sitter-cl" "lem-tree-sitter" ];
            lispLibs =
              o.lispLibs
              ++ (with lisp.pkgs; [
                cl-charms
                cl-setlocale
              ]);
            nativeLibs = [
              pkgs.ncurses
              pkgs.tree-sitter
              ts-wrapper
            ];
            # Add tree-sitter grammar paths (grammars don't have lib/ subdir)
            installPhase = ''
              runHook preInstall
              mkdir -p $out/bin
              install lem $out/bin
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH:${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}" \
                --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"
              runHook postInstall
            '';
          });

          # 2. SDL2 Variant
          lem-sdl2 = lem-base.overrideLispAttrs (o: {
            pname = "lem-sdl2";
            meta.mainProgram = "lem";
            systems = [ "lem-sdl2" "tree-sitter-cl" "lem-tree-sitter" ];
            lispLibs =
              o.lispLibs
              ++ (with lisp.pkgs; [
                sdl2
                sdl2-ttf
                sdl2-image
                trivial-main-thread
              ]);
            nativeLibs = with pkgs; [
              SDL2
              SDL2_ttf
              SDL2_image
              tree-sitter
              ts-wrapper
            ];
            installPhase = ''
              runHook preInstall
              mkdir -p $out/bin
              install lem $out/bin
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH:${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}" \
                --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"
              runHook postInstall
            '';
          });

          # 3. Webview Variant
          lem-webview = lem-base.overrideLispAttrs (o: {
            pname = "lem-webview";
            meta.mainProgram = "lem";
            systems = [ "lem-webview" "tree-sitter-cl" "lem-tree-sitter" ];

            # Use the specific webview entry point
            buildScript = mkBuildScript { entryPoint = "lem-webview:main"; };

            lispLibs =
              o.lispLibs
              ++ [ cl-webview ]
              ++ (with lisp.pkgs; [
                float-features
                command-line-arguments
              ]);

            nativeLibs =
              if pkgs.stdenv.isLinux then
                [
                  pkgs.webkitgtk_4_1
                  pkgs.webkitgtk_6_0
                  pkgs.gtk3
                  pkgs.stdenv.cc.cc.lib
                  c-webview
                  pkgs.tree-sitter
                  ts-wrapper
                ]
              else
                [
                  pkgs.stdenv.cc.cc.lib
                  c-webview
                  pkgs.tree-sitter
                  ts-wrapper
                ];

            postPatch =
              (o.postPatch or "")
              + (
                if pkgs.stdenv.isLinux then
                  ''sed -i 's/fontName:"Monospace"/fontName:"DejaVu Sans Mono"/' frontends/server/frontend/dist/assets/index.js''
                else
                  ''sed -i 's/fontName:"Monospace"/fontName:"Menlo"/' frontends/server/frontend/dist/assets/index.js''
              );

            postInstall =
              if pkgs.stdenv.isLinux then
                ''
                  wrapProgram $out/bin/lem \
                    --set FONTCONFIG_FILE "${pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; }}" \
                    --prefix XDG_DATA_DIRS : "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}" \
                    --prefix XDG_DATA_DIRS : "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}" \
                    --prefix LD_LIBRARY_PATH : "${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}"
                ''
              else
                ''
                  wrapProgram $out/bin/lem \
                    --prefix LD_LIBRARY_PATH : "${tree-sitter-grammars.json}:${tree-sitter-grammars.markdown}:${tree-sitter-grammars.yaml}"
                '';
          });
        in
        {
          overlayAttrs = {
            inherit lem-ncurses lem-sdl2 lem-webview;
          };

          packages = {
            inherit lem-ncurses lem-sdl2 lem-webview;
            default = lem-ncurses;
          };

          apps = {
            lem-ncurses = {
              type = "app";
              program = lem-ncurses;
            };
            lem-sdl2 = {
              type = "app";
              program = lem-sdl2;
            };
            lem-webview = {
              type = "app";
              program = lem-webview;
            };
            default = {
              type = "app";
              program = lem-ncurses;
            };
          };

          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              # Lisp development
              sbcl
              sbclPackages.qlot-cli

              # Build tools
              gnumake
              pkg-config

              # Native libraries for frontends
              ncurses
              SDL2
              SDL2_ttf
              SDL2_image

              # SSL/TLS support
              openssl

              # Tree-sitter support
              tree-sitter
              pkgs.tree-sitter-grammars.tree-sitter-json
              pkgs.tree-sitter-grammars.tree-sitter-markdown
              pkgs.tree-sitter-grammars.tree-sitter-yaml

              # Code formatting
              nixfmt-rfc-style

              # Development tools
              direnv
            ] ++ lib.optionals stdenv.isLinux [
              # Linux-specific dependencies for webview frontend
              webkitgtk_4_1
              gtk3
            ];

            # Set up library paths for native dependencies
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath ([
              pkgs.ncurses
              pkgs.SDL2
              pkgs.SDL2_ttf
              pkgs.SDL2_image
              pkgs.openssl
              pkgs.tree-sitter
              ts-wrapper
              pkgs.tree-sitter-grammars.tree-sitter-json
              pkgs.tree-sitter-grammars.tree-sitter-markdown
              pkgs.tree-sitter-grammars.tree-sitter-yaml
            ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              pkgs.webkitgtk_4_1
              pkgs.gtk3
            ]);

            shellHook = ''
              echo "Lem development environment"
              echo "  SBCL: $(sbcl --version)"
              echo "  qlot: $(qlot --version 2>/dev/null || echo 'available')"
              echo ""
              echo "Quick start:"
              echo "  qlot install    # Install dependencies"
              echo "  make ncurses    # Build terminal version"
              echo "  make sdl2       # Build GUI version"
            '';
          };
        };
    };
}
