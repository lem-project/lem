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
          lisp = pkgs.sbcl;
          sources = import ./_sources/generated.nix {
            inherit (pkgs)
              fetchgit
              fetchurl
              fetchFromGitHub
              dockerTools
              ;
          };
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
              # For websocket transport
              websocket-driver
              clack
              clack-handler-hunchentoot
              event-emitter
              hunchentoot
            ];
          };
          c-async-process = pkgs.stdenv.mkDerivation {
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
          async-process = lisp.buildASDFSystem {
            inherit (sources.async-process) pname src version;
            systems = [ "async-process" ];
            lispLibs = with lisp.pkgs; [
              cffi
            ];
            nativeLibs = [
              c-async-process
            ];
            nativeBuildInputs = with pkgs; [
              pkg-config
            ];
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
          # Build the webview C library from lem-project/webview
          c-webview = pkgs.stdenv.mkDerivation {
            pname = "c-webview";
            version = "unstable";
            src = sources.webview.src;
            nativeBuildInputs = with pkgs; [
              cmake
              ninja
              pkg-config
            ];
            buildInputs = with pkgs; [
              webkitgtk_4_1
              gtk3
            ];
            # Use FETCHCONTENT_SOURCE_DIR to skip network fetch and use pre-fetched source
            configurePhase = ''
              runHook preConfigure
              cmake -G Ninja -B build -S c \
                -DCMAKE_BUILD_TYPE=Release \
                -DFETCHCONTENT_SOURCE_DIR_WEBVIEW=${sources.webview-upstream.src}
              runHook postConfigure
            '';
            buildPhase = ''
              runHook preBuild
              cmake --build build
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              mkdir -p $out/lib
              cp build/lib/libexample.so $out/lib/libwebview.so
              runHook postInstall
            '';
          };
          # Common Lisp webview bindings
          cl-webview = lisp.buildASDFSystem {
            inherit (sources.webview) pname src version;
            systems = [ "webview" ];
            lispLibs = with lisp.pkgs; [
              cffi
              float-features
            ];
            nativeLibs = [
              c-webview
            ];
            # Minimal patch: remove :search-path and fix library name
            postPatch = ''
              # Remove :search-path directive (change "(libwebview" to "libwebview")
              sed -i 's/(define-foreign-library (libwebview/(define-foreign-library libwebview/' webview.lisp
              # Delete the :search-path block (lines containing :search-path through arm64"))))
              sed -i '/:search-path/,/arm64"))))/d' webview.lisp
              # Fix library name (remove version suffix)
              sed -i 's/"libwebview\.so\.0\.12\.0"/"libwebview.so"/' webview.lisp
            '';
          };
          lem-fake-interface = lisp.buildASDFSystem {
            pname = "lem-fake-interface";
            version = "unstable";
            systems = [ "lem-fake-interface" ];
            src = ./.;
            nativeBuildInputs = with pkgs; [
              makeBinaryWrapper
            ];
            postPatch = ''
              sed -i '1i(pushnew :nix-build *features*)' lem.asd
            '';
            buildScript = pkgs.writeText "build-lem.lisp" ''
              (defpackage :nix-cl-user (:use :cl))
              (in-package :nix-cl-user)

              ;; Load ASDF
              (load "${lem-fake-interface.asdfFasl}/asdf.${lem-fake-interface.faslExt}")

              ;; Avoid writing to the global fasl cache
              (asdf:initialize-output-translations '(:output-translations :disable-cache :inherit-configuration))

              ;; Initial load
              (mapcar #'asdf:load-system (uiop:split-string (uiop:getenv "systems")))

              ;; Create executable
              (setf uiop:*image-entry-point* #'lem:main)
              (uiop:dump-image "lem" :executable t :compression t)
            '';
            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              install lem $out/bin
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH" \
                --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"

              runHook postInstall
            '';
            lispLibs = with lisp.pkgs; [
              micros
              async-process
              jsonrpc
              lem-mailbox
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
              # lem-mcp-server
              hunchentoot
            ];
          };
          lem-cli = lem-fake-interface.overrideLispAttrs (o: {
            pname = "lem";
            meta.mainProgram = "lem";
            systems = [
              "lem-ncurses"
              "lem-sdl2"
            ];
            lispLibs =
              o.lispLibs
              ++ (with lisp.pkgs; [
                # lem-curses
                cl-charms
                cl-setlocale
                # sdl2
                sdl2
                sdl2-ttf
                sdl2-image
                trivial-main-thread
              ]);
            nativeLibs = with pkgs; [
              # lem-curses
              ncurses
              # sdl2
              SDL2
              SDL2_ttf
              SDL2_image
            ];
          });
          lem-webview = lem-fake-interface.overrideLispAttrs (o: {
            pname = "lem-webview";
            meta.mainProgram = "lem";
            systems = [
              "lem-webview"
            ];
            # Patch default font in the bundled frontend (dist/ is what lem-server actually uses)
            postPatch = (o.postPatch or "") + ''
              sed -i 's/fontName:"Monospace"/fontName:"DejaVu Sans Mono"/' frontends/server/frontend/dist/assets/index.js
            '';
            # Override buildScript to use lem-webview:main as entry point
            buildScript = pkgs.writeText "build-lem-webview.lisp" ''
              (defpackage :nix-cl-user (:use :cl))
              (in-package :nix-cl-user)

              ;; Load ASDF
              (load "${lem-fake-interface.asdfFasl}/asdf.${lem-fake-interface.faslExt}")

              ;; Avoid writing to the global fasl cache
              (asdf:initialize-output-translations '(:output-translations :disable-cache :inherit-configuration))

              ;; Initial load
              (mapcar #'asdf:load-system (uiop:split-string (uiop:getenv "systems")))

              ;; Create executable with lem-webview:main as entry point
              (setf uiop:*image-entry-point* #'lem-webview:main)
              (uiop:dump-image "lem" :executable t :compression t)
            '';
            lispLibs =
              o.lispLibs
              ++ [
                cl-webview
              ]
              ++ (with lisp.pkgs; [
                float-features
                command-line-arguments
              ]);
            nativeLibs = with pkgs; [
              webkitgtk_4_1
              gtk3
              stdenv.cc.cc.lib  # For libstdc++
              c-webview
            ];
            # Include monospace font and configure fontconfig
            postInstall = let
              fontsConf = pkgs.makeFontsConf {
                fontDirectories = [ pkgs.dejavu_fonts ];
              };
            in ''
              wrapProgram $out/bin/lem \
                --set FONTCONFIG_FILE "${fontsConf}" \
                --prefix XDG_DATA_DIRS : "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}" \
                --prefix XDG_DATA_DIRS : "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
            '';
          });
        in
        {
          overlayAttrs = {
            lem = lem-cli;
            inherit lem-webview;
          };
          packages.lem = lem-cli;
          packages.lem-webview = lem-webview;
          apps.lem = {
            type = "app";
            program = lem-cli;
          };
          apps.lem-webview = {
            type = "app";
            program = lem-webview;
          };
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              rlwrap
              nixfmt
              lem-cli
            ];
          };
        };
    };
}
