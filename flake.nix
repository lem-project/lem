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
            ];
          };
          lem-cli = lem-fake-interface.overrideLispAttrs (o: {
            pname = "lem-cli";
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
        in
        {
          overlayAttrs = {
            lem = lem-cli;
          };
          packages.lem = lem-cli;
          apps.lem = {
            type = "app";
            program = lem-cli;
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
