{
  description = "A flake for lem";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/release-25.11";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];
      perSystem = { config, pkgs, ... }:
        let
          lisp = "sbcl";
          sources = import ./_sources/generated.nix {
            inherit (pkgs) fetchgit fetchurl fetchFromGitHub dockerTools;
          };
          micros = pkgs.${lisp}.buildASDFSystem {
            inherit (sources.micros) pname src version;
            systems = [ "micros" ];
          };
          jsonrpc = pkgs.${lisp}.buildASDFSystem {
            inherit (sources.jsonrpc) pname src version;
            systems = [ "jsonrpc" "jsonrpc/transport/stdio" "jsonrpc/transport/tcp" ];
            lispLibs = with pkgs.${lisp}.pkgs; [
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
          async-process = pkgs.${lisp}.buildASDFSystem {
            inherit (sources.async-process) pname src version;
            systems = [ "async-process" ];
            lispLibs = with pkgs.${lisp}.pkgs; [
              cffi
            ];
            nativeLibs = [
              c-async-process
            ];
            nativeBuildInputs = with pkgs; [
              pkg-config
            ];
          };
          lem-mailbox = pkgs.${lisp}.buildASDFSystem {
            inherit (sources.lem-mailbox) pname src version;
            systems = [ "lem-mailbox" ];
            lispLibs = with pkgs.${lisp}.pkgs; [
              bordeaux-threads
              bt-semaphore
	      queues
              queues_dot_simple-cqueue
            ];
          };
          lem-fake-interface = pkgs.${lisp}.buildASDFSystem {
            pname = "lem-fake-interface";
            version = "unstable";
            systems = [ "lem-fake-interface" ];
            src = ./.;
            nativeBuildInputs = with pkgs; [
              makeBinaryWrapper
            ];
            lispLibs = with pkgs.${lisp}.pkgs; [
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
            buildScript = pkgs.writeText "build-lem.lisp" ''
              (defpackage :nix-cl-user (:use :cl))
              (in-package :nix-cl-user)

              ;; Load ASDF
              (load "${lem-fake-interface.asdfFasl}/asdf.${lem-fake-interface.faslExt}")

              ;; Mark this as a Nix build to disable incompatible features
              (pushnew :nix-build *features*)

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
          };
          lem = lem-fake-interface.overrideLispAttrs (o: {
             pname = "lem";
             meta.mainProgram = "lem";
             systems = [ "lem-ncurses" "lem-sdl2" ];
             lispLibs = o.lispLibs ++ (with pkgs.${lisp}.pkgs; [
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
            lem = config.lem;
            sbcl = pkgs.${lisp}.withOverrides (self: super: { lem = config.lem; });
          };
          packages.lem = lem;
          apps.lem = {
            type = "app";
            program = lem;
          };
        };
    };
}
