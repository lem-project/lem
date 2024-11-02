{
  description = "lem";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  # cribbing a lot from https://github.com/dariof4/lem-flake
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      perSystem = { self', pkgs, lib, system, ... }:
        let
          cl-charms = pkgs.sbclPackages.cl-charms.overrideLispAttrs
            (oldAttrs: { nativeLibs = [ pkgs.ncurses ]; });
          jsonrpc = pkgs.sbclPackages.jsonrpc.overrideLispAttrs (oldAttrs: {
            src = pkgs.fetchFromGitHub {
              owner = "cxxxr";
              repo = "jsonrpc";
              rev = "2af1e0fad429ee8c706b86c4a853248cdd1be933";
              hash = "sha256-N3j9eFS+jj390cjYltRCq9HyyTNUIukAJEzTqR0opU0=";
            };
            systems =
              [ "jsonrpc" "jsonrpc/transport/stdio" "jsonrpc/transport/tcp" ];
            lispLibs = with pkgs.sbclPackages;
              oldAttrs.lispLibs ++ [ cl_plus_ssl quri fast-io trivial-utf-8 ];
          });
          queues = pkgs.sbclPackages.queues.overrideLispAttrs (oldAttrs: {
            systems = [
              "queues"
              "queues.priority-cqueue"
              "queues.priority-queue"
              "queues.simple-cqueue"
              "queues.simple-queue"
            ];
            lispLibs = oldAttrs.lispLibs
              ++ (with pkgs.sbclPackages; [ bordeaux-threads ]);
          });
          micros = pkgs.sbcl.buildASDFSystem {
            pname = "micros";
            version = "unstable-2024-10-01";
            src = pkgs.fetchFromGitHub {
              owner = "lem-project";
              repo = "micros";
              rev = "af94fe5d6688f67a092f604765fb706ebae44e99";
              hash = "sha256-XmKTMJy+8xt2ImlGXSyXdXsLOUFFB0W45ROD4OIvyPY=";
            };
          };
          lem-mailbox = pkgs.sbcl.buildASDFSystem {
            pname = "lem-mailbox";
            version = "unstable-2023-09-10";
            src = pkgs.fetchFromGitHub {
              owner = "lem-project";
              repo = "lem-mailbox";
              rev = "12d629541da440fadf771b0225a051ae65fa342a";
              hash = "sha256-hb6GSWA7vUuvSSPSmfZ80aBuvSVyg74qveoCPRP2CeI=";
            };
            lispLibs = with pkgs.sbcl.pkgs; [
              bordeaux-threads
              bt-semaphore
              queues
            ];
          };
          sdl2 = pkgs.sbclPackages.sdl2.overrideLispAttrs (oldAttrs: {
            src = pkgs.fetchFromGitHub {
              owner = "lem-project";
              repo = "cl-sdl2";
              rev = "24dd7f238f99065b0ae35266b71cce7783e89fa7";
              hash = "sha256-ewMDcM3byCIprCvluEPgHD4hLv3tnUV8fjqOkVrFZSE=";
            };
            lispLibs = oldAttrs.lispLibs
              ++ lib.optional pkgs.stdenv.isDarwin pkgs.sbclPackages.cl-glut;
          });
          sdl2-ttf = pkgs.sbclPackages.sdl2-ttf.overrideLispAttrs (oldAttrs: {
            src = pkgs.fetchFromGitHub {
              owner = "lem-project";
              repo = "cl-sdl2-ttf";
              rev = "f43344efe89cf9ce509e6ce4f7303ebb2ff14434";
              hash = "sha256-1b0SMUipVaLq7WdDgaR9ZZhs0/c1/wyRkULsrBfTvEU=";
            };
            lispLibs = [
              pkgs.sbclPackages.alexandria
              pkgs.sbclPackages.defpackage-plus
              pkgs.sbclPackages.cl-autowrap
              pkgs.sbclPackages.cffi-libffi
              pkgs.sbclPackages.trivial-garbage
              sdl2
            ];
          });
          sdl2-image = pkgs.sbclPackages.sdl2-image.overrideLispAttrs
            (oldAttrs: {
              src = pkgs.fetchFromGitHub {
                owner = "lem-project";
                repo = "cl-sdl2-image";
                rev = "8734b0e24de9ca390c9f763d9d7cd501546d17d4";
                hash = "sha256-TNcPOBKlB5eTlHtDAW/hpkWDMZZ/sFCHnm7dapMm5lg=";
              };
              lispLibs = [
                pkgs.sbclPackages.alexandria
                pkgs.sbclPackages.defpackage-plus
                pkgs.sbclPackages.cl-autowrap
                sdl2
              ];
            });
          async-process = pkgs.sbclPackages.async-process.overrideLispAttrs
            (oldAttrs: {
              pname = "async-process";
              version = "unstable-20241027";
              src = pkgs.fetchFromGitHub {
                owner = "lem-project";
                repo = "async-process";
                rev = "3b16b91d417530dac03559980fb5703206e20c55";
                hash = "sha256-5J3+gc7r/LhrKPXeHGwfghKaXB+AoaXhjS8b4lida3o=";
              };
            });
          lem = pkgs.sbcl.buildASDFSystem {
            pname = "lem";
            version = "unstable";
            src = ./.;
            systems = [ "lem" "lem/extensions" ];
            lispLibs = [ async-process jsonrpc lem-mailbox micros ]
              ++ (with pkgs.sbcl.pkgs; [ # for lem
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
                dexador
                iterate
                closer-mop
                trivia
                str
                parse-number
                trivial-clipboard
                cl-setlocale
                cl-package-locks
                trivial-utf-8
                async-process
                cl-change-case
                swank
                esrap
                bt-semaphore
              ]) ++ (with pkgs.sbcl.pkgs; [ # for lem/extensions
                _3bmd
                _3bmd-ext-code-blocks
                lisp-preprocessor
                trivial-ws
                trivial-open-browser
              ]);
          };
          lem-exec = lem.overrideLispAttrs (oldAttrs: {
            nativeBuildInputs = [ pkgs.openssl pkgs.makeBinaryWrapper ];
            buildScript = pkgs.writeText "build-lem.lisp" ''
              (load (concatenate 'string (sb-ext:posix-getenv "asdfFasl") "/asdf.fasl"))
              (dolist (s (uiop:split-string (uiop:getenv "systems") :separator " "))
                 (asdf:load-system s))
              (sb-ext:save-lisp-and-die
                "lem"
                :executable t
                :purify t
                #+sb-core-compression :compression
                #+sb-core-compression t
                :toplevel #'lem:main)
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp -v lem $out/bin
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH" \
                --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH" \
            '';
          });
        in {
          packages.lem-ncurses = lem-exec.overrideLispAttrs (oldAttrs: {
            systems = [ "lem-ncurses" ];
            lispLibs = oldAttrs.lispLibs ++ [ cl-charms ];
          });
          packages.lem-sdl2 = lem-exec.overrideLispAttrs (oldAttrs: {
            systems = [ "lem-sdl2" ];
            lispLibs = oldAttrs.lispLibs ++ [
              sdl2
              sdl2-ttf
              sdl2-image
              pkgs.sbclPackages.trivial-main-thread
            ];
            nativeLibs = [ pkgs.SDL2 pkgs.SDL2_ttf pkgs.SDL2_image ];
          });

          packages.default = self'.packages.lem-ncurses;
        };
    };
}
