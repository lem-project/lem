# ChangeLog

## [Unreleased]

(processed upto 7fb9c99d9f465efa31de4fcb1fb7615f937de541)

### Added

* An open collective campaign to support Lem financially!
* Prompt for directory creation when it does not exist.

  When openning a file in an non-existing directory `find-file`
  and `read-file` ask the user if the directory should be
  created before opening the file, aborting when the user
  responds 'n'.
* If the user types a wrong buffer name, `select-buffer` now asks
  whether the user wants to create it.
* Added a function `lem:indent-buffer`.
* Now it is possible to run a prebuilt Lem in a Docker container, like this:

  ```
  docker run --rm -ti -v `pwd`:/app 40ants/lem:latest
  ```

* Added ability to output infmation into the log.

  Two new options were added to control if log should be written to the file:

  * `--log-filename /tmp/lem.log` - this will output `INFO`, `WARNING`
    and `ERROR` messages to the file.
  * `--debug` - with this flag Lem will output also `DEBUG` messages to the log.
    This flag should be used along with `--log-filename`.
* Added `directory-does-not-exist` condition.
  
#### scala-mode

* A `scala-mode` was added.

#### dart-mode

* Added highlighting for strings, builtin functions, constants, keywords and
  line comments.
  
#### vi-mode

* `Return` was added to visual mode.
* Now `*` searches symbol at point.
* Added support for search and replace using `%s///`.

#### scheme-mode

* Added error check to scheme slime function.
* Added `*use-scheme-process*` setting.
* Add loaded message for scheme-load-file.
* A scheme slime function to connect to `r7rs-swank`

#### js-mode

* Added an `eslint` command.
* Added a `prettier` to process whole buffer through [js prettier](https://prettier.io/) tool.

#### paredit

* Command `paredit-wrap` was added. It is bound to a `M-(` by default.

### Changed

* `escape-delay` was made configurable.
* Refactored a number of functions:

  - `shortest-wait-timers` refactored and renamed to `get-next-timer-timing-ms`
  - `update-timer`
  - `read-key-1`

* All idle timers now kept in the `*processed-idle-timer-list*` list.
* Added different minibuffer classes:

  * `sticky-minibuffer-window`;
  * `popup-minibuffer-window` (is not fully supported yet).
  
  Both of them are inherited from a `floating-window` class.
  Function `make-minibuffer-window` creates a `sticky-minibuffer-window` object.

#### yaml-mode

* Now `yaml-mode` is autoenabled for `.yml` extension as well as for `.yaml`.

#### diff-mode

* Now `cl-ansi-text` system is used for coloring.
  
#### lisp-mode

* Changed swank protocol read message function to make it more reliable.
  

### Fixed

* String slurping in paredit-mode.
* Completion for filenames inside `"~"`.

  On SBCL Linux, `(pathname-directory "~/")` returns `(:absolute :home)`,
  which `completion-file` don't handle properly.
* A number of typos.
* Choosing an unique name for a buffer when file was saved by `write-file` command.
* Function `kill-ring-rotate` when `*kill-ring*` is empty.
* Command `revert-buffer` now keeps position of the cursor.

#### scheme-mode

* Fixed scheme-eval-region for scheme process.

#### lem-pdcurses

* Resolve compiler warnings, etc.
* Fixed escape key input delaying.

#### nim-mode

* Nim multiline comments were fixed.

### lisp-mode

* Fixed an issue when connection to swank didn't initialized propertly.

  The root of the problem was that on OSX swank:*communication-style* is equal to :fd-handler
  for some reason. In this case swank:create-server does not start threads which
  accept connections on TCP port and process incoming messages.

  
### Thanks to

* Jéssica Milaré
* gos-k
* Ken OKADA
* cxxxr
* Hamayama
* FemtoEmacs
* Talen Bartlett

## [1.6] - 2019-08-29

This version and all previous are not covered by this changelog (yet?).
