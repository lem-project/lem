# Lem is the editor/IDE well-tuned for Common Lisp.

![](https://github.com/Shinmera/lem-icon/blob/gh-pages/icon-blue.svg)

[![Build Status](https://travis-ci.org/cxxxr/lem.svg?branch=master)](https://travis-ci.org/cxxxr/lem)
[![Backers on Open Collective](https://opencollective.com/lem/backers/badge.svg)](#backers) [![Sponsors on Open Collective](https://opencollective.com/lem/sponsors/badge.svg)](#sponsors)

After installing lem, you can start developing in Common Lisp at once. 

You can skip over writing tidy settings or installing many plugins as you do on Emacs.

## Screenshot
![Terminal](screenshots/terminal.png)　　

## Requirement
- [roswell](https://github.com/roswell/roswell)
- ncurses

## Platform
- Linux
- MacOS 10.13+ 

## Installation
Please install roswell at first.

[Roswell Installation Guide](https://github.com/roswell/roswell/wiki/Installation)

After that, please follow the steps bellow.

```
1. install lem by roswell
$ ros install cxxxr/lem

2. add the PATH in the initialization file(such as ~/.bashrc)
export PATH=$PATH:~/.roswell/bin
```

## Updating

```
$ ros update lem
```

## Usage

```
$ lem <filename.lisp>
```

You can watch the screencast on Youtube.

[Screencast](https://youtu.be/YkSJ3p7Z9H0)

## Configuration

Lem loads `~/.lem/init.lisp` when starting up.

You can see an example at [here](lemrc-example).

fukamachi also published his init files on GitHub.
https://github.com/fukamachi/.lem

## Electron version (Experiment)
Electron version is in the experimental stage.

If you try, please follow below steps.

```
$ node -v  # check if your nodejs is 8.9.3LTS+.
v8.9.3
$ npm -v
5.5.1
$ npm install -g cxxxr/lem   # install lem-electron
$ lem-electron               # Electron version will start
```

![Electron](screenshots/electron.png)　　

## Lem on platforms without SBCL thread support (e.g. raspberry pi3)

If installation fails due to `bordeaux-threads`, there is no thread provided by SBCL on your PC.  
You need to use Clozure Common Lisp (CCL) instead of SBCL.  

```
$ ros install ccl-bin
$ ros use ccl-bin
$ ros install cxxxr/lem
```

And also need to specify frontend to `ncurses-ccl`.
You can launch lem as follows:

```
$ lem --frontend ncurses-ccl
```

## How to develop lisp
See https://github.com/cxxxr/lem/wiki/Lisp-Mode

## How to hack on lem itself
see https://github.com/cxxxr/lem/wiki/How-to-hack-on-lem-itself

## Contributors

This project exists thanks to all the people who contribute. [[Contribute]](CONTRIBUTING.md).
<a href="graphs/contributors"><img src="https://opencollective.com/lem/contributors.svg?width=890" /></a>

## Backers

Thank you to all our backers! 🙏 [[Become a backer](https://opencollective.com/lem#backer)]

<a href="https://opencollective.com/lem#backers" target="_blank"><img src="https://opencollective.com/lem/backers.svg?width=890"></a>


## Sponsors

Support this project by becoming a sponsor. Your logo will show up here with a link to your website. [[Become a sponsor](https://opencollective.com/lem#sponsor)]

<a href="https://opencollective.com/lem/sponsor/0/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/0/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/1/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/1/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/2/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/2/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/3/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/3/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/4/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/4/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/5/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/5/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/6/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/6/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/7/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/7/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/8/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/8/avatar.svg"></a>
<a href="https://opencollective.com/lem/sponsor/9/website" target="_blank"><img src="https://opencollective.com/lem/sponsor/9/avatar.svg"></a>



# License
[MIT](https://github.com/cxxxr/lem/blob/master/LICENCE)
