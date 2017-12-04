# lem : Common Lisp editor/IDE with high expansibility

## Screenshot
Terminal(default)

![Terminal](screenshots/terminal.png)　　


Electron version(beta, in progress)
![Electron](screenshots/electron.png)　　

[![Backers on Open Collective](https://opencollective.com/lem/backers/badge.svg)](#backers) [![Sponsors on Open Collective](https://opencollective.com/lem/sponsors/badge.svg)](#sponsors)

## What is lem?
Lem is the editor for Common Lispers.

Lem makes it possible to develop in Common Lisp the moment you've installed it. 

You don't need any tidy setting or install any other plugins.

## Vision
- To be able to extend itself in Common Lisp
- To combine Common Lisp with Web ecosystem

## Requirement
- [roswell](https://github.com/roswell/roswell)
- ncurses

## Platform
- Linux
- MacOS 10.13+ 

## Installation

```
# Terminal version (default)
$ ros install cxxxr/lem

# Electron version (beta)
Please install Node.js and Electron in advance.
$ ros install cxxxr/lem
$ cd ~/.roswell/local-projects/cxxxr/lem/lem-frontend-electron
$ npm install
$ electron .
```

## Usage
If you add the PATH, you can start using lem at once!
```
$ lem <filename.lisp>
```
You can also start lem on roswell's REPL.
```
$ ros run
CL-USER> (ql:quickload :lem)
CL-USER> (lem:lem)
```
You can watch the screencast on Youtube.

[Screencast](https://youtu.be/YkSJ3p7Z9H0)


## Notes
- If you use Rasberry Pi, you need to use CCL(Clozure Common Lisp).  
Please change the CL implementation to CCL as follow:
```
$ ros use ccl-bin
```

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
