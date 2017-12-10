# lem
Lem is the editor/IDE well-tuned for Common Lisp.

After installing lem, you can start developing in Common Lisp at once. 

You can skip over writing tidy settings or installing many plugins as you do on Emacs.

## Screenshot
![Terminal](screenshots/terminal.png)　　

[![Backers on Open Collective](https://opencollective.com/lem/backers/badge.svg)](#backers) [![Sponsors on Open Collective](https://opencollective.com/lem/sponsors/badge.svg)](#sponsors)

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

## Usage

```
$ lem <filename.lisp>
```

You can watch the screencast on Youtube.

[Screencast](https://youtu.be/YkSJ3p7Z9H0)

# Electron version (Beta)
Electron version is in the experimental stage.

Please take the following steps and give it a try!

1. install Node.js in advance.

   [Node.js](https://nodejs.org)


2. install and run lem(Electron) as follows:

```
$ npm install -g cxxxr/lem
$ lem
```

![Electron](screenshots/electron.png)　　


## Plan
- ~To implement the basic vi commands.~

- To implement the frontend using Electron


## Notes

- For Rasberry Pi users: You need to use CCL(Clozure Common Lisp).  
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
