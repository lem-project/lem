## Install
### Ubuntu

```shell
$ sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
```

```shell
$ ros -s lem-sdl2 -e '(lem:lem)'
```

### MacOS

```shell
$ brew install sdl2
$ brew install sdl2_image
$ brew install sdl2_ttf
```

```shell
mkdir ~/common-lisp
cd ~/common-lisp
git clone git@github.com:lem-project/cl-sdl2.git
git clone git@github.com:lem-project/cl-sdl2-ttf.git
git clone git@github.com:lem-project/cl-sdl2-image.git
```

```shell
$ ros -s lem-sdl2 -e '(lem:lem)'
```

If your keyboard is a JIS layout, you need to put the following settings in $HOME/.lem/init.lisp

```common-lisp
#+lem-sdl2
(lem-sdl2:set-keyboard-layout :jis)
```
