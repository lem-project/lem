FROM docker.io/fukamachi/qlot

COPY . .

RUN apt-get update && apt-get install gcc libncurses-dev -y

RUN qlot install

RUN qlot exec sbcl --noinform --load scripts/build-ncurses.lisp

ENTRYPOINT qlot exec sbcl --noinform --eval "(ql:quickload :lem-ncurses)" --eval "(lem:lem)" --quit
