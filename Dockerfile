FROM alpine:latest

WORKDIR /app

COPY . .

RUN apk add --no-cache curl bash build-base ncurses-dev sbcl git

RUN curl -L https://qlot.tech/installer | bash

RUN qlot install && \
    qlot exec sbcl --noinform --load scripts/build-ncurses.lisp

ENTRYPOINT qlot exec sbcl --noinform --eval "(ql:quickload :lem-ncurses)" --eval "(lem:lem)" --quit
