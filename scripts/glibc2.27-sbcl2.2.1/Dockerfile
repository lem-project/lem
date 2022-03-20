FROM ubuntu:18.04
RUN apt-get update && apt-get install -y \
    build-essential git automake libcurl4-openssl-dev curl gcc-multilib lib32z1-dev libncurses5-dev \
    dh-autoreconf bash-completion \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
run ln -s /usr/include/x86_64-linux-gnu/zconf.h /usr/include; \
    ln -s /bin/echo /bin/run-prove
run env CI=true LISP=sbcl-bin/2.2.1 bash -c 'curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh'
run cd /;find . |grep ros

