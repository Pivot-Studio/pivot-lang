FROM ubuntu:22.04 AS runner
RUN apt update
RUN apt install -y git libxml2 build-essential libunwind-dev
RUN useradd -ms /bin/bash pivot && echo "pivot:pivot" | chpasswd
RUN mkdir -p /home/pivot && chown -R pivot:pivot /home/pivot
WORKDIR /home/pivot
USER pivot
COPY ./target/release/plc /usr/bin/plc
COPY ./planglib /home/pivot/pl/planglib
COPY ./target/release/libvm.a /home/pivot/pl/libvm.a
COPY ./test /home/pivot/pltest
ENV KAGARI_LIB_ROOT=/home/pivot/pl/planglib
ENV PL_ROOT=/home/pivot/pl
