FROM ubuntu:22.04 AS runner
USER pivot
COPY ./target/release/plc /usr/bin/plc
COPY ./planglib /home/pivot/pl/planglib
COPY ./target/release/libvm.a /home/pivot/pl/libvm.a
RUN apt update
RUN apt install -y git libxml2 build-essential libunwind-dev
COPY ./test /home/pivot/pltest
ENV KAGARI_LIB_ROOT=/home/pivot/pl/planglib
ENV PL_ROOT=/home/pivot/pl
WORKDIR /home/pivot/
