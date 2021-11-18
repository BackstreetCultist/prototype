FROM debian:latest
RUN apt-get update && apt-get install -y \ 
    curl \
    git \
    default-jdk \
    g++ \
    cabal-install \
    dos2unix
RUN cabal update && cabal install --lib random
WORKDIR /prototype
COPY . .
RUN find . -type f | xargs dos2unix
