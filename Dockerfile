FROM debian:latest
RUN apt-get update && apt-get install -y \ 
    curl \
    default-jdk \
    g++ \
    cabal-install
RUN cabal update && cabal install --lib random
WORKDIR /app
COPY . .
