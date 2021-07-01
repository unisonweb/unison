# How to use unison in a container?
# Benefits: No need to install any haskell related dependencies. All you need is podman/docker installed and git installed if you are using the dockerfile source and not from a registry.
# Container tool used: Podman
  # Podman is a container tool that has nearly the same CLI interface as Docker with few exceptions. You do not need to run a daemon.
# 1. Build
  # Example podman command: 
  # sudo podman build \
  # --file build-with-stack.dockerfile \
  # --build-arg USER_NAME=<1> \
  # --build-arg USER_EMAIL=<2> \
  # --build-arg UCM_PORT=<3> \
  # --build-arg UCM_HOST=<4> \
  # --build-arg UCM_TOKEN=<5> \
  # -t unison 
# 2. Run
  # Example podman command:
  # sudo podman run -it --rm unison
  # The command above will block your terminal. You can either detach Ctrl + P and Ctrl + Q or consult docker documentation or open another terminal and exec into the container to develop *.u files.
# 3. Develop
  # Example podman command:
  # sudo podman exec -it <container name> /bin/bash
  # After that, you can do your development and UCM will be running. You can attach an additional terminal to see program executions.
# 4. Interact with source control
  # You have some options:
    # You can either copy in your git server ssh key into the container and commit/push from there or you could copy the source out of the container and interact with source control on your host machine. 
FROM docker.io/archlinux:latest 
ARG USER_NAME 
ARG USER_EMAIL 
ARG UCM_PORT
ARG UCM_HOST
ARG UCM_TOKEN
RUN [ -z "$USER_NAME" ] && echo "USER_NAME is required" && exit 1 || true
RUN [ -z "$USER_EMAIL" ] && echo "USER_EMAIL is required" && exit 1 || true
RUN [ -z "$UCM_PORT" ] && echo "UCM_PORT is required" && exit 1 || true
RUN [ -z "$UCM_HOST" ] && echo "UCM_HOST is required" && exit 1 || true
RUN [ -z "$UCM_TOKEN" ] && echo "UCM_TOKEN is required" && exit 1 || true
ENV UCM_PORT="$UCM_PORT"
ENV UCM_HOST="$UCM_HOST"
ENV UCM_TOKEN="$UCM_TOKEN"
RUN pacman -Syu vim gcc make git stack --noconfirm
RUN git config --global --add user.name "$USER_NAME"
RUN git config --global --add user.email "$USER_EMAIL"
RUN stack install ghc-8.10.4
RUN mkdir unison
WORKDIR unison
COPY weeder.dhall weeder.dhall
COPY stack.yaml stack.yaml
COPY sql sql
COPY hie.yaml hei.yaml
COPY development.markdown development.markdown
COPY dev-ui-install.sh dev-ui-install.sh 
COPY config config
COPY README.md README.md
COPY LICENSE LICENSE
COPY CREDITS.md CREDITS.md
COPY CONTRIBUTORS.markdown CONTRIBUTORS.markdown
COPY .mergify.yml .mergify.yml
COPY .mailmap .mailmap
COPY yaks yaks
COPY unison-src unison-src 
COPY unison-core unison-core 
COPY scripts scripts
COPY parser-typechecker parser-typechecker
COPY editor-support editor-support
COPY docs docs
COPY deps deps
COPY contrib contrib
COPY codebase2 codebase2
RUN stack build 
RUN stack exec tests
WORKDIR /
RUN $(find /unison/.stack-work -name unison) init
RUN echo "\$(find /unison/.stack-work -name unison)" > unison.sh
EXPOSE $UCM_PORT
CMD ["/bin/sh", "unison.sh"]
