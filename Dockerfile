FROM haskell:7.10.3
WORKDIR /opt/unison
RUN apt-get update -q && \
    apt-get install -qy git nodejs nodejs-legacy && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ADD stack.yaml /opt/unison/stack.yaml
RUN stack setup
ADD editor/stack.yaml /opt/unison/editor/stack.yaml
RUN cd editor && stack setup
ADD shared/unison-shared.cabal /opt/unison/shared/unison-shared.cabal
ADD node/unison-node.cabal /opt/unison/node/unison-node.cabal
RUN stack build --only-dependencies
ADD editor/unison-editor.cabal /opt/unison/editor/unison-editor.cabal
RUN cd editor && stack build --only-dependencies
ADD . /opt/unison/
RUN stack build unison-node
RUN cd editor && stack build && ./make-editor-html
CMD stack exec node
EXPOSE 8080
