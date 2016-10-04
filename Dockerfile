FROM haskell:7.10.3
WORKDIR /opt/unison
RUN apt-get update -q && \
    apt-get install -qy git nodejs nodejs-legacy libcurl4-openssl-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ADD . /opt/unison
RUN stack setup
RUN stack build unison-node
RUN stack --stack-yaml editor.yaml setup
RUN stack --stack-yaml editor.yaml build
RUN ln -s $(stack --stack-yaml editor.yaml path --local-install-root)/bin editor
CMD stack exec node
EXPOSE 8080
