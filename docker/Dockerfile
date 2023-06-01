FROM utdemir/ghc-musl:v24-ghc8107 as unisonbuild

RUN ghcup upgrade
RUN ghcup install stack 
RUN stack config set install-ghc false --global
RUN git clone https://github.com/unisonweb/unison.git /opt/unison
RUN cd /opt/unison && stack install --system-ghc

ADD https://github.com/unisonweb/unison-local-ui/releases/download/latest/unisonLocal.zip /tmp/unisonLocal.zip
RUN unzip -d /srv/unison-ui /tmp/unisonLocal.zip

FROM alpine:3

RUN apk add gmp

COPY --from=unisonbuild /root/.local/bin/unison /usr/local/bin/unison
COPY --from=unisonbuild /srv/unison-ui /srv/unison-ui

ENV INSIDE_EMACS=1

ENV LANG=en_US.UTF-8
ENV UCM_WEB_UI=/srv/unison-ui
ENV UCM_PORT=8080
ENV UCM_HOST=0.0.0.0
ENV UCM_TOKEN=local

EXPOSE 8080 5757

RUN mkdir /codebase
WORKDIR /codebase
ENTRYPOINT ["/usr/local/bin/unison"]

CMD ["+RTS", "-qg2", "-N", "-RTS", "-c", "/codebase"]
