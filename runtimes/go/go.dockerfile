ENV OPEN_RUNTIMES_ENTRYPOINT=main.go

# TODO: Eventuelly do this globally for all runtimes
RUN apk add --update git supervisor && rm -rf /tmp/* /var/cache/apk/*
COPY ./supervisord.conf /etc/supervisord.conf

COPY ./helpers ./helpers
