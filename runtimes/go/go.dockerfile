RUN apk update && apk add bash

ENV OPEN_RUNTIMES_ENTRYPOINT=main.go
COPY ./helpers ./helpers
