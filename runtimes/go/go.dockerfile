ENV OPEN_RUNTIMES_ENTRYPOINT=main.go
RUN apk add git
COPY ./helpers ./helpers
