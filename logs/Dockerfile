# syntax=docker/dockerfile:1
FROM golang:1.25-alpine AS build
WORKDIR /src
COPY go.mod main.go main_test.go ./
RUN go test ./... && CGO_ENABLED=0 go build -trimpath -ldflags="-s -w" -o /logs .

FROM scratch
LABEL maintainer="team@appwrite.io"
LABEL namespace="open-runtimes"
COPY --from=build /logs /logs
USER 65532:65532
EXPOSE 3001
ENTRYPOINT ["/logs"]
