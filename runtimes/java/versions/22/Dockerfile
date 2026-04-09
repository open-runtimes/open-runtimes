# syntax = devthefuture/dockerfile-x:1.4.2
FROM ghcr.io/graalvm/native-image-community:22-ol9

RUN microdnf install -y curl

INCLUDE ./base-before
INCLUDE ./java
INCLUDE ./base-after
