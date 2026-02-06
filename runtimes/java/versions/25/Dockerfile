# syntax = devthefuture/dockerfile-x:1.4.2
FROM eclipse-temurin:25-jdk-jammy

RUN apt update && apt install curl -y

INCLUDE ./base-before
INCLUDE ./java
INCLUDE ./base-after
