# syntax = devthefuture/dockerfile-x:1.4.2
FROM python:3.12.6-bookworm

RUN apt-get update && apt-get install -y gcc pkg-config libhdf5-dev

INCLUDE ./base-before
INCLUDE ./python
INCLUDE ./base-after
