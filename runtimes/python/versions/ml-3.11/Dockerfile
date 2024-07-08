# syntax = devthefuture/dockerfile-x
FROM python:3.11-bookworm

INCLUDE ./base-before

RUN apt-get update && apt-get install -y gcc pkg-config libhdf5-dev

INCLUDE ./python
INCLUDE ./base-after
