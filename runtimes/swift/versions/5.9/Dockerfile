# syntax = devthefuture/dockerfile-x
FROM swiftarm/swift:5.9.2-ubuntu-22.04

# Temporray fix until archive.swiftlang.xyz has fix
RUN rm /etc/apt/sources.list.d/swiftlang.list

INCLUDE ./base-before
INCLUDE ./swift
INCLUDE ./base-after
