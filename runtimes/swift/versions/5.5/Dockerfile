# syntax = devthefuture/dockerfile-x
FROM swiftarm/swift:5.5.3-ubuntu-jammy

# Temporray fix until archive.swiftlang.xyz has fix
RUN rm /etc/apt/sources.list.d/swiftlang.list

INCLUDE ./base-before
INCLUDE ./swift
INCLUDE ./base-after
