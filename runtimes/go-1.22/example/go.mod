module openruntimes/handler

go 1.22.0

// TODO: Dont use local repos for types. Use real github repo
replace openruntimes/types v0.0.0 => /usr/local/server/src/types

require openruntimes/types v0.0.0
