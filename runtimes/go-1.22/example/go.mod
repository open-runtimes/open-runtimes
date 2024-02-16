module openruntimes/handler

go 1.22.0

// TODO: Dont use local repos for types. Use real github repo
replace openruntimes/types v0.0.0 => /usr/local/server/src/types

require openruntimes/types v0.0.0
require github.com/go-resty/resty/v2 v2.11.0

