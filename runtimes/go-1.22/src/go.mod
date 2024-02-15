module openruntimes/server

go 1.22.0

replace openruntimes/handler v0.0.0 => /usr/local/build
replace openruntimes/types v0.0.0 => /usr/local/server/src/types

require openruntimes/handler v0.0.0
require openruntimes/types v0.0.0