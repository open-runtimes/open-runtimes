module openruntimes/server

go 1.22.2

replace openruntimes/handler v0.0.0 => /usr/local/build

require openruntimes/handler v0.0.0
require github.com/open-runtimes/types-for-go/v4 v4.0.0-20240628090809-0af340ef88dd // indirect

// TODO: Release once finished
