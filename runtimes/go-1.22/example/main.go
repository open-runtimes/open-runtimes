package handler

import (
	"openruntimes/types"
)

func Main(Context types.Context) types.ResponseOutput {
	return Context.Res.Send("Workssssss", 200, nil)
}
