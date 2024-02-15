package handler

import "fmt"
import "time"
import (
    tinytime "github.com/wagslane/go-tinytime"
)

func Main() {
	tt := tinytime.New(1585750374)
    tt = tt.Add(time.Hour * 48)
    fmt.Println(tt)
}