package main

import (
  "os"
  "fmt"
)

func main() {
  if e := os.Symlink("symlink.go", "symlink-link.go"); e != nil {
    fmt.Printf("%v\n", e)
  }
}
