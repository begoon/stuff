package main

import (
  "encoding/binary"
  "fmt"
  "math"
  "os"
  "sort"
  "time"
)

type Data []int64

func (s Data) Len() int           { return len(s) }
func (s Data) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s Data) Less(i, j int) bool { return s[i] < s[j] }

func duration(title string, f func()) {
  started := time.Now()
  fmt.Printf("%s", title)
  f()
  fmt.Printf(", Done, %s\n", time.Since(started).String())
}

const (
  sz        = 50000000
  chunks_nb = 4
  chunk_sz  = sz / chunks_nb
)

func main() {
  chunks := make([]Data, chunks_nb)

  f, err := os.Open("trash_for_sort_1.bin")
  if err != nil {
    panic(err)
  }

  duration("Loading", func() {
    for i := range chunks {
      chunks[i] = make(Data, chunk_sz)
      if err := binary.Read(f, binary.BigEndian, &chunks[i]); err != nil {
        panic(err)
      }
    }
  })

  duration("Sorting", func() {
    done := make(chan bool)
    for i := range chunks {
      go func(v Data) {
        sort.Sort(v)
        done <- true
      }(chunks[i])
    }
    for _ = range chunks {
      <-done
    }
  })

  result := make(Data, sz)
  offsets := make([]int, len(chunks))

  duration("Merge", func() {
    for i := range result {
      min_j := -1
      var min int64 = math.MaxInt64
      for j, offset := range offsets {
        if offset < chunk_sz {
          c := chunks[j][offset]
          if c < min {
            min = c
            min_j = j
          }
        }
      }
      offsets[min_j] += 1
      result[i] = min
    }
  })

  if !sort.IsSorted(result) {
    panic("Not sorted!")
  }
}
