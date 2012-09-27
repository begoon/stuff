package main

import "fmt"
import "math"
import "flag"

func main() {
    var N int
    flag.IntVar(&N, "N", 100, "")
    flag.Parse()

    fmt.Printf("%d\n", N)

    seive := make([]bool, N)

    limit := int(math.Sqrt(float64(N))) + 1

    for i := 2; i < limit; i++ {
        if !seive[i] {
            for j := i * i; j < N; j += i  {
                seive[j] = true
            }
        }
    }

    count := 0
    for i := 2; i < N; i++ {
        if !seive[i] {
            count++
        }
    }
    fmt.Printf("%d\n", count)
}
