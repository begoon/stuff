package main

import "fmt"
import "math"
import "flag"

func main() {
    var N int
    flag.IntVar(&N, "N", 100, "")
    flag.Parse()

    fmt.Printf("%d\n", N)

    seive := make([]int, N)

    limit := int(math.Sqrt(float64(N))) + 1

    for i := 2; i < limit; i++ {
        if seive[i] == 0 {
            for j := i * i; j < N; j += i  {
                seive[j] = 1
            }
        }
    }

    count := 0
    for i := 2; i < N; i++ {
        if seive[i] == 0 {
            count++
        }
    }
    fmt.Printf("%d\n", count)
}
