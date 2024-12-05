package main

import (
	"math"
	"os"
	"strings"
)

var weights = map[string]int{
	"1":     1,
	"2":     2,
	"3":     3,
	"4":     4,
	"5":     5,
	"6":     6,
	"7":     7,
	"8":     8,
	"9":     9,
	"0":     0,
	"one":   1,
	"two":   2,
	"three": 3,
	"four":  4,
	"five":  5,
	"six":   6,
	"seven": 7,
	"eight": 8,
	"nine":  9,
	"zero":  0,
}

func main() {
	s, _ := os.ReadFile("input01.txt")
	lines := strings.Split(string(s), "\n")
	total := 0
	for _, line := range lines {
		first := 0
		last := 0
		for _, c := range line {
			if c >= '0' && c <= '9' {
				if first == 0 {
					first = int(c - '0')
				}
				last = int(c - '0')
			}
		}

		total += first*10 + last
	}
	println(total)

	total = 0
	for _, line := range lines {
		first := 0
		last := 0
		firstPosition := math.MaxInt
		lastPosition := math.MinInt
		for k, v := range weights {
			i := strings.Index(line, k)
			if i != -1 {
				if i < firstPosition {
					firstPosition = i
					first = v
				}
			}

			i = strings.LastIndex(line, k)
			if i != -1 {
				if i > lastPosition {
					lastPosition = i
					last = v
				}
			}
		}
		println(line, first, last, first*10+last)

		total += first*10 + last
	}
	println(total)
}
