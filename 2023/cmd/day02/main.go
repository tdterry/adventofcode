package main

import (
	"github.com/davecgh/go-spew/spew"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var lineRegexp = regexp.MustCompile(`^Game (\d+): (.+)$`)
var cubeRegexp = regexp.MustCompile(`^(\d+) (\w+)$`)

func main() {
	s, _ := os.ReadFile("input02.txt")
	lines := strings.Split(string(s), "\n")

	total := 0
	totalPower := 0
	for _, line := range lines {
		if line == "" {
			continue
		}
		println(line)
		matches := lineRegexp.FindStringSubmatch(line)

		gameNum, _ := strconv.Atoi(matches[1])
		hands := strings.Split(matches[2], "; ")
		spew.Dump(gameNum, hands)

		good := true
		minRed := 0
		minBlue := 0
		minGreen := 0
		for _, hand := range hands {
			cubes := strings.Split(hand, ", ")

			for _, cube := range cubes {
				matches := cubeRegexp.FindStringSubmatch(cube)
				spew.Dump(matches)

				count, _ := strconv.Atoi(matches[1])
				color := matches[2]

				switch color {
				case "red":
					if count > minRed {
						minRed = count
					}
					if count > 12 {
						good = false
					}
				case "blue":
					if count > minBlue {
						minBlue = count
					}
					if count > 14 {
						good = false
					}
				case "green":
					if count > minGreen {
						minGreen = count
					}
					if count > 13 {
						good = false
					}
				default:
					good = false
				}
			}
		}

		println(good, minRed*minBlue*minGreen)
		totalPower += minRed * minBlue * minGreen
		if good {
			total += gameNum
		}
	}
	println(total, totalPower)

}
