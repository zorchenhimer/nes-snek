package main

import (
	"os"
	"fmt"
	"math/rand"
)

func main() {
	err := run()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func run() error {

	nums := []int{}
	for i := 0; i < 560; i++ {
		nums = append(nums, i)
	}

	coords := []string{}
	for y := 31; y < 184; y += 8 {
		for x := 16; x < 240; x += 8 {
			coords = append(coords, fmt.Sprintf(".byte %d, %d", y, x))
		}
	}
	if len(coords) != 560 {
		return fmt.Errorf("coords wrong length: %d", len(coords))
	}

	rand.Shuffle(560, func(i, j int){
		nums[i], nums[j] = nums[j], nums[i]
		coords[i], coords[j] = coords[j], coords[i]
	})

	file, err := os.Create("rand.inc")
	if err != nil {
		return fmt.Errorf("unable to open rand.inc: %w", err)
	}
	defer file.Close()

	vals := []int{}
	//for y := 0; y < 20; y++ {
	//	for x := 0; x < 28; x++ {
	//		vals = append(vals, ((y+4)*32)+(x+2))
	//	}
	//}
	for i := 0; i < 20*28; i++ {
		vals = append(vals, i)
	}

	if len(vals) != 560 {
		return fmt.Errorf("vals slice wrong length: %d", len(vals))
	}

	for _, n := range nums {
		fmt.Fprintf(file, ".word $%04X\n", 0x400+vals[n])
	}

	cfile, err := os.Create("coords.inc")
	if err != nil {
		return fmt.Errorf("unable to open coords.inc: %w", err)
	}
	defer cfile.Close()

	for _, c := range coords {
		fmt.Fprintln(cfile, c)
	}

	return nil
}
