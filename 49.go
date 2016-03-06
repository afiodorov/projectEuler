package main

import (
	"fmt"
	"math"
	"sort"
)

func isPrime(x int) bool {
	if x == 2 {
		return true
	}

	if x%2 == 0 {
		return false
	}

	for i := 3; float64(i) <= math.Sqrt(float64(x)); i = i + 2 {
		if x%i == 0 {
			return false
		}
	}

	return true
}

func primesBetween(a, b int) []int {
	var primes []int
	for i := a; i < b; i = i + 2 {
		if isPrime(i) {
			primes = append(primes, i)
		}
	}

	return primes
}

func aToSlice(a int) []int {
	var slice []int
	for i := a; i > 0; i = i / 10 {
		slice = append([]int{i % 10}, slice...)
	}

	return slice
}

func sliceToA(as []int) int {
	a := as[0]
	for i := 1; i < len(as); i++ {
		a *= 10
		a += as[i]
	}
	return a
}

func intersperse(x int, xs int) []int {
	var newInts []int

	slice := aToSlice(xs)
	for i := 0; i <= len(slice); i++ {
		newSlice := make([]int, len(slice)+1)
		copy(newSlice[:i], slice[:i])
		copy(newSlice[i+1:], slice[i:])
		newSlice[i] = x
		newInt := sliceToA(newSlice)
		newInts = append(newInts, newInt)
	}

	return newInts
}

func permutations(x int) []int {
	if x < 10 {
		return []int{x}
	}

	slice := aToSlice(x)

	var result []int
	newSlice := make([]int, len(slice)-1)
	copy(newSlice, slice[1:])
	newInt := sliceToA(newSlice)
	for _, permutation := range permutations(newInt) {
		result = append(result, intersperse(slice[0], permutation)...)
	}

	return result
}

func inSlice(a int, slice []int) bool {
	for _, val := range slice {
		if val == a {
			return true
		}
	}
	return false
}

func candidates(a, b int) [][]int {
	var result [][]int
	primes := primesBetween(a, b)
	for i, prime := range primes {
		primePerms := []int{prime}
		for _, perm := range permutations(prime) {
			if !inSlice(perm, primePerms) && inSlice(perm, primes[i+1:]) {
				primePerms = append(primePerms, perm)
			}
		}
		if len(primePerms) >= 3 {
			sort.Ints(primePerms)
			result = append(result, primePerms)
		}
	}

	return result
}

func findSuitableTripple(x, y int) string {
	var a, b, c int
	for _, candidate := range candidates(x, y) {
		for i := 0; i < len(candidate)-2; i++ {
			for j := i + 1; j < len(candidate)-1; j++ {
				for k := j + 1; k < len(candidate); k++ {
					if candidate[i]+candidate[k] == 2*candidate[j] {
						a = candidate[i]
						b = candidate[j]
						c = candidate[k]
					}
				}
			}
		}
	}

	return fmt.Sprintf("%d%d%d", a, b, c)
}

func main() {
	fmt.Println(findSuitableTripple(1001, 10000))
}
