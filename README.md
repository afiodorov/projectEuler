After losing most of my original solutions I decided to github if I solve
anything. Gives me an incentive to keep my code clean.

## Content:

# 7. 10001st prime.

#### [Problem][p7] | [Solution][s7]

Haskell's show-off example: "Sieve of Eratosthenes"

```shell
time cabal run 7 10001
Preprocessing executable '7' for projecteuler-0.1.0.0...
104743

real    0m5.074s
user    0m4.543s
sys     0m0.503s
```

# 49. Prime permutations

#### [Problem][p49] | [Solution][s49]

This one is written in Go because I am learning go now.

For each prime between 1000 and 10000 generate all permutations, and then
keep permutations that are primes only. Then for all candidate set choose 3 and
check whether smallest + largest = 2 * middle.

# 49 Prime permutations (haskell)

#### [Problem][p49] | [Solution][s49s]

Just to compare it to haskell solution, I am including a 2-liner.

```shel
time cabal run 49
Preprocessing executable '49' for projecteuler-0.1.0.0...
Running 49...
[2969,6299,9629]

real    0m0.974s
user    0m0.750s
sys     0m0.243s
```

# 50. Consecutive prime sum.

#### [Problem][p50] | [Solution][s50]

Very first and naive implementation gave me the right answer, but the time
needs to be improved.

```shell
time cabal run 50 1000000
Preprocessing executable '50' for projecteuler-0.1.0.0...
997651

real    2m54.542s
user    2m54.157s
sys     0m0.350s
```

# 79. Passcode derivation.

#### [Problem][p79] | [Solution][s79]

Solution to 79'th problem without the assumption of non-repeats.
Algorithm: with each successful attempt generate all shortest passwords for
each shortest password generated so far.

```shell
time cabal run 79 ./data/keylog.txt
Preprocessing executable '79' for projecteuler-0.1.0.0...
73162890

real    0m1.245s
user    0m0.910s
sys     0m0.320s
```

# 88. Product-sum numbers.

#### [Problem][p88] | [Solution][s88]

Brute forces decomposition via factorisation and backtracking.

```shell
time cabal run 88
Preprocessing executable '88' for projecteuler-0.1.0.0...
7587457

real    0m8.768s
user    0m8.363s
sys     0m0.390s
```

# 96. Su Doku.

#### [Problem][p96] | [Solution][s96]

Backtracking depth-first algorithm. At each step pick a square with least
number of guesses.

```shell
time cabal run 96 ./data/p096_sudoku.txt
Preprocessing executable '96' for projecteuler-0.1.0.0...
24702

real    0m2.429s
user    0m2.090s
sys     0m0.323s
```

[p7]: https://projecteuler.net/problem=7
[s7]: https://github.com/afiodorov/projectEuler/blob/master/7.hs
[p49]: https://projecteuler.net/problem=49
[s49]: https://github.com/afiodorov/projectEuler/blob/master/49.go
[s49s]: https://github.com/afiodorov/projectEuler/blob/master/49.hs
[p50]: https://projecteuler.net/problem=50
[s50]: https://github.com/afiodorov/projectEuler/blob/master/50.hs
[p79]: https://projecteuler.net/problem=79
[s79]: https://github.com/afiodorov/projectEuler/blob/master/79.hs
[p88]: https://projecteuler.net/problem=88
[s88]: https://github.com/afiodorov/projectEuler/blob/master/88.hs
[p96]: https://projecteuler.net/problem=96
[s96]: https://github.com/afiodorov/projectEuler/blob/master/96.hs

