#!/usr/bin/env python3

# to test Alan we transcribe it (by hand) to Python by making as little changes as possible

from alanUtils import *


def knapsack(n: int, w_max: int, w: list[int], v: list[int]) -> int:
    dp: list[int] = [0] * 10000
    i: int
    w_c: int

    w_c = 0
    while w_c <= w_max:
        dp[w_c] = 0
        w_c = w_c + 1
    i = 0
    while i < n:
        w_c = w_max
        while w_c > 0:
            if w[i] <= w_c and v[i] + dp[w_c - w[i]] > dp[w_c]:
                dp[w_c] = v[i] + dp[w_c - w[i]]
            w_c = w_c - 1
        i = i + 1
    return dp[w_max]


def main() -> None:
    w: list[int] = [0] * 10000
    v: list[int] = [0] * 10000
    n: int
    w_max: int
    res: int

    n = readInteger()
    w_max = readInteger()

    if n <= 0:
        return

    i = 0
    while i < n:
        w[i] = readInteger()
        i = i + 1

    i = 0
    while i < n:
        v[i] = readInteger()
        i = i + 1

    res = knapsack(n, w_max, w, v)

    writeInteger(res)
    writeChar("\n")


if __name__ == "__main__":
    main()
