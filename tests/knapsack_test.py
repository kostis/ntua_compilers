#!/usr/bin/env python3

import random
import subprocess
import functools
import sys


if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} [filename of program to test]")
    exit(1)


def knapsack(n: int, w_max: int, w: list[int], v: list[int]) -> int:
    @functools.cache
    def dp(i: int, w_c: int) -> int:
        if i < 0 or w_c == 0:
            return 0

        if w_c >= w[i]:
            return max(dp(i - 1, w_c), v[i] + dp(i - 1, w_c - w[i]))
        else:
            return dp(i - 1, w_c)

    return dp(n - 1, w_max)


N = 10
SIZE = 100
MAX_W = 100
MAX_V = 100

random.seed(42)

for _ in range(N):
    cur_size = random.randint(SIZE // 2, 3 * SIZE // 2)
    w = [random.randint(1, MAX_W) for _ in range(cur_size)]
    v = [random.randint(1, MAX_V) for _ in range(cur_size)]

    w_max = random.randint(1, sum(w) - 1)

    true_sol = knapsack(cur_size, w_max, w, v)

    stdin = f"{cur_size} {w_max}\n{' '.join(map(str, w))}\n{' '.join(map(str, v))}"
    res = subprocess.run(sys.argv[1], capture_output=True, text=True, input=stdin)
    assert not res.stderr, res.stderr
    assert res.returncode == 0, res.returncode
    assert int(res.stdout) == true_sol

print("OK")
