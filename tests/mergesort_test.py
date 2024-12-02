#!/usr/bin/env python3

import random
import subprocess
import sys

if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} [filename of program to test]")
    exit(1)

N = 10
SIZE = 100
MIN = -1000
MAX = 1000

random.seed(42)

for _ in range(N):
    cur_size = random.randint(SIZE // 2, 3 * SIZE // 2)
    arr = [random.randint(MIN, MAX) for _ in range(cur_size)]
    stdin = f"{cur_size}\n{' '.join(map(str, arr))}"
    res = subprocess.run([sys.argv[1]], capture_output=True, text=True, input=stdin)
    assert not res.stderr, res.stderr
    assert res.returncode == 0, res.returncode
    assert all(
        x == y for x, y in zip(sorted(arr), map(int, res.stdout.split()), strict=True)
    )

print("OK")
