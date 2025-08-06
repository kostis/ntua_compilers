#!/usr/bin/env python3

import random
import subprocess
import sys

if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} [filename of program to test]")
    exit(1)


N = 10
random.seed(42)

for _ in range(N):
    b = random.randint(1, 30)
    e = random.randint(1, 100)
    m = random.randint(1, int(1e9))

    res = subprocess.run(
        [sys.argv[1]], capture_output=True, text=True, input=f"{b} {e} {m}"
    )
    assert not res.stderr, res.stderr
    assert res.returncode == 0, res.returncode
    assert int(res.stdout) == pow(b, e, m)

print("OK")
