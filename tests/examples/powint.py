#!/usr/bin/env python3


# to test Alan we transcribe it (by hand) to Python by making as little changes as possible

from alanUtils import *


def powint(base: int, expon: int, mod: int) -> int:
    res: int
    base = base % mod
    res = 1
    while expon > 0:
        if expon % 2 == 1:
            res = (res * base) % mod
        base = (base * base) % mod
        expon //= 2
    return res


def main() -> None:
    b: int
    e: int
    m: int
    res: int

    b = readInteger()
    e = readInteger()
    m = readInteger()
    res = powint(b, e, m)
    writeInteger(res)
    writeChar("\n")


if __name__ == "__main__":
    main()
