#!/usr/bin/env python3

# to test Alan we transcribe it (by hand) to Python by making as little changes as possible

from alanUtils import *


def merge(x: list[int], start: int, mid: int, end: int) -> None:
    # through recursion create a "recursive array" in callstack (through the 'cur' variables).
    def aux(x_itr: int, l: int, r: int) -> None:
        # presumably (given the scope rules) x, mid, end are visible inside aux.

        cur: int = 1000

        if x_itr >= end:
            return

        if l < mid and r < end:
            if x[l] < x[r]:
                cur = x[l]
                l = l + 1
            else:
                cur = x[r]
                r = r + 1
        else:
            if l < mid:
                cur = x[l]
                l = l + 1
            else:
                return  # r < end, in which case these elements are already in correct position

        aux(x_itr + 1, l, r)
        x[x_itr] = cur

    aux(start, start, mid)


def mergesort(x: list[int], l: int, r: int) -> None:
    # sort [l,r) segment
    if l == r - 1:
        return
    mergesort(x, l, (l + r) // 2)
    mergesort(x, (l + r) // 2, r)

    # merge (separate function to allocate local variables in them and reduce stack consumption)
    merge(x, l, (l + r) // 2, r)


def main() -> None:
    x: list[int] = [0] * 10000
    n: int
    i: int

    n = readInteger()

    if n <= 0:
        return

    i = 0
    while i < n:
        x[i] = readInteger()
        i = i + 1

    mergesort(x, 0, n)

    writeInteger(x[0])
    i = 1
    while i < n:
        writeChar(" ")
        writeInteger(x[i])
        i = i + 1

    writeChar("\n")


if __name__ == "__main__":
    main()
