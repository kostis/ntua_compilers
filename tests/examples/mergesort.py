#!/usr/bin/env python3

# to test Alan we transcribe it (by hand) to Python by making as little changes as possible

from alanUtils import *


def merge(x: list[int], start: int, mid: int, end: int) -> None:
    tmp: list[int] = [0] * (end - start)
    tmp_idx: int
    l_itr: int
    r_itr: int
    x_itr: int
    tmp_idx2: int

    tmp_idx = 0
    l_itr = start
    r_itr = mid

    while l_itr < mid and r_itr < end:
        if x[l_itr] < x[r_itr]:
            tmp[tmp_idx] = x[l_itr]
            l_itr = l_itr + 1
        else:
            tmp[tmp_idx] = x[r_itr]
            r_itr = r_itr + 1
        tmp_idx = tmp_idx + 1
    while l_itr < mid:
        tmp[tmp_idx] = x[l_itr]
        tmp_idx = tmp_idx + 1
        l_itr = l_itr + 1
    # if r < end, then these elements are already in correct position
    x_itr = start
    tmp_idx2 = 0
    while tmp_idx2 < tmp_idx:
        x[x_itr] = tmp[tmp_idx2]
        x_itr = x_itr + 1
        tmp_idx2 = tmp_idx2 + 1


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
