buffer: list[str] = []


def readInteger() -> int:
    global buffer
    if not buffer:
        buffer = input().split()
    return int(buffer.pop(0))


def writeInteger(n: int) -> None:
    print(n, end="")


def writeChar(c: str) -> None:
    print(c, end="")
