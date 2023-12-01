from pathlib import Path
from sys.info import os_is_windows
from sys.param_env import env_get_string
from testing import assert_equal

from memory.buffer import Buffer, NDBuffer
from tensor import Tensor

def part1():
    let path = Path("../data/day1/part1_test.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    for l in range(len(lines) - 1):
        let line = lines[l]

        var b = -1
        var e = -1
        let size = len(line)
        for i in range(size):
            let c0 = line[i]
            let c1 = line[size - 1 - i]
            if isdigit(ord(c0)) and b == -1:
                b = ord(c0) - 48
            if isdigit(ord(c1)) and e == -1:
                e = ord(c1) - 48
        result += b * 10 + e
    return result

def part2():
    let path = Path("../data/day1/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    var numbers = DynamicVector[String](9)
    numbers.push_back("one")
    numbers.push_back("two")
    numbers.push_back("three")
    numbers.push_back("four")
    numbers.push_back("five")
    numbers.push_back("six")
    numbers.push_back("seven")
    numbers.push_back("eight")
    numbers.push_back("nine")

    for l in range(len(lines) - 1):
        let line = lines[l]

        var b = -1
        var e = -1
        var bpos = len(line)
        var epos = -1

        let size = len(line)
        for i in range(size):
            let c0 = line[i]
            let c1 = line[size - 1 - i]
            if isdigit(ord(c0)) and b == -1:
                b = ord(c0) - 48
                bpos = i

            if isdigit(ord(c1)) and e == -1:
                e = ord(c1) - 48
                epos = size - 1 - i

        var nbpos = len(line)
        var nepos = -1

        var nb = -1
        var ne = -1

        for n in range(9):
            let p0 = line.find(numbers[n])
            let p1 = line.rfind(numbers[n])

            if p0 != -1 and nbpos >= p0:
                nb = n + 1
                nbpos = p0
            if p1 != -1 and nepos <= p1:
                ne = n + 1
                nepos = p1

        let x = nb if bpos > nbpos else b
        let y = e if epos > nepos else ne
        result += x * 10 + y

    return result


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
