from pathlib import Path


fn extrapolate(inout numbers: DynamicVector[Int]) -> Int:
    var curr_len = len(numbers)
    var all_zeros = False
    while not all_zeros:
        all_zeros = True
        for i in range(1, curr_len):
            numbers[i - 1] = numbers[i] - numbers[i - 1]
            if numbers[i - 1] != 0:
                all_zeros = False
        curr_len -= 1

    var result = 0
    for i in range(curr_len - 1, len(numbers)):
        result += numbers[i]

    return result


fn extrapolate_backwards(inout numbers: DynamicVector[Int]) -> Int:
    let curr_len = len(numbers)
    var all_zeros = False
    var size = len(numbers)

    while not all_zeros:
        all_zeros = True
        for i in range(curr_len - 1, curr_len - size, -1):
            numbers[i] = numbers[i] - numbers[i - 1]
            if numbers[i] != 0:
                all_zeros = False
        size -= 1

    var result = 0
    for i in range(curr_len - size - 1, -1, -1):
        result = numbers[i] - result

    return result


def part1():
    let path = Path("../data/day9/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    for i in range(len(lines) - 1):
        var numbers = DynamicVector[Int]()
        let str = lines[i].split(" ")
        for j in range(len(str)):
            numbers.push_back(atol(str[j]))

        result += extrapolate(numbers)

    return result


def part2():
    let path = Path("../data/day9/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    for i in range(len(lines) - 1):
        var numbers = DynamicVector[Int]()
        let str = lines[i].split(" ")
        for j in range(len(str)):
            numbers.push_back(atol(str[j]))

        result += extrapolate_backwards(numbers)

    return result


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
