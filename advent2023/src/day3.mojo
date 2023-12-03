from pathlib import Path
from utils.vector import UnsafeFixedVector


fn get_next_number(str: String, pos: Int) -> StaticIntTuple[3]:
    var result = -1
    var next_pos = len(str)
    var num_pos = -1

    var i = pos
    while not isdigit(ord(str[i])) and i < len(str):
        i += 1

    while i < len(str):
        if isdigit(ord(str[i])):
            if result == -1:
                result = 0
                num_pos = i
            result = result * 10 + ord(str[i]) - 48
            i += 1
        else:
            next_pos = i
            break
    return StaticIntTuple[3](result, num_pos, next_pos)


fn is_symbol(col: Int, row: Int, borrowed lines: DynamicVector[String]) -> Bool:
    if col < 0 or row < 0 or col >= len(lines[row]) or row >= len(lines):
        return False
    let c = lines[row][col]
    if not isdigit(ord(c)) and c != ".":
        return True
    return False


fn is_part_number(
    col: Int, row: Int, size: Int, borrowed lines: DynamicVector[String]
) -> Bool:
    for i in range(col - 1, col + size + 1):
        if is_symbol(i, row - 1, lines):
            return True

    if is_symbol(col - 1, row, lines):
        return True

    if is_symbol(col + size, row, lines):
        return True

    for i in range(col - 1, col + size + 1):
        if is_symbol(i, row + 1, lines):
            return True

    return False


def part1():
    let path = Path("../data/day3/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    for l in range(len(lines) - 1):
        let line = lines[l]

        var pos = 0
        while pos < len(line):
            let p = get_next_number(line, pos)
            let number = p[0]
            if number == -1:
                break
            let col = p[1]
            let size = p[2] - p[1]
            let b = is_part_number(col, l, size, lines)
            pos = p[2]
            if b:
                result += number

    return result


fn update_gear(
    col: Int,
    row: Int,
    cols: Int,
    rows: Int,
    inout values: UnsafeFixedVector[Int],
    inout counts: UnsafeFixedVector[Int],
    borrowed lines: DynamicVector[String],
    number: Int,
):
    if col < 0 or row < 0 or col >= cols or row >= rows:
        return

    let c = lines[row][col]
    let idx = row * cols + col
    if c != "*":
        return

    counts[idx] += 1

    if counts[idx] > 2:
        return
    values[idx] *= number


fn update_number(
    col: Int,
    row: Int,
    cols: Int,
    rows: Int,
    size: Int,
    inout values: UnsafeFixedVector[Int],
    inout counts: UnsafeFixedVector[Int],
    borrowed lines: DynamicVector[String],
    number: Int,
):
    for i in range(col - 1, col + size + 1):
        update_gear(i, row - 1, cols, rows, values, counts, lines, number)
        update_gear(i, row + 1, cols, rows, values, counts, lines, number)
    update_gear(col - 1, row, cols, rows, values, counts, lines, number)
    update_gear(col + size, row, cols, rows, values, counts, lines, number)


def part2():
    let path = Path("../data/day3/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    let cols = len(lines[0])
    let rows = len(lines)

    var values = UnsafeFixedVector[Int](cols * rows)
    var counts = UnsafeFixedVector[Int](cols * rows)

    for i in range(cols * rows):
        values.append(1)
        counts.append(0)

    for l in range(len(lines) - 1):
        let line = lines[l]
        var pos = 0
        while pos < len(line):
            let p = get_next_number(line, pos)
            let number = p[0]
            if number == -1:
                break
            let col = p[1]
            let size = p[2] - p[1]
            pos = p[2]
            let row = l
            update_number(
                col, l, cols, rows, size, values, counts, lines, number
            )

    for i in range(cols * rows):
        if counts[i] == 2:
            result += values[i]

    return result


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
