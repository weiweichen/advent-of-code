from math import gcd, lcm, max, min
from pathlib import Path


struct Point(CollectionElement, Stringable):
    var x: Int
    var y: Int

    fn __init__(inout self: Self, x: Int, y: Int):
        self.x = x
        self.y = y

    fn __moveinit__(inout self, owned existing: Self):
        self.x = existing.x
        self.y = existing.y

    fn __copyinit__(inout self, existing: Self):
        self.x = existing.x
        self.y = existing.y

    fn __str__(self) -> String:
        let res = String("y: ") + self.y + ", x: " + self.x
        return res


fn solve(borrowed lines: DynamicVector[String], rate: Int64) -> Int64:
    var points = DynamicVector[Point]()
    var empty_cols = DynamicVector[Int64]()
    var empty_rows = DynamicVector[Int64]()
    var acc: Int64 = 0
    for i in range(len(lines) - 1):
        var empty = True
        for j in range(len(lines[i])):
            if lines[i][j] == "#":
                points.push_back(Point(j, i))
                empty = False
        if empty:
            acc += 1
        empty_rows.push_back(acc)

    acc = 0
    for j in range(len(lines[0])):
        var empty = True
        for i in range(len(lines) - 1):
            if lines[i][j] == "#":
                empty = False

        if empty:
            acc += 1
        empty_cols.push_back(acc)

    # print_no_newline("empty rows: ")
    # for i in range(len(empty_rows)):
    #     print_no_newline(empty_rows[i])
    # print("")

    # print_no_newline("empty cols: ")
    # for i in range(len(empty_cols)):
    #     print_no_newline(empty_cols[i])
    # print("")

    var res: Int64 = 0
    for i in range(len(points)):
        for j in range(i + 1, len(points)):
            let max_x = max(points[i].x, points[j].x)
            let min_x = min(points[i].x, points[j].x)
            let max_y = max(points[i].y, points[j].y)
            let min_y = min(points[i].y, points[j].y)
            let more_rows: Int64 = empty_rows[max_y] - empty_rows[min_y]
            let more_cols: Int64 = empty_cols[max_x] - empty_cols[min_x]

            let dis = (max_x - min_x) + (max_y - min_y) + (
                more_rows + more_cols
            ) * (rate - 1)
            res += dis
            # print(points[i])
            # print(points[j])
            # print(more_rows)
            # print(more_cols)
            # print(dis)

    return res


def part1():
    let path = Path("../data/day11/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")
    return solve(lines, 1)


def part2():
    let path = Path("../data/day11/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")
    return solve(lines, 1000000)


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
