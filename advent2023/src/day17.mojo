from math import gcd, lcm, max, min
from pathlib import Path
from package.traits import Sortable, HashKeyT
from package.hashset import HashSet
from package.priorityqueue import PriorityQueue


struct Point(Stringable, HashKeyT, Sortable):
    var row: Int
    var col: Int
    var value: Int
    var steps: String
    var dir: String

    fn __init__(
        inout self: Self, row: Int, col: Int, steps: String, dir: String
    ):
        self.row = row
        self.col = col
        self.value = 0
        self.steps = steps
        self.dir = dir

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.row = existing.row
        self.col = existing.col
        self.value = existing.value
        self.steps = existing.steps
        self.dir = existing.dir

    fn __copyinit__(inout self: Self, existing: Self):
        self.row = existing.row
        self.col = existing.col
        self.value = existing.value
        self.steps = existing.steps
        self.dir = existing.dir

    fn __str__(self: Self) -> String:
        return (
            String("[")
            + self.row
            + ", "
            + self.col
            + "]: value "
            + self.value
            + " steps: "
            + self.steps
            + " dir: "
            + self.dir
        )

    fn __eq__(self: Self, rhs: Self) -> Bool:
        return (
            self.row == rhs.row
            and self.col == rhs.col
            and self.steps == rhs.steps
        )

    fn hash(self: Self) -> Int:
        let p: Int = 31
        let m: Int = 10000009

        var hash_value: Int = 0
        var p_pow: Int = 1
        var v = DynamicVector[Int]()
        v.push_back(self.row)
        v.push_back(self.col)
        for i in range(len(v)):
            hash_value = (hash_value + int(v[i] * p_pow)) % m
            p_pow = (p_pow * p) % m

        for i in range(len(self.steps)):
            hash_value = (hash_value + int(ord(self.steps[i]) * p_pow)) % m
            p_pow = (p_pow * p) % m

        return hash_value

    fn is_out(self: Self, height: Int, width: Int) -> Bool:
        return (
            self.row < 0
            or self.row >= height
            or self.col < 0
            or self.col >= width
            or len(self.steps) > 3
        )

    fn next(self: Self, dir: String) -> Point:
        let row = self.row
        let col = self.col
        let steps = self.steps
        if self.dir == "n":
            if dir == "l":
                return Point(row, col - 1, "w", "w")
            elif dir == "r":
                return Point(row, col + 1, "e", "e")
            elif dir == "n":
                return Point(row - 1, col, steps + "n", "n")
        elif self.dir == "s":
            if dir == "l":
                return Point(row, col - 1, "w", "w")
            elif dir == "r":
                return Point(row, col + 1, "e", "e")
            elif dir == "s":
                return Point(row + 1, col, steps + "s", "s")
        elif self.dir == "w":
            if dir == "l":
                return Point(row + 1, col, "s", "s")
            elif dir == "r":
                return Point(row - 1, col, "n", "n")
            elif dir == "w":
                return Point(row, col - 1, steps + "w", "w")
        elif self.dir == "e":
            if dir == "l":
                return Point(row - 1, col, "n", "n")
            elif dir == "r":
                return Point(row + 1, col, "s", "s")
            elif dir == "e":
                return Point(row, col + 1, steps + "e", "e")

        # print(self.dir + " --> " + dir)

        return Point(row, col, steps, "")

    fn __lt__(self, rhs: Self) -> Bool:
        return self.value < rhs.value

    fn __le__(self, rhs: Self) -> Bool:
        return self.value <= rhs.value

    fn part1_next_points(
        self: Self,
        height: Int,
        width: Int,
        borrowed blocks: DynamicVector[DynamicVector[Int]],
    ) -> DynamicVector[Point]:
        var result = DynamicVector[Point]()
        var p = self.next("l")
        if not p.is_out(height, width):
            p.value = self.value + blocks[p.row][p.col]
            result.push_back(p)
        p = self.next("r")
        if not p.is_out(height, width):
            p.value = self.value + blocks[p.row][p.col]
            result.push_back(p)
        p = self.next(self.dir)
        if not p.is_out(height, width):
            p.value = self.value + blocks[p.row][p.col]
            result.push_back(p)
        return result

    fn is_out_grid(self: Self, height: Int, width: Int) -> Bool:
        return (
            self.row < 0
            or self.row >= height
            or self.col < 0
            or self.col >= width
        )

    fn part2_next_points(
        self: Self,
        height: Int,
        width: Int,
        borrowed blocks: DynamicVector[DynamicVector[Int]],
    ) -> DynamicVector[Point]:
        var result = DynamicVector[Point]()

        var p = self.next("l")
        if len(self.steps) >= 4 and not p.is_out_grid(height, width):
            p.value = self.value + blocks[p.row][p.col]
            result.push_back(p)

        p = self.next("r")
        if len(self.steps) >= 4 and not p.is_out_grid(height, width):
            p.value = self.value + blocks[p.row][p.col]
            result.push_back(p)

        p = self.next(self.dir)
        if len(self.steps) < 10 and not p.is_out_grid(height, width):
            p.value = self.value + blocks[p.row][p.col]
            result.push_back(p)

        return result


def part1():
    let path = Path("../data/day17/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var blocks = DynamicVector[DynamicVector[Int]]()
    var init_block = DynamicVector[Int]()
    init_block.resize(len(lines[0]), 0)
    blocks.resize(len(lines) - 1, init_block)
    let height = len(lines) - 1
    let width = len(lines[0])
    print(String("height: ") + height)
    print(String("width: ") + width)

    for i in range(height):
        for j in range(width):
            blocks[i][j] = atol(lines[i][j])
            # print_no_newline(blocks[i][j])
        # print("")

    var seen = HashSet[Point](3042161)
    var work_list = PriorityQueue[Point](100000)

    work_list.push(Point(0, 0, "e", "e"))
    work_list.push(Point(0, 0, "s", "s"))

    var count: Int = 0
    let total = 10

    while work_list.size > 0:
        count += 1
        let curr = work_list.top()
        work_list.pop_front()

        if seen.contains(curr):
            continue
        seen.insert(curr)
        # print(curr)

        let next_points = curr.part1_next_points(height, width, blocks)
        # print(String("Curr: ") + curr.__str__())
        for i in range(len(next_points)):
            let next = next_points[i]
            if next.row == height - 1 and next.col == width - 1:
                return next.value

            if seen.contains(next):
                continue
            work_list.push(next)

    return -1


def part2():
    let path = Path("../data/day17/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var blocks = DynamicVector[DynamicVector[Int]]()
    var init_block = DynamicVector[Int]()
    init_block.resize(len(lines[0]), 0)
    blocks.resize(len(lines) - 1, init_block)
    let height = len(lines) - 1
    let width = len(lines[0])
    print(String("height: ") + height)
    print(String("width: ") + width)

    for i in range(height):
        for j in range(width):
            blocks[i][j] = atol(lines[i][j])
            # print_no_newline(blocks[i][j])
        # print("")

    var seen = HashSet[Point](3042161)
    var work_list = PriorityQueue[Point](100000)

    work_list.push(Point(0, 0, "e", "e"))
    work_list.push(Point(0, 0, "s", "s"))

    var count: Int = 0
    let total = 10

    while work_list.size > 0:
        count += 1
        let curr = work_list.top()
        work_list.pop_front()

        if seen.contains(curr):
            continue
        seen.insert(curr)
        # print(curr)

        let next_points = curr.part2_next_points(height, width, blocks)
        # print(String("Curr: ") + curr.__str__())
        for i in range(len(next_points)):
            let next = next_points[i]
            if next.row == height - 1 and next.col == width - 1:
                return next.value

            if seen.contains(next):
                continue
            work_list.push(next)

    return -1


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
