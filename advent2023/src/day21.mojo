from math import gcd, lcm, max, min
from pathlib import Path
from package.hasher import StringHasher
from package.hashmap import HashMap
from package.slowqueue import SlowQueue
from package.traits import HashKeyT, Equitable


struct Point(Stringable, CollectionElement):
    var row: Int
    var col: Int

    fn __init__(inout self: Self, row: Int, col: Int):
        self.row = row
        self.col = col

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.row = existing.row
        self.col = existing.col

    fn __copyinit__(inout self: Self, existing: Self):
        self.row = existing.row
        self.col = existing.col

    fn __str__(self: Self) -> String:
        return String("[") + self.row + ", " + self.col + "]"

    fn __eq__(self: Self, rhs: Self) -> Bool:
        return self.row == rhs.row and self.col == rhs.col

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

        return hash_value

    fn is_out(self: Self, height: Int, width: Int) -> Bool:
        return (
            self.row < 0
            or self.row >= height
            or self.col < 0
            or self.col >= width
        )

    fn next_points(self: Self, height: Int, width: Int) -> DynamicVector[Point]:
        var result = DynamicVector[Point]()
        let dirs = StaticTuple[4, StaticIntTuple[2]](
            (0, 1), (0, -1), (-1, 0), (1, 0)
        )
        for i in range(4):
            let p = Point(self.row + dirs[i][0], self.col + dirs[i][1])
            if not p.is_out(height, width):
                result.push_back(p)
        return result


fn print_garden(borrowed garden: DynamicVector[String]):
    for i in range(len(garden) - 1):
        print(garden[i])


def part1():
    let path = Path("../data/day21/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var garden = lines
    let height = len(garden) - 1
    let width = len(garden[0])

    var start = Point(0, 0)

    for y in range(height):
        for x in range(width):
            if garden[y][x] == "S":
                start.row = y
                start.col = x
                break

    var curr_points = DynamicVector[Point]()
    curr_points.push_back(start)
    let total_step = 64

    for step in range(total_step):
        # print(String("curr_points size: ") + len(curr_points))
        # print_garden(garden)
        var next_points = DynamicVector[Point]()
        for i in range(len(curr_points)):
            let p = curr_points[i]
            garden[p.row]._buffer[p.col] = ord(".")

        for i in range(len(curr_points)):
            let p = curr_points[i]
            let next_ps = p.next_points(height, width)
            for i in range(len(next_ps)):
                let p1 = next_ps[i]
                if garden[p1.row]._buffer[p1.col] == ord("."):
                    garden[p1.row]._buffer[p1.col] = ord("O")
                    next_points.push_back(p1)
        curr_points = next_points

    # print(String("curr_points size: ") + len(curr_points))
    # print_garden(garden)
    return len(curr_points)


fn walk(
    borrowed input_garden: DynamicVector[String], start: Point, total_step: Int
) -> Int:
    var curr_points = DynamicVector[Point]()
    curr_points.push_back(start)
    var garden = input_garden
    let height = len(input_garden) - 1
    let width = len(input_garden[0])

    for step in range(total_step):
        # print(String("curr_points size: ") + len(curr_points))
        # print_garden(garden)
        var next_points = DynamicVector[Point]()
        for i in range(len(curr_points)):
            let p = curr_points[i]
            garden[p.row]._buffer[p.col] = ord(".")

        for i in range(len(curr_points)):
            let p = curr_points[i]
            let next_ps = p.next_points(height, width)
            for i in range(len(next_ps)):
                let p1 = next_ps[i]
                if garden[p1.row]._buffer[p1.col] == ord(".") or garden[
                    p1.row
                ]._buffer[p1.col] == ord("S"):
                    garden[p1.row]._buffer[p1.col] = ord("O")
                    next_points.push_back(p1)
        curr_points = next_points

    print(String("curr_points size: ") + len(curr_points))
    # print_garden(garden)
    return len(curr_points)


def part2():
    # very input sepcific solution, observations:
    # - S is in the center of garden
    # - center row and col don't have any rocks
    # - garden size 131 * 131
    # - 26501365 = 131 * N + 65 (ahhhhhhhhhhhh)
    # - step 65 => diamond shape with complete diamond edges all with "O"
    # - no rocks on the edges of the garden
    # - after 130 steps, looking at the garden that has been fully reached the number of "O" starts to repeat between even and odd steps
    # - result should look like a giant diamond with
    #   - fully reached center garden tiles:
    #     (1 + ... + N - 1) * 2 + N     odd step tiles
    #     (1 + ... + N - 2) * 2 + N - 1 even step tiles
    #   - 4 tip tiles as if starting from the center of the edges and walk 130 steps (starting position is step 1 already)
    #   - 4 types of triangles each starting from 4 corners and walk 64 stesp, each type has N tiles
    #   - 4 types of pentagons each starting from 4 corners and walk 64 + 131 stesp, each type has N-1 tiles

    let path = Path("../data/day21/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")
    let height = len(lines) - 1
    let width = len(lines[0])
    let garden = lines

    var start = Point(0, 0)

    for y in range(height):
        for x in range(width):
            if lines[y][x] == "S":
                start.row = y
                start.col = x
                break

    print(String("height: ") + height + " width: " + width)
    print(start)

    var curr_points = DynamicVector[Point]()
    curr_points.push_back(start)
    let p_center_left = Point(65, 0)
    let p_center_right = Point(65, 130)
    let p_center_top = Point(0, 65)
    let p_center_bottom = Point(130, 65)

    let t1 = walk(garden, p_center_left, 130)
    let t2 = walk(garden, p_center_right, 130)
    let t3 = walk(garden, p_center_top, 130)
    let t4 = walk(garden, p_center_bottom, 130)

    let p_top_left = Point(0, 0)
    let p_top_right = Point(0, 130)
    let p_bottom_left = Point(130, 0)
    let p_bottom_right = Point(130, 130)

    let tr1 = walk(garden, p_top_left, 64)
    let tr2 = walk(garden, p_top_right, 64)
    let tr3 = walk(garden, p_bottom_left, 64)
    let tr4 = walk(garden, p_bottom_right, 64)

    let tz1 = walk(garden, p_top_left, 64 + 131)
    let tz2 = walk(garden, p_top_right, 64 + 131)
    let tz3 = walk(garden, p_bottom_left, 64 + 131)
    let tz4 = walk(garden, p_bottom_right, 64 + 131)

    let N = (26501365 - 65) // height
    let NOdd = N * (N - 1) + N
    let NEven = (N - 1) * (N - 2) + N - 1

    let p_center = Point(65, 65)
    let odd = walk(garden, p_center, 130)
    let even = walk(garden, p_center, 131)
    print(String("N: ") + N)
    print(String("NEven: ") + NEven)
    print(String("NOdd: ") + NOdd)

    let center = NOdd * odd + NEven * even
    let tip = t1 + t2 + t3 + t4
    let tr = (tr1 + tr2 + tr3 + tr4) * N
    let tz = (tz1 + tz2 + tz3 + tz4) * (N - 1)

    return center + tip + tr + tz


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
