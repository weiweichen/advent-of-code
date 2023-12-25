from math import gcd, lcm, max, min
from pathlib import Path
from package.hasher import IntHasher, StringHasher
from package.hashmap import HashMap
from package.hashset import HashSet
from package.slowqueue import SlowQueue
from package.traits import HashKeyT, Equitable


struct Point(Stringable, CollectionElement, HashKeyT):
    var row: Int
    var col: Int
    var value: Int
    var dist: Int

    fn __init__(inout self: Self, row: Int, col: Int, value: Int):
        self.row = row
        self.col = col
        self.value = value
        self.dist = 0

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.row = existing.row
        self.col = existing.col
        self.value = existing.value
        self.dist = existing.dist

    fn __copyinit__(inout self: Self, existing: Self):
        self.row = existing.row
        self.col = existing.col
        self.value = existing.value
        self.dist = existing.dist

    fn __str__(self: Self) -> String:
        return (
            String("[")
            + self.row
            + ", "
            + self.col
            + "]: "
            + self.value
            + " dist: "
            + self.dist
        )

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

    fn next_points(
        self: Self,
        height: Int,
        width: Int,
        garden: DynamicVector[String],
        slope_slippery: Bool,
    ) -> DynamicVector[Point]:
        var result = DynamicVector[Point]()
        let dirs = StaticTuple[4, StaticIntTuple[2]](
            (0, 1), (0, -1), (-1, 0), (1, 0)
        )

        let ch = garden[self.row][self.col]
        var p = Point(0, 0, 0)
        if slope_slippery:
            if ch == "v":
                p = Point(self.row + 1, self.col, self.value + 1)
                if not p.is_out(height, width) and garden[p.row][p.col] != "#":
                    result.push_back(p)
            elif ch == ">":
                p = Point(self.row, self.col + 1, self.value + 1)
                if not p.is_out(height, width) and garden[p.row][p.col] != "#":
                    result.push_back(p)
            elif ch == "<":
                p = Point(self.row, self.col - 1, self.value + 1)
                if not p.is_out(height, width) and garden[p.row][p.col] != "#":
                    result.push_back(p)
            elif ch == "^":
                p = Point(self.row - 1, self.col, self.value + 1)
                if not p.is_out(height, width) and garden[p.row][p.col] != "#":
                    result.push_back(p)
            else:
                for i in range(4):
                    let p = Point(
                        self.row + dirs[i][0],
                        self.col + dirs[i][1],
                        self.value + 1,
                    )
                    if (
                        not p.is_out(height, width)
                        and garden[p.row][p.col] != "#"
                    ):
                        result.push_back(p)
        else:
            for i in range(4):
                let p = Point(
                    self.row + dirs[i][0], self.col + dirs[i][1], self.value + 1
                )
                if not p.is_out(height, width) and garden[p.row][p.col] != "#":
                    result.push_back(p)

        return result


fn print_garden(borrowed garden: DynamicVector[String]):
    for i in range(len(garden) - 1):
        print(garden[i])


fn dfs(
    borrowed start: Point,
    borrowed end: Point,
    inout seen: HashSet[Point],
    inout max_value: Int,
    height: Int,
    width: Int,
    borrowed garden: DynamicVector[String],
    slope_slippery: Bool,
):
    if start == end:
        max_value = max(max_value, start.value)
    if seen.contains(start):
        return

    _ = seen.insert(start)
    let next = start.next_points(height, width, garden, slope_slippery)
    for i in range(len(next)):
        dfs(
            next[i], end, seen, max_value, height, width, garden, slope_slippery
        )
    seen.erase(start)


fn dfs2(
    borrowed start: Point,
    borrowed end: Point,
    inout seen: HashSet[Point],
    inout max_value: Int,
    height: Int,
    width: Int,
    inout condensed_graph: HashMap[Point, DynamicVector[Point]],
    borrowed dist: Int,
) raises:
    if start == end:
        if dist > max_value:
            print(dist)

        max_value = max(max_value, dist)

    if seen.contains(start):
        return

    _ = seen.insert(start)

    if not condensed_graph.contains(start):
        return

    let neighbours = condensed_graph[start]

    for i in range(len(neighbours)):
        dfs2(
            neighbours[i],
            end,
            seen,
            max_value,
            height,
            width,
            condensed_graph,
            neighbours[i].dist + dist,
        )
    seen.erase(start)


def part1():
    let path = Path("../data/day23/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    let height = len(lines) - 1
    let width = len(lines[0])

    var seen = HashSet[Point](1128889)

    let garden = lines
    var start = Point(0, 1, 0)
    var end = Point(0, 1, 1)
    for i in range(width):
        if garden[0][i] == ".":
            start = Point(0, i, 0)
        if garden[height - 1][i] == ".":
            end = Point(height - 1, i, 0)
    print(start)
    print(end)

    var result: Int = 0
    dfs(start, end, seen, result, height, width, garden, True)
    return result


def part2():
    let path = Path("../data/day23/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    let height = len(lines) - 1
    let width = len(lines[0])

    let garden = lines
    var start = Point(0, 1, 0)
    var end = Point(0, 1, 1)
    for i in range(width):
        if garden[0][i] == ".":
            start = Point(0, i, 0)
        if garden[height - 1][i] == ".":
            end = Point(height - 1, i, 0)
    print(start)
    print(end)

    var nodes: Int = 0
    var nodes_to_process: Int = 0

    var seen = HashSet[Point](1128889)
    var condensed_graph = HashMap[Point, DynamicVector[Point]](1128889)
    var condensed_nodes = DynamicVector[Point]()
    var work_list = SlowQueue[Point](height * width)

    work_list.push_back(start)
    while work_list.size > 0:
        let curr = work_list.front()
        work_list.pop_front()

        if seen.contains(curr):
            continue

        var next_points = DynamicVector[Point]()
        let neighbours = curr.next_points(height, width, garden, False)
        for i in range(len(neighbours)):
            var dist: Int = 1
            var prev = curr
            var p = neighbours[i]
            var stop: Bool = False
            while True:
                let neighbours = p.next_points(height, width, garden, False)
                if len(neighbours) == 1 and neighbours[0] == prev:
                    stop = True
                    break
                if len(neighbours) > 2:
                    break

                for j in range(len(neighbours)):
                    if neighbours[j] == prev:
                        continue
                    dist += 1
                    prev = p
                    p = neighbours[j]
                    break

            # if stop:
            #    continue

            p.dist = dist
            next_points.push_back(p)
            work_list.push_back(p)

        seen.insert(curr)

        # print(String("add condensed node:") + curr)
        # for i in range(len(next_points)):
        #    print(String("next: ") + next_points[i])
        # condensed_nodes.push_back(curr)
        condensed_graph.insert_or_update(curr, next_points)

    var result: Int = 0
    seen.clear()
    dfs2(start, end, seen, result, height, width, condensed_graph, 0)
    return result


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
