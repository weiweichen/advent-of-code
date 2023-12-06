from pathlib import Path
from math import max, min


fn update_values(
    inout values: DynamicVector[Int64],
    borrowed ranges: DynamicVector[DynamicVector[Int64]],
):
    for i in range(len(values)):
        let v = values[i]

        for r in range(len(ranges)):
            let des = ranges[r][0]
            let src = ranges[r][1]
            let step = ranges[r][2]
            if v >= src and v < src + step:
                values[i] = des + v - src
                break


fn get_range1(
    borrowed lines: DynamicVector[String],
    inout pos: Int,
    inout ranges: DynamicVector[DynamicVector[Int64]],
) raises:
    ranges.clear()
    var i = pos
    while len(lines[i]) != 0:
        let numbers = lines[i].split(" ")
        var dss = DynamicVector[Int64]()
        for j in range(len(numbers)):
            dss.push_back(atol(numbers[j]))
        ranges.push_back(dss)
        i += 1
    pos = i + 2


def part1():
    let path = Path("../data/day5/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    let seeds = lines[0].split("seeds: ")[1].split(" ")

    var values = DynamicVector[Int64]()
    for i in range(len(seeds)):
        values.push_back(atol(seeds[i]))

    var pos = 3
    var ranges = DynamicVector[DynamicVector[Int64]]()
    for i in range(7):
        get_range1(lines, pos, ranges)
        update_values(values, ranges)

    var min = values[0]
    for i in range(1, len(values)):
        min = values[i] if values[i] < min else min
    return min


struct Range(CollectionElement):
    var b: Int64
    var e: Int64
    var step: Int64

    fn __init__(inout self: Self, b: Int64, e: Int64, step: Int64):
        self.b = b
        self.e = e
        self.step = step

    fn __moveinit__(inout self, owned existing: Self):
        self.b = existing.b
        self.e = existing.e
        self.step = existing.step

    fn __copyinit__(inout self, existing: Self):
        self.b = existing.b
        self.e = existing.e
        self.step = existing.step


fn get_range2(
    borrowed lines: DynamicVector[String],
    inout pos: Int,
    inout ranges: DynamicVector[Range],
) raises:
    ranges.clear()
    var i = pos
    while len(lines[i]) != 0:
        let numbers = lines[i].split(" ")
        let b = atol(numbers[0])
        let e = atol(numbers[1])
        let s = atol(numbers[2])
        ranges.push_back(Range(b, e, s))
        i += 1
    pos = i + 2


fn update_ranges(
    inout ranges: DynamicVector[Range],
    borrowed target_ranges: DynamicVector[Range],
) -> DynamicVector[Range]:
    var result = DynamicVector[Range]()
    while len(ranges) > 0:
        let curr = ranges.pop_back()
        let l0 = curr.b
        let r0 = curr.e

        var changed = False
        for i in range(len(target_ranges)):
            let target = target_ranges[i]
            let l1 = target.e
            let r1 = target.e + target.step

            let l = max(l0, l1)
            let r = min(r0, r1)

            # no overlap
            if l >= r:
                continue

            # add valid range to result
            let des_l = l - target.e + target.b
            let des_r = des_l + r - l
            result.push_back(Range(des_l, des_r, 0))
            changed = True

            # no new ranges
            if l0 >= l and r0 <= r:
                break

            # add left-overs back to ranges
            if l0 < l and r0 <= r:
                ranges.push_back(Range(l0, l, 0))
            if r0 > r:
                ranges.push_back(Range(r, r0, 0))

        if not changed:
            result.push_back(curr)

    return result


def part2():
    let path = Path("../data/day5/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    let seeds = lines[0].split("seeds: ")[1].split(" ")
    var seed_ranges = DynamicVector[Range]()
    for i in range(0, len(seeds), 2):
        let b = atol(seeds[i])
        let s = atol(seeds[i + 1])
        seed_ranges.push_back(Range(b, b + s, 0))

    var pos = 3
    var ranges = DynamicVector[Range]()

    for i in range(7):
        get_range2(lines, pos, ranges)
        let curr_res = update_ranges(seed_ranges, ranges)
        seed_ranges = curr_res

    var min = seed_ranges[0].b
    for i in range(1, len(seed_ranges)):
        min = seed_ranges[i].b if seed_ranges[i].b < min else min
    return min


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
