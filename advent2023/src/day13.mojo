from math import gcd, lcm, max, min
from pathlib import Path
from package.hashmap import HashKeyT, HashMap


struct MapKey(HashKeyT):
    var key: String

    fn __init__(inout self: Self, borrowed key: String):
        self.key = key

    fn hash(borrowed self: Self) -> Int:
        var result = 0
        for i in range(len(self.key)):
            let ch = self.key[i]
            if ch == ".":
                result = result * 2 + 1
            else:
                result = result * 2
        return result

    fn __eq__(self, rhs: Self) -> Bool:
        return self.key == rhs.key

    fn __copyinit__(inout self, existing: Self):
        self.key = existing.key

    fn __moveinit__(inout self, owned existing: Self):
        self.key = existing.key


fn find_mirror(borrowed lava: DynamicVector[String]) raises -> Int:
    var map = HashMap[MapKey, Int](3042161)
    var count = 0
    var row_counts = DynamicVector[Int]()
    row_counts.resize(len(lava), -1)

    for i in range(len(lava)):
        if map.contains(lava[i]):
            row_counts[i] = map[lava[i]]
        else:
            row_counts[i] = count
            _ = map.insert_or_update(lava[i], count)
            count += 1

    # print_no_newline("row counts: ")
    # for i in range(len(lava)):
    #     print_no_newline(row_counts[i])
    #     print_no_newline(" ")
    # print("")

    for i in range(len(lava) - 1):
        if row_counts[i] == row_counts[i + 1]:
            let bound = i + 1 if i < int(len(lava) / 2) else len(lava) - i - 1
            for j in range(1, bound):
                if lava[i - j] != lava[i + 1 + j]:
                    break
            else:
                return i + 1

    return 0


fn rotate(borrowed lava: DynamicVector[String]) -> DynamicVector[String]:
    var result = DynamicVector[String]()
    for j in range(len(lava[0])):
        var row = String("")
        for i in range(len(lava)):
            row += lava[i][j]
        result.push_back(row)
    return result


fn is_one_off(borrowed lava: DynamicVector[String], i: Int, j: Int) -> Bool:
    let r1 = lava[i]
    let r2 = lava[j]
    var count = 0
    for i in range(len(r1)):
        if r1[i] != r2[i]:
            count += 1
        if count > 1:
            return False
    return count == 1


fn find_mirror_with_smudge(borrowed lava: DynamicVector[String]) raises -> Int:
    var map = HashMap[MapKey, Int](3042161)
    var count = 0
    var row_counts = DynamicVector[Int]()
    row_counts.resize(len(lava), -1)

    for i in range(len(lava)):
        if map.contains(lava[i]):
            row_counts[i] = map[lava[i]]
        else:
            row_counts[i] = count
            _ = map.insert_or_update(lava[i], count)
            count += 1

    # print_no_newline("row counts: ")
    # for i in range(len(lava)):
    #     print_no_newline(row_counts[i])
    #     print_no_newline(" ")
    # print("")

    for i in range(len(lava) - 1):
        let bound = i + 1 if i < int(len(lava) / 2) else len(lava) - i - 1
        var one_off_count = 0
        for j in range(0, bound):
            let r1 = i - j
            let r2 = i + 1 + j
            let one_off = is_one_off(lava, r1, r2)
            one_off_count += 1 if one_off else 0
            if lava[i - j] != lava[i + 1 + j] and (
                one_off_count > 1 or not one_off
            ):
                break
        else:
            if one_off_count == 1:
                # print(String("mirror_with_smudge found: ") + (i + 1))
                return i + 1

    return 0


def part1():
    let path = Path("../data/day13/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    var i = 0
    while i < len(lines):
        var lava = DynamicVector[String]()
        while len(lines[i]) != 0:
            lava.push_back(lines[i])
            i += 1

        i += 1
        result += find_mirror(lava) * 100
        result += find_mirror(rotate(lava))

    return result


def part2():
    let path = Path("../data/day13/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var result = 0
    var i = 0
    while i < len(lines):
        var lava = DynamicVector[String]()
        while len(lines[i]) != 0:
            lava.push_back(lines[i])
            i += 1

        i += 1
        result += find_mirror_with_smudge(lava) * 100
        result += find_mirror_with_smudge(rotate(lava))

    return result


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
