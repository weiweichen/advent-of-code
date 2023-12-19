from math import gcd, lcm, max, min
from pathlib import Path
from package.hasher import StringHasher
from package.hashmap import HashMap
from package.slowqueue import SlowQueue
from package.traits import HashKeyT, Equitable

struct Part(CollectionElement, Stringable):
    var values: DynamicVector[Int]

    fn __init__(inout self: Self, borrowed line: String) raises:
        self.values = DynamicVector[Int]()
        self.values.resize(4, 0)

        let group = line.split("{")[1].split("}")[0].split(",")
        for i in range(len(group)):
            let pair = group[i].split("=")
            let v = atol(pair[1])
            if pair[0] == "x":
                self.values[0] = v
            elif pair[0] == "m":
                self.values[1] = v
            elif pair[0] == "a":
                self.values[2] = v
            elif pair[0] == "s":
                self.values[3] = v

    fn __init__(inout self: Self, x: Int, m: Int, a: Int, s: Int):
        self.values = DynamicVector[Int]()
        self.values.resize(4, 0)
        self.values[0] = x
        self.values[1] = m
        self.values[2] = a
        self.values[3] = s

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.values = existing.values ^

    fn __copyinit__(inout self: Self, existing: Self):
        self.values = existing.values

    fn __str__(self: Self) -> String:
        return (
            String("x: ")
            + self.values[0]
            + " m: "
            + self.values[1]
            + " a: "
            + self.values[2]
            + " s: "
            + self.values[3]
        )
    fn sum(borrowed self: Self) -> Int:
        return self.values[0] + self.values[1] + self.values[2] + self.values[3]


struct Clause(CollectionElement, Stringable):
    var category: Int
    var op: String
    var bound: Int
    var dest: String

    fn __init__(inout self: Self, borrowed line: String) raises:
        let s0 = line.split(":")
        self.bound = 0
        self.op = ""
        self.category = -1
        self.dest = line

        if len(s0) == 2:
            self.dest = s0[1]
            let s1 = s0[0].split("<")
            if len(s1) == 2:
                self.category = 0 if s1[0] == "x" else (1 if s1[0] == "m" else (2 if s1[0] == "a" else 3))
                self.op = "<"
                self.bound = atol(s1[1])
                return

            let s2 = s0[0].split(">")
            if len(s2) == 2:
                self.category = 0 if s2[0] == "x" else (1 if s2[0] == "m" else (2 if s2[0] == "a" else 3))
                self.op = "<"
                self.op = ">"
                self.bound = atol(s2[1])
            else:
                raise Error("Can't create Clause from: " + line)

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.category = existing.category
        self.op = existing.op ^
        self.bound = existing.bound
        self.dest = existing.dest ^

    fn __copyinit__(inout self: Self, existing: Self):
        self.category = existing.category
        self.op = existing.op
        self.bound = existing.bound
        self.dest = existing.dest

    fn __str__(self: Self) -> String:
        if self.category != -1:
            var c: String = ""
            if self.category == 0:
                c = "x"
            elif self.category == 1:
                c = "m"
            elif self.category == 2:
                c = "a"
            else:
                c = "s"

            return c
                + self.op
                + self.bound
                + " dest: "
                + self.dest

        return String("dest: ") + self.dest

    fn ok(self: Self, borrowed part: Part) -> Bool:
        if self.category == -1:
            return True
        let v = part.values[self.category]
        if self.op == ">":
            return v > self.bound
        return v < self.bound

    fn apply_ranges(borrowed self: Self, borrowed input: Ranges) -> Ranges:
        var result = input
        if self.category != -1:
            if self.op == "<":
                if result.ranges[self.category].upper > self.bound:
                    result.ranges[self.category].upper = self.bound
                    result.name = self.dest
            else:
                if result.ranges[self.category].lower < self.bound + 1:
                    result.ranges[self.category].lower = self.bound + 1
                    result.name = self.dest
        else:
            result.name = self.dest
        return result

    fn apply_counter_range(borrowed self: Self, inout input: Ranges) -> Ranges:
        var result = input
        if self.op == "<":
            if result.ranges[self.category].lower < self.bound:
                result.ranges[self.category].lower = self.bound
        else:
            if result.ranges[self.category].upper > self.bound + 1:
                result.ranges[self.category].upper = self.bound + 1
        return result


struct Rule(CollectionElement, Stringable):
    var name: String
    var clauses: DynamicVector[Clause]

    fn __init__(inout self: Self, borrowed line: String) raises:
        let s = line.split("{")
        self.name = s[0]
        let clauses_str = s[1].split("}")[0].split(",")
        self.clauses = DynamicVector[Clause]()

        for i in range(len(clauses_str)):
            self.clauses.push_back(Clause(clauses_str[i]))

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.name = existing.name ^
        self.clauses = existing.clauses ^

    fn __copyinit__(inout self: Self, existing: Self):
        self.name = existing.name
        self.clauses = existing.clauses

    fn __str__(self: Self) -> String:
        var result = self.name + ": \n"
        for i in range(len(self.clauses)):
            result += self.clauses[i].__str__() + "\n"
        return result


fn check_part(borrowed part: Part, inout rules: HashMap[StringHasher, Rule]) raises -> Bool:
    var curr: String = "in"

    while curr != "R" and curr != "A":
        let rule = rules[curr]
        for i in range(len(rule.clauses)):
            let clause = rule.clauses[i]
            if clause.ok(part):
                curr = clause.dest
                break

    return curr == "A"

def part1():
    let path = Path("../data/day19/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var rules = HashMap[StringHasher, Rule](3042161)
    var i: Int = 0
    while i < len(lines):
        if len(lines[i]) == 0:
            i += 1
            break
        let rule = Rule(lines[i])
        rules.insert_or_update(rule.name, rule)
        # print(rule)
        i += 1

    var count = 0
    var result: Int = 0
    while i < len(lines) - 1:
        let part = Part(lines[i])
        #print(part)
        if(check_part(part, rules)):
            result += part.sum()
        i += 1
        count += 1
    return result


struct Range(CollectionElement, Stringable, Equitable):
    var lower: Int # inclusive
    var upper: Int # not inclusive

    fn __init__(inout self: Self, lower: Int, upper: Int):
        self.lower = lower
        self.upper = upper

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.lower = existing.lower
        self.upper = existing.upper

    fn __copyinit__(inout self: Self, existing: Self):
        self.lower = existing.lower
        self.upper = existing.upper

    fn __str__(borrowed self: Self) -> String:
        return String("[") + self.lower + " , " + self.upper + "]"

    fn is_valid(borrowed self: Self) -> Bool:
        return self.lower < self.upper

    fn __eq__(self, rhs: Self) -> Bool:
        return self.lower == rhs.lower and self.upper == rhs.upper

struct Ranges(CollectionElement, Stringable, Equitable):
    var name: String
    var ranges: DynamicVector[Range]

    fn __init__(inout self: Self, borrowed name: String, borrowed ranges: StaticTuple[4, StaticIntTuple[2]]):
        self.name = name
        self.ranges = DynamicVector[Range]()

        for i in range(len(ranges)):
            self.ranges.push_back(Range(ranges[i][0], ranges[i][1]))

    fn __moveinit__(inout self: Self, owned exisiting: Self):
        self.name = exisiting.name ^
        self.ranges = exisiting.ranges ^

    fn __copyinit__(inout self: Self, exisiting: Self):
        self.name = exisiting.name
        self.ranges = exisiting.ranges

    fn __str__(borrowed self: Self) -> String:
        var result = self.name + ": "

        for i in range(len(self.ranges)):
            result += self.ranges[i].__str__()

        return result

    fn is_valid(borrowed self: Self) -> Bool:
        for i in range(len(self.ranges)):
            if not self.ranges[i].is_valid():
                return False
        return True

    fn __eq__(self, rhs: Self) -> Bool:
        if self.name != rhs.name:
            return False
        for i in range(len(self.ranges)):
            if not (self.ranges[i] == rhs.ranges[i]):
                return False
        return True

    fn num(borrowed self: Self) -> Int:
        var result: Int = 1
        for i in range(len(self.ranges)):
            result *= (self.ranges[i].upper - self.ranges[i].lower)
        return result


fn apply_ranges(borrowed ranges: Ranges, inout rules: HashMap[StringHasher, Rule], inout work_list: SlowQueue[Ranges]) raises:
    let rule = rules[ranges.name]
    var curr_ranges = ranges

    for i in range(len(rule.clauses)):
        let clause = rule.clauses[i]
        let r0 = clause.apply_ranges(curr_ranges)
        if r0.name != "R" and r0.is_valid():
            work_list.push_back(r0)

        if clause.category == -1:
            break

        curr_ranges = clause.apply_counter_range(curr_ranges)
        if not curr_ranges.is_valid():
            break


def part2():
    let path = Path("../data/day19/part2.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var rules = HashMap[StringHasher, Rule](3042161)
    var i: Int = 0
    while i < len(lines):
        if len(lines[i]) == 0:
            i += 1
            break
        let rule = Rule(lines[i])
        rules.insert_or_update(rule.name, rule)
        # print(rule)
        i += 1

    var work_list = SlowQueue[Ranges](1000000)
    let init_range = Ranges("in", StaticTuple[4, StaticIntTuple[2]]((1, 4001), (1, 4001), (1, 4001), (1, 4001)))
    work_list.push_back(init_range)

    var result: Int = 0
    while work_list.size != 0:
        let curr = work_list.front()
        work_list.pop_front()
        if not curr.is_valid():
            continue
        #print(curr)

        if curr.name == "A":
            result += curr.num()
            continue
        apply_ranges(curr, rules, work_list)

    return result

fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
