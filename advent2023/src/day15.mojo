from math import gcd, lcm, max, min
from pathlib import Path
from package.hashmap import HashMapKeyT, HashMap


fn holiday_hash(borrowed sequence: String) -> Int:
    var result = 0
    for i in range(len(sequence)):
        let v = ord(sequence[i])
        result += v
        result *= 17
        result %= 256
    return result


struct Step(Stringable):
    var label: String
    var sign: String
    var value: Int

    fn __init__(inout self: Self, borrowed sequence: String) raises:
        if sequence[len(sequence) - 1] == "-":
            self.label = sequence.split("-")[0]
            self.sign = "-"
            self.value = 0
        else:
            let s = sequence.split("=")
            self.label = s[0]
            self.sign = "="
            self.value = atol(s[1])

    fn __str__(self: Self) -> String:
        if self.sign == "-":
            return self.label + self.sign
        else:
            return self.label + self.sign + self.value


struct Box(CollectionElement, Stringable):
    var label: String
    var value: Int

    fn __init__(inout self: Self, borrowed label: String, value: Int) raises:
        self.label = label
        self.value = value

    fn __moveinit__(inout self: Self, owned existing: Self, /):
        self.label = existing.label
        self.value = existing.value

    fn __copyinit__(inout self: Self, existing: Self, /):
        self.label = existing.label
        self.value = existing.value

    fn __str__(self: Self) -> String:
        return String("[") + self.label + " " + self.value + "] "


fn action(
    inout boxes: DynamicVector[DynamicVector[Box]], borrowed step: Step
) raises:
    let hash = holiday_hash(step.label)
    for i in range(len(boxes[hash])):
        if boxes[hash][i].label == step.label:
            if step.sign == "-":
                for j in range(i, len(boxes[hash]) - 1):
                    boxes[hash][j] = boxes[hash][j + 1]
                _ = boxes[hash].pop_back()
                if len(boxes[hash]) == 0:
                    # This shouldn't be needed but
                    # DynamicVector[DynamicVector[T]] has
                    # a bug right now and it crashes later
                    # with push_back if I don't reinitialzed here.
                    boxes[hash] = DynamicVector[Box]()
            else:
                boxes[hash][i].value = step.value
            break
    else:
        if step.sign == "=":
            boxes[hash].push_back(Box(step.label, step.value))


fn print_boxes(borrowed boxes: DynamicVector[DynamicVector[Box]]):
    for i in range(len(boxes)):
        for j in range(len(boxes[i])):
            let value = boxes[i][j].value
            if value != -1:
                print_no_newline(boxes[i][j])
        if len(boxes[i]) > 0:
            print("")


def part1():
    let path = Path("../data/day15/part1.txt")
    let text = path.read_text()
    let sequences = text.split("\n")[0].split(",")

    var result = 0
    for i in range(len(sequences)):
        result += holiday_hash(sequences[i])

    return result


def part2():
    let path = Path("../data/day15/part2.txt")
    let text = path.read_text()
    let sequences = text.split("\n")[0].split(",")

    var result = 0
    var boxes = DynamicVector[DynamicVector[Box]]()
    for i in range(256):
        boxes.push_back(DynamicVector[Box]())

    for i in range(len(sequences)):
        let step = Step(sequences[i])
        action(boxes, step)
        # print_boxes(boxes)
        # print_no_newline(step)
        # print(String(": ") + hash)

    for i in range(len(boxes)):
        for j in range(len(boxes[i])):
            let value = boxes[i][j].value
            if value != -1:
                let curr = (i + 1) * (j + 1) * value
                result += curr

    return result


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
