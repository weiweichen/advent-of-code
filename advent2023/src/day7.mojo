from pathlib import Path
from package.quicksort import quicksort, Sortable


struct Cards(Sortable):
    var card: String
    var points: Int
    var has_wild_card: Bool

    fn __init__(
        inout self: Self, card: String, points: Int, has_wild_card: Bool
    ):
        self.card = card
        self.points = points
        self.has_wild_card = has_wild_card

    fn __moveinit__(inout self, owned existing: Self):
        self.card = existing.card
        self.points = existing.points
        self.has_wild_card = existing.has_wild_card

    fn __copyinit__(inout self, existing: Self):
        self.card = existing.card
        self.points = existing.points
        self.has_wild_card = existing.has_wild_card

    fn __lt__(self, rhs: Self) -> Bool:
        for i in range(len(self.card)):
            if self.card[i] != rhs.card[i]:
                return self.get_idx(i) < rhs.get_idx(i)
        return False

    fn __le__(self, rhs: Self) -> Bool:
        for i in range(len(self.card)):
            if self.card[i] != rhs.card[i]:
                return self.get_idx(i) < rhs.get_idx(i)
        return True

    fn get_idx(borrowed self: Cards, idx: Int) -> Int:
        let ch = self.card[idx]
        if isdigit(ord(ch)):
            return ord(ch) - 48

        if ch == "T":
            return 10
        if ch == "J":
            if self.has_wild_card:
                return 0
            else:
                return 11
        if ch == "Q":
            return 12
        if ch == "K":
            return 13
        if ch == "A":
            return 14
        return 15

    fn get_rank(borrowed self: Cards) -> Int:
        var occurences = StaticIntTuple[16](
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        )

        for i in range(0, len(self.card)):
            let idx: Int = self.get_idx(i)
            occurences[idx] += 1

        var counts = StaticIntTuple[6](0, 0, 0, 0, 0, 0)

        let b = 1 if self.has_wild_card else 0
        for i in range(b, len(occurences)):
            let v = occurences[i]
            if v == 0:
                continue
            counts[v] += 1

        if not self.has_wild_card:
            if counts[5] == 1:
                return 6
            if counts[4] == 1:
                return 5
            if counts[3] == 1 and counts[2] == 1:
                return 4
            if counts[3] == 1:
                return 3
            if counts[2] == 2:
                return 2
            if counts[2] == 1:
                return 1

        else:
            if counts[5] == 1:
                return 6
            if counts[4] == 1 and occurences[0] == 1:
                return 6
            if counts[3] == 1 and occurences[0] == 2:
                return 6
            if counts[2] == 1 and occurences[0] == 3:
                return 6
            if occurences[0] >= 4:
                return 6

            if counts[4] == 1:
                return 5
            if counts[3] == 1 and occurences[0] == 1:
                return 5
            if counts[2] == 1 and occurences[0] == 2:
                return 5
            if occurences[0] >= 3:
                return 5

            if counts[3] == 1 and counts[2] == 1:
                return 4
            if counts[2] == 2 and occurences[0] == 1:
                return 4
            if counts[2] == 1 and occurences[0] == 2:
                return 4

            if counts[3] == 1:
                return 3
            if counts[2] == 1 and occurences[0] == 1:
                return 3
            if occurences[0] >= 2:
                return 3

            if counts[2] == 2:
                return 2
            if counts[2] == 1 and occurences[0] == 1:
                return 2

            if counts[2] == 1:
                return 1
            if occurences[0] > 0:
                return 1

        return 0


def solve(has_wild_card: Bool, input_file_path: String):
    let path = Path(input_file_path)
    let text = path.read_text()
    let lines = text.split("\n")

    var ranked = DynamicVector[DynamicVector[Cards]]()
    for i in range(7):
        ranked.push_back(DynamicVector[Cards]())

    for i in range(len(lines) - 1):
        let v = lines[i].split(" ")
        let card = Cards(v[0], atol(v[1]), has_wild_card)
        let rank = card.get_rank()
        ranked[rank].push_back(card)

    var result = 0
    var r = 1
    for i in range(len(ranked)):
        quicksort[Cards](ranked[i], 0, len(ranked[i]) - 1)
        let rank = ranked[i]
        for j in range(len(rank)):
            let card = rank[j]
            result += r * card.points
            r += 1
    return result


def part1():
    return solve(False, "../data/day7/part1.txt")


def part2():
    return solve(True, "../data/day7/part2.txt")


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        let answer2 = part2()
        print(answer2)

    except e:
        print(e)
