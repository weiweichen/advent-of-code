from math import gcd, lcm, max, min
from pathlib import Path
from package.hasher import IntHasher, StringHasher
from package.hashmap import HashMap
from package.hashset import HashSet
from package.slowqueue import SlowQueue
from package.traits import HashKeyT, Equitable


@value
struct Node(CollectionElement, Stringable):
    var name: String
    var neighbours: DynamicVector[Int]
    var id: Int

    fn __init__(inout self: Self, borrowed name: String, id: Int):
        self.name = name
        self.id = id
        self.neighbours = DynamicVector[Int]()

    fn __str__(borrowed self: Self) -> String:
        var result = String("")
        result += "name: " + self.name + " id: " + self.id
        result += " neighbours: ["
        for i in range(len(self.neighbours)):
            result += self.neighbours[i]
            if i < len(self.neighbours) - 1:
                result += ", "
        result += "]"
        return result


fn min_vector(
    borrowed v1: DynamicVector[Int], borrowed v2: DynamicVector[Int]
) -> DynamicVector[Int]:
    var i = 0
    var j = 0
    while i < len(v1) and j < len(v2):
        if v1[i] < v2[j]:
            return v1
        if v1[i] > v2[j]:
            return v2
        i += 1
        j += 1
    if i == len(v1):
        return v1
    return v2


fn global_min_cut(
    inout graph: DynamicVector[DynamicVector[Int]],
) -> DynamicVector[Int]:
    var result = DynamicVector[Int]()
    let int_max: Int = 2147483647
    let int_min: Int = -2147483648
    var best: Int = int_max
    let n = len(graph)
    var co = DynamicVector[DynamicVector[Int]]()
    co.resize(n, DynamicVector[Int]())
    for i in range(n):
        co[i].push_back(i)

    for ph in range(1, n):
        print(ph)
        var s: Int = 0
        var t: Int = 0
        var w = graph[0]

        for it in range(0, n - ph):
            w[t] = int_min
            s = t
            var max_element = w[0]
            var max_pos: Int = 0
            for i in range(1, len(w)):
                if max_element < w[i]:
                    max_element = w[i]
                    max_pos = i
            t = max_pos

            for i in range(n):
                w[i] += graph[t][i]

        if best > w[t] - graph[t][t]:
            best = w[t] - graph[t][t]
            result = co[t]
        elif best == w[t] - graph[t][t]:
            result = min_vector(result, co[t])

        for i in range(len(co[t])):
            co[s].push_back(co[t][i])

        for i in range(n):
            graph[s][i] += graph[t][i]

        for i in range(n):
            graph[i][s] = graph[s][i]

        graph[0][t] = int_min

    return result


def part1():
    # Took very long time to finish :open-eye-laughing
    #    1177.56 real      1155.05 user         1.64 sys
    let path = Path("../data/day25/part1.txt")
    let text = path.read_text()
    let lines = text.split("\n")

    var name_id_map = HashMap[StringHasher, Int](1997)
    var nodes = DynamicVector[Node]()
    var count: Int = 0
    for i in range(len(lines) - 1):
        let s = lines[i].split(": ")
        if not name_id_map.contains(s[0]):
            let node = Node(s[0], count)
            name_id_map.insert_or_update(node.name, count)
            nodes.push_back(node)
            count += 1

        let n = s[1].split(" ")
        for j in range(len(n)):
            if not name_id_map.contains(n[j]):
                let node = Node(n[j], count)
                name_id_map.insert_or_update(node.name, count)
                nodes.push_back(node)
                count += 1

    for i in range(len(lines) - 1):
        let s = lines[i].split(": ")
        let name = s[0]
        let n = s[1].split(" ")
        let curr_id = name_id_map[name]
        for i in range(len(n)):
            nodes[curr_id].neighbours.push_back(name_id_map[n[i]])

    for i in range(len(nodes)):
        print(nodes[i])
    print(len(nodes))

    var adj = DynamicVector[DynamicVector[Int]]()
    var init = DynamicVector[Int]()
    init.resize(len(nodes), 0)
    adj.resize(len(nodes), init)

    for i in range(len(nodes)):
        let node = nodes[i]
        let id = node.id
        for j in range(len(node.neighbours)):
            adj[id][node.neighbours[j]] = 1
            adj[node.neighbours[j]][id] = 1

    # for i in range(len(nodes)):
    #    var str = String("")
    #    for j in range(len(nodes)):
    #        str += adj[i][j]
    #        str += " "
    #    print(str)

    let cut = global_min_cut(adj)
    return len(cut) * (len(nodes) - len(cut))


def part2():
    let path = Path("../data/day25/part2_test.txt")
    let text = path.read_text()
    let lines = text.split("\n")


fn main():
    try:
        let answer1 = part1()
        print(answer1)
        # let answer2 = part2()
        # print(answer2)

    except e:
        print(e)
