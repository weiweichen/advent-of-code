
from package.traits import HashKeyT

struct HashSet[K: HashKeyT]:
    var data: DynamicVector[DynamicVector[K]]
    var capacity: Int
    var size: Int
    var vec: DynamicVector[K]

    fn __init__(inout self: Self, capacity: Int):
        self.capacity = capacity
        self.data = DynamicVector[DynamicVector[K]]()
        self.data.resize(self.capacity, DynamicVector[K]())
        self.size = 0
        self.vec = DynamicVector[K]()

    fn __copyinit__(inout self: Self, existing: Self):
        self.capacity = existing.capacity
        self.data = existing.data
        self.size = existing.size
        self.vec = existing.vec

    # return False => update
    # return True => insert
    fn _rehash(borrowed self: Self, value: Int) -> Int:
        return value % self.capacity

    fn insert(inout self: Self, key: K) -> Bool:
        let hash = self._rehash(key.hash())
        for i in range(len(self.data[hash])):
            if self.data[hash][i] == key:
                return False

        self.data[hash].push_back(key)
        self.vec.push_back(key)
        self.size += 1
        return True

    fn contains(borrowed self, key: K) -> Bool:
        let hash = self._rehash(key.hash())
        for i in range(len(self.data[hash])):
            if self.data[hash][i] == key:
                return True
        return False

    fn __len__(self) -> Int:
        return self.size

    fn clear(inout self: Self):
        # this is super inefficient btw
        self.data = DynamicVector[DynamicVector[K]]()
        self.data.resize(self.capacity, DynamicVector[K]())
        self.vec.clear()
        self.size = 0

    fn erase(inout self: Self, key: K):
        let hash = self._rehash(key.hash())
        for i in range(len(self.data[hash])):
            if self.data[hash][i] == key:
                for j in range(i + 1, len(self.data[hash])):
                    self.data[hash][j-1] = self.data[hash][j]
                _ = self.data[hash].pop_back()
        # fix self.vec

    fn content(borrowed self: Self)->DynamicVector[K]:
        return self.vec

    fn intersect(borrowed self: Self, borrowed other: Self) -> HashSet[K]:
        var result = HashSet[K](self.capacity)
        let other_content = other.content()
        for i in range(len(other_content)):
            let v = other_content[i]
            if self.contains(v):
                _ = result.insert(v)
        return result
