
from package.traits import HashKeyT

struct HashSet[K: HashKeyT]:
    var data: DynamicVector[DynamicVector[K]]
    var capacity: Int
    var size: Int

    fn __init__(inout self: Self, capacity: Int):
        self.capacity = capacity
        self.data = DynamicVector[DynamicVector[K]]()
        self.data.resize(self.capacity, DynamicVector[K]())
        self.size = 0

    # return False => update
    # return True => insert
    fn _rehash(inout self: Self, value: Int) -> Int:
        return value % self.capacity

    fn insert(inout self: Self, key: K) -> Bool:
        let hash = self._rehash(key.hash())
        for i in range(len(self.data[hash])):
            if self.data[hash][i] == key:
                return False

        self.data[hash].push_back(key)
        self.size += 1
        return True

    fn contains(inout self, key: K) -> Bool:
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
        self.size = 0
