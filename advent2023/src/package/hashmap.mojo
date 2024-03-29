from package.traits import HashKeyT

struct _HashMapDataElementType[K: HashKeyT, V: Copyable](CollectionElement):
    var key: K
    var value: V

    fn __init__(inout self: Self, key: K, value: V):
        self.key = key
        self.value = value

    fn __moveinit__(inout self, owned existing: Self):
        self.key = existing.key
        self.value = existing.value

    fn __copyinit__(inout self, existing: Self):
        self.key = existing.key
        self.value = existing.value


struct HashMap[K: HashKeyT, V: Copyable]:
    var data: DynamicVector[DynamicVector[_HashMapDataElementType[K, V]]]
    var capacity: Int
    var size: Int

    fn __init__(inout self: Self, capacity: Int):
        self.capacity = capacity
        self.data = DynamicVector[
            DynamicVector[_HashMapDataElementType[K, V]]
        ]()
        self.data.resize(
            self.capacity, DynamicVector[_HashMapDataElementType[K, V]]()
        )
        self.size = 0

    # return False => update
    # return True => insert
    fn _rehash(inout self: Self, value: Int) -> Int:
        return value % self.capacity

    fn insert_or_update(inout self: Self, key: K, value: V) -> Bool:
        let hash = self._rehash(key.hash())
        for i in range(len(self.data[hash])):
            let pair = self.data[hash][i]
            if pair.key == key:
                # key already exist
                self.data[hash][i].value = value
                return False

        self.data[hash].push_back(_HashMapDataElementType(key, value))
        self.size += 1
        return True

    fn contains(inout self, key: K) -> Bool:
        let hash = self._rehash(key.hash())

        for i in range(len(self.data[hash])):
            let pair = self.data[hash][i]
            if pair.key == key:
                return True
        return False

    fn __getitem__(inout self: Self, key: K) raises -> V:
        let hash = self._rehash(key.hash())
        for i in range(len(self.data[hash])):
            let pair = self.data[hash][i]
            if pair.key == key:
                return self.data[hash][i].value

        raise Error("map doesn't have the key")

    fn find(inout self: Self, key: K) raises -> _HashMapDataElementType[K, V]:
        let hash = self._rehash(key.hash())

        for i in range(len(self.data[hash])):
            let pair = self.data[hash][i]
            if pair.key == key:
                return __get_address_as_lvalue((self.data[hash].data + i).value)

        raise Error("map doesn't have the key")


    fn __len__(self) -> Int:
        return self.size

    fn clear(inout self: Self):
        # this is super inefficient btw
        self.data = DynamicVector[
            DynamicVector[_HashMapDataElementType[K, V]]
        ]()
        self.data.resize(
            self.capacity, DynamicVector[_HashMapDataElementType[K, V]]()
        )
        self.size = 0
