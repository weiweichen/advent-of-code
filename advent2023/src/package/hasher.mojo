from package.traits import HashKeyT

struct StringHasher(HashKeyT):
    var data: String

    fn __init__(inout self: Self, borrowed data: String):
        self.data = data

    fn __moveinit__(inout self: Self, owned existing: Self):
        self.data = existing.data ^

    fn __copyinit__(inout self: Self, existing: Self):
        self.data = existing.data

    fn __eq__(self, rhs: Self) -> Bool:
        return self.data == rhs.data

    fn hash(borrowed self: Self) -> Int:
        let p: Int = 31
        let m: Int = 10000009

        var hash_value: Int = 0
        var p_pow: Int = 1
        for i in range(len(self.data)):
            hash_value = (hash_value + int(ord(self.data[i]) * p_pow)) % m
            p_pow = (p_pow * p) % m

        return hash_value

@value
struct IntHasher(HashKeyT):
    var data: Int

    fn __init__(inout self: Self, borrowed data: Int):
        self.data = data

    fn __eq__(self, rhs: Self) -> Bool:
        return self.data == rhs.data

    fn hash(borrowed self: Self) -> Int:
        return self.data
