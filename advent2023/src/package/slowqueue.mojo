struct SlowQueue[T: CollectionElement]:
    var head: Int
    var tail: Int
    var capacity: Int
    var size: Int
    var data: DynamicVector[T]

    fn __init__(inout self: Self, capacity: Int):
        self.head = 0
        self.tail = 0
        self.capacity = capacity
        self.size = 0
        self.data = DynamicVector[T](capacity)

    fn push_back(inout self: Self, data: T):
        if self.tail == self.capacity - 1:
            if self.size * 2 > self.capacity:
                # resize buffer
                let tmp = self.data
                self.capacity *= 2
                self.data.reserve(self.capacity * 2)
                # shift data
                for i in range(self.head, self.tail):
                    self.data[i - self.head] = tmp[i]
            else:
                for i in range(self.head, self.tail):
                    self.data[i - self.head] = self.data[i]

            self.head = 0
            self.tail = self.size
            self.data[self.tail] = data
            self.tail += 1
        else:
            self.data[self.tail] = data
            self.tail += 1
        self.size += 1

    fn front(inout self: Self) raises -> T:
        if self.size <= 0:
            raise Error("Queue is empty.")

        return self.data[self.head]

    fn empty(inout self: Self) -> Bool:
        return self.size == 0

    fn pop_front(inout self: Self) raises:
        if self.size <= 0:
            raise Error("Queue is empty.")
        self.head += 1
        self.size -= 1

    fn clear(inout self: Self):
        self.head = 0
        self.tail = 0
        self.size = 0
