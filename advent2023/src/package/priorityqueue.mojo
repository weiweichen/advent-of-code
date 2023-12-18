from package.traits import Sortable, HashKeyT


struct PriorityQueue[T: Sortable]:
    var data: DynamicVector[T]
    var size: Int
    var capacity: Int

    fn __init__(inout self: Self, capacity: Int):
        self.capacity = capacity
        self.size = 0
        self.data = DynamicVector[T](capacity)

    fn top(inout self: Self) -> T:
        return self.data[0]

    fn adjust_down(inout self: Self, idx: Int):
        if idx >= self.size:
            return

        var curr = idx
        while True:
            let curr_value = self.data[curr]
            var left_value = self.data[curr]
            var right_value = self.data[curr]
            let left = curr * 2 + 1
            let right = curr * 2 + 2
            if left >= self.size:
                # leaf node
                break

            if left < self.size:
                left_value = self.data[left]
            if right < self.size:
                right_value = self.data[right]
            if curr_value <= left_value and curr_value <= right_value:
                break
            elif curr_value <= left_value:
                # adjust right
                self.data[curr] = right_value
                self.data[right] = curr_value
                curr = right
            elif curr_value <= right_value:
                # curr_value > left_value and curr_value <= right_value
                self.data[curr] = left_value
                self.data[left] = curr_value
                curr = left
            else:
                # curr_value > left_value and curr_value > right_value
                if left_value < right_value:
                    self.data[curr] = left_value
                    self.data[left] = curr_value
                    curr = left
                else:
                    self.data[curr] = right_value
                    self.data[right] = curr_value
                    curr = right

    fn adjust_up(inout self: Self, idx: Int):
        var curr = idx
        while True:
            if curr <= 0:
                break

            let parent: Int = (curr - 1) // 2
            let curr_value = self.data[curr]
            let parent_value = self.data[parent]
            if curr_value < parent_value:
                self.data[parent] = curr_value
                self.data[curr] = parent_value
            curr = parent

    fn pop_front(inout self: Self):
        self.data[0] = self.data[self.size - 1]
        self.size -= 1
        self.adjust_down(0)

    fn push(inout self: Self, value: T):
        if self.size + 1 > self.capacity:
            self.capacity *= 2
            var new_data = DynamicVector[T](self.capacity * 2)
            for i in range(self.size):
                new_data.push_back(self.data[i])
            self.data = new_data

        self.data[self.size] = value
        self.size += 1

        self.adjust_up(self.size - 1)
