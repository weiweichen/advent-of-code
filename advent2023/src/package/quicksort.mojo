trait Sortable(CollectionElement):
    fn __lt__(self, rhs: Self) -> Bool:
        pass


fn _swap[
    type: CollectionElement
](inout vector: DynamicVector[type], i: Int, j: Int):
    let tmp = vector[j]
    vector[j] = vector[i]
    vector[i] = tmp


fn _partition[
    type: Sortable
](inout vector: DynamicVector[type], low: Int, high: Int) -> Int:
    let pivot = vector[high]
    var i = (low - 1)

    for j in range(low, high + 1):
        if vector[j] < pivot:
            i += 1
            _swap(vector, i, j)

    _swap(vector, i + 1, high)
    return i + 1


fn quicksort[
    type: Sortable
](inout vector: DynamicVector[type], low: Int, high: Int):
    if low >= high:
        return

    let pi = _partition[type](vector, low, high)
    quicksort[type](vector, low, pi - 1)
    quicksort[type](vector, pi + 1, high)
