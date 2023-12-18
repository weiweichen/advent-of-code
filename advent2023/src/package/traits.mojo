trait Sortable(CollectionElement):
    fn __lt__(self, rhs: Self) -> Bool:
        pass

    fn __le__(self, rhs: Self) -> Bool:
        pass

trait Hashable:
    fn hash(borrowed self: Self) -> Int:
        pass


trait Equitable:
    fn __eq__(self, rhs: Self) -> Bool:
        pass

trait HashKeyT(Hashable, Equitable, CollectionElement):
    pass
