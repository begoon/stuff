from heapq import merge

data = [[1, 2], [4, 5, 6], [3]]
print(list(merge(*data)))

# To prove that the solution is lazy -- "n" is called only once for each number.


def n(v):
    print(f'n({v})')
    return v


data = [[n(1), n(2)], [n(4), n(5), n(6)], [n(3)]]
print(list(merge(*data)))
