# Some example solutions for backtracking problems in Third Python Problem Collection.

from fractions import Fraction

# Cutting sticks

def cutting_sticks(sticks):
    s, n = sum(sticks), 0
    while n * (n + 1) < 2 * s:
        n += 1
    assert (n * (n + 1)) == 2 * s

    best = n * n * n
    def recurse(cuts, k, cost):
        nonlocal best
        if cost >= best:
            return
        if k == 0:
            best = cost
        else:
            for i, s in enumerate(sticks):
                if s >= k:
                    if sticks[i] > k:
                        cost -= cuts[i] * cuts[i]
                        cuts[i] += 1
                        cost += cuts[i] * cuts[i]
                    sticks[i] -= k
                    recurse(cuts, k - 1, cost)
                    sticks[i] += k
                    if sticks[i] > k:
                        cost -= cuts[i] * cuts[i]
                        cuts[i] -= 1
                        cost += cuts[i] * cuts[i]

    recurse([0 for _ in sticks], n, 0)
    assert best < n * n * n
    return best

# Car match

def car_match(rows, trucks):
    n, m = len(rows), len(trucks)
    colours, mm = [], 3 * m
    for truck in trucks:
        colours.extend([truck, truck, truck])
    colours.append(-1)  # Artificial sentinel colour in the end

    def recurse(d, i):
        best = 0
        if d <= mm:
            colour = colours[d]
            while i < n:
                # Try taking out car from row i, if headed by car of right colour
                if rows[i] and rows[i][-1] == colour:
                    rows[i].pop()
                    best = max(best, 1 + recurse(d + 1, i if colour == colours[d + 1] else 0))
                    rows[i].append(colour)
                    if d + best == mm:
                        return best
                # Skip row i
                i += 1
        return best

    return 3 * (recurse(0, 0) // 3)

# Farthest points

def farthest_points(pos, p, k):
    n, best = len(pos), 0
    def d(x, y):
        return min(abs(x - y), p - max(x, y) + min(x, y))

    def recurse(f, j, i, dist, k_):
        nonlocal best
        if i + k_ > n or dist <= best:
            return
        elif k_ == 0:
            best = max(best, dist)
        else:
            # Take in point i
            recurse(f, i, i + 1, min(dist, d(pos[j], pos[i]), d(f, pos[i])), k_ - 1)
            # Leave out point i
            recurse(f, j, i + 1, dist, k_)

    for c in range(n - k + 1):
        recurse(pos[c], c, c + 1, p, k - 1)
    return best

# Descending suffix game

def descending_suffix_game(board, n):
    # Set up dancing links structure for doubly linked list of points
    succ = [i + 1 for i in range(n + 1)]
    pred = [i - 1 for i in range(n + 1)]
    succ[n] = 0
    pred[0] = n

    # Unlink node i from dancing list
    def unlink(i):
        succ[pred[i]] = succ[i]
        pred[succ[i]] = pred[i]

    # Link node i back to dancing list
    def link(i):
        succ[pred[i]] = pred[succ[i]] = i

    # Compute the descending suffix score for standing in the current board
    def score():
        prev, total = 0, 0
        for c in reversed(board):
            if c > prev:
                total += c
                prev = c
            else:
                break
        return Fraction(total, 1)

    def recurse():
        stand, hit, m = score(), 0, 0
        i = succ[0]
        while i != 0:
            unlink(i)
            board.append(i)
            m += 1
            hit += recurse()
            board.pop()
            link(i)
            i = succ[i]
        hit = hit * (Fraction(1, m) if m > 0 else 1)
        return max(hit, stand)

    for i in board:
        unlink(i)
    return recurse()
