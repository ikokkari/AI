# Examples of backtracking in Python, taken from the hardest problems in CCPS 109 Python problems.

# Idea from https://twitter.com/MauriceAshley/status/1630994622255169546

def queen_captures_all(queen, pawns):
    def is_between(queen_, pawn1, pawn2):
        if pawn2 is None:
            return True
        qx, qy = queen_
        px1, py1 = pawn1
        px2, py2 = pawn2
        return abs(px1-qx) <= abs(px2-qx) and abs(py1-qy) <= abs(py2-qy)

    def list_nearest(queen_, pawns_):
        qx, qy = queen_
        nearest = [None] * 8
        for pawn in pawns_:
            px, py = pawn
            dx, dy = px-qx, py-qy
            if dx == 0 and dy > 0 and is_between(queen_, pawn, nearest[0]):
                nearest[0] = pawn  # N
            elif dx == 0 and dy < 0 and is_between(queen_, pawn, nearest[4]):
                nearest[4] = pawn  # S
            elif dy == 0 and dx > 0 and is_between(queen_, pawn, nearest[2]):
                nearest[2] = pawn  # E
            elif dy == 0 and dx < 0 and is_between(queen_, pawn, nearest[6]):
                nearest[6] = pawn  # W
            elif dy == dx and dx > 0 and is_between(queen_, pawn, nearest[1]):
                nearest[1] = pawn  # NE
            elif dy == dx and dx < 0 and is_between(queen_, pawn, nearest[5]):
                nearest[5] = pawn  # SW
            elif dy == -dx and dx > 0 and is_between(queen_, pawn, nearest[3]):
                nearest[3] = pawn  # SE
            elif dy == -dx and dx < 0 and is_between(queen_, pawn, nearest[7]):
                nearest[7] = pawn  # NW
        return nearest

    def can_capture_all(queen_, pawns_):
        if len(pawns_) == 0:
            return True
        for pawn in list_nearest(queen_, pawns_):
            if pawn is not None:
                pawns_.remove(pawn)
                if can_capture_all(pawn, pawns_):
                    return True
                pawns_.add(pawn)
        return False

    return can_capture_all(queen, set(pawns))

__gin_ranks = {
    'ace': 1, 'two': 2, 'three': 3, 'four': 4, 'five': 5, 'six': 6, 'seven': 7, 'eight': 8,
    'nine': 9, 'ten': 10, 'jack': 11, 'queen': 12, 'king': 13
}

def count_deadwood(hand):
    hand = sorted([(__gin_ranks[rank], suit) for (rank, suit) in hand])
    hand_set = set(hand)
    rank_counts = dict()
    for (rank, suit) in hand:
        rank_counts[rank] = rank_counts.get(rank, 0) + 1
    best_overall = 666

    def recurse(pos, runs, sets, sofar):
        nonlocal best_overall
        if sofar > best_overall:
            return 666
        if pos == -1:
            if all(len(r) > 2 for r in runs) and all(len(s) > 2 for s in sets):
                if sofar < best_overall:
                    best_overall = sofar
                return 0
            else:
                return 666
        (rank, suit) = hand[pos]
        rank_counts[rank] -= 1
        best = 666

        # Join an existing run
        for r in runs:
            (rank2, suit2) = r[-1]
            if suit == suit2 and rank2 == rank+1:
                r.append((rank, suit))
                best = min(best, recurse(pos-1, runs, sets, sofar))
                r.pop()
        # Start a new run
        if (rank-1, suit) in hand_set and (rank-2, suit) in hand_set:
            runs.append([(rank, suit)])
            best = min(best, recurse(pos-1, runs, sets, sofar))
            runs.pop()
        # Join an existing set
        for s in sets:
            (rank2, suit2) = s[-1]
            if rank2 == rank:
                s.append((rank, suit))
                best = min(best, recurse(pos-1, runs, sets, sofar))
                s.pop()
        # Start a new set
        if rank_counts[rank] > 1:
            sets.append([(rank, suit)])
            best = min(best, recurse(pos-1, runs, sets, sofar))
            sets.pop()
        # Leave as deadwood
        value = rank if rank < 10 else 10
        best = min(best, value + recurse(pos-1, runs, sets, sofar + value))

        rank_counts[rank] += 1
        return best

    return recurse(len(hand)-1, [], [], 0)


def laser_aliens(n, aliens):
    row_aliens = [set() for _ in range(n)]
    col_aliens = [set() for _ in range(n)]
    for (row, col) in aliens:
        row_aliens[row].add(col)
        col_aliens[col].add(row)
    rows = sorted(set(r for (r, c) in aliens), key=lambda r: -len(row_aliens[r]))

    def solve(row_idx, limit):
        if limit < 0:
            return False
        if row_idx == len(rows):
            return True
        curr_row = rows[row_idx]
        if len(row_aliens[curr_row]) == 0:
            return solve(row_idx+1, limit)
        if len(row_aliens[curr_row]) <= limit:
            stack = []
            for c in row_aliens[curr_row]:
                for r in col_aliens[c]:
                    if r != curr_row:
                        stack.append((r, c))
                        row_aliens[r].remove(c)
            if solve(row_idx+1, limit-len(row_aliens[curr_row])):
                return True
            for (r, c_) in stack:
                row_aliens[r].add(c_)
        return solve(row_idx+1, limit-1)

    for limit_ in range(n+1):
        if solve(0, limit_):
            return limit_
    assert False