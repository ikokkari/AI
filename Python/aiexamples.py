# Examples of backtracking in Python, taken from the hardest problems in CCPS 109 Python problems.

# Idea from https://twitter.com/MauriceAshley/status/1630994622255169546

from fractions import Fraction
from bisect import bisect_left
from functools import lru_cache


def queen_captures_all(queen, pawns):
    # Determine if pawn1 is closer to queen than pawn2.
    def is_between(queen_, pawn1, pawn2):
        if pawn2 is None:
            return True
        qx, qy = queen_
        px1, py1 = pawn1
        px2, py2 = pawn2
        return abs(px1-qx) <= abs(px2-qx) and abs(py1-qy) <= abs(py2-qy)

    # Generate list of the nearest pawns from the queen for all eight compass directions.
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

    # Backtracking recursion to determine if queen can capture all pawns one at the time.
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
    M = 666
    hand = sorted([(__gin_ranks[rank], suit) for (rank, suit) in hand])
    hand_set = set(hand)
    # Counter for how many times each rank appears in hand
    rank_counts = [sum(1 for (r, s) in hand if r == rank) for rank in range(14)]
    best_overall = M

    def backtrack(pos, runs, sets, deadwood_sofar):
        nonlocal best_overall
        if deadwood_sofar >= best_overall:
            return M
        if pos == -1:
            if all(len(r) > 2 for r in runs) and all(len(s) > 2 for s in sets):
                if deadwood_sofar < best_overall:
                    best_overall = deadwood_sofar
                return 0
            else:
                return M
        (rank, suit) = hand[pos]
        rank_counts[rank] -= 1
        best = M

        # Join an existing run?
        was_run = False
        for r in runs:
            (rank2, suit2) = r[-1]
            if suit == suit2 and rank2 == rank+1:
                was_run = True
                r.append((rank, suit))
                best = min(best, backtrack(pos-1, runs, sets, deadwood_sofar))
                r.pop()
        # Start a new run
        if not was_run and (rank-1, suit) in hand_set and (rank-2, suit) in hand_set:
            runs.append([(rank, suit)])
            best = min(best, backtrack(pos-1, runs, sets, deadwood_sofar))
            runs.pop()
        # Join an existing set?
        was_set = False
        for s in sets:
            (rank2, suit2) = s[-1]
            if rank2 == rank:
                was_set = True
                s.append((rank, suit))
                best = min(best, backtrack(pos-1, runs, sets, deadwood_sofar))
                s.pop()
        # Start a new set?
        if not was_set and rank_counts[rank] > 1:
            sets.append([(rank, suit)])
            best = min(best, backtrack(pos-1, runs, sets, deadwood_sofar))
            sets.pop()
        # Leave as deadwood?
        value = rank if rank < 10 else 10
        best = min(best, value + backtrack(pos-1, runs, sets, deadwood_sofar + value))

        rank_counts[rank] += 1
        return best

    return backtrack(len(hand)-1, [], [], 0)


def laser_aliens(n, aliens):
    # Precompute sets of aliens in each row and column.
    row_aliens = [set() for _ in range(n)]
    col_aliens = [set() for _ in range(n)]
    for (row, col) in aliens:
        row_aliens[row].add(col)
        col_aliens[col].add(row)
    # Sort the rows based on how many aliens are on that row.
    rows = sorted(set(r for (r, c) in aliens), key=lambda r: -len(row_aliens[r]))

    best_solution = n

    # Recursive backtracking search to find the solution within given limit.
    def solve(row_idx, so_far):
        nonlocal best_solution
        # Negative and positive base cases of the recursion.
        if so_far >= best_solution:
            return
        if row_idx == len(rows):
            best_solution = min(best_solution, so_far)
            return
        curr_row = rows[row_idx]
        # Have all the aliens that were on this row been eliminated already?
        if len(row_aliens[curr_row]) == 0:
            return solve(row_idx+1, so_far)
        # Try shooting one laser through this row.
        if len(row_aliens[curr_row]) > 1:
            solve(row_idx+1, so_far+1)
        # Try shooting laser through every column that has an alien on this row.
        if len(row_aliens[curr_row]) + so_far <= best_solution:
            undo_stack = []
            for c in row_aliens[curr_row]:
                for r in col_aliens[c]:
                    if r != curr_row:
                        undo_stack.append((r, c))
                        row_aliens[r].remove(c)
            solve(row_idx+1, so_far + len(row_aliens[curr_row]))
            for (r, c) in undo_stack:
                row_aliens[r].add(c)
        # Didn't work either way.
        return False

    solve(0, 0)
    return best_solution


def word_board(board, words):
    n = len(board)
    found, visited = set(), set()

    def search(x_, y_, word):
        visited.add((x_, y_))
        idx = bisect_left(words, word)
        if idx < len(words):
            if words[idx] == word:  # Found a word, record it
                found.add(word)
            if words[idx].startswith(word):  # If the current word can be extended, continue recursively
                for (dx, dy) in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                    nx, ny = x_+dx, y_+dy  # Coordinates of the neighbour
                    if 0 <= nx < n and 0 <= ny < n and (nx, ny) not in visited:
                        search(nx, ny, word+board[nx][ny])
        visited.remove((x_, y_))

    for x in range(n):
        for y in range(n):
            search(x, y, board[x][y])
    return sorted(found)


def costas_array(rows):
    n = len(rows)
    dvs = set()
    placed = dict()
    to_fill = [row for (row, col) in enumerate(rows) if col is None]
    still_free = [True for _ in range(n)]

    def place_rook(row, col, undo_stack=None):
        for prev_row in placed:
            prev_col = placed[prev_row]
            dx, dy = row-prev_row, col-prev_col
            if (dx, dy) in dvs or (-dx, -dy) in dvs:
                return False
            dvs.add((dx, dy))
            if undo_stack is not None:
                undo_stack.append((dx, dy))
        placed[row] = col
        still_free[col] = False
        return True

    for (row, col) in enumerate(rows):
        if col is not None:
            if not place_rook(row, col):
                return False

    def fill_remaining():
        if len(to_fill) == 0:
            return True
        row = to_fill.pop()
        undo_stack = []
        for col in range(n):
            if still_free[col]:
                if place_rook(row, col, undo_stack):
                    if fill_remaining():
                        return True
                    still_free[col] = True
                    del placed[row]
                while len(undo_stack) > 0:
                    dvs.remove(undo_stack.pop())
        to_fill.append(row)
        return False

    return fill_remaining()


__zero = Fraction(0)
__one = Fraction(1)
__one_eighth = Fraction(1, 8)


def accumulate_dice(d, goal):
    one_d = Fraction(1, d)

    @lru_cache(maxsize=10000)
    def prob(s, k):
        if k == 0:
            return __one if s == 0 else __zero
        else:
            total = __zero if s < goal else prob(s, k-1)
            for sp in range(max(0, s-d), min(goal, s)):
                total += prob(sp, k-1) * one_d
            return total

    return [prob(s, goal) for s in range(goal, goal+d)]


__knight_moves = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]


@lru_cache(maxsize=10000)
def knight_survival(n, x, y, k):
    if x < 0 or x >= n or y < 0 or y >= n:
        return __zero
    elif k == 0:
        return __one
    return sum(knight_survival(n, x+dx, y+dy, k-1) for (dx, dy) in __knight_moves) * __one_eighth
