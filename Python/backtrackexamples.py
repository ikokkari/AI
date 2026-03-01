# Some example solutions for backtracking problems in Third Python Problem Collection.

from bisect import bisect_left

# Subset sum (from PythonExamples/recursion.py)

def subset_sum(items, goal):
    # Base case for success in this search branch.
    if goal == 0:
        return []
    # Base case for failure in this search branch.
    if len(items) == 0 or goal < 0:
        return None
    # Extract the last item from the items.
    last = items.pop()
    # Try taking the last item into chosen subset.
    answer = subset_sum(items, goal-last)
    if answer is not None:
        answer.append(last)
    else:
        # Try not taking the last item into subset.
        answer = subset_sum(items, goal)
    # Restore the last item back to the items.
    items.append(last)
    return answer

# Borgesian dictionary

def find_all_words(letters, words):
    n = len(letters)
    # Initialize the dancing links node list for letters that have been taken
    succ = [i + 1 for i in range(n+1)]
    pred = [i - 1 for i in range(n+1)]
    succ[n] = 0
    pred[0] = n
    result = []

    def is_word(word):
        i = bisect_left(words, word)
        return i < len(words) and words[i] == word

    def starts_word(word):
        i = bisect_left(words, word)
        return i < len(words) and words[i].startswith(word)

    def recurse(word_sofar):
        if is_word(word_sofar):
           result.append(word_sofar)
        elif not starts_word(word_sofar):
            return
        prev = '$'
        i = succ[n]
        while i < n:
            if letters[i] != prev:
                pred[succ[i]] = pred[i]
                succ[pred[i]] = succ[i]
                prev = letters[i]
                recurse(word_sofar + prev)
                succ[pred[i]] = i
                pred[succ[i]] = i
            i = succ[i]

    recurse("")
    return result

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

# Invaders must die

def laser_aliens(n, aliens):
    # Precompute sets of aliens in each row and column for quick lookup.
    row_aliens = [set() for _ in range(n)]
    col_aliens = [set() for _ in range(n)]
    for (row, col) in aliens:
        row_aliens[row].add(col)
        col_aliens[col].add(row)
    # Sort the row numbers based on how many aliens are on that row.
    rows = sorted(set(r for (r, c) in aliens), key=lambda r: len(row_aliens[r]))

    # Branch and bound, the best solution found so far in branch and bound recursion.
    best_solution = n

    # Recursive backtracking search to find the best solution.
    def solve(row_idx, beams_fired_so_far):
        nonlocal best_solution
        # Branch and bound failure cutoff.
        if beams_fired_so_far >= best_solution:
            return
        # Complete solution cutoff, all aliens in all rows eliminated.
        if row_idx == len(rows):
            best_solution = beams_fired_so_far
            return
        # The actual row that is processed at this level of recursion.
        curr_row = rows[row_idx]
        # Try shooting one beam through this row, provided that at least two aliens remain.
        if len(row_aliens[curr_row]) > 1:
            solve(row_idx + 1, beams_fired_so_far + 1)
        # Try shooting beams through every column that has an alien on this row.
        if len(row_aliens[curr_row]) + beams_fired_so_far < best_solution:
            # Remove the aliens in all such remaining columns.
            undo_stack = []
            for c in row_aliens[curr_row]:
                for r in col_aliens[c]:
                    # Be careful not to modify the current row while iterating through it.
                    if r != curr_row:
                        undo_stack.append((r, c))
                        row_aliens[r].remove(c)
            solve(row_idx + 1, beams_fired_so_far + len(row_aliens[curr_row]))
            # Restore the aliens removed with these vertical beams.
            for (r, c_) in undo_stack:
                row_aliens[r].add(c_)

    solve(0, 0)
    return best_solution
