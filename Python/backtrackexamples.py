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
