# Independent Dominating Set

def independent_dominating_set(edges):
    n = len(edges)
    
    def is_valid(selected, dominated):
        """Check if current selection is valid independent dominating set"""
        # Check independence (no two selected nodes are neighbors)
        for node in selected:
            for neighbor in edges[node]:
                if neighbor in selected:
                    return False
        
        # Check domination (all nodes are either selected or have selected neighbor)
        for node in range(n):
            if node not in selected and node not in dominated:
                return False
        
        return True
    
    def backtrack(node, selected, dominated):
        """Try to build independent dominating set"""
        # Base case: processed all nodes
        if node == n:
            if is_valid(selected, dominated):
                return len(selected)
            else:
                return float('inf')
        
        best = float('inf')
        
        # Option 1: Include this node in the set
        if all(neighbor not in selected for neighbor in edges[node]):
            new_selected = selected | {node}
            new_dominated = dominated | {node} | set(edges[node])
            result = backtrack(node + 1, new_selected, new_dominated)
            best = min(best, result)
        
        # Option 2: Don't include this node
        # (must ensure it gets dominated by a neighbor eventually)
        result = backtrack(node + 1, selected, dominated)
        best = min(best, result)
        
        return best
    
    return backtrack(0, set(), set())


# Blocking Pawns

def blocking_pawns(n, queens):
    def get_between_squares(q1, q2):
        """Get all squares between two queens on same row, column, or diagonal"""
        r1, c1 = q1
        r2, c2 = q2
        
        squares = []
        
        # Same row
        if r1 == r2:
            for c in range(min(c1, c2) + 1, max(c1, c2)):
                squares.append((r1, c))
        # Same column
        elif c1 == c2:
            for r in range(min(r1, r2) + 1, max(r1, r2)):
                squares.append((r, c1))
        # Same diagonal
        elif abs(r1 - r2) == abs(c1 - c2):
            dr = 1 if r2 > r1 else -1
            dc = 1 if c2 > c1 else -1
            r, c = r1 + dr, c1 + dc
            while (r, c) != q2:
                squares.append((r, c))
                r += dr
                c += dc
        
        return squares
    
    def is_blocked(q1, q2, pawns_placed):
        """Check if two queens are blocked by at least one pawn"""
        between = get_between_squares(q1, q2)
        for square in between:
            if square in pawns_placed:
                return True
        return False
    
    def find_attacking_pairs(queens_set, pawns_placed):
        """Find all pairs of queens that attack each other and aren't blocked"""
        pairs = []
        queens_list = list(queens_set)
        for i in range(len(queens_list)):
            for j in range(i + 1, len(queens_list)):
                q1, q2 = queens_list[i], queens_list[j]
                r1, c1 = q1
                r2, c2 = q2
                
                # Check if they attack each other
                if r1 == r2 or c1 == c2 or abs(r1 - r2) == abs(c1 - c2):
                    # Check if not already blocked
                    if not is_blocked(q1, q2, pawns_placed):
                        pairs.append((q1, q2))
        
        return pairs
    
    def backtrack(queens_set, pawns_placed):
        # Find attacking pairs that aren't blocked
        attacking_pairs = find_attacking_pairs(queens_set, pawns_placed)
        
        # Base case: no attacking pairs
        if not attacking_pairs:
            return len(pawns_placed)
        
        # Choose first attacking pair
        q1, q2 = attacking_pairs[0]
        between = get_between_squares(q1, q2)
        
        best = float('inf')
        
        # Try placing a pawn at each position between them
        for pawn_pos in between:
            if pawn_pos not in pawns_placed and pawn_pos not in queens_set:
                new_pawns = pawns_placed | {pawn_pos}
                result = backtrack(queens_set, new_pawns)
                best = min(best, result)
        
        return best
    
    queens_set = set(queens)
    return backtrack(queens_set, set())


# Set Splitting

def set_splitting(n, subsets):
    def backtrack(black):
        # Find all unicolor subsets
        unicolor = []
        for i, subset in enumerate(subsets):
            black_count = sum(1 for e in subset if e in black)
            white_count = len(subset) - black_count
            
            # If all black or all white, it's unicolor (invalid)
            if black_count == len(subset):
                unicolor.append(i)
            elif white_count == len(subset):
                # All white - this coloring is invalid
                return False
        
        # Base case: no unicolor subsets
        if not unicolor:
            return True
        
        # Choose a unicolor subset
        subset_idx = unicolor[0]
        subset = subsets[subset_idx]
        
        # Try coloring each element in this subset white
        for element in subset:
            # Skip if already white
            if element not in black:
                continue
            
            # Color this element white
            new_black = black.copy()
            new_black.remove(element)
            
            if backtrack(new_black):
                return True
        
        return False
    
    # Start with all elements black
    black = set(range(n))
    return backtrack(black)


# Domino Poppers

def domino_pop(dominoes):
    def matches(pip1, pip2):
        return pip1 == pip2 or pip1 == 0 or pip2 == 0
    
    def backtrack(stack):
        # Base case: no more dominoes to process
        if not stack:
            return 0
        
        # If only one domino left, can't eliminate it
        if len(stack) == 1:
            return 1
        
        best = len(stack)  # Worst case: keep all dominoes
        
        # Try to find matching pairs
        for i in range(len(stack) - 1):
            # Check if dominoes at positions i and i+1 match
            right_pip_i = stack[i][1]
            left_pip_i_plus_1 = stack[i + 1][0]
            
            if matches(right_pip_i, left_pip_i_plus_1):
                # Remove this pair and recurse
                new_stack = stack[:i] + stack[i+2:]
                result = backtrack(new_stack)
                best = min(best, result)
        
        return best
    
    return backtrack(list(dominoes))


# Unity Partition

def unity_partition(n):
    from fractions import Fraction
    
    def backtrack(target_sum, target_frac, current_list, start):
        # Base case: we've used up both the sum and the fraction
        if target_sum == 0 and target_frac == 0:
            return current_list[:]
        
        # Dead ends
        if target_sum <= 0 or target_frac <= 0:
            return None
        
        # Try each possible next number
        for num in range(start, target_sum + 1):
            reciprocal = Fraction(1, num)
            
            # Skip if reciprocal is too large
            if reciprocal > target_frac:
                continue
            
            # Try adding this number
            current_list.append(num)
            result = backtrack(target_sum - num, target_frac - reciprocal, current_list, num + 1)
            
            if result is not None:
                return result
            
            current_list.pop()
        
        return None
    
    result = backtrack(n, Fraction(1, 1), [], 2)
    return result if result else []


# Colonel Blotto and the Spymaster

def colonel_blotto(friend, enemy, prize):
    n = len(friend)
    
    def backtrack(friend_left, enemy_left, prize_left):
        # Base case: no more battlefields
        if not enemy_left:
            return 0
        
        # Choose the next enemy battlefield to assign a unit to
        enemy_idx = 0
        enemy_strength = enemy_left[enemy_idx]
        battlefield_prize = prize_left[enemy_idx]
        
        # New lists without this battlefield
        new_enemy = enemy_left[1:]
        new_prize = prize_left[1:]
        
        best = 0
        
        # Try assigning each remaining friendly unit to this battlefield
        for i in range(len(friend_left)):
            friend_strength = friend_left[i]
            new_friend = friend_left[:i] + friend_left[i+1:]
            
            # Calculate prize for this battlefield
            current_prize = 0
            if friend_strength > enemy_strength:
                current_prize = battlefield_prize
            
            # Recurse with remaining units and battlefields
            total = current_prize + backtrack(new_friend, new_enemy, new_prize)
            best = max(best, total)
        
        return best
    
    return backtrack(friend, enemy, prize)

# Fox and Hounds

def fox_and_hounds(fox, hounds):
    def is_valid(pos):
        r, c = pos
        return 0 <= r < 8 and 0 <= c < 8 and (r + c) % 2 == 1
    
    def fox_moves(fox_pos):
        r, c = fox_pos
        moves = []
        for dr, dc in [(-1, -1), (-1, 1), (1, -1), (1, 1)]:
            new_pos = (r + dr, c + dc)
            if is_valid(new_pos):
                moves.append(new_pos)
        return moves
    
    def hound_moves(hound_pos):
        r, c = hound_pos
        moves = []
        for dc in [-1, 1]:
            new_pos = (r + 1, c + dc)
            if is_valid(new_pos):
                moves.append(new_pos)
        return moves
    
    def fox_turn(fox_pos, hounds_pos):
        # Fox wins if it reaches row 0
        if fox_pos[0] == 0:
            return True
        
        # Get all possible fox moves
        possible_moves = fox_moves(fox_pos)
        
        # Remove positions occupied by hounds
        possible_moves = [m for m in possible_moves if m not in hounds_pos]
        
        # Fox loses if it has no moves
        if not possible_moves:
            return False
        
        # Fox wins if any move leads to a position where hounds can't win
        for move in possible_moves:
            if not hounds_turn(move, hounds_pos):
                return True
        
        return False
    
    def hounds_turn(fox_pos, hounds_pos):
        # Try moving each hound
        for i in range(len(hounds_pos)):
            hound = hounds_pos[i]
            possible_moves = hound_moves(hound)
            
            for move in possible_moves:
                # Check if move is not occupied by another hound or fox
                if move not in hounds_pos and move != fox_pos:
                    new_hounds = list(hounds_pos)
                    new_hounds[i] = move
                    new_hounds = tuple(sorted(new_hounds))
                    
                    # Hounds win if fox can't win from this position
                    if not fox_turn(fox_pos, new_hounds):
                        return True
        
        return False
    
    return fox_turn(fox, tuple(sorted(hounds)))


# Post Correspondence Problem

def post_correspondence_problem(first, second, lo, hi):
    def backtrack(first_str, second_str):
        # If strings match and length is in range, we found a solution
        if first_str == second_str and lo <= len(first_str) <= hi:
            return True
        
        # If strings are too long, stop
        if len(first_str) > hi or len(second_str) > hi:
            return False
        
        # Try appending each possible string pair
        for i in range(len(first)):
            new_first = first_str + first[i]
            new_second = second_str + second[i]
            
            if backtrack(new_first, new_second):
                return True
        
        return False
    
    return backtrack('', '')
