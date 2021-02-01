import java.util.*;

/* In the Java Collection Framework, an Iterator<E> does not really have to be
 * backed by some actual Collection<E> that contains the elements, but the iterator
 * can generate each element in purely computational means. (This way an iterator
 * could even be infinite, if this computational process never ends!) The outside
 * world that accesses these generated elements with methods next and hasNext is
 * none the wiser, since it cannot see inside this black box anyway.
 * 
 * Here, a computational iterator that produces all permutations of integers from
 * 0 to n - 1, with an option to use a PermPredicate to keep only those permutations
 * that satisfy the condition implicitly defined by that PermPredicate.
 * 
 * Implementing backtracking algorithm with state tables instead of recursion allows
 * the generation of solutions to be paused at each generated permutation, with the
 * rest of the program doing something else, and then restarted from where it left
 * off to generate the next permutation.
 */

public class Permutations implements Iterator<List<Integer>> {

    // Functional interface to define a permutation predicate to allow the outside
    // user to generate only the subset of permutations that satisfies additional
    // constraints defined by the given predicate.
    public interface Predicate {
        /**
         * Given that first {@code n-1} elements of the list satisfy this predicate,
         * determine whether first {@code n} elements of that list also satisfy it.
         * @param elements The list of elements to check.
         * @return Whether the first {@code n} elements satisfy that predicate.
         */
        default public boolean test(List<Integer> elements, int n) { return true; }
    }
    
    // Generate all permutations of numbers 0, ..., n - 1.
    private int n;
    // Simulated next and prev pointers of the cyclic "dancing list" of available values.
    // The value n is used as sentinel that is never removed from this list.
    private int[] prev, next;
    // The current permutation. Instead of creating a new List object for each new
    // permutation, we shall reuse the same object for all different permutations.
    private List<Integer> current;
    // The unmodifiable decorator view of the list that we give out to the world.
    private List<Integer> giveOut;
    // Has the current permutation yet been given out?
    private boolean currentGivenOut;
    // The level that the iterative backtracking is currently at.
    private int level;
    // The predicate used to filter the permutations.
    private Permutations.Predicate pred;
    // Whether the values for given position are iterated in ascending order.
    private boolean ascending = true;
    
    public Permutations(int n) { this(n, null); }
    
    public Permutations(int n, Permutations.Predicate pred) {
        this.n = n;
        this.pred = pred;
        // Initialize the dancing list and the current permutation.
        prev = new int[n+1];
        next = new int[n+1];
        current = new ArrayList<Integer>(n);
        giveOut = Collections.unmodifiableList(current);
        
        // Arrange the integers from 0 to n - 1 in the dancing list.
        for(int i = 0; i < n+1; i++) {
            next[i] = (i+1) % (n+1); prev[i] = i > 0 ? i-1 : n;
            if(i < n) { current.add(n); }
        }
        currentGivenOut = true;
        level = 0;
    }
    
    public Permutations setAscending(boolean ascending) {
        this.ascending = ascending; return this;
    }
    
    public boolean hasNext() {
        // If the previous solution is still in cache, the next solution exists.
        if(currentGivenOut == false) { return true; }
        // Otherwise, compute the next solution with backtracking.
        while(true) {
            // The generation terminates when backtracking past top level 0.
            if(level == -1) { return false; }
            // The current partial solution has been completely filled.
            else if(level == n) {
                currentGivenOut = false;
                level--;
                return true;
            }
            else {
                // The current value at the level we are in.
                int cv = current.get(level);
                // Link the current value back into dancing list.
                next[prev[cv]] = cv; prev[next[cv]] = cv;
                // Try the next value available in the dancing list.
                cv = ascending? next[cv]: prev[cv];
                current.set(level, cv);                
                // If this next value is the sentinel n, backtrack to previous level.
                if(cv == n) { level--; }
                else {
                    // If the predicate accepts the current partial solution, advance.
                    if(pred == null || pred.test(current, level + 1)) {
                        next[prev[cv]] = next[cv]; // Unlink value from dancing list.
                        prev[next[cv]] = prev[cv];
                        level++; // Advance to the next level in backtracking.
                    }
                    // Otherwise, stay at the current level.
                }
            }            
        }
    }
    
    public List<Integer> next() {
        if(hasNext()) { currentGivenOut = true; return giveOut; }
        else { return null; }
    }     
    
    // Pretty print all permutations produced by Permutation generator p.
    private static int emit(Permutations p, int cols, boolean verbose) {
        int count = 0;
        while(p.hasNext()) {
            ++count;
            List<Integer> perm = p.next();
            if(verbose) {
                System.out.print(perm + " ");
                if(count % cols == 0) { System.out.println(""); }
            }
        }
        if(verbose && count % cols > 0) { System.out.println(""); }
        return count;
    }
    
    private static int emit(Permutations p, int cols) {
        return emit(p, cols, true);
    }
    
    public static void main(String[] args) {
        Permutations p = new Permutations(4);
        System.out.println("All permutations of 0-3, ascending.");
        emit(p, 4);
        
        p = new Permutations(4).setAscending(false);
        System.out.println("\nAll permutations of 0-3, descending.");
        emit(p, 4);
        
        // Accept a partial solution if its consecutive values differ by at least k.
        class DifferenceAtLeast implements Permutations.Predicate {
            private int k;
            public DifferenceAtLeast(int k) { this.k = k; }
            public boolean test(List<Integer> elements, int n) {
                if(n == 1) { return true; }
                return Math.abs(elements.get(n-1) - elements.get(n-2)) >= k;
            }            
        }        
        System.out.println("\nAll 0-8 permutations with consecutive difference of at least 4.");
        p = new Permutations(9, new DifferenceAtLeast(4));
        emit(p, 2);
        
        // Accept a partial solution if each value is at most k away from its sorted position.
        class DistanceAtMost implements Permutations.Predicate {
            private int k;
            public DistanceAtMost(int k) { this.k = k; }
            public boolean test(List<Integer> elements, int n) {
                return Math.abs(elements.get(n-1) - (n-1)) <= k;
            }
        }
        System.out.println("\nAll 0-5 permutations of maximum element displacement of 2.");
        p = new Permutations(6, new DistanceAtMost(2));
        emit(p, 3);
        
        // Accept a partial solution if its values alternate going up and down.
        class Zigzag implements Permutations.Predicate {
            public boolean test(List<Integer> elements, int n) {
                if(n < 3) { return true; }
                int a = elements.get(n - 3);
                int b = elements.get(n - 2);
                int c = elements.get(n - 1);
                return (a < b && b > c) || (a > b && b < c);
            }
        }
        
        // Accept a partial solution if it is a binary max-heap.
        class MaxHeap implements Permutations.Predicate {
            public boolean test(List<Integer> elements, int n) {
                if(n < 2) { return true; }
                return elements.get((n/2)-1) > elements.get(n-1);
            }
        }        
        
        // Accept a partial solution if it can become an involution ("self-inverse").
        class Involution implements Permutations.Predicate {
            public boolean test(List<Integer> elements, int n) {
                if(n < 2) { return true; }
                int e = elements.get(n-1);
                if(e < n - 1) {
                    return elements.get(e) == n - 1;
                }
                else { return true; } // So far, so good.
            }
        }
        
        System.out.println("\nTo finish up, let's count some combinatorial permutations.");
        System.out.println("\nn\tZigzag\tHeap\tInvol");
        for(int n = 1; n < 13; n++) {            
            // http://oeis.org/A056971
            Permutations p1 = new Permutations(n, new MaxHeap());
            // https://en.wikipedia.org/wiki/Alternating_permutation
            Permutations p2 = new Permutations(n, new Zigzag());
            // http://oeis.org/A000085
            Permutations p3 = new Permutations(n, new Involution());
            System.out.println(n + "\t" + emit(p1, 0, false) + "\t" + emit(p2, 0, false)
            + "\t" + emit(p3, 0, false));
        }
        
        System.out.println("\nAnd we are all done!");
    }
}
