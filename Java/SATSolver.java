import java.util.*;
import java.io.*;

public class SATSolver {
    
    // Some methods for measuring the effectiveness of the algorithm.
    private static int callCount = 0;
    public static int getCallCount() { return callCount; }
    private static int pureLiteralCount = 0;
    public static int getPureLiteralCount() { return pureLiteralCount; }
    
    // Calculate the index in the solution array where literal is stored.
    private static int getIdx(int literal) {
        if(literal < 0) { return 2 * (-literal) - 1; }
        else { return 2 * literal - 2; }
    }

    /**
     * Read a SAT problem from a standard DIMACS problem file.
     * @param filename The name of the file.
     * @return The solution vector that was found, or null if there is no solution.
     */
    public static boolean[] readDimacsProblem(String filename) throws IOException {
        Scanner s = new Scanner(new File(filename));
        int[][] clauses = null;
        int vars = 0, cloc = 0;
        while(s.hasNextLine()) {
            String line = s.nextLine();
            if(line.charAt(0) == 'c') { continue; }
            else if(line.charAt(0) == 'p') {
                String[] info = line.split("\\s+");
                int c = Integer.parseInt(info[3]);
                clauses = new int[c][];
            }
            else {
                String[] info = line.split("\\s+");
                int[] clause = new int[info.length - 1];
                for(int loc = 0; loc < info.length - 1; loc++) {
                     clause[loc] = Integer.parseInt(info[loc]);
                     if(Math.abs(clause[loc]) > vars) { vars = Math.abs(clause[loc]); }
                }
                clauses[cloc++] = clause;
            }
        }
        System.out.println("Read problem with " + vars + " variables and " + cloc + " clauses."); 
        long startTime = System.currentTimeMillis();
        boolean[] solution = SATSolver.solveDPLL(vars, clauses);
        long endTime = System.currentTimeMillis();
        System.out.println("Solved in " + (endTime - startTime) + " ms with "
        + SATSolver.getCallCount() + " recursive calls.");
        return solution;
    }
    
    /**
     * Solve the set of CNF clauses for variables 1, ..., {@code n} using the DPLL algorithm.
     * @param n The number of propositional variables in the system.
     * @param clauses The array of clauses. Each row of this two-dimensional array is one clause,
     * the elements representing the literals of the clause. Positive element {@code x} stands for the 
     * positive literal <i>x</i>, negative element {@code -x} stands for the negative literal <i>not-x.</i>
     * @return Solution array of {@code n + 1} elements giving the truth value of each variable,
     * or {@code null} if there is no solution to the clauses.
     */
    public static boolean[] solveDPLL (int n, int[][] clauses) {
        // Initialize the counters.
        callCount = pureLiteralCount = 0;
        // Initialize the solution array and the active set data structures (see below).
        solution = new int[2 * n];
        active.clear();
        activeUnits.clear();
        inClauses = new ArrayList<List<Integer>>();
        for(int i = 0; i < 2 * n; i++) {
            inClauses.add(new ArrayList<Integer>());
        }
        clauseCount = new int[2 * n];
        possible = new int[2 * clauses.length];
        // Preprocess the clauses.
        cloc = 0;
        SATSolver.clauses = new int[clauses.length][];
        for(int c = 0; c < clauses.length; c++) {
            // Ignore nonexistent and empty clauses.
            if(clauses[c] == null || clauses[c].length == 0) { continue; }
            SATSolver.clauses[cloc] = clauses[c];
            // Initially, any one of the literals could be made true.
            possible[cloc] = SATSolver.clauses[cloc].length;
            // Add the clause to the approriate set of active clauses.
            if(possible[cloc] == 1) { 
                activeUnits.add(cloc);
            } 
            else { 
                active.add(cloc);
            }
            for(int literal: SATSolver.clauses[cloc]) {
                if(literal == 0 || literal < -n || literal > n) {
                    throw new IllegalArgumentException("Illegal literal value " + literal + " in clause " + c);
                }
                inClauses.get(getIdx(literal)).add(cloc);
                clauseCount[getIdx(literal)]++;
            }
            cloc++;
        }
        
        inactive = new boolean[SATSolver.clauses.length];
        decisions = new int[n + 1];
        // Initialize the queue of the literals to examine.
        literalQueue = new IntHeap(clauseCount, n, true);
        for(int literal = 1; literal <= n; literal++) {
            literalQueue.offer(literal); 
            literalQueue.offer(-literal);
        }
        // Solve the system recursively.
        if(DPLL(1) > -1) { return null; }
        // Convert the integer solution array to truth values to return to caller.
        boolean[] solutionB = new boolean[n + 1];
        for(int literal = 1; literal <= n; literal++) {
            solutionB[literal] = solution[getIdx(literal)] > 0;
        }
        return solutionB;
    }

    // The state variables of the search during the recursive backtracking DPLL algorithm.
    
    // Stack used to remember which actions to unroll when backtracking.
    private static LinkedList<Integer> stack = new LinkedList<Integer>();
    // List of literals that are known to be currently pure.
    private static LinkedList<Integer> pureLiterals = new LinkedList<Integer>();
    // Current state of each literal. If zero, unset. If positive, set true at that
    // level of recursion. If negative, set false at that level of recursion.
    private static int[] solution;
    // The list of clauses in which each literal is in.
    private static List<List<Integer>> inClauses;
    // The set of non-unit clauses that are currently active.
    private static Set<Integer> active = new HashSet<>();
    // The set of unit clauses that are currently active.
    private static TreeSet<Integer> activeUnits = new TreeSet<>();
    // Array of counters of how many clauses are still possible for each literal.
    private static int[] possible;
    // The clauses as arrays of integer literals.
    private static int[][] clauses;
    private static int cloc = 0;
    // Quick lookup table of which clauses are no longer active.
    private static boolean[] inactive;
    // The priority queue that contains the literals that are still unassigned.
    private static IntHeap literalQueue;
    // Counter of how many active clauses each literal appears in.
    private static int[] clauseCount;
    // Decisions made in this recursion path.
    private static int[] decisions;
    
    // Named constants used as opcodes when unrolling the stack.
    private static final int MARK = Integer.MAX_VALUE;
    private static final int REMOVE_UNIT = Integer.MAX_VALUE - 1;
    private static final int REMOVE_ACT = Integer.MAX_VALUE - 2;
    private static final int TO_UNIT = Integer.MAX_VALUE - 3;
    private static final int DECREMENT = Integer.MAX_VALUE - 4;
    private static final int LITERAL = Integer.MAX_VALUE - 5;
    private static final int ENQUEUE = Integer.MAX_VALUE - 6;
    
    /* 
     * Remove all clauses that contain the literal made true from the active sets.
     */
    private static void makeLiteralTrue(int level, int literal) {
        int idx = getIdx(literal);
        int idxn = getIdx(-literal);
        
        // Make this literal true and its negation false in the current solution.
        solution[idx] = +level;
        solution[idxn] = -level;
        // Add the correct opcodes to unroll this decision when backtracking.
        stack.addLast(literal);
        stack.addLast(LITERAL);
       
        // For each clause where this literal appears, remove it from the active sets.
        for(int cl: inClauses.get(idx)) {
            // Ignore the "doubly true" clauses already made true by some previous literal.
            if(inactive[cl]) { continue; }
            // Decrease the clause count for every literal that appears in this clause.
            for(int lit: clauses[cl]) {
                --clauseCount[getIdx(lit)];
                literalQueue.decrease(lit);
                // If this literal no longer appears in any active clause, its negation becomes pure.
                if(clauseCount[getIdx(lit)] == 0 && clauseCount[getIdx(-lit)] > 0) {
                    pureLiterals.add(-lit);
                }
            }
            // Remove the clause from the appropriate set.
            inactive[cl] = true;
            assert possible[cl] > 0; 
            if(possible[cl] == 1) { // This clause is a unit clause.
                assert activeUnits.contains(cl);
                activeUnits.remove(cl);
                stack.addLast(cl); 
                stack.addLast(REMOVE_UNIT);
            }
            else { // This clause is not an unit clause.
                assert active.contains(cl);
                active.remove(cl);
                stack.addLast(cl); 
                stack.addLast(REMOVE_ACT);
            }
        }
        
        // For each active clause where the negation of this literal appears, decrement the count
        // of possible literals that still remain in that clause.
        for(int cl: inClauses.get(idxn)) {
            // Again, ignore the clauses that are already made true by previous assignments.
            if(inactive[cl]) { continue; }
            // That clause now has one fewer possible literals that could be made true.
            --possible[cl];
            // If that clause becomes a unit clause, move it to the set of active unit clauses.
            if(possible[cl] == 1) {
                assert active.contains(cl);
                active.remove(cl);
                activeUnits.add(cl);
                stack.addLast(cl);
                stack.addLast(TO_UNIT);
            }
            else { // Otherwise, just note that its possible literals count was decremented.
                stack.addLast(cl);
                stack.addLast(DECREMENT);
            }
        }
    }

    /*
     * Unroll the actions that were performed when making some literal true.  
     */
    private static void unrollStack() {
        do {
            // Find out which action to unroll. 
            int op = stack.removeLast();
            // All actions have been unrolled at this level.
            if(op == MARK) { return; }
            // Pop the operand of the action from the stack.
            int cl = stack.removeLast();
            // Undo removing the clause from the set of active unit clauses.
            if(op == REMOVE_UNIT) {
                assert inactive[cl];
                activeUnits.add(cl);
                inactive[cl] = false;
                for(int lit: clauses[cl]) { 
                    clauseCount[getIdx(lit)]++;
                    literalQueue.increase(lit);
                }
            }
            // Undo removing the clause from the set of active non-unit clauses.
            else if(op == REMOVE_ACT) {
                assert inactive[cl];
                active.add(cl);
                inactive[cl] = false;
                for(int lit: clauses[cl]) {
                    clauseCount[getIdx(lit)]++;
                    literalQueue.increase(lit);
                }
            }
            // Undo changing the clause from non-unit clause to unit clause.
            else if(op == TO_UNIT) {
                assert activeUnits.contains(cl);
                activeUnits.remove(cl);
                active.add(cl);
                possible[cl]++;
            }
            // Undo decrementing the possible literal count of this clause.
            else if(op == DECREMENT) {
                possible[cl]++;
            }
            // Undo setting the value of a literal whose value was forced. 
            else if(op == LITERAL) {
                solution[getIdx(cl)] = 0; // cl is here a literal, not a clause...
                solution[getIdx(-cl)] = 0;
            }
            // Undo taking a literal from the queue when it was already assigned.
            else if(op == ENQUEUE) {
                literalQueue.offer(cl); // cl is here a literal, not a clause
            }
            else { assert false; } // No other possible actions exist.
        } while(true);
    }

    /*
     * The recursive implementation of the DPLL algorithm to solve a system of CNF formulas.
     */
    private static int DPLL(int level) {
        // Place a mark to the stack so that unrolling this action knows where to stop.
        stack.addLast(MARK);
        callCount++;
        
        boolean unitClauseCutoff = false; // Did any unit clause create a contradiction?
        int jumpLevel = level - 1; // What level to return from this level.
        // Unit clause and pure literal propagation are handled in a while-loop without
        // growing the recursion stack, since neither action involves any choice.
        unitClauseLoop:
        while(pureLiterals.size() > 0 || activeUnits.size() > 0) {
            if(activeUnits.size() > 0) {
                int c = activeUnits.first(); // The unit clause to process.
                // Find the one literal that is still unassigned in this clause.
                for(int literal: clauses[c]) {
                    int idx = getIdx(literal);    
                    if(solution[idx] == 0) { // This is the one
                        int idxn = getIdx(-literal);
                        // Check the clauses that contain negated literal to create cutoff.
                        for(int cl: inClauses.get(idxn)) {
                            if(!inactive[cl] && possible[cl] == 1) {
                                pureLiterals.clear(); unitClauseCutoff = true; break unitClauseLoop;
                            }
                        }
                        makeLiteralTrue(level, literal);
                        continue unitClauseLoop;
                    }
                }
                assert false; // Unreachable, as every unit clause must have one unassigned literal.
            }
            else {
                pureLiteralCount++;
                int literal = pureLiterals.removeFirst();
                assert clauseCount[getIdx(-literal)] == 0;
                int idx = getIdx(literal);
                // Unit clause elimination might have already done this, so better check.
                if(solution[idx] == 0 && clauseCount[idx] > 0) {
                    makeLiteralTrue(level, literal);
                }
            }
        }

        if(!unitClauseCutoff) {
            // No active clauses remain, so the entire problem has been solved.
            if(active.size() == 0) { return -1; }
            int literal; // The literal to process at this level of recursion.
            do { // Pop the next literal from the queue until we get an unassigned one.
                literal = literalQueue.poll();
                if(solution[getIdx(literal)] != 0) {
                    stack.addLast(literal); stack.addLast(ENQUEUE);
                }
            } while(solution[getIdx(literal)] != 0);

            // Choose the order in which way to try the branches for this literal.
            int lit;
            if(clauseCount[getIdx(literal)] < clauseCount[getIdx(-literal)]) { lit = -literal; }
            else { lit = literal; }
            // Recursively try out both ways to assign this literal and its negation.
            for(int i = 0; i < 2; i++) {
                // Place a mark to the stack so that unrolling this action knows where to stop.
                stack.addLast(MARK);
                // Make the chosen literal true and prune the active clauses accordingly.
                decisions[level] = lit;
                makeLiteralTrue(level, lit);
                // Continue the recursion from the reduced set of active clauses.
                int result = DPLL(level + 1);
                // If a solution is found, return it posthaste without unrolling the decisions.
                if(result == -1) { return result; }
                // Unroll the choice of making this literal true.
                unrollStack();
                // If forced to backjump, no point trying out the other branch.
                if(result < level) { break; }
                //Try the negated literal for the next round of this loop.
                lit = -lit;
            }
            // Push the original literal back to the queue.
            literalQueue.offer(literal);
        }

        // Unroll the actions done by unit clauses and pure literals, and return to the previous level.
        unrollStack();
        return jumpLevel;
    }
}
