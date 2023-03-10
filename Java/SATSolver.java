import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.function.IntUnaryOperator;

// DPLL backtracking SAT Solver with optimizations for unit clauses.
// Ilkka Kokkarinen, November 5 2018, ilkka.kokkarinen@gmail.com.

public class SATSolver {

    private static final int UNASSIGNED = -1;

    // Encode positive and negative integers into non-negative indices.
    private static int idx(int lit) {
        return lit > 0 ? 2 * (lit-1) : 2 * (-lit) - 1;
    }       
   
    /**
     * Solve the given instance of propositional logic satisfiability.
     * @param n Total number of propositional variables 1, ..., {@code n}.
     * @param clauses The individual clauses given as an array where each element is an array
     * representing one clause. A clause is given as integers where positive value means a
     * positive literal, and a negative value means a negative literal.
     * @return Array of {@code n+1} truth values whose element in position {@code i} gives
     * the truth value of propositional variable {@code i}.
     */
    public static boolean[] solve(int n, final int[][] clauses) {
        return solve(n, clauses, false, -1);
    }
    
    /**
     * Solve the given instance of propositional logic satisfiability.
     * @param n Total number of propositional variables 1, ..., {@code n}.
     * @param clauses The individual clauses given as an array where each element is an array
     * representing one clause. A clause is given as integers where positive value means a
     * positive literal, and a negative value means a negative literal.
     * @param verbose Whether this function should print out reports.
     * @param giveUp After how many advances should the search give up. Use -1 for never give up.
     * @return Array of {@code n+1} truth values whose element in position {@code i} gives
     * the truth value of propositional variable {@code i}.
     */
    public static boolean[] solve(int n, final int[][] clauses, boolean verbose, long giveUp) {
        // Counters for measurement and debugging of this algorithm.
        long unitClauseC = 0, forwardCheckingC = 0, advanceC = 0;
        // Whether clauses should be sorted before backtracking begins.
        final boolean SORT_CLAUSES = true;
        // Required delay between sorting the literals in a clause.
        final long SORT_THRESHOLD = 50;
        
        // Number of clauses to solve.
        int m = clauses.length;
        int HEAD = m;
        // Keep the unsatisfied clauses in dancing list, with extra node m used as sentinel header.
        int[] next = new int[m + 1];
        int[] prev = new int[m + 1];                
        for(int i = 0; i <= m; i++) {
            next[i] = i < HEAD ? i + 1: 0;
            prev[i] = i > 0 ? i - 1: HEAD;
        }
        
        // List of clauses where each literal appears.
        List<List<Integer>> literalInClause = new ArrayList<>();
        // Which recursionLevel of recursion each literal was taken to the solution.
        int[] assignedAtLevel = new int[2 * n];
        for(int proposition = 0; proposition < 2 * n; proposition++) {
            literalInClause.add(new ArrayList<>());
            assignedAtLevel[proposition] = UNASSIGNED;
        }
        
        // How many chances each clause has remaining to become true.
        int[] chances = new int[m + 1];
        // List of unit clauses known at the moment.
        LinkedList<Integer> unitClauses = new LinkedList<Integer>();
        // The literal that was used in each recursionLevel in current partial solution.
        int[] literalUsed = new int[m + 1];
        // The iterationStage of iteration of possibilities at each recursionLevel.
        // 0 = try the current literal, 1 = try its negation, 2 = backtrack.
        int[] iterationStage = new int[m + 1];
        // At which recursionLevel each clause became satisfied.
        int[] satisfiedAtLevel = new int[m];
        
        // Fill in the various tables used in the backtracking algorithm.
        for(int clause = 0; clause < m; clause++) {
            satisfiedAtLevel[clause] = UNASSIGNED;
            if(clauses[clause] == null) { // Just in case the caller was sloppy.
                next[prev[clause]] = next[clause]; prev[next[clause]] = prev[clause];
                next[clause] = prev[clause] = clause;
            }
            else {
                chances[clause] = clauses[clause].length;
                if(chances[clause] == 1) {
                    unitClauses.push(clause);
                }
                for(int lit: clauses[clause]) {
                    literalInClause.get(idx(lit)).add(clause);
                }
            }
        }                  
        
        // How many unsatisfied clauses each literal still occurs in.
        int[] inActiveClausesCount = new int[2 * n];
        // Initialize the counters for literals in clauses.
        for(int lit = -n; lit <= n; lit++) {
            if(lit != 0) {
                inActiveClausesCount[idx(lit)] = literalInClause.get(idx(lit)).size();
            }
        }        

        // Sort the clauses according to how many other clauses their literals are connected to.
        if(SORT_CLAUSES) {
            // How many clauses the literals in the given clause connect to.
            IntUnaryOperator clauseConnections = c -> {
                int total = 0;
                for(int lit: clauses[c]) {
                    total += 2 * inActiveClausesCount[idx(lit)]; // Same literal, bigger weight
                    total += inActiveClausesCount[idx(-lit)]; // Opposite literal, some weight
                }
                return total;
            };
            
            // Comparator to sort clauses based on their connections to clauses.
            Comparator<Integer> clauseComparator = (c1, c2) -> {
                int n1 = clauseConnections.applyAsInt(c1);
                int n2 = clauseConnections.applyAsInt(c2);
                return n2 - n1;
            };
            
            // Rearrange the dancing list based on sorting by these connection counts. 
            ArrayList<Integer> clausePerm = new ArrayList<>();
            for(int i = 0; i < m; i++) {
                if(next[i] != i) { clausePerm.add(i); }
            }
            if(verbose) {
                System.out.println("Instance with " + n + " variables and " +
                clausePerm.size() + " clauses, of which " + unitClauses.size() + " are unit.");
            }

            // Sort the permutation of clause indices.
            clausePerm.sort(clauseComparator);

            // Re-link all the clauses according to the sorted permutation.
            int curr = clausePerm.get(0);
            next[HEAD] = curr;
            prev[curr] = HEAD;
            for(int clauseIndex = 1; clauseIndex < clausePerm.size(); clauseIndex++) {
                int clause = clausePerm.get(clauseIndex);
                next[curr] = clause; prev[clause] = curr;
                curr = clause;
            }
            next[curr] = HEAD; prev[HEAD] = curr;
        }
        
        // Whether the choice of the literal at each recursionLevel was forced by previous choices.
        boolean[] forcedChoice = new boolean[m];
        // Time stamps for when the propositions inside each clause were last sorted.
        long[] lastSorted = new long[m];
        
        Arrays.fill(lastSorted, -SORT_THRESHOLD - 1);
        // The recursionLevel that the iterated backtracking is currently at.
        int recursionLevel = 0;
        
        // Backtrack until return from top recursionLevel, or every clause is satisfied.
        while(recursionLevel >= 0 && next[HEAD] != HEAD) {
            // If the current recursionLevel does not have an assigned literal, find and choose one now.
            if(literalUsed[recursionLevel] == 0) {
                int clauseToUse = UNASSIGNED; // Unsatisfied clause to take the literal from.
                forcedChoice[recursionLevel] = false;
                // Use an unsatisfied unit clause, if one exists.
                while(unitClauses.size() > 0) {
                    int unitClause = unitClauses.pop();
                    if(satisfiedAtLevel[unitClause] == UNASSIGNED) { // Found one, let's use this one.
                        clauseToUse = unitClause;
                        unitClauseC++;
                        forcedChoice[recursionLevel] = true;
                        break;
                    }
                }
                // Otherwise, use an available literal from the first unsatisfied clause.
                if(clauseToUse == UNASSIGNED) {
                    clauseToUse = next[HEAD];
                    assert chances[clauseToUse] > 1; // Not a unit clause...
                    // Sort the literals of the chosen clause based on remaining clause counts.
                    if(advanceC - lastSorted[clauseToUse] > SORT_THRESHOLD) {
                        lastSorted[clauseToUse] = advanceC;
                        int[] literals = clauses[clauseToUse];
                        // Simple insertion sort.
                        for(int i = 1; i < literals.length; i++) {
                            assert assignedAtLevel[idx(literals[i])] == UNASSIGNED;
                            int j = i;
                            while(j > 0 && inActiveClausesCount[idx(literals[j])] > inActiveClausesCount[idx(literals[j-1])]) {
                                int tmp = literals[j]; literals[j] = literals[j-1]; literals[j-1] = tmp; j--;
                            }
                        }    
                    }
                }
                // From the chosen clause, use the first literal whose negation has not been assigned yet.
                for(int literal: clauses[clauseToUse]) {
                    if(assignedAtLevel[idx(-literal)] == UNASSIGNED) {
                        literalUsed[recursionLevel] = literal; break;
                    }
                }                
            }            
            
            // Use the literal that was chosen for this recursionLevel. One must exist at this point.
            int literalToUse = literalUsed[recursionLevel];
            
            // Stage 0: Try making the chosen literal true and see what happens.
            // Stage 1: Try making the chosen literal false and see what happens.
            // Stage 2: Backtrack to most recent earlier choice point.
            
            // If at iterationStage 1 or 2, undo the effect of making current literal true. However,
            // skip the undo of iterationStage 1 if the choice of that literal was forced.
            if(iterationStage[recursionLevel] > 0 && (iterationStage[recursionLevel] == 1 || !forcedChoice[recursionLevel])) {
                // Depending on stage of iteration, use either current literal or its negation.
                int oppositeLiteral = iterationStage[recursionLevel] == 1 ? literalToUse: -literalToUse;
                // That literal is no longer part of the solution.
                assignedAtLevel[idx(oppositeLiteral)] = UNASSIGNED;
                // Clauses that became satisfied at this recursionLevel are no longer satisfied.
                for(int clause: literalInClause.get(idx(oppositeLiteral))) {
                    if(satisfiedAtLevel[clause] == recursionLevel) {
                        satisfiedAtLevel[clause] = UNASSIGNED;
                        prev[next[clause]] = next[prev[clause]] = clause; // Back to dancing list you go.
                        for(int li: clauses[clause]){
                            if(assignedAtLevel[idx(li)] == UNASSIGNED && assignedAtLevel[idx(-li)] == UNASSIGNED) {
                                ++inActiveClausesCount[idx(li)];
                            }
                        }
                    }
                }
                // Each clause that contains the opposite literal gets another chance.
                for(int c: literalInClause.get(idx(-oppositeLiteral))) {
                    if(satisfiedAtLevel[c] == UNASSIGNED) { ++chances[c]; }
                }
            }            

            // If at iterationStage 0 or 1, propagate the effects of making this literal true.
            // However, skip the iterationStage 1 if the choice of literal was forced at this recursionLevel.
            if(iterationStage[recursionLevel] < 2 && (iterationStage[recursionLevel] == 0 || !forcedChoice[recursionLevel])) {
                // Use the literal at iterationStage 0, and its negation at iterationStage 1.
                literalToUse = iterationStage[recursionLevel] == 0 ? literalToUse: -literalToUse;
                assignedAtLevel[idx(literalToUse)] = recursionLevel;
                boolean itsStillGood = false;
                int unitClausesAdded = 0; // Keep track of unit clauses added this level, needed for undo.
                for(int clause: literalInClause.get(idx(literalToUse))) {
                    if(satisfiedAtLevel[clause] == UNASSIGNED) {
                        itsStillGood = true; // Literal becomes good.
                        satisfiedAtLevel[clause] = recursionLevel;
                        prev[next[clause]] = prev[clause]; next[prev[clause]] = next[clause];
                        // Literals of that clause now occur in one fewer unsatisfied clause.
                        for(int literal: clauses[clause]) {
                            if(assignedAtLevel[idx(literal)] == UNASSIGNED && assignedAtLevel[idx(-literal)] == UNASSIGNED) {
                                --inActiveClausesCount[idx(literal)];
                            }
                        }
                    }
                }
                // Forward checking cutoff to recognize a futile branch.
                for(int clause: literalInClause.get(idx(-literalToUse))) {
                    if(satisfiedAtLevel[clause] == UNASSIGNED) {
                        // If some clause has become impossible to satisfy, force a cutoff.
                        if(--chances[clause] == 0) {
                            if(itsStillGood) { forwardCheckingC++; }
                            itsStillGood = false;
                        }
                        // Bring the clause into the set of known unit clauses.
                        else if(chances[clause] == 1) {
                            unitClauses.push(clause);
                            unitClausesAdded++;
                        }
                    }
                }
                
                iterationStage[recursionLevel]++; // Advance to next iterationStage at the current recursionLevel.
                if(itsStillGood) { // Advance to the next recursionLevel, or give up.
                    recursionLevel = (++advanceC != giveUp) ? recursionLevel + 1 : -1;
                }
                else { // Cancel all unit clauses added at this iterationStage.
                    for(int count = 0; count < unitClausesAdded; count++) { unitClauses.pop(); }
                }
            }
            else { // If iterationStage == 2 at current recursionLevel, backtrack to previous level.
                unitClauses.clear(); // None of the added unit clauses is good anymore.
                literalUsed[recursionLevel] = iterationStage[recursionLevel] = 0;
                recursionLevel--;
            } 
        } // End of while-loop of simulated backtracking.
        
        if(verbose) {
            System.out.print(advanceC + " advances, ");
            System.out.print(unitClauseC + " unit clauses, ");
            System.out.println(forwardCheckingC + " forward checking cutoffs.");
        }
        
        // Reconstruct the solution from the literals that were taken in.
        if(next[m] == m) { // No unsatisfied clauses remain, so a working solution was found.
            boolean[] result = new boolean[n + 1];
            for(int level = 0; level < recursionLevel; level++) {
                int literal = literalUsed[level];
                if(literal > 0 && assignedAtLevel[idx(literal)] > UNASSIGNED) { result[literal] = true; }
                else if(literal < 0 && assignedAtLevel[idx(-literal)] > UNASSIGNED) { result[-literal] = true; }
            }
            return result;
        }
        else { return null; } // No solution was found.
    }
    
    /**
     * Read the problem instance of SAT from a standard DIMACS file.
     * @param filename The name of the file.
     * @return The solution vector that was found, or null if there is no solution.
     */
    public static boolean[] readDimacsProblem(String filename) throws IOException {
        Scanner scanner = new Scanner(new File(filename));
        int[][] clauses = null;
        int variableCount = 0, clauseCount = 0;
        while(scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if(line.charAt(0) == 'c') { continue; } // comment line in DIMACS
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
                     if(Math.abs(clause[loc]) > variableCount) { variableCount = Math.abs(clause[loc]); }
                }
                clauses[clauseCount++] = clause;
            }
        }
        System.out.println("Read SAT instance with " + variableCount + " variables and " + clauseCount + " clauses.");
        long startTime = System.currentTimeMillis();
        boolean[] solution = SATSolver.solve(variableCount, clauses, true, -1);
        long endTime = System.currentTimeMillis();
        System.out.println("Finished in " + (endTime - startTime) + " ms.");
        if(solution == null) { System.out.println("This instance was unsatisfiable."); }
        else { 
            int assignedTrueCount = 0;
            for(int i = 1; i <= variableCount; i++) {
                if(solution[i]) assignedTrueCount++; }

            System.out.println("Solution has " + assignedTrueCount + " variables set true.");
        }
        return solution;
    }
}