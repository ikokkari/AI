import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.function.IntUnaryOperator;

// DPLL backtracking SAT Solver with optimizations for unit clauses.
// Ilkka Kokkarinen, November 5 2018, ilkka.kokkarinen@gmail.com.

public class SATSolver {

    // Encode positive and negative integers into nonnegative indices.
    private static int idx(int lit) {
        return lit > 0 ? 2 * (lit-1) : 2 * (-lit) - 1;
    }       
   
    /**
     * Solve the given instance of propositional logic satisfiability.
     * @param n Total number of propositional variables 1, ..., {@code n}.
     * @param clauses The individual clauses given as an array whose each element is an array
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
     * @param giveup After how many advances should the search give up. Use -1 for never give up.
     * @return Array of {@code n+1} truth values whose element in position {@code i} gives
     * the truth value of propositional variable {@code i}.
     */
    public static boolean[] solve(int n, final int[][] clauses, boolean verbose, long giveup) {
        // Counters for measurement and debugging of this algorithm.
        long unitClauseC = 0, forwardCheckingC = 0, advanceC = 0;
        // Whether clauses should be sorted before backtracking begins.
        final boolean SORT_CLAUSES = true;
        // Required delay between sorting the literals in a clause.
        final long SORT_THRESHOLD = 50;
        
        // Number of clauses to solve.
        int m = clauses.length;        
        // Keep the unsatisfied clauses in dancing list, with m used as sentinel.
        int[] next = new int[m + 1];
        int[] prev = new int[m + 1];                
        for(int i = 0; i <= m; i++) {
            next[i] = i < m ? i + 1: 0;
            prev[i] = i > 0 ? i - 1: m;
        }
        
        // List of clauses where each literal appears.
        List<List<Integer>> literalInClause = new ArrayList<>();
        // Which level of recursion each literal was taken to the solution.
        int[] takenAt = new int[2 * n];
        for(int i = 0; i < 2 * n; i++) { 
            literalInClause.add(new ArrayList<>());
            takenAt[i] = -1;
        }
        
        // How many chances each clause has remaining to become true.
        int[] chances = new int[m + 1];
        // The set of unit clauses known at the moment.
        LinkedList<Integer> unitClauses = new LinkedList<Integer>();
        // The literal used for each level in the current partial solution.
        int[] literalUsed = new int[m + 1];
        // The stage of iteration of possibilities at each level.
        // 0 = try the current literal, 1 = try its negation, 2 = backtrack.
        int[] stage = new int[m + 1];
        // Which level each clause became satisfied.
        int[] satisfiedAt = new int[m];
        
        // Fill in the various tables used in the backtracking algorithm.
        for(int i = 0; i < m; i++) {
            satisfiedAt[i] = -1;
            if(clauses[i] == null) { // Just in case.
                next[prev[i]] = next[i]; prev[next[i]] = prev[i];
                next[i] = prev[i] = i;
            }
            else {
                chances[i] = clauses[i].length;
                if(chances[i] == 1) { 
                    unitClauses.push(i);
                }
                for(int lit: clauses[i]) {
                    literalInClause.get(idx(lit)).add(i);
                }
            }
        }                  
        
        // How many unsatisfied clauses each literal still occurs in.
        int[] inClauseCount = new int[2 * n];
        // Initialize the counters for literals in clauses.
        for(int lit = -n; lit <= n; lit++) {
            if(lit != 0) {
                inClauseCount[idx(lit)] = literalInClause.get(idx(lit)).size();
            }
        }        
        
        if(SORT_CLAUSES) {
            // How many clauses the literals in the given clause connect to.
            IntUnaryOperator clauseConnections = c -> {
                int total = 0;
                for(int lit: clauses[c]) { total += inClauseCount[idx(lit)]; }
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
            Collections.sort(clausePerm, clauseComparator);        
            int curr = clausePerm.get(0); next[m] = curr; prev[curr] = m;
            for(int i = 1; i < clausePerm.size(); i++) {
                int c = clausePerm.get(i);
                next[curr] = c; prev[c] = curr;
                curr = c;
            }
            next[curr] = m; prev[m] = curr;
        }
        
        // Whether the choice of the literal at each level was forced by previous choices.
        boolean[] forcedChoice = new boolean[m];
        // When was each clause last sorted.
        long[] lastSorted = new long[m];
        
        Arrays.fill(lastSorted, -SORT_THRESHOLD - 1);
        // The level that the iterated backtracking is currently at.
        int level = 0;
        
        // Backtrack until return from top level, or every clause is satisfied.
        while(level >= 0 && next[m] != m) {            
            // If the current level does not have an assigned literal, choose one.
            if(literalUsed[level] == 0) {                
                int c = -1; // Unsatisfied clause to take the literal from.
                forcedChoice[level] = false;
                // Use an unsatisfied unit clause, if one exists.
                while(unitClauses.size() > 0) {
                    int cc = unitClauses.pop();
                    if(satisfiedAt[cc] == -1) { 
                        c = cc; unitClauseC++; forcedChoice[level] = true; break; 
                    }
                }
                // Otherwise, use an available literal from the next unsatisfied clause.
                if(c == -1) {
                    c = next[m]; assert chances[c] > 1; // Not a unit clause...
                    // Sort the literals of the chosen clause based on remaining clause counts.
                    if(advanceC - lastSorted[c] > SORT_THRESHOLD) {
                        lastSorted[c] = advanceC;
                        int[] cc = clauses[c];
                        for(int i = 1; i < cc.length; i++) {
                            assert takenAt[idx(cc[i])] == -1;
                            int j = i;
                            while(j > 0 && inClauseCount[idx(cc[j])] > inClauseCount[idx(cc[j-1])]) {
                                int tmp = cc[j]; cc[j] = cc[j-1]; cc[j-1] = tmp; j--;
                            }
                        }    
                    }
                }
                // From the chosen clause, use the first literal that can still be made true.                
                for(int lit: clauses[c]) { 
                    if(takenAt[idx(-lit)] == -1) { literalUsed[level] = lit; break; }
                }                
            }            
            
            // Use the literal that was chosen for this level. One must now exist.
            int lit = literalUsed[level]; assert lit != 0;
            
            // Stage 0: Try making the chosen literal true and see what happens.
            // Stage 1: Try making the chosen literal false and see what happens.
            // Stage 2: Backtrack to most recent earlier choice point.
            
            // If at stage 1 or 2, undo the effect of making current literal true. However,
            // skip the undo of stage 1 if the choice of that literal was forced.
            if(stage[level] > 0 && (stage[level] == 1 || !forcedChoice[level])) {
                int opp = stage[level] == 1 ? lit: -lit;
                // That literal is no longer part of the solution.
                takenAt[idx(opp)] = -1;
                // Clauses that became satisfied at this level are no longer satisfied.
                for(int c: literalInClause.get(idx(opp))) {
                    if(satisfiedAt[c] == level) {
                        satisfiedAt[c] = -1;
                        prev[next[c]] = next[prev[c]] = c; // Back to dancing list you go.
                        for(int li: clauses[c]){
                            if(takenAt[idx(li)] == -1 && takenAt[idx(-li)] == -1) {
                                ++inClauseCount[idx(li)];
                            }
                        }
                    }
                }
                // Each clause that contains the opposite literal gets another chance.
                for(int c: literalInClause.get(idx(-opp))) { 
                    if(satisfiedAt[c] == -1) { ++chances[c]; }
                }
            }            

            // If at stage 0 or 1, propagate the effects of making this literal true.
            // However, skip the stage 1 if the choice of literal was forced at this level.
            if(stage[level] < 2 && (stage[level] == 0 || !forcedChoice[level])) {
                // Use the literal at stage 0, and its negation at stage 1.
                lit = stage[level] == 0 ? lit: -lit; 
                takenAt[idx(lit)] = level;
                boolean itsStillGood = false;
                int unitClausesAdded = 0;
                for(int c: literalInClause.get(idx(lit))) {
                    if(satisfiedAt[c] == -1) {
                        itsStillGood = true; // Literal becomes good.
                        satisfiedAt[c] = level;
                        prev[next[c]] = prev[c]; next[prev[c]] = next[c];
                        // Literals of that clause now occur in one fewer unsatisfied clause.
                        for(int li: clauses[c]) {
                            if(takenAt[idx(li)] == -1 && takenAt[idx(-li)] == -1) {
                                --inClauseCount[idx(li)];
                            }
                        }
                    }
                }                
                // Forward checking cutoff to recognize a futile branch.
                for(int c: literalInClause.get(idx(-lit))) {
                    if(satisfiedAt[c] == -1) {
                        // If some clause has become impossible to satisfy, force a cutoff.
                        if(--chances[c] == 0) { 
                            if(itsStillGood) { forwardCheckingC++; }
                            itsStillGood = false;
                        }
                        // Bring the clause into the set of known unit clauses.
                        else if(chances[c] == 1) { 
                            unitClauses.push(c); unitClausesAdded++; 
                        }
                    }
                }
                
                stage[level]++; // Advance to next stage at the current level.
                if(itsStillGood) { // Advance to the next level, or give up.
                    level = (++advanceC != giveup) ? level + 1 : -1;
                }
                else { // Cancel all unit clauses added at this stage.
                    for(int i = 0; i < unitClausesAdded; i++) { unitClauses.pop(); }
                }
            }
            else { // If at stage 2 at current level, backtrack to previous level.
                unitClauses.clear(); // None of the added unit clauses is good any more.
                literalUsed[level] = stage[level] = 0;
                level--;
            } 
        } // End of while-loop
        
        if(verbose) {
            System.out.print(advanceC + " advances, ");
            System.out.print(unitClauseC + " unit clauses, ");
            System.out.println(forwardCheckingC + " forward checking cutoffs.");
        }
        
        // Reconstruct the solution from the literals that were taken in.
        if(next[m] == m) { // No unsatisfied clauses remain, so a solution was found.
            boolean[] result = new boolean[n + 1];
            for(int i = 0; i < level; i++) {
                int lit = literalUsed[i];
                if(lit > 0 && takenAt[idx(lit)] > -1) { result[lit] = true; }
                else if(lit < 0 && takenAt[idx(-lit)] > -1) { result[-lit] = true; }                 
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
        System.out.println("Read SAT instance with " + vars + " variables and " + cloc + " clauses."); 
        long startTime = System.currentTimeMillis();
        boolean[] solution = SATSolver.solve(vars, clauses, true, -1);
        long endTime = System.currentTimeMillis();
        System.out.println("Finished in " + (endTime - startTime) + " ms.");
        if(solution == null) { System.out.println("This instance was unsatisfiable."); }
        else { 
            int count = 0;
            for(int i = 1; i <= vars; i++) { if(solution[i]) count++; }
            System.out.println("Solution has " + count + " variables set true.");
        }
        return solution;
    }
}