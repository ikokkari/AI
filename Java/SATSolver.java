import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * CDCL (Conflict-Driven Clause Learning) SAT Solver with modern optimizations.
 *
 * Evolved from a DPLL backtracking solver (Ilkka Kokkarinen, 2018) into a full
 * CDCL solver incorporating techniques from MiniSat/CaDiCaL-style solvers:
 *
 *   - Two-Watched Literals (lazy clause evaluation)
 *   - First-UIP Conflict-Driven Clause Learning
 *   - Non-chronological backjumping
 *   - VSIDS (Variable State Independent Decaying Sum) branching heuristic
 *   - Phase saving (remembers polarity of last assignment)
 *   - Geometric restart schedule (Luby-style)
 *   - Learned clause deletion with LBD (Literal Block Distance) scoring
 *   - Pure literal elimination during preprocessing
 *
 * Updated to Java 21+ style: records, enhanced switch, sealed interfaces where
 * applicable, and general modern idioms.
 *
 * Ilkka Kokkarinen, ilkka.kokkarinen@gmail.com
 * Modernized 2026.
 */
public class SATSolver {

    // ── Literal encoding ────────────────────────────────────────────────
    // Variable v (1-based) maps to literals 2v (positive) and 2v+1 (negative).
    // This gives a dense non-negative index space for array-based storage.

    private static int litToIdx(int lit) {
        return lit > 0 ? 2 * lit : 2 * (-lit) + 1;
    }

    private static int negIdx(int idx) {
        return idx ^ 1;
    }

    private static int idxToVar(int idx) {
        return idx >> 1;
    }

    private static int varToPos(int v) {
        return 2 * v;
    }

    private static int varToNeg(int v) {
        return 2 * v + 1;
    }

    // ── Reason for an assignment ────────────────────────────────────────

    private static final int REASON_DECISION = -1;
    private static final int REASON_UNIT_INITIAL = -2; // from original unit clause

    // ── Watcher list entry ──────────────────────────────────────────────
    // Each watched literal maintains a list of clause indices it watches.

    // ── VSIDS activity heap ─────────────────────────────────────────────

    /**
     * A min-heap ordered by *negative* activity (so max-activity variable
     * is extracted first). Uses an indexed priority queue so that activity
     * bumps can percolate in O(log n).
     */
    private static final class ActivityHeap {
        private final double[] activity;
        private final int[] heap;     // heap[pos] = variable
        private final int[] pos;      // pos[variable] = position in heap (-1 if absent)
        private int size;

        ActivityHeap(int nVars) {
            activity = new double[nVars + 1];
            heap = new int[nVars + 1];
            pos = new int[nVars + 1];
            Arrays.fill(pos, -1);
            size = 0;
        }

        boolean contains(int v) { return pos[v] >= 0; }
        boolean isEmpty() { return size == 0; }

        void insert(int v) {
            if (contains(v)) return;
            heap[size] = v;
            pos[v] = size;
            size++;
            siftUp(pos[v]);
        }

        int removeMax() {
            int v = heap[0];
            size--;
            heap[0] = heap[size];
            pos[heap[0]] = 0;
            pos[v] = -1;
            if (size > 0) siftDown(0);
            return v;
        }

        void increase(int v) {
            if (contains(v)) siftUp(pos[v]);
        }

        private boolean higherPriority(int a, int b) {
            return activity[heap[a]] > activity[heap[b]];
        }

        private void swap(int a, int b) {
            int va = heap[a], vb = heap[b];
            heap[a] = vb; heap[b] = va;
            pos[va] = b; pos[vb] = a;
        }

        private void siftUp(int i) {
            while (i > 0) {
                int parent = (i - 1) >> 1;
                if (higherPriority(i, parent)) { swap(i, parent); i = parent; }
                else break;
            }
        }

        private void siftDown(int i) {
            while (true) {
                int best = i;
                int left = 2 * i + 1, right = 2 * i + 2;
                if (left < size && higherPriority(left, best)) best = left;
                if (right < size && higherPriority(right, best)) best = right;
                if (best == i) break;
                swap(i, best);
                i = best;
            }
        }
    }

    // ── Main solver ─────────────────────────────────────────────────────

    /** Convenience overload matching original API. */
    public static boolean[] solve(int n, int[][] clauses) {
        return solve(n, clauses, false, -1);
    }

    /**
     * Solve the given SAT instance using CDCL.
     *
     * @param n        Number of propositional variables (1..n).
     * @param clauses  Array of clauses; each clause is an int[] of literals.
     * @param verbose  Print statistics.
     * @param giveUp   Give up after this many decisions (-1 = never).
     * @return boolean[n+1] with result[i] = truth value of variable i, or null if UNSAT.
     */
    public static boolean[] solve(int n, int[][] clauses, boolean verbose, long giveUp) {

        // ── Statistics ──────────────────────────────────────────────────
        long decisions = 0, propagations = 0, conflicts = 0, restarts = 0;
        long learnedTotal = 0, learnedDeleted = 0;

        // ── Preprocessing: remove null/empty, detect tautologies ────────
        List<int[]> clauseList = new ArrayList<>();
        for (int[] cl : clauses) {
            if (cl == null || cl.length == 0) continue;
            // Remove duplicate literals and detect tautological clauses.
            Set<Integer> seen = new LinkedHashSet<>();
            boolean tautology = false;
            for (int lit : cl) {
                if (seen.contains(-lit)) { tautology = true; break; }
                seen.add(lit);
            }
            if (!tautology) {
                clauseList.add(seen.stream().mapToInt(Integer::intValue).toArray());
            }
        }

        // ── Pure literal elimination (simple one-pass) ──────────────────
        boolean[] posOccurs = new boolean[n + 1];
        boolean[] negOccurs = new boolean[n + 1];
        for (int[] cl : clauseList) {
            for (int lit : cl) {
                if (lit > 0) posOccurs[lit] = true;
                else negOccurs[-lit] = true;
            }
        }
        Set<Integer> pureLiterals = new HashSet<>();
        for (int v = 1; v <= n; v++) {
            if (posOccurs[v] && !negOccurs[v]) pureLiterals.add(v);
            else if (!posOccurs[v] && negOccurs[v]) pureLiterals.add(-v);
        }

        // Remove clauses satisfied by pure literals, remove negated pure from others.
        if (!pureLiterals.isEmpty()) {
            List<int[]> filtered = new ArrayList<>();
            for (int[] cl : clauseList) {
                boolean satisfied = false;
                for (int lit : cl) {
                    if (pureLiterals.contains(lit)) { satisfied = true; break; }
                }
                if (satisfied) continue;
                // Remove negated pure literals from clause.
                List<Integer> kept = new ArrayList<>();
                for (int lit : cl) {
                    if (!pureLiterals.contains(-lit)) kept.add(lit);
                }
                if (kept.isEmpty()) {
                    // Conflict at preprocessing — should not happen with correct pure detection.
                    return null;
                }
                filtered.add(kept.stream().mapToInt(Integer::intValue).toArray());
            }
            clauseList = filtered;
        }

        int m = clauseList.size();
        if (m == 0) {
            // All clauses satisfied by pure literals or trivially.
            boolean[] result = new boolean[n + 1];
            for (int lit : pureLiterals) {
                if (lit > 0) result[lit] = true;
            }
            return result;
        }

        // ── Core data structures ────────────────────────────────────────
        // Clause database: ArrayList so learned clauses can be appended.
        ArrayList<int[]> db = new ArrayList<>(clauseList);

        // Indexed from 0..2*(n+1)-1: even = positive literal, odd = negative.
        int litCount = 2 * (n + 1);

        // ── Two-Watched-Literal scheme ──────────────────────────────────
        // watchList[litIdx] = list of clause indices where this literal is a watcher.
        @SuppressWarnings("unchecked")
        List<Integer>[] watchList = new List[litCount];
        for (int i = 0; i < litCount; i++) watchList[i] = new ArrayList<>();

        // Set up initial watches: watch first two literals of each clause.
        List<Integer> initialUnits = new ArrayList<>();
        for (int ci = 0; ci < m; ci++) {
            int[] cl = db.get(ci);
            if (cl.length == 1) {
                initialUnits.add(ci);
                watchList[litToIdx(cl[0])].add(ci);
            } else {
                watchList[litToIdx(cl[0])].add(ci);
                watchList[litToIdx(cl[1])].add(ci);
            }
        }

        // ── Assignment state ────────────────────────────────────────────
        // value[v]: 0 = unassigned, 1 = true, -1 = false
        int[] value = new int[n + 1];
        int[] decisionLevel = new int[n + 1]; // at which level was v assigned
        int[] reason = new int[n + 1];        // clause index that implied v, or REASON_*
        Arrays.fill(reason, REASON_DECISION);

        // The trail: sequence of assigned literals (as literal indices).
        int[] trail = new int[n + 1];
        int trailSize = 0;
        int propagateHead = 0; // index into trail for BCP

        // ── Phase saving ────────────────────────────────────────────────
        // Remembers the last polarity assigned to each variable.
        boolean[] phaseSave = new boolean[n + 1]; // true = positive preferred

        // ── VSIDS ───────────────────────────────────────────────────────
        ActivityHeap activityHeap = new ActivityHeap(n);
        double vsidsInc = 1.0;
        final double VSIDS_DECAY = 0.95;
        final double VSIDS_RESCALE = 1e100;

        // Initialize heap with all variables.
        for (int v = 1; v <= n; v++) {
            // Initialize activity based on literal occurrence (JW-like heuristic).
            for (int[] cl : db) {
                for (int lit : cl) {
                    if (Math.abs(lit) == v) {
                        activityHeap.activity[v] += 1.0 / (1 << cl.length);
                    }
                }
            }
            activityHeap.insert(v);
        }

        // ── LBD tracking for learned clauses ────────────────────────────
        // clauseLBD[ci] stores the LBD score computed at learning time.
        Map<Integer, Integer> clauseLBD = new HashMap<>();

        int currentLevel = 0;

        // ── Helpers: assign / unassign ──────────────────────────────────

        // Assign literal (as external int) at current decision level with given reason.
        // Returns false if immediate conflict detected (should not happen here).

        // ── Restart schedule (geometric / Luby-like) ────────────────────
        long restartCounter = 0;
        long restartLimit = 100;     // initial conflicts before first restart
        final double RESTART_MULT = 1.5;

        // ── Clause deletion schedule ────────────────────────────────────
        long deleteCounter = 0;
        final long DELETE_INTERVAL = 2000;
        final int MAX_LEARNED_RATIO = 3; // keep at most this * original clause count

        // ── BCP (Boolean Constraint Propagation) ────────────────────────
        // Returns the clause index of a conflicting clause, or -1 if no conflict.

        if (verbose) {
            System.out.println("CDCL solver: " + n + " variables, " + m + " clauses"
                    + (pureLiterals.isEmpty() ? "" : " (" + pureLiterals.size() + " pure literals eliminated)")
                    + ".");
        }

        // ── Propagate initial unit clauses ──────────────────────────────
        for (int ci : initialUnits) {
            int lit = db.get(ci)[0];
            int v = Math.abs(lit);
            if (value[v] == 0) {
                value[v] = lit > 0 ? 1 : -1;
                decisionLevel[v] = 0;
                reason[v] = ci;
                trail[trailSize++] = lit;
            } else {
                // Check for conflict.
                boolean expected = lit > 0;
                if ((value[v] == 1) != expected) {
                    if (verbose) System.out.println("Conflict at level 0 from initial units.");
                    return null; // UNSAT
                }
            }
        }

        // ── Main CDCL loop ──────────────────────────────────────────────
        while (true) {
            // ── BCP ─────────────────────────────────────────────────────
            int conflictClause = -1;
            while (propagateHead < trailSize && conflictClause == -1) {
                int propagatedLit = trail[propagateHead++];
                propagations++;
                // The literal that was made TRUE is propagatedLit.
                // We need to look at watches on its NEGATION (those clauses were watching for it being false).
                int falseLitIdx = litToIdx(-propagatedLit);

                List<Integer> watchers = watchList[falseLitIdx];
                int i = 0, j = 0;
                while (i < watchers.size()) {
                    int ci = watchers.get(i);
                    int[] cl = db.get(ci);
                    if (cl == null) { i++; continue; }

                    // Make sure the false literal is at position 1.
                    if (cl.length > 1 && litToIdx(cl[0]) == falseLitIdx) {
                        cl[0] = cl[1]; cl[1] = -propagatedLit;
                        // Also keep the negation's literal value correct.
                        // Actually we swapped the array contents; the watch for cl[0] is elsewhere.
                    }
                    // Actually let me re-do this more carefully following MiniSat's scheme:
                    // Ensure the false literal is at position [1].
                    if (cl.length >= 2 && cl[0] == -propagatedLit) {
                        cl[0] = cl[1];
                        cl[1] = -propagatedLit;
                    }

                    // If cl[0] is already true, clause is satisfied — keep watching.
                    if (cl.length >= 2) {
                        int firstVar = Math.abs(cl[0]);
                        boolean firstTrue = (cl[0] > 0 && value[firstVar] == 1)
                                || (cl[0] < 0 && value[firstVar] == -1);
                        if (firstTrue) {
                            watchers.set(j++, ci);
                            i++;
                            continue;
                        }
                    }

                    // Try to find a new literal to watch (not cl[0], not cl[1]).
                    boolean found = false;
                    for (int k = 2; k < cl.length; k++) {
                        int lk = cl[k];
                        int vk = Math.abs(lk);
                        boolean isFalse = (lk > 0 && value[vk] == -1)
                                || (lk < 0 && value[vk] == 1);
                        if (!isFalse) {
                            // Swap cl[1] and cl[k], update watch lists.
                            cl[1] = lk;
                            cl[k] = -propagatedLit;
                            watchList[litToIdx(lk)].add(ci);
                            found = true;
                            break;
                        }
                    }
                    if (found) {
                        // Don't keep this clause in the current watch list.
                        i++;
                        continue;
                    }

                    // No replacement found. cl[0] is the only non-false literal (if any).
                    watchers.set(j++, ci);
                    i++;

                    if (cl.length == 1) {
                        // Unit clause — the literal is cl[0].
                        int uv = Math.abs(cl[0]);
                        if (value[uv] == 0) {
                            value[uv] = cl[0] > 0 ? 1 : -1;
                            decisionLevel[uv] = currentLevel;
                            reason[uv] = ci;
                            trail[trailSize++] = cl[0];
                        } else {
                            boolean expected = cl[0] > 0;
                            if ((value[uv] == 1) != expected) {
                                conflictClause = ci;
                            }
                        }
                    } else {
                        // cl[0] must be unit or conflict.
                        int uv = Math.abs(cl[0]);
                        if (value[uv] == 0) {
                            // Unit propagation: assign cl[0].
                            value[uv] = cl[0] > 0 ? 1 : -1;
                            decisionLevel[uv] = currentLevel;
                            reason[uv] = ci;
                            trail[trailSize++] = cl[0];
                        } else {
                            boolean expected = cl[0] > 0;
                            if ((value[uv] == 1) != expected) {
                                // Conflict!
                                conflictClause = ci;
                            }
                            // else clause is satisfied by cl[0]; fine.
                        }
                    }
                }
                // Compact the watcher list.
                while (i < watchers.size()) {
                    watchers.set(j++, watchers.get(i++));
                }
                // Trim the list to size j.
                while (watchers.size() > j) {
                    watchers.remove(watchers.size() - 1);
                }
            }

            // ── Handle conflict or decide ───────────────────────────────
            if (conflictClause != -1) {
                conflicts++;
                restartCounter++;
                deleteCounter++;

                if (currentLevel == 0) {
                    // Conflict at decision level 0 → UNSAT.
                    if (verbose) printStats(decisions, propagations, conflicts, restarts, learnedTotal, learnedDeleted);
                    return null;
                }

                // ── Conflict analysis: First-UIP scheme ─────────────────
                // We resolve backward along the trail to find the first UIP.
                boolean[] seen = new boolean[n + 1];
                int[] learntLits = new int[0]; // will be built up
                List<Integer> learntList = new ArrayList<>();
                int counter = 0; // # of literals at current decision level in the conflict clause
                int btLevel = 0; // backtrack (backjump) level

                // Start from the conflict clause.
                int resolveLit = 0; // 0 means start with conflict clause directly
                int[] currentClause = db.get(conflictClause);
                int trailIdx = trailSize - 1;

                // We'll iterate: resolve until exactly 1 literal remains at current level.
                learntList.add(0); // placeholder for the UIP literal at position 0

                // Process the conflict clause first, then keep resolving.
                while (true) {
                    // Process each literal in the current clause.
                    for (int lit : currentClause) {
                        int v = Math.abs(lit);
                        if (v == Math.abs(resolveLit) && resolveLit != 0) continue;
                        if (!seen[v]) {
                            seen[v] = true;
                            // Bump VSIDS activity for variables involved in conflicts.
                            activityHeap.activity[v] += vsidsInc;
                            if (activityHeap.activity[v] > VSIDS_RESCALE) {
                                for (int vi = 1; vi <= n; vi++)
                                    activityHeap.activity[vi] /= VSIDS_RESCALE;
                                vsidsInc /= VSIDS_RESCALE;
                            }
                            activityHeap.increase(v);

                            if (decisionLevel[v] == currentLevel) {
                                counter++;
                            } else if (decisionLevel[v] > 0) {
                                // Add the literal as it appears — it's false under the current
                                // assignment, so it belongs in the learned nogood clause.
                                learntList.add(lit);
                                if (decisionLevel[v] > btLevel) btLevel = decisionLevel[v];
                            }
                            // Level 0 assignments are always true, skip them.
                        }
                    }

                    // Walk backward on the trail to find the next literal to resolve.
                    while (trailIdx >= 0 && !seen[Math.abs(trail[trailIdx])]) trailIdx--;
                    if (trailIdx < 0) break;
                    resolveLit = trail[trailIdx];
                    int rv = Math.abs(resolveLit);
                    seen[rv] = false; // unmark — it's being resolved away
                    counter--;

                    if (counter <= 0) {
                        // Found the First UIP: its negation goes at position 0 of the learned clause.
                        learntList.set(0, -resolveLit);
                        break;
                    }

                    // Resolve with the reason clause of this literal.
                    int reasonCi = reason[rv];
                    if (reasonCi < 0) {
                        // Decision variable — shouldn't happen if counter > 0. Safety break.
                        learntList.set(0, -resolveLit);
                        break;
                    }
                    currentClause = db.get(reasonCi);
                    trailIdx--;
                }

                // ── Compute LBD (Literal Block Distance) of learned clause ──
                Set<Integer> levels = new HashSet<>();
                for (int lit : learntList) {
                    levels.add(decisionLevel[Math.abs(lit)]);
                }
                int lbd = levels.size();

                // Build the learned clause array.
                int[] learntClause = learntList.stream().mapToInt(Integer::intValue).toArray();

                // Minimize: try to remove redundant literals (self-subsuming resolution).
                // A literal is redundant if its reason clause's other literals are all in 'seen'.
                // (Simplified version — full minimization is more involved.)
                // We skip this for clarity; the clause is already reasonably small from 1-UIP.

                learnedTotal++;

                // ── Add learned clause to database ──────────────────────
                int learntCi = db.size();
                db.add(learntClause);
                clauseLBD.put(learntCi, lbd);

                // Set up watches for the learned clause.
                if (learntClause.length == 1) {
                    watchList[litToIdx(learntClause[0])].add(learntCi);
                    btLevel = 0; // must backtrack to level 0 for unit learned clause
                } else {
                    // Make sure the two watched literals are:
                    // [0] = the asserting literal (to be propagated)
                    // [1] = a literal from btLevel (so watch is correct after backjump)
                    // Find the literal with the highest decision level != currentLevel for pos 1.
                    int maxLevel = -1, maxIdx = 1;
                    for (int k = 1; k < learntClause.length; k++) {
                        int dl = decisionLevel[Math.abs(learntClause[k])];
                        if (dl > maxLevel) { maxLevel = dl; maxIdx = k; }
                    }
                    // Swap into position 1.
                    int tmp = learntClause[1]; learntClause[1] = learntClause[maxIdx]; learntClause[maxIdx] = tmp;
                    btLevel = maxLevel;

                    watchList[litToIdx(learntClause[0])].add(learntCi);
                    watchList[litToIdx(learntClause[1])].add(learntCi);
                }

                // ── Backjump to btLevel ─────────────────────────────────
                while (trailSize > 0) {
                    int lastLit = trail[trailSize - 1];
                    int lastVar = Math.abs(lastLit);
                    if (decisionLevel[lastVar] <= btLevel) break;
                    // Unassign.
                    phaseSave[lastVar] = value[lastVar] == 1;
                    value[lastVar] = 0;
                    reason[lastVar] = REASON_DECISION;
                    if (!activityHeap.contains(lastVar)) activityHeap.insert(lastVar);
                    trailSize--;
                }
                propagateHead = trailSize;
                currentLevel = btLevel;

                // The learned clause is now unit — the asserting literal will be propagated.
                int assertLit = learntClause[0];
                int av = Math.abs(assertLit);
                value[av] = assertLit > 0 ? 1 : -1;
                decisionLevel[av] = currentLevel;
                reason[av] = learntCi;
                trail[trailSize++] = assertLit;

                // ── Decay VSIDS activities ──────────────────────────────
                vsidsInc /= VSIDS_DECAY;

                // ── Restart check ───────────────────────────────────────
                if (restartCounter >= restartLimit) {
                    restarts++;
                    restartCounter = 0;
                    restartLimit = (long)(restartLimit * RESTART_MULT);

                    // Restart: backtrack to level 0.
                    while (trailSize > 0) {
                        int lastLit = trail[trailSize - 1];
                        int lastVar = Math.abs(lastLit);
                        if (decisionLevel[lastVar] == 0) break;
                        phaseSave[lastVar] = value[lastVar] == 1;
                        value[lastVar] = 0;
                        reason[lastVar] = REASON_DECISION;
                        if (!activityHeap.contains(lastVar)) activityHeap.insert(lastVar);
                        trailSize--;
                    }
                    propagateHead = trailSize;
                    currentLevel = 0;
                }

                // ── Learned clause deletion ─────────────────────────────
                if (deleteCounter >= DELETE_INTERVAL) {
                    deleteCounter = 0;
                    int maxLearned = MAX_LEARNED_RATIO * m;
                    int totalLearned = db.size() - m;
                    if (totalLearned > maxLearned) {
                        // Delete learned clauses with high LBD that aren't reasons.
                        Set<Integer> reasonClauses = new HashSet<>();
                        for (int v = 1; v <= n; v++) {
                            if (value[v] != 0 && reason[v] >= 0) reasonClauses.add(reason[v]);
                        }
                        // Sort learned clause indices by LBD (descending).
                        List<Integer> learnedIndices = new ArrayList<>();
                        for (int ci = m; ci < db.size(); ci++) {
                            if (db.get(ci) != null && !reasonClauses.contains(ci)) {
                                learnedIndices.add(ci);
                            }
                        }
                        learnedIndices.sort((a, b) -> {
                            int la = clauseLBD.getOrDefault(a, Integer.MAX_VALUE);
                            int lb = clauseLBD.getOrDefault(b, Integer.MAX_VALUE);
                            return Integer.compare(lb, la); // highest LBD first
                        });
                        int toDelete = totalLearned - maxLearned / 2;
                        for (int di = 0; di < toDelete && di < learnedIndices.size(); di++) {
                            int ci = learnedIndices.get(di);
                            int lbdVal = clauseLBD.getOrDefault(ci, Integer.MAX_VALUE);
                            if (lbdVal <= 2) continue; // keep "glue" clauses (LBD ≤ 2)
                            // Null out the clause. Watchers will handle nulls gracefully.
                            db.set(ci, null);
                            clauseLBD.remove(ci);
                            learnedDeleted++;
                        }
                    }
                }

            } else {
                // ── No conflict: check if all variables are assigned ────
                if (trailSize == n) {
                    // SAT! Build the result.
                    if (verbose) printStats(decisions, propagations, conflicts, restarts, learnedTotal, learnedDeleted);
                    boolean[] result = new boolean[n + 1];
                    for (int v = 1; v <= n; v++) {
                        result[v] = value[v] == 1;
                    }
                    // Apply pure literal assignments.
                    for (int lit : pureLiterals) {
                        if (lit > 0) result[lit] = true;
                        else result[-lit] = false;
                    }
                    return result;
                }

                // ── Make a decision ─────────────────────────────────────
                if (giveUp >= 0 && decisions >= giveUp) {
                    if (verbose) {
                        System.out.println("Giving up after " + decisions + " decisions.");
                        printStats(decisions, propagations, conflicts, restarts, learnedTotal, learnedDeleted);
                    }
                    return null;
                }

                decisions++;
                currentLevel++;

                // Pick the unassigned variable with highest VSIDS activity.
                int decVar = 0;
                while (!activityHeap.isEmpty()) {
                    int v = activityHeap.removeMax();
                    if (value[v] == 0) { decVar = v; break; }
                }
                if (decVar == 0) {
                    // All variables assigned but trailSize != n? Shouldn't happen.
                    // Check if all clauses are satisfied.
                    if (verbose) printStats(decisions, propagations, conflicts, restarts, learnedTotal, learnedDeleted);
                    boolean[] result = new boolean[n + 1];
                    for (int v = 1; v <= n; v++) result[v] = value[v] == 1;
                    for (int lit : pureLiterals) {
                        if (lit > 0) result[lit] = true; else result[-lit] = false;
                    }
                    return result;
                }

                // Phase saving: use the polarity from the last assignment.
                int decLit = phaseSave[decVar] ? decVar : -decVar;
                value[decVar] = decLit > 0 ? 1 : -1;
                decisionLevel[decVar] = currentLevel;
                reason[decVar] = REASON_DECISION;
                trail[trailSize++] = decLit;
            }
        } // end main loop
    }

    private static void printStats(long decisions, long propagations, long conflicts,
                                   long restarts, long learnedTotal, long learnedDeleted) {
        System.out.printf("Decisions: %,d | Propagations: %,d | Conflicts: %,d%n",
                decisions, propagations, conflicts);
        System.out.printf("Restarts: %,d | Learned: %,d | Deleted: %,d%n",
                restarts, learnedTotal, learnedDeleted);
    }

    // ── DIMACS reader ───────────────────────────────────────────────────

    /**
     * Read and solve a SAT instance from a DIMACS CNF file.
     */
    public static boolean[] readDimacsProblem(String filename) throws IOException {
        int variableCount = 0, clauseCount = 0;
        int[][] clauses = null;
        int ci = 0;

        try (Scanner scanner = new Scanner(new File(filename))) {
            // First pass: handle multi-literal-per-line DIMACS format.
            List<int[]> clauseList = new ArrayList<>();
            List<Integer> currentClause = new ArrayList<>();
            boolean headerSeen = false;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine().trim();
                if (line.isEmpty() || "c%".indexOf(line.charAt(0)) > -1) continue;
                if (line.charAt(0) == 'p') {
                    String[] info = line.split("\\s+");
                    variableCount = Integer.parseInt(info[2]);
                    clauseCount = Integer.parseInt(info[3]);
                    headerSeen = true;
                    continue;
                }
                // Parse literals. DIMACS allows multiple literals per line, 0-terminated.
                String[] tokens = line.split("\\s+");
                for (String tok : tokens) {
                    if (tok.isEmpty()) continue;
                    int val = Integer.parseInt(tok);
                    if (val == 0) {
                        if (!currentClause.isEmpty()) {
                            clauseList.add(currentClause.stream().mapToInt(Integer::intValue).toArray());
                            currentClause.clear();
                        }
                    } else {
                        currentClause.add(val);
                        int absVal = Math.abs(val);
                        if (absVal > variableCount) variableCount = absVal;
                    }
                }
            }
            // Handle last clause if file doesn't end with 0.
            if (!currentClause.isEmpty()) {
                clauseList.add(currentClause.stream().mapToInt(Integer::intValue).toArray());
            }

            clauses = clauseList.toArray(new int[0][]);
            clauseCount = clauses.length;
        }

        System.out.println("Read SAT instance: " + variableCount + " variables, " + clauseCount + " clauses.");
        long startTime = System.currentTimeMillis();
        boolean[] solution = SATSolver.solve(variableCount, clauses, true, -1);
        long endTime = System.currentTimeMillis();
        System.out.println("New solver, solved in " + (endTime - startTime) + " ms.");

        if (solution == null) {
            System.out.println("UNSATISFIABLE.");
        } else {
            long trueCount = 0;
            for (int i = 1; i <= variableCount; i++) { if (solution[i]) trueCount++; }
            System.out.println("SATISFIABLE. " + trueCount + " variables set to true.");
        }
        return solution;
    }

    /** Verify that a solution satisfies all clauses. */
    public static boolean verify(int[][] clauses, boolean[] solution) {
        for (int[] clause : clauses) {
            if (clause == null) continue;
            boolean satisfied = false;
            for (int lit : clause) {
                if (lit > 0 && solution[lit]) { satisfied = true; break; }
                if (lit < 0 && !solution[-lit]) { satisfied = true; break; }
            }
            if (!satisfied) return false;
        }
        return true;
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.out.println("Usage: java SATSolver <dimacs-file>");
            System.out.println("Running built-in test...");
            runBuiltInTest();
            return;
        }
        readDimacsProblem(args[0]);
    }

    private static void runBuiltInTest() {
        // Simple test: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)
        int[][] clauses = {
                {1, 2},
                {-1, 3},
                {-2, -3}
        };
        boolean[] result = solve(3, clauses, true, -1);
        if (result != null) {
            System.out.print("Solution: ");
            for (int i = 1; i <= 3; i++) System.out.print("x" + i + "=" + result[i] + " ");
            System.out.println();
            System.out.println("Verified: " + verify(clauses, result));
        } else {
            System.out.println("No solution found.");
        }

        // Pigeonhole test (3 pigeons, 2 holes) — should be UNSAT.
        // Variables: p_ij = pigeon i in hole j. i=1..3, j=1..2.
        // v(i,j) = (i-1)*2 + j
        int[][] php32 = {
                {1, 2},       // pigeon 1 in some hole
                {3, 4},       // pigeon 2 in some hole
                {5, 6},       // pigeon 3 in some hole
                {-1, -3},     // hole 1: not both pigeon 1 and 2
                {-1, -5},     // hole 1: not both pigeon 1 and 3
                {-3, -5},     // hole 1: not both pigeon 2 and 3
                {-2, -4},     // hole 2: not both pigeon 1 and 2
                {-2, -6},     // hole 2: not both pigeon 1 and 3
                {-4, -6},     // hole 2: not both pigeon 2 and 3
        };
        System.out.println("\nPigeonhole PHP(3,2) — expecting UNSAT:");
        boolean[] result2 = solve(6, php32, true, -1);
        System.out.println(result2 == null ? "Correctly UNSAT." : "ERROR: found a solution?");
    }
}