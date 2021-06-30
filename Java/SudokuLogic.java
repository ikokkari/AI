import java.util.*;
import java.util.function.*;

// Demonstrate the SATSolver class by encoding a Sudoku poblem into propositional logic clauses
// and solving it there, translating the solution back to the Sudoku solution.

public class SudokuLogic {

    // The index of tile in coordinates (x, y).
    private static int getIdx(int x, int y) {
        return 9 * y + x;
    }
    // The x-coordinate of tile of index idx.
    private static int getX(int idx) { return idx % 9; }
    // The y-coordinate of tile of index idx.
    private static int getY(int idx) { return idx / 9; }
    // The block number of tile of index idx.
    private static int getB(int idx) { 
        int dx = getX(idx) / 3;
        int dy = getY(idx) / 3;
        return 3 * dx + dy;
    }
    // List of neighbours for Sudoku tiles, precomputed below.
    private static List<List<Integer>> neighbours = new ArrayList<List<Integer>>(81);

    // Executed at class initialization: precompute the list of neighbours for each tile.
    static {
        for(int i = 0; i < 81; i++) {
            neighbours.add(new ArrayList<Integer>());
        }
        for(int i = 0; i < 81; i++) {
            for(int j = i + 1; j < 81; j++) {
                if(i == j) { continue; } // No tile is its own neighbour.
                // Tiles with same x, same y or same block number are neighbours.
                if(getX(i) == getX(j) || getY(i) == getY(j) || getB(i) == getB(j)) {
                    neighbours.get(i).add(j);
                    neighbours.get(j).add(i);
                }
            }
        }
    }

    private static int litIdx(int x, int y, int z) { return 81 * x + 9 * y + z + 1; }

    /** 
     * Solve the Sudoku puzzle with the given 9*9 board.
     * @param board The 9*9 integer array that contains the puzzle, with 0 denoting an empty tile.
     * @return Truth value telling if search was successful, in which case the solution is in the {@code board} array.
     */
    public static boolean solve(int[][] board) {
        int loc = 0;
        int[][] clauses = new int[7371][];

        // If a tile has value v, then none of its neighbours can have the value v.
        for(int i = 0; i < 81; i++) {
            int v = board[getX(i)][getY(i)];
            for(int n: neighbours.get(i)) {
                // Each pair (i, n) of neighbour tiles needs to be handled only once.
                if(i > n) { continue; }
                // For tile with no initial value, create clause allowing any value.
                if(v == 0) {
                    for(int vv = 0; vv < 9; vv++) {
                        int[] c = new int[2];
                        c[0] = -litIdx(getX(i), getY(i), vv);
                        c[1] = -litIdx(getX(n), getY(n), vv);
                        clauses[loc++] = c;
                    }
                }
                // If a tile has a known value, none of its neighbours can have that value.
                else {
                    int[] c = new int[1]; // Nice unit clauses to propagate in the solver.
                    c[0] = -litIdx(getX(n), getY(n), v - 1);
                    clauses[loc++] = c;
                }
            }
        }

        // Every tile must have at least one value. (We don't need to constrain each tile to
        // have at most one value, since the neighbour tile constraints will ensure that.)
        for(int i = 0; i < 81; i++) {
            int x = getX(i);
            int y = getY(i);
            if(board[x][y] == 0) {
                int[] c = new int[9];
                for(int v = 0; v < 9; v++) {
                    c[v] = litIdx(x, y, v);
                }
                clauses[loc++] = c;
            }
            else {
                int[] c = new int[1]; // Another nice unit clause to propagate.
                c[0] = litIdx(x, y, board[x][y] - 1);
                clauses[loc++] = c;
            }
        }
        
        System.out.println("Created " + loc + " clauses for Sudoku.");
        
        long startTime = System.currentTimeMillis();
        boolean[] solution = SATSolver.solveDPLL(9 * 9 * 9, clauses);
        long endTime = System.currentTimeMillis();
        System.out.println("Solved in " + (endTime - startTime) + " ms with "
        + SATSolver.getCallCount() + " recursive calls.");
        
        for(int x = 0; x < 9; x++) {
            for(int y = 0; y < 9; y++) {
                for(int v = 0; v < 9; v++) {
                    if(solution[litIdx(x, y, v)]) {
                        board[x][y] = v + 1;
                    }
                }
            }
        }
        return false;
    }

    private static void printBoard(int[][] board) {
        for(int x = 0; x < 9; x++) {
            for(int y = 0; y < 9; y++) {
                System.out.print(board[x][y] + " ");
            }
            System.out.println("");
        }
    }
    
    public static void main(String[] args) {
        // https://puzzling.stackexchange.com/questions/305/why-is-this-considered-to-be-the-worlds-hardest-sudoku
        int[][] testBoard = {
            {8,0,0,0,0,0,0,0,0},
            {0,0,3,6,0,0,0,0,0},
            {0,7,0,0,9,0,2,0,0},

            {0,5,0,0,0,7,0,0,0},
            {0,0,0,0,4,5,7,0,0},
            {0,0,0,1,0,0,0,3,0},

            {0,0,1,0,0,0,0,6,8},
            {0,0,8,5,0,0,0,1,0},
            {0,9,0,0,0,0,4,0,0}
            };            
        solve(testBoard);
        printBoard(testBoard);        
    }
}
