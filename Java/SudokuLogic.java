import java.util.ArrayList;
import java.util.List;

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
    private static final List<List<Integer>> neighbours = new ArrayList<>(81);

    // Executed at class initialization: precompute the list of neighbours for each tile.
    static {
        for(int i = 0; i < 81; i++) {
            neighbours.add(new ArrayList<Integer>());
        }
        for(int i = 0; i < 81; i++) {
            for(int j = i + 1; j < 81; j++) {
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
        int currPos = 0;
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
                        clauses[currPos++] = c;
                    }
                }
                // If a tile has a known value, none of its neighbours can have that value.
                else {
                    int[] c = new int[1]; // Nice unit clauses to propagate in the solver.
                    c[0] = -litIdx(getX(n), getY(n), v - 1);
                    clauses[currPos++] = c;
                }
            }
        }

        
        // Every tile must have at least one value. (We don't need to constrain each tile to
        // have at most one value, since the neighbour tile constraints will ensure that.)
        for(int i = 0; i < 81; i++) {
            int x = getX(i);
            int y = getY(i);
            int[] c;// Another nice unit clause to propagate.
            if(board[x][y] == 0) {
                c = new int[9];
                for(int v = 0; v < 9; v++) {
                    c[v] = litIdx(x, y, v);
                }
            }
            else {
                c = new int[1];
                c[0] = litIdx(x, y, board[x][y] - 1);
            }
            clauses[currPos++] = c;
        }        
        
        long startTime = System.currentTimeMillis();
        boolean[] solution = SATSolver.solve(9 * 9 * 9, clauses, true, -1);
        long endTime = System.currentTimeMillis();
        System.out.println("Solution found in " + (endTime - startTime) + " ms.");
        assert solution != null;
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

    /* A few test boards. */
    public static void test1() {
        int[][] testBoard = {
                {0,0,0,0,0,0,0,1,2},
                {0,0,0,0,0,0,0,0,3},
                {0,0,2,3,0,0,4,0,0},
                {0,0,1,8,0,0,0,0,5},
                {0,6,0,0,7,0,8,0,0},
                {0,0,0,0,0,9,0,0,0},
                {0,0,8,5,0,0,0,0,0},
                {9,0,0,0,4,0,5,0,0},
                {4,7,0,0,0,6,0,0,0}
            };

        solve(testBoard);
        printBoard(testBoard);
    }

    public static void test2() {
        int[][] testBoard = {
                {7,9,0,0,0,0,0,0,3},
                {4,0,0,0,0,0,0,6,0},
                {8,0,1,0,0,4,0,0,2},
                {0,0,5,0,0,0,0,0,0},
                {3,0,0,1,0,0,0,0,0},
                {0,4,0,0,0,6,2,0,9},
                {2,0,0,0,3,0,5,0,6},
                {0,3,0,6,0,5,4,2,1},
                {0,0,0,0,0,0,3,0,0}
            };

        solve(testBoard);
        printBoard(testBoard);
    }
    
    public static void test3() {
        int[][] testBoard = {
                {0,0,0,3,0,0,6,0,0},
                {9,0,6,0,0,0,0,8,0},
                {0,0,5,2,0,0,1,0,0},
                {0,0,0,0,2,1,0,0,5},
                {0,5,0,0,8,0,0,0,0},
                {3,0,0,0,7,0,0,0,4},
                {5,6,0,0,0,0,7,0,0},
                {0,0,0,0,0,0,0,6,2},
                {0,4,3,7,0,0,0,0,0}
            };

        solve(testBoard);
        printBoard(testBoard);
    }

    public static void main(String[] args) {
        test1();
        test2();
        test3();
    }
}