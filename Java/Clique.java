import java.util.Arrays;

public class Clique {

    private static int[] bestSoFar;

    public static int[] findFirstClique(boolean[][] adjacencyMatrix) {
        int n = adjacencyMatrix.length;
        int[] clique = new int[n];
        bestSoFar = new int[0];
        int[] degree = new int[n];
        for(int u = 0; u < n; u++) {
            for(int v = u+1; v < n; v++) {
                if(adjacencyMatrix[u][v]) {
                    degree[u]++;
                    degree[v]++;
                }
            }
        }
        findFirstClique(adjacencyMatrix, degree, clique, 0);
        return bestSoFar;
    }

    private static void findFirstClique(boolean[][] adjacencyMatrix, int[] degree, int[] clique, int k) {
        if(bestSoFar.length < k) { bestSoFar = Arrays.copyOfRange(clique, 0, k); }
        int upTo = clique.length - (bestSoFar.length - k + 1);
        outer:
        // Loop through all possible nodes for the k:th element of the clique.
        for(int u = k > 0 ? clique[k-1] + 1 : 0; u <= upTo; u++) {
            if(degree[u] < bestSoFar.length) { continue; }
            // If that node is not connected to some node in this clique, move on to next case.
            for(int v = 0; v < k; v++) {
                if(!adjacencyMatrix[u][clique[v]]) { continue outer; }
            }
            // Use node u as the k:th element of the clique, and fill in the rest recursively.
            clique[k] = u;
            findFirstClique(adjacencyMatrix, degree, clique, k+1);
        }
    }
}