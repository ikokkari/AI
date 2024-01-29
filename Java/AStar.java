import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

public class AStar {
    
    // Count of how many nodes were expanded in the most recent search.
    private static int expandedCount;

    /**
     * Returns the count of how many nodes were expanded in the most recent search.
     * @return The node expansion count.
     */
    public static int getExpandedCount() { return expandedCount; }
    
    // A data structure to store a vertex that we have found in the search, along with
    // the distance to that vertex and reference from what node we arrived to this node.
    private static class SearchNode<V> {
        /**
         * The vertex of the original state space stored in this node.
         */
        public V vertex;
        /**
         * The parent node of this node in the search tree.
         */
        public SearchNode<V> parent;
        /**
         * The distance from root node to this node.
         */
        public double g;

        /**
         * Public constructor for this class.
         * @param vertex The vertex of the original state space stored in this node.
         * @param parent The parent node of this node in the search tree.
         * @param g The distance from root node to this node.
         */
        public SearchNode(V vertex, SearchNode<V> parent, double g) {
            this.vertex = vertex; this.parent = parent; this.g = g;
        }
    }
    
    // Comparator used by the search priority queue to sort the search nodes in order
    // of increasing value of the sum g + h.
    private static class AStarComparator<V> implements Comparator<SearchNode<V>> {
        private final Function<V, Double> h; // The heuristic function.
        public AStarComparator(Function<V, Double> h) {
            this.h = h;
        }
        public int compare(SearchNode<V> n1, SearchNode<V> n2) {
            double d1 = h.apply(n1.vertex) + n1.g;
            double d2 = h.apply(n2.vertex) + n2.g;
            return Double.compare(d1, d2);
        }
    }
    
    /**
     * Find the shortest path from the start vertex to some goal vertex using the A* algorithm.
     * @param edges Function giving the list of edges from given vertex.
     * @param cost Function giving the cost of moving along an edge from one vertex to another.
     * @param start The vertex to start the search from.
     * @param goalTest Predicate to check whether given vertex is a goal.
     * @param h The heuristic lower bound function used by the A* algorithm. To guarantee
     * finding the shortest path, this function must be both admissible and monotonic.
     * @return List of nodes that comprise the shortest path from start to goal.
     */
    public static <V> List<V> shortestPath(
        Function<V, List<V>> edges,
        BiFunction<V, V, Double> cost,
        V start,
        Predicate<V> goalTest,
        Function<V, Double> h
    )
    {
        // Initialize the node expansion count.
        expandedCount = 0;
        // The vertices that have been visited, along with so far best g-values.
        Map<V, Double> visited = new HashMap<>();
        // Initialize the trivial heuristic, if no heuristic has been given.
        if(h == null) { h = v -> 0.0; }
        // The frontier of search nodes waiting to be expanded.
        PriorityQueue<SearchNode<V>> frontier = new PriorityQueue<>(new AStarComparator<>(h));
        // Initialize the search frontier to contain the start node.
        frontier.offer(new SearchNode<>(start, null, 0));
        visited.put(start, 0.0);
    
        while(frontier.size() > 0) {
            // Extract the node with lowest (g + h) value to be expanded next.
            SearchNode<V> currentNode = frontier.poll();
            // The vertex of the original graph stored in the current node.
            V currentState = currentNode.vertex;
            // Expand this node.
            expandedCount++;
            // If the current state is a goal, finish the search and return the answer path.
            if(goalTest.test(currentState)) {
                LinkedList<V> solutionPath = new LinkedList<>();
                // Follow the parent pointers to build the path in reverse order.
                while(currentNode != null) {
                    solutionPath.addFirst(currentNode.vertex);
                    currentNode = currentNode.parent;
                }
                return solutionPath;
            }
            // Add the undiscovered neighbours of the current state to the search frontier.
            for(V nextState: edges.apply(currentState)) {
                // Neighbour node path total cost.
                double nextG = currentNode.g + cost.apply(currentState, nextState);
                // If we have already visited some at least as good path to this neighbour, skip it.
                if(visited.getOrDefault(nextState, Double.POSITIVE_INFINITY) <= nextG) { continue; }
                // Otherwise, create a new node for this neighbour.
                frontier.offer(new SearchNode<>(nextState, currentNode, nextG));
                visited.put(nextState, nextG);
            }           
        }
        // Search completed with no solution found.
        return null;
    }
}