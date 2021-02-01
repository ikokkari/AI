import java.util.*;
import java.util.function.*;

// Generalization of Dijkstra's algorithm to use the given heuristic function to
// focus the search.

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
        public V vertex;
        public SearchNode<V> prev;
        public double g;
        public SearchNode(V vertex, SearchNode<V> prev, double g) {
            this.vertex = vertex; this.prev = prev; this.g = g;
        }
    }
    
    // Comparator used by the search priority queue to sort the search nodes in order
    // of increasing value of the sum g + h.
    private static class SearchNodeComparator<V> implements Comparator<SearchNode<V>> {
        private Function<V, Double> h; // The heuristic function used.
        public SearchNodeComparator(Function<V, Double> h) {
            this.h = h;
        }
        public int compare(SearchNode<V> n1, SearchNode<V> n2) {
            double d1 = h.apply(n1.vertex) + n1.g;
            double d2 = h.apply(n2.vertex) + n2.g;
            return d1 < d2 ? -1: (d1 > d2? +1: 0);
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
        // The vertices that have been discovered, along with so far best g-values.
        Map<V, Double> discovered = new HashMap<V, Double>();
        // Initialize the null heuristic.
        if(h == null) { h = v -> 0.0; }
        // The search nodes waiting to be processed.
        PriorityQueue<SearchNode<V>> frontier = 
            new PriorityQueue<>(new SearchNodeComparator<V>(h));
        // Initialize the search frontier to contain the start node.
        frontier.offer(new SearchNode<V>(start, null, 0));
        discovered.put(start, 0.0);
    
        while(frontier.size() > 0) {
            // Extract the node with lowest (g + h) value to be expanded next.
            SearchNode<V> curr = frontier.poll();
            // The vertex of the original graph stored in the current node.
            V current = curr.vertex;
            // If this vertex has previously been expanded with a lower cost, skip it now.
            if(discovered.get(current) < curr.g) { continue; }
            // Otherwise, expand this vertex.
            expandedCount++;
            // If the current vertex is a goal, finish the search and return the answer path.
            if(goalTest.test(current)) { 
                LinkedList<V> solutionPath = new LinkedList<V>();
                // Follow the prev pointers to build the path in reverse order.
                while(curr != null) {
                    solutionPath.addFirst(curr.vertex);
                    curr = curr.prev;
                }
                return solutionPath;
            }
            // Add the undiscovered neighbours of the current vertex to the search frontier.
            for(V next: edges.apply(current)) {
                // Neighbour node path total cost.
                double ng = curr.g + cost.apply(current, next);
                // If we have already discovered some at least as good path to this neighbour, skip it.
                if(discovered.getOrDefault(next, Double.POSITIVE_INFINITY) <= ng) { continue; }
                // Otherwise, create/add new node for this neighbour.
                frontier.offer(new SearchNode<V>(next, curr, ng));
                discovered.put(next, ng);
            }           
        }
        // Search completed with no solution found.
        return null;
    }
}
