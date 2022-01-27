import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

public class BFS {
    
    // A data structure to store a vertex that we have found in the search, along with
    // the parent reference from which node the search arrived to this node.
    private static class SearchNode<V> {
        public V vertex;
        public SearchNode<V> parent;
        public SearchNode(V vertex, SearchNode<V> parent) {
            this.vertex = vertex; this.parent = parent;
        }
    }
    
    /**
     * Find the shortest path from start vertex to some goal using the frontier search algorithm.
     * @param edges Function giving the list of edges from given vertex.
     * @param start The vertex to start the search from.
     * @param goalTest Predicate to check whether given vertex is a goal.
     * @param bfs Whether to use BFS (FIFO) or DFS (LIFO) discipline in the frontier.
     * @return Vertex list of the shortest path from start to the first discovered goal.
     */
    public static <V> List<V> shortestPath(
        Function<V, List<V>> edges,
        V start,
        Predicate<V> goalTest,
        boolean bfs
    )
    {
        // The vertices that have already been visited in this search.
        Set<V> visited = new HashSet<V>();
        // The search nodes discovered and waiting to be visited.
        LinkedList<SearchNode<V>> frontier = new LinkedList<>();
        // Initialize the search frontier to contain the start vertex.
        frontier.offer(new SearchNode<V>(start, null));
    
        while(frontier.size() > 0) {
            // Extract the node to be processed next.
            SearchNode<V> currentNode = frontier.removeFirst();
            V currentState = currentNode.vertex;
            // If this vertex has already been visited, skip it now.
            if(visited.contains(currentState)) { continue; }
            // If this vertex is a goal, finish the search and build up the answer path.
            if(goalTest.test(currentState)) {
                LinkedList<V> solutionPath = new LinkedList<V>();
                // Follow the parent pointers to build the path in reverse order.
                while(currentNode != null) {
                    solutionPath.addFirst(currentNode.vertex);
                    currentNode = currentNode.parent;
                }
                return solutionPath;
            }
            // The current vertex has now been visited.
            visited.add(currentState);
            // Expand the current node and look at its neighbours.
            for(V next: edges.apply(currentState)) {
                if(visited.contains(next)) { continue; }
                // Add the new search node to the search frontier.
                if(bfs) { // BFS, add the node to the back of the queue.
                    frontier.addLast(new SearchNode<V>(next, currentNode));
                }
                else { // DFS, add the node to the front of the queue.
                    frontier.addFirst(new SearchNode<V>(next, currentNode));
                }
            }
            
        }
        return null;
    }
}