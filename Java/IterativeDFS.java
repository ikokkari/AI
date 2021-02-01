import java.util.*;
import java.util.function.*;
import java.io.*;

public class IterativeDFS {

    /**
     * Perform iterative deepening depth first search in a graph to find the shortest path
     * from the start node to some goal node.
     * @param edges Function giving the list of neighbours of each node in the graph.
     * @param start The node in which to start searching.
     * @param goalTest Predicate to determine whether the current node is a goal node.
     * @param maxDepth Maximum level depth before giving up the search.
     * @return The list of nodes on the shortest path from start node to a goal node.
     */
    public static <E> List<E> search(
        Function<E,List<E>> edges,
        E start,
        Predicate<E> goalTest,
        int maxDepth
    )
    {
        LinkedList<E> path = new LinkedList<E>();
        Set<E> discovered = new HashSet<E>();
        for(int currDepth = 1; currDepth <= maxDepth; currDepth++) {
            int result = search(edges, start, goalTest, currDepth, path, discovered);
            if(result > 0) { return path; }
            if(result == 0) { return null; } // Search failed without cutoff
            // Search failed because of cutoff, so we must continue to next round.
            discovered.clear(); // Each iteration starts afresh.
        }
        return null;
    }
    
    // The recursive DFS with depth limit. Returns +1 for success (the path contains the
    // found path), 0 for failure due to reaching the end of graph, and -1 for failure due
    // to a iterative deepening limit cutoff.
    
    private static <E> int search(
        Function<E,List<E>> edges,
        E start,
        Predicate<E> goalTest,
        int depthLimit,     // The remaining depth limit
        LinkedList<E> path, // The path of nodes accumulated so far.
        Set<E> discovered   // The set of nodes discovered so far. 
    )
    {
        // Record whether some recursive search has failed due to cutoff.
        boolean cutoffOccurred = false;
        // Failure base case of recursive search.
        if(depthLimit == 0) { return -1; }
        // Append the current node to the accumulated path.
        path.offerLast(start);
        // Success base case.
        if(goalTest.test(start)) { return +1; }
        // This node has been discovered.
        discovered.add(start); 
        // Loop through the neighbours of the current node.
        for(E neighbour: edges.apply(start)) {
            // If this neighbour has already been discovered in search, skip it.
            if(discovered.contains(neighbour)) { continue; }
            // Perform a recursive DFS starting from that neighbour node.
            int result = search(edges, neighbour, goalTest, depthLimit - 1, path, discovered);
            // Either return success or note if the search failed because of limit cutoff.
            if(result > 0) { return result; }
            if(result == -1) { cutoffOccurred = true; }
        }
        // Remove the current node from the end of the accumulated path and report failure.
        path.removeLast();
        // Uncomment the next line to guaranteed finding the true shortest path. However, this
        // will make execution enormously slow in highly connected state spaces, as the search
        // tries all possible ways to zigzag its way through every possible unsuccessful path.
        // discovered.remove(start);
        
        // Return the negative answer.
        return cutoffOccurred? -1: 0;
    }
    
}
