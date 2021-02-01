import java.util.*;
import java.util.function.*;
import java.util.stream.*;
import java.io.*;

/* Inspired by the work of Donald Knuth in Stanford Graphbase and TAOCP: Volume 4. */

public class SearchMain {
    // Scrabble letter values from a to z.
    private static int[] letterCost = {
      1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10
    //a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q   r  s  t  u  v  w  x  y  z
    };
    
    // Count the number of positions where the two words differ.
    private static double hammingDistance(String w1, String w2) {
        int dist = 0;
        for(int i = 0; i < w1.length(); i++) {
            if(w1.charAt(i) != w2.charAt(i)) { dist++; }
        }
        return dist;
    }
    
    // The cost of moving from word w1 to w2, using Scrabble letter costs.
    private static double scrabbleDistance(String w1, String w2) {
        int dist = 0;
        for(int i = 0; i < w1.length(); i++) {
            if(w1.charAt(i) != w2.charAt(i)) {
                dist += letterCost[w2.charAt(i) - 'a'];
            }
        }
        return dist;
    }
    
    public static void main(String[] args) throws FileNotFoundException {
        // Read in the list of words from sgb-words.
        ArrayList<String> words = new ArrayList<String>();
        Scanner sc = new Scanner(new File("sgb-words.txt"));
        sc.useDelimiter("\\n");
        while(sc.hasNextLine()) { words.add(sc.nextLine()); }
        sc.close();
        System.out.println("Read in " + words.size() + " words.");
        System.out.println("Building the neighbourhood graph. Please wait...");
        
        // Neighbourhood graph without distances.
        Map<String, List<String>> neighbourMap = new HashMap<>();
        int maxNeighbours = 0;
        String maxNeighboursWord = "";
        for(String w1: words) {
            List<String> neighbours = new LinkedList<>();
            for(String w2: words) {
                if(hammingDistance(w1, w2) == 1) { neighbours.add(w2); }
            }
            neighbourMap.put(w1, neighbours);
            if(neighbours.size() > maxNeighbours) {
                maxNeighbours = neighbours.size(); maxNeighboursWord = w1;
            }
        }
        
        System.out.print("Finished building the neighbourhood graph. The word ");
        System.out.println("'" + maxNeighboursWord + "' has most neighbours, " + maxNeighbours + ".");
        int count = 0;
        for(String word: neighbourMap.get(maxNeighboursWord)) {
            System.out.print(word + " ");
            if(++count % 13 == 0) { System.out.println(""); }
        }
        if(count % 13 != 1) { System.out.println(""); }
        Random rng = new Random(12345);
        System.out.println("\nHere are some random words and their neighbours.");
        for(int i = 0; i < 15; i++) {
            String word = words.get(rng.nextInt(words.size()));
            System.out.println(word + " : " + neighbourMap.getOrDefault(word, null));
        }
        
        // Just for fun, let's find out how many words are singletons without neighbours.
        System.out.println("\nThe singleton words in the graph are:");
        int singletonCount = 0;
        for(String word: words) {
            if(neighbourMap.get(word).size() == 0) {
                System.out.print(" " + word);
                if(++singletonCount % 13 == 0) { System.out.println(""); }
            }
        }
        if(singletonCount % 13 != 1) { System.out.println(""); }
        System.out.println("The wordlist contains total of " + singletonCount + " singleton words.");        
        
        System.out.println("\nNext, we shall find some shortest paths using Scrabble letter costs.");
        for(int i = 0; i < 10; i++) {
            String start = words.get(rng.nextInt(words.size()));
            String goal = words.get(rng.nextInt(words.size()));
            System.out.print("\nLooking for shortest path from '" + start + "' to '" + goal + "'... ");
            System.out.flush();
            List<String> result = AStar.<String>shortestPath( 
                v -> neighbourMap.get(v),
                (v1, v2) -> scrabbleDistance(v1, v2),
                start,
                word -> word.equals(goal),
                word -> scrabbleDistance(word, goal)
            );
            System.out.print(AStar.getExpandedCount() + " expansions. ");
            if(result == null) { System.out.println("No path exists."); }
            else {
                int total = 0;
                boolean first = true;
                String prev = "";
                for(String word: result) {
                    if(first) {
                        first = false;
                        System.out.print("\n" + word);
                    }
                    else {
                        int e = (int)(scrabbleDistance(prev, word));
                        total += e;
                        System.out.print(" -> " + word + "(" + e + ")");
                    }
                    prev = word;
                }
                System.out.println(" = " + total);
            }
        }
        System.out.println("\nFinally, let's compare the solutions of A*, Uniform cost, BFS and Iterative DFS.");
        for(int i = 0; i < 10; i++) {
            String start = words.get(rng.nextInt(words.size()));
            String goal = words.get(rng.nextInt(words.size()));
            System.out.println("\nLooking for path from '" + start + "' to '" + goal + "'... ");
            List<String> astarResult = AStar.<String>shortestPath(
                v -> neighbourMap.get(v),
                (v1, v2) -> 1.0,
                start,
                word -> word.equals(goal),
                word -> hammingDistance(word, goal)
            );
            int astarExpand = AStar.getExpandedCount();
            List<String> ucResult = AStar.<String>shortestPath(
                v -> neighbourMap.get(v),
                (v1, v2) -> 1.0,
                start,
                word -> word.equals(goal),
                word -> 0.0
            );
            int ucExpand = AStar.getExpandedCount();
            List<String> bfsResult = BFS.<String>shortestPath(
                v -> neighbourMap.get(v),
                start,
                word -> word.equals(goal),
                true
            );
            System.out.print("AStar with " + astarExpand + " expansions: ");
            if(astarResult == null) { System.out.println("No solution."); }
            else { System.out.println(astarResult.size() + " " + astarResult); }
            System.out.print("Uniform cost with " + ucExpand + " expansions: ");
            if(ucResult == null) { System.out.println("No solution."); }
            else { System.out.println(ucResult.size() + " " + ucResult); }
            System.out.print("BFS: ");
            if(bfsResult == null) { System.out.println("No solution."); }
            else { System.out.println(bfsResult.size() + " " + bfsResult); }
        }
        System.out.println("\nAnd we are all done!");
    }
}
