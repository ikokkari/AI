import java.util.Random;

// Implement a binary heap for integers used to represent literals in SATSolver.
// The priority queue keeps track of the position of each literal in the queue,
// and therefore can support operations of removing a literal and updating its
// priority on the fly.

public class IntHeap {
    
    // The elements of this heap.
    private final int[] data;
    // The location of each literal in the heap (0 if absent).
    private final int[] loc;
    // The number of elements currently in the heap.
    private int size;
    // The array from which the literal priorities are taken.
    private final int[] clauseCount;
    
    // The array index that represents the literal x for -n <= x <= n.
    private static int getIdx(int lit) {
        return lit > 0 ? 2 * (lit-1) : 2 * (-lit) - 1;
    }  
    
    // Constructor for the binary heap.
    public IntHeap(int[] clauseCount, int n) {
        data = new int[2 * n + 1];
        loc = new int[2 * n];
        size = 0;
        this.clauseCount = clauseCount;
    }
    
    // Current size of the heap.
    public int getSize() {
        return size;
    }
    
    // Offer the literal e to this heap.
    public void offer(int e) {
        data[++size] = e;
        int idx = getIdx(e);
        assert loc[idx] == 0;
        loc[idx] = size;
        siftup(size);
    }
    
    // Extract the literal with the highest clause count.
    public int poll() {
        assert size > 0;
        int e = data[1];
        assert loc[getIdx(e)] == 1;
        loc[getIdx(e)] = 0;
        data[1] = data[size--];
        if(size > 0) {
            loc[getIdx(data[1])] = 1;
            siftdown(1);
        }
        return e;
    }
    
    // Remove the literal e from the heap.
    public void remove(int e) {
        int pos = loc[getIdx(e)];
        assert pos > 0;
        int e2 = data[size--];
        data[pos] = e2;
        loc[getIdx(e2)] = pos;
        loc[getIdx(e)] = 0;
        if(pos < size) { siftdown(pos); siftup(pos); }
    }
    
    // Decrease the priority of literal e and update the heap.
    public void decrease(int e) {
        int pos = loc[getIdx(e)];
        if(pos > 0) { siftdown(pos); }
    }
    
    // Increase the priority of literal e and update the heap.
    public void increase(int e) {
        int pos = loc[getIdx(e)];
        if(pos > 0) { siftup(pos); }
    }
    
    // Move the element in position pos up in the heap as needed.
    private void siftup(int pos) {
        int e = data[pos];
        int v = clauseCount[getIdx(e)];
        while(pos > 1) {
            int ppos = pos / 2;
            int pe = data[ppos];
            assert loc[getIdx(pe)] == ppos;
            int pv = clauseCount[getIdx(pe)];
            if(pv < v) {
                loc[getIdx(pe)] = pos;
                data[pos] = pe;
                pos = ppos;
            }
            else { break; }
        }
        data[pos] = e;
        loc[getIdx(e)] = pos;
    }
    
    // Move the element in position pos down in the heap as needed.
    private void siftdown(int pos) {
        int e = data[pos];
        int v = clauseCount[getIdx(e)];
        while(2 * pos <= size) {
            int cpos = 2 * pos;
            if(cpos + 1 <= size && clauseCount[getIdx(data[cpos])] < clauseCount[getIdx(data[cpos+1])]) {
                cpos = cpos + 1;
            }
            int ce = data[cpos];
            assert loc[getIdx(ce)] == cpos;
            int cv = clauseCount[getIdx(data[cpos])];
            if(cv > v) {
                data[pos] = ce;
                loc[getIdx(ce)] = pos;
                pos = cpos;
            } else { break; }
        }
        data[pos] = e;
        loc[getIdx(e)] = pos;
    }
}