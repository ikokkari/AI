import java.util.Random;

// Implement a binary heap for integers used to represent literals in SATSolver.

public class IntHeap {
    
    // The elements of this heap.
    private int[] data;
    // The location of each literal in the heap (0 if absent)
    private int[] loc;
    // The number of elements currently in the heap.
    private int size;
    // The array from which the literal priorities are taken.
    private int[] clauseCount;
    // Whether the priority is highest-first or lowest-first.
    private boolean highestFirst;
    
    // The array index that represents the literal x for -n <= x <= n.
    private static int getIdx(int literal) {
        if(literal < 0) { return 2 * (-literal) - 1; }
        else { return 2 * literal - 2; }
    }
    
    // Constructor for the binary heap.
    public IntHeap(int[] clauseCount, int n, boolean highestFirst) {
        data = new int[2 * n + 1];
        loc = new int[2 * n + 1];
        size = 0;
        this.clauseCount = clauseCount;
        this.highestFirst = highestFirst;
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
    
    // Decrease the priority of literal e and update the heap.
    public void decrease(int e) {
        int idx = loc[getIdx(e)];
        if(idx > 0) { 
            if(highestFirst) { siftdown(idx); } else { siftup(idx); }
        }
    }
    
    // Increase the priority of literal e and update the heap.
    public void increase(int e) {
        int idx = loc[getIdx(e)];
        if(idx > 0) { 
            if(highestFirst) { siftup(idx); } else { siftdown(idx); }
        }
    }
    
    // Move the element in position idx up in the heap as needed.
    private void siftup(int idx) {
        int e = data[idx];
        int v = clauseCount[getIdx(e)];
        while(idx > 1) {
            int pidx = idx / 2;
            int pe = data[pidx];
            assert loc[getIdx(pe)] == pidx;
            int pv = clauseCount[getIdx(pe)];
            if(pv < v) {
                loc[getIdx(pe)] = idx;
                data[idx] = pe;
                idx = pidx;
            }
            else { break; }
        }
        data[idx] = e;
        loc[getIdx(e)] = idx;
    }
    
    // Move the element in position idx down in the heap as needed.
    private void siftdown(int idx) {
        int e = data[idx];
        int v = clauseCount[getIdx(e)];
        while(2 * idx <= size) {
            int cidx = 2 * idx;
            if(cidx + 1 <= size && clauseCount[getIdx(data[cidx])] < clauseCount[getIdx(data[cidx+1])]) {
                cidx = cidx + 1;
            }
            int ce = data[cidx];
            assert loc[getIdx(ce)] == cidx;
            int cv = clauseCount[getIdx(data[cidx])];
            if(cv > v) {
                data[idx] = ce;
                loc[getIdx(ce)] = idx;
                idx = cidx;
            } else { break; }
        }
        data[idx] = e;
        loc[getIdx(e)] = idx;
    }
}
