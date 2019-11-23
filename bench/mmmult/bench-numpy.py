import sys
import time
import numpy as np

def perf(n, m):
    ma = np.zeros((n, n), dtype=np.float64)
    mb = np.zeros((n, n), dtype=np.float64)
    mc = np.zeros((n, n), dtype=np.float64)
    for i in range(n):
        ma[i,:] = np.random.rand(n)
        mb[i,:] = np.random.rand(n)

    start = time.process_time()
    for i in range(m):
        mc += np.matmul(ma, mb)
    elapsed_time = time.process_time() - start
    print("Total elapsed time ({0} repeat): {1}sec.".format(m, elapsed_time))

if __name__ == "__main__":
    args = sys.argv
    n = int(args[1])
    m = int(args[2])
    perf(n, m)
