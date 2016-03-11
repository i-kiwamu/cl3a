#include <iostream>
#include <random>
#include <time.h>
#include <Eigen/Dense>

using namespace Eigen;
using namespace std;

int bench(int n, int m) {
    std::random_device rnd;
    std::mt19937 mt(rnd());
    std::uniform_real_distribution<> rand1(0.0, 1.0);
    VectorXd va(n);
    VectorXd vb(n);
    VectorXd dp(m);

    for (int i = 0; i < n; ++i) {
        va(i) = rand1(mt);
        vb(i) = rand1(mt);
    }

    clock_t start = clock();
    for (int i = 0; i < m; ++i) {
        dp(i) = va.dot(vb);
    }
    double res = dp(m-1);
    clock_t end = clock();
    // std::cout << "Result: " << (double)res << "\n";
    std::cout << "Total time (" << (int)m << " repeat): " << (double)(end-start)/CLOCKS_PER_SEC << "sec.\n";
    return 0;
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cout << "Require exactly 2 arguments, but recieves " << (int)argc << "arguments!\n";
        return 1;
    }
    int n = atoi(argv[1]);
    int m = atoi(argv[2]);
    bench(n, m);
    return 0;
}
