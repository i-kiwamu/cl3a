# cl3a
[![Build Status](https://travis-ci.org/i-kiwamu/cl3a.svg?branch=master)](https://travis-ci.org/i-kiwamu/cl3a)

Common Lisp Library of Linear Algebra

*This is still under alpha version!*

## What is this?
This library provides suites of calculatins for linear algebra like [OpenBLAS](https://github.com/xianyi/OpenBLAS) or [Eigen](http://eigen.tuxfamily.org/index.php?title=Main_Page). The aims of this library are

* to provide sufficiently fast library of linear algebra for Lispers
* to explore the capability of linear algebra calculations **not** to use foreign function interface (FFI)

## Requirements
* Intel X86-64
* SBCL (>= 1.4.15)

## Benchmark
### Dot product
Performance of dot product in 10^5 repeat by 3.4 GHz Intel Core i7

![performance of dot product](https://github.com/i-kiwamu/cl3a/raw/master/bench/dotprod/performance_dotprod.png)

### Matrix-vector multiplication
Performance of multiplication between matrix and vector in 10^3 repeat by 3.4 GHz Intel Core i7

![performance of matrix-vector multiplication](https://github.com/i-kiwamu/cl3a/raw/master/bench/mvmult/performance_mvmult.png)

### Matrix-matrix multiplication
Performance of matrix multiplication in 10 repeat by 3.4 GHz Intel Core i7

![performance of matrix-matrix multiplication](https://github.com/i-kiwamu/cl3a/raw/master/bench/mmmult/performance_mmmult.png)

