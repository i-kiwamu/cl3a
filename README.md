# cl3a
[![Build Status](https://travis-ci.org/i-kiwamu/cl3a.svg?branch=master)](https://travis-ci.org/i-kiwamu/cl3a)

Common Lisp Library of Linear Algebra

*This is still under alpha version!*

## What is this?
This library provides suites of calculatins for linear algebra like [OpenBLAS](https://github.com/xianyi/OpenBLAS) or [Eigen](http://eigen.tuxfamily.org/index.php?title=Main_Page). The aims of this library are

* to provide fast library of linear algebra for Lispers
* to explore the capability of linear algebra calculations written in **pure Common Lisp**

## Benchmark
Performance of dot product in 10^5 repeat by 2.8 GHz Intel Core i7

| N      | cl3a (SBCL 1.3.3) |        | OpenBLAS (gfortran 5.3.0) |        |
|-------:|------------------:|-------:|--------------------------:|-------:|
|        | Run time (sec)    | Gflops | Run time (sec)            | Gflops |
| 512    | 0.049             | 2.088  | 0.369                     | 0.277  |
| 1024   | 0.077             | 2.658  | 0.381                     | 0.537  |
| 2048   | 0.151             | 2.712  | 0.412                     | 0.994  |
| 4096   | 0.288             | 2.844  | 0.547                     | 1.497  |
| 8192   | 0.599             | 2.735  | 0.710                     | 2.307  |
| 16384  | 1.167             | 2.808  | 1.140                     | 2.874  |
| 32768  | 2.317             | 2.828  | 2.086                     | 3.142  |
| 65536  | 4.589             | 2.856  | 3.722                     | 3.522  |
| 131072 | 9.269             | 2.828  | 7.106                     | 3.689  |
