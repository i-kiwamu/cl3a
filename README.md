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

| N      | cl3a (SBCL 1.3.2) |        | OpenBLAS (gfortran 5.3.0) |        |
|-------:|------------------:|-------:|--------------------------:|-------:|
|        | Run time (sec)    | Gflops | Run time (sec)            | Gflops |
| 512    | 0.101             | 1.013  | 0.369                     | 0.277  |
| 1024   | 0.159             | 1.287  | 0.381                     | 0.537  |
| 2048   | 0.259             | 1.581  | 0.412                     | 0.994  |
| 4096   | 0.444             | 1.845  | 0.547                     | 1.497  |
| 8192   | 0.880             | 1.862  | 0.710                     | 2.307  |
| 16384  | 1.724             | 1.901  | 1.140                     | 2.874  |
| 32768  | 3.379             | 1.939  | 2.086                     | 3.142  |
| 65536  | 6.800             | 1.928  | 3.722                     | 3.522  |
| 131072 | 13.763            | 1.905  | 7.106                     | 3.689  |
