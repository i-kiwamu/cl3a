# cl3a
[![Build Status](https://travis-ci.org/i-kiwamu/cl3a.svg?branch=master)](https://travis-ci.org/i-kiwamu/cl3a)

Common Lisp Library of Linear Algebra

*This is still under alpha version!*

## What is this?
This library provides suites of calculatins for linear algebra like [OpenBLAS](https://github.com/xianyi/OpenBLAS) or [Eigen](http://eigen.tuxfamily.org/index.php?title=Main_Page). The aims of this library are

* to provide sufficiently fast library of linear algebra for Lispers
* to explore the capability of linear algebra calculations **not** to use foreign function interface (FFI)

## Benchmark
Performance of dot product in 10^5 repeat by 3.4 GHz Intel Core i7

<table>
    <tr>
        <th>N</th>
        <th colspan="2">cl3a (SBCL 1.4.3)</th>
        <th colspan="2">OpenBLAS 0.2.20 (gfortran 8.1.0)</th>
        <th colspan="2">Eigen 3.3.4 (clang++ 9.1.0)</th>
    </tr>
    <tr>
        <td></td>
        <td>Time (sec)</td><td>Gflops</td>
        <td>Time (sec)</td><td>Gflops</td>
        <td>Time (sec)</td><td>Gflops</td>
    </tr>
    <tr>
        <td>512</td>
        <td>0.0136</td><td>7.55</td>
        <td>0.118</td><td>0.87</td>
        <td>0.0139</td><td>7.36</td>
    </tr>
    <tr>
        <td>1024</td>
        <td>0.023</td><td>8.75</td>
        <td>0.121</td><td>1.69</td>
        <td>0.029</td><td>7.15</td>
    </tr>
    <tr>
        <td>2048</td>
        <td>0.044</td><td>9.32</td>
        <td>0.130</td><td>1.69</td>
        <td>0.029</td><td>7.15</td>
    </tr>
    <tr>
        <td>4096</td>
        <td>0.121</td><td>6.79</td>
        <td>0.183</td><td>4.48</td>
        <td>0.124</td><td>6.62</td>
    </tr>
    <tr>
        <td>8192</td>
        <td>0.238</td><td>6.89</td>
        <td>0.259/td><td>6.33</td>
        <td>0.246</td><td>6.67</td>
    </tr>
    <tr>
        <td>16384</td>
        <td>0.498</td><td>6.58</td>
        <td>0.455</td><td>7.20</td>
        <td>0.496</td><td>6.60</td>
    </tr>
    <tr>
        <td>32768</td>
        <td>1.095</td><td>5.99</td>
        <td>0.950</td><td>6.90</td>
        <td>1.101</td><td>5.95</td>
    </tr>
    <tr>
        <td>65536</td>
        <td>2.185</td><td>6.00</td>
        <td>1.808</td><td>7.25</td>
        <td>2.239</td><td>5.85</td>
    </tr>
    <tr>
        <td>131072</td>
        <td>4.407</td><td>5.95</td>
        <td>3.448</td><td>7.60</td>
        <td>4.433</td><td>5.91</td>
    </tr>
</table>
