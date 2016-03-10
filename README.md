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

<table>
    <tr>
        <th>N</th>
        <th colspan="2">cl3a (SBCL 1.3.3)</th>
        <th colspan="2">OpenBLAS 0.2.15 (gfortran 5.3.0)</th>
        <th colspan="2">Eigen 3.2.8 (clang++ 5.3.0)</th>
    </tr>
    <tr>
        <td></td>
        <td>Time (sec)</td><td>Gflops</td>
        <td>Time (sec)</td><td>Gflops</td>
        <td>Time (sec)</td><td>Gflops</td>
    </tr>
    <tr>
        <td>512</td>
        <td>0.057</td><td>1.798</td>
        <td>0.369</td><td>0.277</td>
        <td>0.014</td><td>7.154</td>
    </tr>
    <tr>
        <td>1024</td>
        <td>0.098</td><td>2.090</td>
        <td>0.381</td><td>0.537</td>
        <td>0.028</td><td>7.206</td>
    </tr>
    <tr>
        <td>2048</td>
        <td>0.179</td><td>2.294</td>
        <td>0.412</td><td>0.994</td>
        <td>0.056</td><td>7.315</td>
    </tr>
    <tr>
        <td>4096</td>
        <td>0.314</td><td>2.608</td>
        <td>0.547</td><td>1.497</td>
        <td>0.146</td><td>5.614</td>
    </tr>
    <tr>
        <td>8192</td>
        <td>0.626</td><td>2.618</td>
        <td>0.710</td><td>2.307</td>
        <td>0.295</td><td>5.558</td>
    </tr>
    <tr>
        <td>16384</td>
        <td>1.235</td><td>2.652</td>
        <td>1.140</td><td>2.874</td>
        <td>0.573</td><td>5.716</td>
    </tr>
    <tr>
        <td>32768</td>
        <td>2.552</td><td>2.568</td>
        <td>2.086</td><td>3.142</td>
        <td>1.617</td><td>4.054</td>
    </tr>
    <tr>
        <td>65536</td>
        <td>5.045</td><td>2.598</td>
        <td>3.722</td><td>3.522</td>
        <td>3.275</td><td>4.002</td>
    </tr>
    <tr>
        <td>131072</td>
        <td>10.194</td><td>2.571</td>
        <td>7.106</td><td>3.689</td>
        <td>6.557</td><td>3.998</td>
    </tr>
</table>
