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
        <td>0.033</td><td>3.100</td>
        <td>0.400</td><td>0.256</td>
        <td>0.014</td><td>7.154</td>
    </tr>
    <tr>
        <td>1024</td>
        <td>0.056</td><td>3.655</td>
        <td>0.386</td><td>0.530</td>
        <td>0.028</td><td>7.206</td>
    </tr>
    <tr>
        <td>2048</td>
        <td>0.106</td><td>3.863</td>
        <td>0.413</td><td>0.992</td>
        <td>0.056</td><td>7.315</td>
    </tr>
    <tr>
        <td>4096</td>
        <td>0.212</td><td>3.864</td>
        <td>0.539</td><td>1.520</td>
        <td>0.146</td><td>5.614</td>
    </tr>
    <tr>
        <td>8192</td>
        <td>0.415</td><td>3.948</td>
        <td>0.718</td><td>2.282</td>
        <td>0.295</td><td>5.558</td>
    </tr>
    <tr>
        <td>16384</td>
        <td>0.876</td><td>3.741</td>
        <td>1.165</td><td>2.813</td>
        <td>0.573</td><td>5.716</td>
    </tr>
    <tr>
        <td>32768</td>
        <td>2.023</td><td>3.239</td>
        <td>2.058</td><td>3.184</td>
        <td>1.617</td><td>4.054</td>
    </tr>
    <tr>
        <td>65536</td>
        <td>4.059</td><td>3.229</td>
        <td>3.749</td><td>3.496</td>
        <td>3.275</td><td>4.002</td>
    </tr>
    <tr>
        <td>131072</td>
        <td>8.194</td><td>3.199</td>
        <td>7.109</td><td>3.687</td>
        <td>6.557</td><td>3.998</td>
    </tr>
</table>
