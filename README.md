<!---
layout: page
title: "README (English)"
permalink: /
-->

# True B-Spline Interpolation
How to generate control points (green) for a uniform B-Spline curve (blue) that **passes through all data points (red)**.  
**すべての データ点（赤）を通る** 一様Ｂスプライン曲線（青）のための 制御点（緑）を生成する方法。

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_SCREENSHOT/True%20B-Spline%20Interpolation.png)

----
## ■ 1. B-Spline Curve is not Interpolation
The uniform B-Spline curve (BSC) is a method to generate smooth and controllable segmented polynomials by arranging **uniform B-Spline basis functions (CBS: B-Spline basis)** with different weights (control point positions) in equal intervals. Here, if we consider the control point sequence (CPs) as a discrete signal, it can be interpreted as a kind of signal processing using the B-spline basis as a filter.  
一様Ｂスプライン曲線（ＢＳＣ）は、重み（制御点位置）の異なる **一様Ｂスプライン基底関数（ＣＢＳ：ＢＳ基底）** を等間隔に並べることで、滑らかで制御性の高い区分多項式を生成する手法である。ここで 制御点列（ＣＰｓ）を離散信号とみなせば、ＢＳ基底をフィルタとして用いる一種の信号処理であると解釈できる。

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Continuous%20Uniform%20B-Spline%20curve.svg)

The B-Spline basis can freely change the degree of the polynomial. In zero-degree (1st-order), it is equivalent to the segmented staircase approximation using square waves, and In the 1st-degree (2nd-order), it is equivalent to the segmented linear approximation using triangular waves.    
ＢＳ基底は多項式の次数を自在に変化させることができる。０次（１階）では矩形波による区分階段近似と等価となり、１次（２階）では三角波による区分線形近似と等価となる。

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Continuous%20Uniform%20B-Spline%20basis%20function%20(SD).svg)

> ```CBS[ N_, X_ ] = BSplineBasis[ N-1, (X+N/2)/N ];```  @ Wolfram Language
>
> N：order（階数）= 1 + degree（次数）

$`{\rm CBS}_1(x) = \begin{dcases}
 1 & | x| \leq \frac{1}{2} \\
 0 & \text{otherwise} \\
\end{dcases}`$

$`{\rm CBS}_2(x) = \begin{dcases}
 1-| x|  & | x| \leq 1 \\
 0 & \text{otherwise} \\
\end{dcases}`$

$`{\rm CBS}_3(x) = \begin{dcases}
 \frac{1}{4} \left(3-4 | x| ^2\right) & | x| <\frac{1}{2} \\
 \frac{1}{8} (2 | x| -3)^2 & \frac{1}{2}\leq | x| \leq \frac{3}{2} \\
 0 & \text{otherwise} \\
\end{dcases}`$

$`{\rm CBS}_4(x) = \begin{dcases}
 \frac{1}{6} \left(3 | x| ^3-6 | x| ^2+4\right) & | x| <1 \\
 -\frac{1}{6} (| x| -2)^3 & 1\leq | x| \leq 2 \\
 0 & \text{otherwise} \\
\end{dcases}`$

$`{\rm CBS}_5(x) = \begin{dcases} 
 \frac{1}{192} \left(48 | x| ^4-120 | x| ^2+115\right) & | x| <\frac{1}{2} \\
 \frac{1}{96} \left(-16 | x| ^4+80 | x| ^3-120 | x| ^2+20 | x| +55\right) & \frac{1}{2}\leq | x| <\frac{3}{2} \\
 \frac{1}{384} (2 | x| -5)^4 & \frac{3}{2}\leq | x| \leq \frac{5}{2} \\
 0 & \text{otherwise} \\
\end{dcases}`$

$`{\rm CBS}_6(x) = \begin{dcases}
 \frac{1}{60} \left(-5 | x| ^5+15 | x| ^4-30 | x| ^2+33\right) & | x| <1 \\
 \frac{1}{120} \left(5 | x| ^5-45 | x| ^4+150 | x| ^3-210 | x| ^2+75 | x| +51\right) & 1\leq | x| <2 \\
 -\frac{1}{120} (| x| -3)^5 & 2\leq | x| \leq 3 \\
 0 & \text{otherwise} \\
\end{dcases}`$

The frequency distribution of the B-Spline basis is defined as the power of the Sinc function. Since it has a strong low-pass characteristic, filtering by the B-Spline basis significantly attenuates the high-frequency components inherent in the control point sequence. In other words, the smoothing effect is so strong that the generating curve is very smooth, but it is an **approximate curve** that does not pass through any control points.   
ＢＳ基底の周波数分布は、Sinc 関数の累乗として定義される。強い低域通過特性を持っているため、ＢＳ基底によるフィルタリングは、制御点列が本来持つ高周波成分を大幅に減衰させる。つまり平滑化の作用が強いため、生成される曲線は非常に滑らかであるが、制御点を通らない **近似曲線** となる。

$`\displaystyle \widehat{{\rm CBS}}_N(\omega) = {\left[\frac{2 \sin \left(\frac{\omega }{2}\right)}{\omega }\right]}^N`$

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Continuous%20Uniform%20B-Spline%20basis%20function%20(FD).svg)

## ■ 2. B-Spline Basis as Discrete Filter
Although the B-Spline basis is a continuous filter, it can be regarded as a discrete filter if we focus only on the control points' values. In other words, whether or not the curve passes through the control point depends on the frequency response of the **Discrete B-Spline basis function (DBS: discrete B-Spline basis)**.  
ＢＳ基底は連続フィルタであるが、制御点での値のみに注目すると、離散フィルタとみなすことができる。つまり曲線が制御点を通るか否かは、**離散Ｂスプライン基底関数（ＤＢＳ：離散ＢＳ基底）** の周波数特性に依存している。

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Discrete%20Uniform%20B-Spline%20basis%20function%20(SD).svg)

The frequency distribution of the discrete B-Spline basis is periodic. Still, above the 3rd-order, it has a low-pass characteristic most attenuated at the Nyquist frequency (π). Conversely, discrete B-Spline basis below the 2nd-order do not attenuate the high-frequency components, so the generated curve always passes through the control point.  
離散ＢＳ基底の周波数分布は周期性を持つが、３階以上では ナイキスト周波数（π）において最も減衰する低域通過特性を持つ。逆に２階以下のＢＳ基底は高周波成分を減衰させないので、生成される曲線は必ず制御点を通過する。

$`\displaystyle \widehat{{\rm DBS}}_1(\omega)=1`$

$`\displaystyle \widehat{{\rm DBS}}_2(\omega)=1`$

$`\displaystyle \widehat{{\rm DBS}}_3(\omega)=\frac{1}{4} (\cos (\omega )+3)`$

$`\displaystyle \widehat{{\rm DBS}}_4(\omega)=\frac{1}{3} (\cos (\omega )+2)`$

$`\displaystyle \widehat{{\rm DBS}}_5(\omega)=\frac{1}{192} (76 \cos (\omega )+\cos (2 \omega )+115)`$

$`\displaystyle \widehat{{\rm DBS}}_6(\omega)=\frac{1}{60} (26 \cos (\omega )+\cos (2 \omega )+33)`$

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Discrete%20Uniform%20B-Spline%20basis%20function%20(FD).svg)

## ■ 3. Discrete High-Enhancement filter
In order to pass a curve through the control points, it is necessary to restore the original high-frequency components of the control point sequence. To do this, we can design a **Discrete High-Enhancement filter (DHE)** that cancels the low-pass characteristics of the discrete B-Spline basis. In other words, its frequency distribution is the inverse of that in the discrete B-Spline basis.  
制御点に曲線を通すためには、制御点列が持つ本来の高周波成分を復元する必要がある。そのためには、離散ＢＳ基底の低域通過特性を打ち消す **離散高域強調フィルタ（ＤＨＥ）** を設計すればよい。つまりその周波数分布は、離散ＢＳ基底のそれの逆数となる。

$`\displaystyle \widehat{{\rm DHE}}_1(\omega)=1`$

$`\displaystyle \widehat{{\rm DHE}}_2(\omega)=1`$

$`\displaystyle \widehat{{\rm DHE}}_3(\omega)=\frac{4}{\cos (\omega )+3}`$

$`\displaystyle \widehat{{\rm DHE}}_4(\omega)=\frac{3}{\cos (\omega )+2}`$

$`\displaystyle \widehat{{\rm DHE}}_5(\omega)=\frac{192}{76 \cos (\omega )+\cos (2 \omega )+115}`$

$`\displaystyle \widehat{{\rm DHE}}_6(\omega)=\frac{60}{26 \cos (\omega )+\cos (2 \omega )+33}`$

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Discrete%20High-Enhancement%20Filter%20function%20(FD).svg)

A discrete high-enhancement filter below the 4th-order can be derived analytically using the [**Hypergeometric function**](https://en.wikipedia.org/wiki/Hypergeometric_function).  
４階以下の離散高域強調フィルタは、[**超幾何関数**](https://ja.wikipedia.org/wiki/%E8%B6%85%E5%B9%BE%E4%BD%95%E9%96%A2%E6%95%B0)を用いて解析的に導くことができる。

$`\displaystyle {\rm DHE}_1(i)=\delta (i)`$

$`\displaystyle {\rm DHE}_2(i)=\delta (i)`$

$`\displaystyle {\rm DHE}_3(i)=2 \, _3\tilde{F}_2\left(\frac{1}{2},1,1;1-i,i+1;-1\right)`$

$`\displaystyle {\rm DHE}_4(i)=3 \, _3\tilde{F}_2\left(\frac{1}{2},1,1;1-i,i+1;-2\right)`$

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Discrete%20High-Enhancement%20Filter%20function%20(SD).svg)

At first glance, the expression seems complicated, but we can see that the absolute value follows a simple exponential function (CHE).  
一見複雑な関数に思えるが、絶対値が 単純な指数関数（ＣＨＥ）に沿っていることが分かる。

$`\displaystyle \left|{\rm CHE}_3(x)\right|=\sqrt{2}\left(3 -2\sqrt{2}\right)^{\left| x\right| }`$

$`\displaystyle \left|{\rm CHE}_4(x)\right|=\sqrt{3}\left(2-\sqrt{3}\right)^{\left| x\right| }`$

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Continuous%20High-Enhancement%20Filter%20function%20(AbsSD).svg)

Furthermore, since the original function's sign is alternating positive and negative, we can eliminate the hypergeometric function and derive a very simple expression (CHE).   
さらに、元々の関数の符合が正負を交互に繰り返していることから、超幾何関数を排して 非常に簡単な式（ＣＨＥ）を導くことができる。

$`\displaystyle {\rm CHE}_3(x)=\sqrt{2}\left(2\sqrt{2}-3\right)^{\left| x\right| }`$

$`\displaystyle {\rm CHE}_4(x)=\sqrt{3}\left(\sqrt{3}-2\right)^{\left| x\right| }`$

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Continuous%20High-Enhancement%20Filter%20function%20(SD).svg)

The discrete high-enhancement filter is an infinite impulse response (IIR) filter. Therefore, it is necessary to cut off the coefficient sequence with an appropriate number of taps in a practical implementation.  
離散高域強調フィルタは、無限インパルス応答（ＩＩＲ）フィルタである。ゆえに実際の実装では、適当なタップ数で打ち切る必要がある。

| $`{\rm DHE}_3`$                               | $`{\rm DHE}_4`$                             |
| --------------------------------------------- | ------------------------------------------- |
| $`{\rm DHE}_3(\pm 0)=\sqrt{2}`$               | $`{\rm DHE}_4(\pm 0)=\sqrt{3}`$             |
| $`{\rm DHE}_3(\pm 1)=4-3 \sqrt{2}`$           | $`{\rm DHE}_4(\pm 1)=3-2 \sqrt{3}`$         |
| $`{\rm DHE}_3(\pm 2)=17 \sqrt{2}-24`$         | $`{\rm DHE}_4(\pm 2)=7 \sqrt{3}-12`$        |
| $`{\rm DHE}_3(\pm 3)=140-99 \sqrt{2}`$        | $`{\rm DHE}_4(\pm 3)=45-26 \sqrt{3}`$       |
| $`{\rm DHE}_3(\pm 4)=577 \sqrt{2}-816`$       | $`{\rm DHE}_4(\pm 4)=97 \sqrt{3}-168`$      |
| $`{\rm DHE}_3(\pm 5)=4756-3363 \sqrt{2}`$     | $`{\rm DHE}_4(\pm 5)=627-362 \sqrt{3}`$     |
| $`{\rm DHE}_3(\pm 6)=19601 \sqrt{2}-27720`$   | $`{\rm DHE}_4(\pm 6)=1351 \sqrt{3}-2340`$   |
| $`{\rm DHE}_3(\pm 7)=161564-114243 \sqrt{2}`$ | $`{\rm DHE}_4(\pm 7)=8733-5042 \sqrt{3}`$   |
| $`{\rm DHE}_3(\pm 8)=665857 \sqrt{2}-941664`$ | $`{\rm DHE}_4(\pm 8)=18817 \sqrt{3}-32592`$ |

The frequency distribution of a finite impulse response (FIR) filter, i.e., an approximated discrete high-enhancement filter (AHE), is almost identical to that of an ideal discrete high-enhancement filter (DHE). Empirically, 7-taps (±3) for the 3rd-order case and 9-taps (±4) for the 4th-order case are enough to guarantee accuracy.  
有限インパルス応答（ＦＩＲ）フィルタ，すなわち 近似的な離散高域強調フィルタ（ＡＨＥ）の周波数分布は、理想的な離散高域強調フィルタ（ＤＨＥ）のそれとほぼ一致する。経験的には、３階では７タップ（±３）、４階では９タップ（±４）あれば、十分な精度が保証される。

![](https://github.com/LUXOPHIA/TrueBSplineInterpolation/raw/master/--------/_README/Approximate%20Discrete%20High-Enhancement%20Filter%20function%20(FD).svg)

----
## ■ 4. Reference
* [True B-Spline Interpolation.nb](./--------/True%20B-Spline%20Interpolation.nb) @ [Mathematica](https://www.wolfram.com/mathematica/)

----
[![Delphi Starter](http://img.en25.com/EloquaImages/clients/Embarcadero/%7B063f1eec-64a6-4c19-840f-9b59d407c914%7D_dx-starter-bn159.png)](https://www.embarcadero.com/jp/products/delphi/starter)
