# atomSurfaceInteractionsSI


[![DOI](https://zenodo.org/badge/451110437.svg)](https://zenodo.org/badge/latestdoi/451110437)


* A Mathematica package for carrying out temperature dependent atom-surface interactions for Silicon

* Documentation for functions can be found in atomSurfaceInteractionsSI.m

* This package was made to coencide with a paper currently submitted.

## Example usage of "atomSurfaceInteractionsSI"
A mathematica package can be called using the following commands
```Mathematica 
<< "atomSurfaceInteractionsSI`"
```
```Mathematica 
ParallelNeeds["atomSurfaceInteractionsSI`"]
```

### Coefficients C<sub>3</sub>(T), C<sub>4</sub>(T) can be calculated in atomic units
* Input is the temperature in kelvins output is in atomic unis

#### Input
```Mathematica
Column@{C30[300],c30[1200],cFour[300],cFour[1200]}
```
#### Output
```Mathematica
0.049157474910736720926696931099180
0.048069552198726293996164215985588
15.342546111691665341578420821219
15.880743713011591292711286267971
```
### atomSurfaceInteractionsSI supports many atoms. 
* The atom used for the atom-surface interaction can be selected as an option
* If no option is selected the default is "hydrogen"
#### Input
```Mathematica
c30[300, element -> "hydrogen"]
```
#### Output
```Mathematica
0.10565340763052804213727070168717
```
* Below is an example which creates a table of C<sub>3</sub>(T) and C<sub>4</sub>(T) values for multiple atoms at room temperature

#### Input
```Mathematica
els = {"hydrogen", "helium", "lithium", "beryllium", "neon", "sodium","rubidium", "krypton", "argon", "xenon"};
MatrixForm@Prepend[
  Table[{
    Capitalize@els[[i]],
    c30[300, element -> els[[i]]],
    cFour[300, element -> els[[i]]]}, {i, Length[els]}],
  {"element", "\!\(\*SubscriptBox[\(C\), \(3\)]\)(T=300K)", 
   "\!\(\*SubscriptBox[\(C\), \(4\)]\)(T=300K)"}]
```
#### Output
| Element  | C<sub>3</sub>(T=300K) | C<sub>4</sub>(T=300K) |
| -------- | --------------------- | --------------------- |
|Hydrogen  | 0.1057                | 50.0004               |
|Helium    | 0.0492                | 15.3425               | 
|Lithium   | 1.0570                | 1820.61               | 
|Beryllium | 0.5506                | 418.675               |
|Neon      | 0.1084                | 29.5214               | 
|Sodium    | 1.1678                | 1804.94               |
|Rubidium  | 6.4872                | 3547.80               | 
|Krypton   | 0.5870                | 182.779               |
|Argon     | 0.3925                | 122.951               | 
|Xenon     | 0.9182                | 303.079               |

###  Output units can be changed to be given in electron volts and angstroms
* C<sub>3</sub>(T) -> \[eVÅ<sup>3</sup>\] and C<sub>4</sub>(T) -> \[eVÅ<sup>4</sup>\]
* Input is the temperature in kelvins output is in eVÅ<sup>n</sup>
#### Input
```Mathematica
Column@
 {c30[300, outputUnits -> "eV"],
  c30[1200, outputUnits -> "eV"],
  cFour[300, outputUnits -> "eV"],
  cFour[1200, outputUnits -> "eV"]}
```
#### Output
```Mathematica
0.198218
0.193831
32.738
33.8864
```
###  C<sub>3</sub>(T) and C<sub>4</sub>(T) can easily be plotted as a function of temperature

#### Input
```Mathematica
GraphicsRow[{
  ListLinePlot[
   Evaluate@ParallelTable[{T, c30[T]}, {T, 10, 1500, (1500 - 10)/35}],
   PlotTheme -> "Scientific",
   FrameLabel -> {"Temperature", TraditionalForm@Subscript[C, 3]}],
  ListLinePlot[
   Evaluate@ParallelTable[{T, cFour[T]}, {T, 10, 1500, (1500 - 10)/35}],
   PlotTheme -> "Scientific",
   FrameLabel -> {"Temperature", TraditionalForm@Subscript[C, 4]}]}]
```
#### Output
![](/images/coeffs.png)

###  The dielectric functions derived using the Dirac - Lorentz fit, the Clausius-Mossoti fit, and the tabulated data given in [M.A. Green](https://doi.org/10.1016/j.solmat.2008.06.009) can be quickly calculated and compared
* input is temperature in Kelvins and ω in atomic units. output is in atomic units

#### Input
```Mathematica
N@{diFunSiSL[300, .07], diFunSiCM[300, .07], diFunSiGr[300, .07]}
```
#### Output
```Mathematica
{15.081 +0.123618 i,14.6614 +0.0965488 i,14.7832 +0.113017 i}
```
* Plots of the dielectric function at various temperatures can be created 

#### Input
```Mathematica
GraphicsRow[{
  Plot[
   Evaluate@
    ParallelTable[diFunSiSL[iT, I*\[Omega]], {iT, 300, 1500, 300}],
   {\[Omega], 10^-6, 10^2},
   ScalingFunctions -> {"Log", "Linear"},
   PlotTheme -> "Scientific",
   FrameLabel -> {"i\[Omega]", "Dielectric Function"}],
  Plot[
   Evaluate@
    ParallelTable[Re@diFunSiSL[iT, \[Omega]], {iT, 300, 1500, 300}],
   {\[Omega], 0, .25},
   PlotTheme -> "Scientific",
   FrameLabel -> {"\[Omega]", "Re Dielectric Function"}],
  Plot[
   Evaluate@
    ParallelTable[Im@diFunSiSL[iT, \[Omega]], {iT, 300, 1500, 300}],
   {\[Omega], 0, .25},
   PlotTheme -> "Scientific",
   FrameLabel -> {"\[Omega]", "IM Dielectric Function"}]}]
```
#### Output
![](/images/diFuns.png)

### 3d plots showing the dielectric function as a function of temperature and frequency can be made easily.

#### Input
```Mathematica
GraphicsRow[{
  Plot3D[Re@diFunSiSL[iT, i\[Omega]], {i\[Omega], 0, .25}, {iT, 300, 
    1500},
   PlotTheme -> "Scientific",
   BoxRatios -> 1,
   AxesLabel -> {"\[Omega]", "T [K]", "Re\[Epsilon]"}],
  Plot3D[Im@diFunSiSL[iT, i\[Omega]], {i\[Omega], 0, .25}, {iT, 300, 
    1500},
   PlotTheme -> "Scientific",
   BoxRatios -> 1,
   AxesLabel -> {"\[Omega]", "T [K]", "Im\[Epsilon]"}]}]
```
#### Output
![](/images/diFuns3d.png)

### Dynamic polarizability as derived in paper. inputs is ω output is in atomic units default element is helium but element -> "rubidium" will change atom used to rubidium

#### Input
```Mathematica
GraphicsRow[{
  Plot[
   dynPol[I*\[Omega]],
   {\[Omega], 10^-6, 10^3},
   ScalingFunctions -> {"Log", "Linear"},
   PlotTheme -> "Scientific",
   FrameLabel -> {"i\[Omega]", "Dynamic Polarizability"}],
  Plot[
   dynPol[I*\[Omega], element -> "rubidium"],
   {\[Omega], 10^-6, 10^3},
   ScalingFunctions -> {"Log", "Linear"},
   PlotTheme -> "Scientific",
   FrameLabel -> {"i\[Omega]", "Dynamic Polarizability"}]}]
```
#### Output
![](/images/dynPlts.png)

### Dynamic polarizability plotted on the real axis

#### Input
```Mathematica
Plot[
 dynPol[\[Omega], element -> "hydrogen"],
 {\[Omega], 0, 0.475},
 PlotRange -> {Full, {-60, 60}},
 AspectRatio -> 1,
 ScalingFunctions -> {"Linear", "Linear"},
 PlotTheme -> "Scientific",
 FrameLabel -> {"\[Omega]", "Dynamic Polarizability"}]
```

#### Output
![](/images/dynPlts2.png)

### the full Casimir-Polder potential as described in Eq.(17) of [Phys. Rev. A 70, 053619](https://arxiv.org/abs/cond-mat/0407495)

#### Input

```Mathematica
datF[T_] := 
  datF[T] = 
   ParallelTable[{\[Omega], CPfull[T, \[Omega]]}, {\[Omega], 10^6, 
     10^7, (10^7 - 10^6)/75}];
```
```Mathematica
ListPlot[
 {datF[300], datF[900], datF[1500]},
 Joined -> True,
 FrameLabel -> {"z/\!\(\*SubscriptBox[\(a\), \(0\)]\)", 
   "V/\!\(\*SubscriptBox[\(E\), \(h\)]\)"},
 PlotLegends -> {"300K", "900K", "1500K"},
 PlotTheme -> "Scientific"]
```
#### Output
![](/images/CP.png)

     
