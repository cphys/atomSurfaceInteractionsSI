(* ::Package:: *)

BeginPackage["atomSurfaceInteractionsSI`"];
 
dynPol::usage = 
 "dynPol[\[Omega]Value]
	input:       \[Omega]Values (float):               frequency in atomic units

	options:     workingPrecision(32):                 the precision used in internal computations
                     numberOfTerms(All):                   number of oscillators to include in sum
                     element(helium):                      atom to use in atom-surface calculation 

	returns:     dynamic polarizability of selected element. ";
   
hFun::usage = 
 "hFun[temperature, omega,  pValue]
	input:       temperature:                          temperature [K]
                     omega:                                frequency atomic units
		     pValue:                               pvalue

	options:     workingPrecision(32):                 the precision used in internal computations

	returns:     H function. ";
	
c30::usage = 
 "c30[temperature]
	input:       temperature:                          temperature [K]

	options:     workingPrecision(32):                 the precision used in internal computations
                     precisionGoal (MachinePrecision):     digits of precision sought                     
                     element(helium):                      atom to use in atom-surface calculation
                     method(DoubleExponential):            integration method to use                    
                     maxRecursion(100):                    maximum number of recursive subdivisions
                     outputUnits(atom):                    units  to use in output 
 		 
	returns:     C3 term from Casimir-Polder potential. ";
	
cFour::usage = 
 "cFour[temperature]
	input:       temperature:                          temperature [K]

	options:     workingPrecision(32):                 the precision used in internal computations               
                     element(helium):                      atom to use in atom-surface calculation
                     outputUnits(atom):                    units  to use in output 
	 
	returns:     C4 term from Casimir-Polder potential. ";
	
diFunSiSL::usage = 
 "diFunSiSL[temperature, omega]
	input:       temperature:                          temperature [K]
		     omega(float):                         frequency in atomic units

	options:     workingPrecision(32):                 the precision used in internal computations          
		 
	returns:     dielectric function of silicon SL";		
		
diFunSiCM::usage = 
 "diFunSiCM[temperature, omega]
	input:       temperature:                          temperature [K]
		     omega:                                (float)frequency in atomic units

	options:     workingPrecision(32):                 the precision used in internal computations
		 
	returns:     dielectric function of silicon CM";	
	
diFunSiGr::usage = 
 "diFunSiGr[temperature, omega]
	input:       temperature:                          temperature [K]
		     omega:                                (float)frequency in atomic units

	options:     workingPrecision(32):                 the precision used in internal computations
 
	returns:     dielectric function of silicon Gr";	

			
casimirPolderSi::usage = 
 "casimirPolderSi[temperature, zValue]
	input:       temperature:                          temperature [K]
		     zValue:                               atom-wall distance

	options:     workingPrecision(32):                 the precision used in internal computations
                     precisionGoal (MachinePrecision):     digits of precision sought                     
                     element(helium):                      atom to use in atom-surface calculation
	 
	returns:     Integral term from Casimir-Polder potential. ";

f0::usage = 
 "f0[temperature, zValue]
	input:       temperature:                          temperature [K]
		     zValue:                               atom-wall distance


	options:     workingPrecision(32):                 the precision used in internal computations
                     outputUnits(atom):                    units  to use in output                     
                     element(helium):                      atom to use in atom-surface calculation		 
	returns:     f0 term for Casimir-Polder potential. ";

						
CPintegrand::usage = 
 "CPintegrand[numbTerms, temperature, zValue]
	input:       numbTerms:                            (function) potential as a function of
                     temperature:                          temperature [K]
		     zValue:                               atom-wall distance

	options:     workingPrecision(32):                 the precision used in internal computations
                     precisionGoal (MachinePrecision):     digits of precision sought                     
                     element(helium):                      atom to use in atom-surface calculation
		 
	returns:     Integrand for CP-potential. ";	
						
CPfull::usage = 
 "CPfull[temperature,zValue]
	input:       temperature:                          temperature [K]
		     zValue:                               atom-wall distance

	options:     workingPrecision(32):                 the precision used in internal computations
                     precisionGoal (MachinePrecision):     digits of precision sought                     
                     element(helium):                      atom to use in atom-surface calculation
                     nSumTerms(50):                        number of terms to use before extrapolation 		
	 
	returns:     Full temperature dependent Casimir-Polder potential. ";
	
EheV::usage = "Hartree energy in [eV]";
EhJoules::usage = "Hartree energy in [J]";
Ehatom::usage = "Hartree energy atomic units";

aoMeters::usage = "Bohr radius in meterss";
aoAngst::usage = "Bohr radius in angstroms";
a0atom::usage = "Bohr radius in atomic units";

hbarc::usage = "\[HBar]c [Subscript[eVa, 0]]s";
hbarJs::usage = "Planks constant in [J*s";
hbareVs::usage = "Planks constant in [eV*s]";
hbaratom::usage = "Planks constant in atomic units";

catom::usage = "Speed of light atomic units";

mH3eV::usage = "Mass of 3-He atom[eV/c^2]";
mH3kg::usage = "Mass of 3-He[kg]";
mekg::usage = "electron rest mass[kg]";
mH3atom::usage = "Mass of 3-He atomic units";

eps::usage = "potential depth[eV]";
epsatom::usage = "potential depth atom";

lAngst::usage = "l in Angstroms";
lmeters::usage = "l in meters";
latom::usage = "l in atomic units";

kbatom::usage = "boltzmann constant in atomic units";


Begin["`Private`"]

(* Hartree energy in [eV] *)
EheV=27.211386245988;
(* Hartree energy in [J] *)
EhJoules=4.3597447222071*10^-18;
(* Hartree energy atomic units *)
Ehatom=1;

(* Bohr radius in meters *)
aoMeters=5.29177*10^-11;
(* Bohr radius in angstroms *)
aoAngst=0.529177;
(* Bohr radius in atomic units *)
a0atom=1;

(*\[HBar]c [Subscript[eVa, 0]]*)
hbarc=3728.940985719334`;
(*Planks constant in [J*s]*)
hbarJs=1.0546*10^-34;
(*Planks constant in [eV*s]*)
hbareVs=6.582119569*10^-16;
(*Planks constant in atomic units *)
hbaratom=1;

(* Speed of light atomic units *)
catom=137.036;

(*Mass of 3-He atom[eV/c^2]*)
mH3eV=2.80941352*10^9; 
(*Mass of 3-He[kg]*)
mH3kg=3.0*1.6748*10^-27; 
(*electron rest mass[kg]*)
mekg=9.1093837015*10^-31; 
(*Mass of 3-He atomic units*)
mH3atom=mH3kg/mekg;

(*potential depth[eV]*)
eps=8.0*10^-3;
(*potential depth atom *)
epsatom=eps/EheV;

(* l in Angstroms *)
lAngst=93;
(* l in meters *)
lmeters=lAngst*10^-10;
(* l in atomic units *)
latom=lAngst/(aoAngst);

(* boltzmann constant in atomic units *)
kbatom=3.1668115634556072660807*10^-6;

dynPol[
      \[Omega]Value_,
      OptionsPattern[{
      workingPrecision -> 32,
      numberOfTerms -> All,
      element -> "helium"}]] := 
      Module[{
      importData,
      \[HBar], dZ, nA, dynPolZero,
      nAtoms, en, fn, fnc,  dat,
      ec, dynP,      
      \[Omega] = \[Omega]Value,
      el = OptionValue[element],
      nt = OptionValue[numberOfTerms],
      wp = OptionValue[workingPrecision]},
      
      el = SetPrecision[el, wp];
      \[Omega] = SetPrecision[\[Omega], wp];
      
      importData =
      Association[
      {
      "hydrogen" -> {
                   {0.3748, 0.4164},
                   {0.444207, 0.079142},
                   {0.4685, 0.029006},
                   {0.479744, 0.013945},
                   {0.485852, 0.0078035},
                   {0.489535, 0.0048164},
                   {0.491925, 0.003185},
                   {0.493564, 0.0022172},
                   {0.494736, 0.0016062},
                   {0.495603, 0.0012011},
                   {0.496263, 0.0009219},
                   {0.496776, 0.0007231},
                   {0.497183, 0.00057769},
                   {0.497512, 0.00046886},
                   {0.497781, 0.00038577},
                   {0.498004, 0.00032124},
                   {0.498191, 0.00027035},
                   {0.498349, 0.00022967},
                   {0.498484, 0.00019677},
                   {0.4986, 0.00016987},
                   {0.498701, 0.00014767},
                   {0.498788, 0.00012917},
                   {0.498865, 0.00011364},
                   {0.498933, 0.00010051},
                   {0.498994, 0.000089321},
                   {0.499048, 0.000079736},
                   {0.499096, 0.000071476},
                   {0.499139, 0.000064319},
                   {0.499178, 0.000058087},
                   {0.499213, 0.000052635},
                   {0.499245, 0.000047845},
                   {0.499274, 0.000043619},
                   {0.499301, 0.000039877},
                   {0.499325, 0.000036551},
                   {0.499347, 0.000033585},
                   {0.499368, 0.000030931},
                   {0.499387, 0.00002855},
                   {0.499405, 0.000026407},
                   {0.499421, 0.000024474}},
               
      "helium" -> {
                   {0.7797479639585245`, 0.2762549063942177`},
                   {0.8484322938984807`, 0.07345984370593696`},
                   {0.8725049939467012`, 0.029873079868208037`},
                   {0.8836668779867115`, 0.015044577643240147`},
                   {0.8897379494528027`, 0.00863058034689009`},
                   {0.8934020590531018`, 0.0054072942086211715`},
                   {0.8957818907172712`, 0.003610771451003529`},
                   {0.8974143917802622`, 0.0025304630494613965`},
                   {0.8985826213137053`, 0.0018419591736945952`}},
                  
      "lithium" ->{
                   {0.067906000, 0.249},
                   {0.067907900 ,0.498},
                   {0.140906300 ,0.00314},
                   {0.140916200 ,0.00157},
                   {0.166167400 ,0.00141},
                   {0.166167400 ,0.00281},
                   {0.177767900 ,0.00173},
                   {0.177767900 ,0.000887},
                   {0.184034100 ,0.000527},
                   {0.184034100 ,0.00105},
                   {0.187800000 ,0.000337},
                   {0.187800000 ,0.000675},
                   {0.190234300 ,0.000458},
                   {0.190234300 ,0.000229},
                   {0.191904900 ,0.000162},
                   {0.191904900 ,0.000324},
                   {0.193093600 ,0.000174},
                   {0.193093600 ,0.000349},
                   {0.193959100 ,0.000137},
                   {0.193959100 ,0.000273},
                   {0.194642600 ,0.00022},
                   {0.194642600 ,0.00011},
                   {0.195161000 ,0.0000891},
                   {0.195161000 ,0.000178}},              
                                  
     "beryllium" ->{{0.100143300 ,0.000000003900},
                   {0.193942363 ,1.390000000000},
                   {0.268402067 ,0.000000000570},
                   {0.274234031 ,0.008980000000},
                   {0.305432870 ,0.000120000000},
                   {0.319492749 ,0.000692000000},
                   {0.326899425 ,0.000678000000},
                   {0.331253445 ,0.000537000000},
                   {0.334020925 ,0.000047000000},
                   {0.334023240 ,0.000358000000},
                   {0.335844536 ,0.000307000000}},
                  
     "neon" ->    {
                   {0.6126415897851555`,0.01095},
                   {0.6191545530865429`,0.1432},
                   {0.7235277781154128`,0.0129`},
                   {0.726893211216547`,0.0167`},
                   {0.7364706163382146`,0.00459},
                   {0.7401114058630265`,0.0137},
                   {0.7425731657819971`,0.00687},
                   {0.7559542760535727`,0.00645`},
                   {0.759342938033761`,0.00461`},
                   {0.7607956391068579`,0.00246`},
                   {0.7610309711087674`,0.00692`},
                   {0.7645884348529848`,0.00447`},
                   {0.7698719051143058`,0.00324`},
                   {0.7722460597941148`,0.00133`},
                   {0.7723762593351442`,0.0041`},
                   {0.7733359895658615`,0.00156`},
                   {0.7759257131971001`,0.00229`}},
                                     
     "sodium" ->   {{0.0772579570, 0.32000000},
                   {0.0773363080 ,0.64100000},
                   {0.1379319550 ,0.00446000},
                   {0.1574156720 ,0.00900000},
                   {0.1596556370 ,0.00064800},
                   {0.1596668930 ,0.00131000},
                   {0.1699344500 ,0.00019600},
                   {0.1699403300 ,0.00039800},
                   {0.1756018910 ,0.00008030},
                   {0.1756053090 ,0.00016400},
                   {0.1790563660 ,0.00004220},
                   {0.1790586000 ,0.00008650},
                   {0.1813168960 ,0.00002930},
                   {0.1813184040 ,0.00006050},
                   {0.1828766700 ,0.00001760},
                   {0.1828777310 ,0.00003520},
                   {0.1839980520 ,0.00001780},
                   {0.1839988350 ,0.00003570},
                   {0.1848312140 ,0.00001310},
                   {0.1848318060 ,0.00002620},
                   {0.1854671000 ,0.00001010},
                   {0.1854675560 ,0.00002030},
                   {0.1859634170 ,0.00000770},
                   {0.1859637770 ,0.00001500},
                   {0.1863582190 ,0.00000610},
                   {0.1863585060 ,0.00001200},
                   {0.1866774080 ,0.00000500},
                   {0.1866776450 ,0.00001000},
                   {0.1869391370 ,0.00000413},
                   {0.1869393330 ,0.00000826},
                   {0.1871564150 ,0.00000344},
                   {0.1871566420 ,0.00000688}},
                 
     "rubidium" ->{
                   {0.151635`, 1.18`*^-6},
                   {0.151383`, 1.43`*^-6},
                   {0.108054`, 0.004`},
                   {0.126826`, 0.000559`},
                   {0.135938`, 0.00015`},
                   {0.141059`, 0.0000601`},
                   {0.144225`, 0.0000301`},
                   {0.14632`, 0.0000185`},
                   {0.147778`, 9.95`*^-6},
                   {0.148833`, 6.34`*^-6},
                   {0.149621`, 5.17`*^-6},
                   {0.150225`, 3.43`*^-6},
                   {0.150699`, 2.43`*^-6},
                   {0.151077`, 1.9`*^-6}},                  
                  
     "krypton" ->{
                   {0.368683736228198,0.205},
                   {0.3911461688851,0.189},
                   {0.442352462570834,0.0053},
                   {0.454021318271019,0.082},
                   {0.455150544624679,0.154},
                   {0.47295552930022,0.014},
                   {0.477901530241002,0.0435},
                   {0.479081671652322,0.0105},
                   {0.481369440749098,0.061},
                   {0.481926531894721,0.113},
                   {0.490608385088603,0.0015},
                   {0.493262933917402,0.0439},
                   {0.493783663464577,0.0203}},
                  
     "argon" ->   {
                   {0.427158937798129 ,0.0675},
                   {0.434673378069486,0.263},
                   {0.517796530130754,0.027},
                   {0.520095074123345,0.0932},
                   {0.523864468568321,0.0119},
                   {0.525649852635292,0.106}},
                  
     "xenon" ->   {
                   {0.403462258465202, 0.0025},
                   {0.422436026812292,0.004},
                   {0.433892291466077,0.0042},
                   {0.425662229800745,0.006},
                   {0.429334157742711,0.0088},
                   {0.364445662479696,0.0105},
                   {0.41431843639063,0.0222},
                   {0.419767149062525,0.0227},
                   {0.431881512895331,0.0288},
                   {0.436389743269365,0.029},
                   {0.437616513667066,0.0386},
                   {0.435122852921937,0.063},
                   {0.41021647544779,0.084},
                   {0.389293163894544,0.086},
                   {0.431418512094196,0.097},
                   {0.426555855266543,0.191},
                   {0.351680744099899,0.191},
                   {0.310036385485495,0.265},
                   {0.382230638629398,0.379}}
                   }];
      
        dZ = Association[
              {"hydrogen" -> 4.50711,
               "helium" -> 1.383,
               "lithium"  -> 164.1125,
               "beryllium" -> 37.74,
               "neon"  -> 2.66110,
               "sodium"  -> 162.7,
               "rubidium" -> 319.8035716991117`,
               "krypton" -> 16.476,
               "argon"  -> 11.083,
               "xenon"  -> 27.32}];
               
        nA = Association[
              {"hydrogen" -> 1,
               "helium" -> 2,
               "lithium"-> 3,
               "beryllium"-> 4,
               "neon"  -> 10,
               "sodium"->11,
               "rubidium" -> 37,
               "krypton"-> 36,
               "argon" -> 18,
               "xenon" -> 54}];
        
        dynPolZero = dZ[el];
        nAtoms = nA[el];
        
        \[HBar] = 1;
        dat = SetPrecision[importData[el], wp];
        nt = If[nt <= Length[dat], nt, Length[dat], Length[dat]];
        dat = SetPrecision[Take[importData[el], nt], wp];
        
        en = dat[[All, 1]];
        fn = dat[[All, 2]];
        
         dynP[\[Omega]p_] := Sum[
         SetPrecision[fn[[i]]/((en[[i]])^2 - (\[HBar]*\[Omega]p)^2), wp], {i, Length[fn]}];
        
        fnc = SetPrecision[nAtoms, wp] - SetPrecision[Total[fn], wp];
        
        ec = Sqrt[fnc/(dynPolZero - dynP[0])];
        
        Return[SetPrecision[dynP[\[Omega]] + fnc/(ec ^2 - (\[HBar]*\[Omega])^2), wp]]]
        
diFunSiCM[
	temp_,
	energy_,
	OptionsPattern[{
	workingPrecision -> 32,
	roomTemperature -> 293}]] := 
  diFunSiCM[temp, energy] =
    Module[{
    ak,
    \[Omega]k,
    \[Gamma]k,
    \[Gamma]pk,
    \[CapitalDelta]T,
    coeffs, fitFun,rhoFun,
    \[Omega] = energy,
    T = temp,
    rmTmp = OptionValue[roomTemperature],
    wp = OptionValue[workingPrecision]},
   
   \[Omega] = SetPrecision[\[Omega], wp];
   T = SetPrecision[T, wp];
   
   \[CapitalDelta]T = SetPrecision[(T - rmTmp)/rmTmp, wp];
   
   coeffs = {
   ak[1]->0.0048696128333626375542549925093309867190883716484779276889`32.-
   0.0008936146662200208985501543160480642566885605565071363909`32. \[CapitalDelta]T+
   0.0007853554005988041416461844798016686777979381100426017871`32. \[CapitalDelta]T^2,
   ak[2]->0.7722063733252617625892424059712809167105707294371352126484`32.+
   0.0059835947019468031853435991544884247281253621596770454425`32. \[CapitalDelta]T+
   0.0005586097160367060800590383095668561284519802189854474088`32. \[CapitalDelta]T^2,
   \[Omega]k[1]->0.1289322568371133982452705357634681028850394688941310723087`32.-
   0.004570590465075703632374111485379289584620194089455898097`32. \[CapitalDelta]T+
   0.0009420524819284027934288701710058733496263579561688334749`32. \[CapitalDelta]T^2,
   \[Omega]k[2]->0.312947418993925004135227646050038866618358894374744851117`32.+
   0.0006404754327359217805907301952854304624688071596870436859`32. \[CapitalDelta]T-
   0.0006526791512090928730831331487105815297675163905465442224`32. \[CapitalDelta]T^2,
   \[Gamma]k[1]->0.0187518089756987185091815628732589625677689652830381904637`32.+
   0.0009273743897571212275140220256663896686527115880474067856`32. \[CapitalDelta]T+
   0.0016510924454748934125206477800967654327022067977832572907`32. \[CapitalDelta]T^2,
   \[Gamma]k[2]->0.0974187443360465869447508041242735458068527746320664114541`32.+
   0.0481407270741869772555733246144993797245545670965852840827`32. \[CapitalDelta]T-
   0.0151765450422552680330623092033012039621613938077261403962`32. \[CapitalDelta]T^2,
   \[Gamma]pk[1]->0.1386917953069810006160834682622870207756410759295265993979`32.+
   0.0116097579932173565553556902215582402916131729468944843792`32. \[CapitalDelta]T-
   0.0154301624764129483240179697881735859894588841613788670849`32. \[CapitalDelta]T^2,
   \[Gamma]pk[2]->0.0950465426170113452851010675270748842034803967604293999891`32.+
   0.0560661525053894053710405452826046683261067296286291117479`32. \[CapitalDelta]T-
   0.0194768645992677257314602442739274097063117131696813558843`32. \[CapitalDelta]T^2};
   
   fitFun =
   Sum[(ak[k]*(\[Omega]k[k]^2 - I*\[Gamma]pk[k]*\[Omega]))/(\[Omega]k[k]^2 - I*\[Omega]*\[Gamma]k[k] - \[Omega]^2),{k, 2}];
   rhoFun=SetPrecision[fitFun /. coeffs, wp];
   
   Return[(-1-2*(rhoFun))/(-1+(rhoFun))]]

                    
diFunSiSL[
	temp_,
	energy_,
	OptionsPattern[{
	workingPrecision -> 32,
	roomTemperature -> 293}]] := 
  diFunSiSL[temp, energy] =
    Module[{
    ak,
    \[Omega]k,
    \[Gamma]k,
    \[Gamma]pk,
    \[CapitalDelta]T,
    coeffs, fitFun,
    \[Omega] = energy,
    T = temp,
    rmTmp = OptionValue[roomTemperature],
    wp = OptionValue[workingPrecision]},
   
   \[Omega] = SetPrecision[\[Omega], wp];
   T = SetPrecision[T, wp];
   
   \[CapitalDelta]T = SetPrecision[(T - rmTmp)/rmTmp, wp];
   
   coeffs = {
   ak[1]->2.8919838690701850528055142365337798028879593376224077450042`32.-
   0.633872208298529363459618043674649094322131523947129524857`32. \[CapitalDelta]T+
   0.3889683358354273538425166190418525167676666334871495496889`32. \[CapitalDelta]T^2,
   ak[2]->7.8641838151479108315469249859312748215349689888198978358678`32.+
   1.1209788546238364447450716803378585700076457242434586626545`32. \[CapitalDelta]T-
   0.3754427036937993257765664268361298131258977505166395324107`32. \[CapitalDelta]T^2,
   \[Omega]k[1]->0.1255316808844417556447471687664107606254147851360644238252`32.-
   0.0021536437226537829159945728121532685301851745474090454224`32. \[CapitalDelta]T-
   0.0000990985335284814181762031913334199458272252646660941171`32. \[CapitalDelta]T^2,
   \[Omega]k[2]->0.1543875997079725833192207826984552208272053847112291703529`32.-
   0.004407568082221593289041611816218116609924712610216100702`32. \[CapitalDelta]T+
   0.0003685367926367219893855669957768947444201922197919461636`32. \[CapitalDelta]T^2,
   \[Gamma]k[1]->0.0130810703467084695189864924564736417849404188235771776043`32.-
   0.0003701311300762934649180324426570142948269268014773979785`32. \[CapitalDelta]T+
   0.00181856785725007198106032807203161139979907845184369528`32. \[CapitalDelta]T^2,
   \[Gamma]k[2]->0.0298038760801996059018739534363429262679720492740772656176`32.+
   0.0040109150721308753881256573312601701314174366256471700993`32. \[CapitalDelta]T-
   0.0004602127078162135966670626162316693279245696842013445619`32. \[CapitalDelta]T^2,
   \[Gamma]pk[1]->0.057316385781155560172601707109158535918662171652026441465`32.-
   0.0126109127331666601066020628642770350556260697991091204531`32. \[CapitalDelta]T-
   0.0012106922165056061105917496170684722216593652964651564729`32. \[CapitalDelta]T^2,
   \[Gamma]pk[2]->0.0086717632665016052720581658218628429839454678302110727485`32.+
   0.0168824657490159140785240359493340756808737863435663119145`32. \[CapitalDelta]T-
   0.0041169889805495414735311227164094634358078124140203500022`32. \[CapitalDelta]T^2};
   
   fitFun =
   1+Sum[(ak[k]*(\[Omega]k[k]^2 - I*\[Gamma]pk[k]*\[Omega]))/(\[Omega]k[k]^2 - I*\[Omega]*\[Gamma]k[k] - \[Omega]^2),{k, 2}];
   Return[ SetPrecision[fitFun /. coeffs, wp]]]
          
hFun[
  temperature_,
  omega_,
  pValue_,
  OptionsPattern[{
  workingPrecision -> 32}]] :=
  hFun[temperature, omega, pValue] =
   Module[{
     Hfun,eps,
     T = temperature,
     \[Omega] = omega,
     p = pValue,
     wp = OptionValue[workingPrecision]},
     
     eps = SetPrecision[diFunSiCM[T, \[Omega], workingPrecision -> wp], wp];
     p = SetPrecision[p, wp];
     
     Hfun = 
     SetPrecision[(Sqrt[eps - 1 + p^2] - 
         p)/(Sqrt[eps - 1 + p^2] + p) + 
         (1 - 2*p^2)*(Sqrt[eps - 1 + p^2] - 
         p*eps)/(Sqrt[eps - 1 + p^2] + p*eps), wp]]
         
         
dhFun[
    temperature_,
    omega_,
    pValue_,
    OptionsPattern[{
      workingPrecision -> 32}]] :=
 dhFun[temperature, omega, pValue] =
     Module[{
    dHfun, eps,
    T = temperature,
    \[Omega] = omega,
    p = pValue,
    wp = OptionValue[workingPrecision]},
   
   eps = SetPrecision[diFunSiCM[T, \[Omega], workingPrecision -> wp], wp]; 
   p = SetPrecision[p, wp]; 
   dHfun = SetPrecision[(p (eps^2 (-1 + 3 p^2) + 
   (-1 + p) (1 + p) (3 - 4 p Sqrt[-1 + eps + p^2] + 
   8 p^2 (-1 + p (p + Sqrt[-1 + eps + p^2]))) + 
   2 eps (2 + p^2 (-5 + 2 p (2 p + 
   Sqrt[-1 + eps + p^2])))))/(Sqrt[-1 + 
   eps + p^2] (p + Sqrt[-1 + eps + p^2])^2 (eps p + 
   Sqrt[-1 + eps + p^2])^2), wp]]
   
   
ddhFun[
    temperature_,
    omega_,
    pValue_,
    OptionsPattern[{
      workingPrecision -> 32}]] :=
 ddhFun[temperature, omega, pValue] =
     Module[{
     ddHfun, eps,
     T = temperature,
     \[Omega] = omega,
     p = pValue,
     wp = OptionValue[workingPrecision]},
     eps = SetPrecision[diFunSiCM[T, \[Omega], workingPrecision -> wp], wp];
     p = SetPrecision[p, wp];
     ddHfun = 
     SetPrecision[
     1/4 (-(2/((-1 + eps + p^2) (p + Sqrt[-1 + eps + p^2])^2)) - 
     1/((-1 + eps + p^2)^(3/2) (p + Sqrt[-1 + eps + p^2])) + 
     (-1 + 2 p^2)/((-1 + eps + p^2)^(3/2) (eps p + Sqrt[-1 + eps + p^2])) - 
     (2 (-1 + 2 p^2) (-1 + 4 p^2 (-1 + eps + p^2)))/((-1 + 
     eps + p^2) (eps p + Sqrt[-1 + eps + p^2])^2) + 
     ((1 - 2 p^2) (-eps p + Sqrt[-1 + eps + p^2]) (8 p^2 +3/(-1 + eps +p^2) + 
     (p (-8 + 9 eps + 8 p^2))/(-1 + eps + p^2)^(3/2)))/(eps p + 
     Sqrt[-1 + eps + p^2])^3 + (-3 + 3 eps + 2 p (p - 
     Sqrt[-1 + eps + p^2]))/((-1 + eps + p^2)^(3/2) (p + 
     Sqrt[-1 + eps + p^2])^3)), wp]]

         
c30[
    temperature_,
    OptionsPattern[{ 
    workingPrecision -> 32,
    element-> "helium",
    precisionGoal -> MachinePrecision,
    maxRecursion -> 100,
    outputUnits->"atom",
    method -> "DoubleExponential"}]] :=   
     Module[{
     \[Omega], \[HBar], alpha, int, eps, c3,
     T = temperature,
     mth = OptionValue[method],
     mr = OptionValue[maxRecursion],
     wp = OptionValue[workingPrecision],
     el = OptionValue[element],
     pg = OptionValue[precisionGoal],
     ou = OptionValue[outputUnits]},
     
     \[HBar] = 1;
     
     (* The dielectric function *)
     eps = SetPrecision[diFunSiCM[T, I * \[Omega], workingPrecision -> wp], wp];
     alpha = SetPrecision[dynPol[I*\[Omega], element -> el, workingPrecision -> wp],wp];
     
     int = SetPrecision[\[HBar]/(4*\[Pi])*alpha*(eps - 1)/(eps + 1), wp] /. \[Omega] -> x;
     
     c3= NIntegrate[int,{x, 0, \[Infinity]},
             Method -> mth,
             MaxRecursion->mr,
             WorkingPrecision -> wp,
             PrecisionGoal -> pg];
     If[ou!="eV",c3,c3*EheV*aoAngst^3]]
     
         
cFour[
  temperature_,
  OptionsPattern[{
      workingPrecision -> 32,
      element -> "helium",
      outputUnits->"atom"}]] := 
   Module[{
    Hfun, \[HBar], c, c4, alpha, p,
    T = temperature,
    wp = OptionValue[workingPrecision],
    el = OptionValue[element],
    ou = OptionValue[outputUnits]},
   
   \[HBar] = SetPrecision[1, wp];
   c = SetPrecision[137.13086704788267`, wp];
   
   (* Here we define the H-function *)
   Hfun=SetPrecision[hFun[T, 0, p, workingPrecision -> wp], wp];
   alpha = SetPrecision[dynPol[0, element -> el, workingPrecision -> wp],wp];  
   c4 = (\[HBar]*c)/(2*\[Pi])*3/8*alpha*
     NIntegrate[Hfun/p^4, {p, 1, \[Infinity]}, 
      WorkingPrecision -> wp];
   If[ou!="eV", c4, c4*EheV*aoAngst^4]] 
   
diFunSiGr[
  	temp_,
  	energy_,
  	OptionsPattern[{
    	workingPrecision -> 32,
    	roomTemperature -> 293,
    	interpolationOrderCn->1,
    	interpolationOrderCk->1,
    	interpolationOrdern->1,
    	interpolationOrderk->1}]] := 
   diFunSiGr[temp, energy] =
      Module[{
    evF, nF, kF, cn, ck,
    nFInterp,
    kFInterp,
    cnInterp,
    ckInterp,
    nInterp,
    kInterp,
    epsInterp,
    coeffs, evEh,
    \[Omega] = energy,
    T = temp,
    iCn = OptionValue[interpolationOrderCn],
    iCk = OptionValue[interpolationOrderCk],
    in = OptionValue[interpolationOrdern],
    ik = OptionValue[interpolationOrderk],
    rmTmp = OptionValue[roomTemperature],
    wp = OptionValue[workingPrecision]},
   
   evEh = SetPrecision[27.211386245988,wp];
   \[Omega] = SetPrecision[\[Omega], wp];
   T = SetPrecision[T, wp];
   
   coeffs = {
     {4.95936749732399`, 1.665`, 3.665`,2.9`, -0.9`},
     {4.76862259358076`, 1.757`, 4.084`,2, -1.5`},
     {4.59200694196666`, 2.068`, 4.68`,0, -3.1`},
     {4.42800669403928`, 2.959`,5.287`, -4.8`, -3.3`},
     {4.27531680803792`, 4.356`, 5.286`, -9, 0.8`},
     {4.13280624776999`, 4.976`, 4.234`, -3.8`,2.5`},
     {3.99948991719677`, 5.121`, 3.598`, -1.6`,3.2`},
     {3.87450585728437`, 5.112`, 3.303`, -1.3`,1.5`},
     {3.75709658888181`, 5.195`, 3.1`, -1.2`,0.7`},
     {3.64659374803234`, 5.301`, 2.977`, -1,0.3`},
     {3.54240535523142`, 5.494`, 2.938`, -1.8`,0},
     {3.44400520647499`, 6.026`,2.966`, -4.1`, -1.4`},
     {3.35092398467837`, 6.891`,2.171`, -4.4`, 4.2`},
     {3.26274177455526`, 6.616`, 0.946`, -2.3`,9.1`},
     {3.17908172905384`, 6.039`, 0.445`, 1, 26},
     {3.09960468582749`, 5.613`, 0.296`, 2.1`,33},
     {3.02400457153902`, 5.33`, 0.227`, 2.1`,31},
     {2.95200446269285`, 5.119`, 0.176`, 1.9`,29},
     {2.8833531961186`, 4.949`, 0.138`, 1.8`,29},
     {2.81782244166136`, 4.812`, 0.107`, 1.7`,28},
     {2.75520416517999`, 4.691`, 0.086302`, 1.6`,28},
     {2.69530842245869`, 4.587`, 0.071381`, 1.6`,29},
     {2.6379614347468`, 4.497`, 0.062086`, 1.5`,29},
     {2.58300390485624`, 4.419`, 0.055004`, 1.4`,30},
     {2.53028953945101`, 4.35`, 0.049131`, 1.4`,30},
     {2.47968374866199`, 4.294`, 0.044165`, 1.3`,31},
     {2.43106249868823`, 4.241`, 0.039367`, 1.3`,31},
     {2.38431129679038`, 4.193`, 0.036415`, 1.2`,32},
     {2.33932429119056`, 4.151`, 0.033108`, 1.2`,33},
     {2.29600347098333`, 4.112`, 0.030295`, 1.2`,33},
     {2.25425795332909`, 4.077`, 0.027968`, 1.1`,33},
     {2.21400334701964`, 4.045`, 0.025758`, 1.1`,34},
     {2.17516118303684`, 4.015`, 0.024131`, 1.1`,34},
     {2.13765840401896`, 3.988`, 0.022524`, 1.1`,34},
     {2.10142690564576`, 3.963`, 0.021081`, 1,34},
     {2.066403123885`, 3.94`, 0.019934`, 1,34},
     {2.0325276628377`, 3.918`, 0.018446`, 1,35},
     {1.99974495859838`, 3.898`, 0.017367`, 1,35},
     {1.96800297512857`, 3.879`, 0.016444`, 1,35},
     {1.93725292864218`, 3.861`, 0.015432`, 1,35},
     {1.9074490374323`, 3.844`, 0.014431`, 0.9`,35},
     {1.8785482944409`, 3.828`, 0.013498`, 0.9`,35},
     {1.85051026019552`, 3.813`, 0.012743`, 0.9`,36},
     {1.82329687401617`, 3.798`, 0.011905`, 0.9`,36},
     {1.79687228163913`, 3.784`, 0.011201`, 0.9`,36},
     {1.77120267761571`, 3.772`, 0.010528`, 0.9`,37},
     {1.74625616102957`, 3.759`, 0.010057`, 0.9`,37},
     {1.7220026032375`, 3.748`, 0.0096257`, 0.9`,37},
     {1.69841352648082`, 3.737`, 0.0089461`, 0.9`,37},
     {1.67546199233919`, 3.727`, 0.008362`, 0.8`,37},
     {1.653122499108`, 3.717`, 0.0078185`, 0.8`,37},
     {1.63137088727763`, 3.708`, 0.007197`, 0.8`,37},
     {1.61018425237792`, 3.699`, 0.0067402`, 0.8`,37},
     {1.58954086452692`, 3.691`, 0.0063933`, 0.8`,37},
     {1.56942009408987`, 3.683`, 0.005834`, 0.8`,38},
     {1.54980234291375`, 3.675`, 0.0054113`, 0.8`, 40},
     {1.53066898065555`, 3.668`, 0.0049955`, 0.8`,41},
     {1.51200228576951`, 3.661`, 0.0046134`, 0.8`,42},
     {1.49378539076024`, 3.654`, 0.0042734`, 0.7`,44},
     {1.47600223134643`, 3.647`, 0.0039439`, 0.7`,45},
     {1.45863749921294`, 3.641`, 0.003612`, 0.7`,46},
     {1.4416765980593`, 3.635`, 0.0032781`, 0.7`, 47},
     {1.42510560267931`, 3.63`, 0.0029839`, 0.7`,49},
     {1.40891122083068`, 3.624`, 0.0026821`, 0.7`,51},
     {1.39308075767528`, 3.619`, 0.0024293`, 0.7`,52},
     {1.37760208259`, 3.614`, 0.0021701`, 0.7`,54},
     {1.36246359816593`, 3.609`, 0.0019625`, 0.7`,56},
     {1.34765421122934`, 3.604`, 0.0017571`, 0.7`,57},
     {1.33316330573226`, 3.6`, 0.0015467`, 0.7`, 59},
     {1.3189807173734`, 3.595`, 0.0013689`, 0.7`,62},
     {1.3050967098221`, 3.591`, 0.0011793`, 0.7`,65},
     {1.29150195242812`, 3.587`, 0.0010237`, 0.7`,69},
     {1.27818749931031`, 3.583`, 0.00087225`, 0.7`,73},
     {1.26514476972551`, 3.579`, 0.00074866`, 0.7`,78},
     {1.25236552962727`, 3.575`, 0.00062238`, 0.7`,83},
     {1.239841874331`, 3.572`, 0.0005093`, 0.7`,90},
     {1.22756621220891`, 3.568`, 0.00041071`, 0.7`,97},
     {1.21553124934411`, 3.565`, 0.00032386`, 0.7`,105},
     {1.20372997507864`, 3.562`, 0.00024753`, 0.7`,112},
     {1.19215564839519`, 3.559`, 0.00018704`, 0.6`,120},
     {1.18080178507714`, 3.556`, 0.0001362`, 0.6`, 135},
     {1.16966214559528`, 3.553`, 0.000093631`, 0.6`,145},
     {1.15873072367383`, 3.55`, 0.000068118`, 0.6`,155},
     {1.14800173549166`, 3.547`, 0.000053285`, 0.6`,160},
     {1.13746960947798`, 3.545`, 0.000040768`, 0.6`,165},
     {1.12712897666454`, 3.542`, 0.000030637`, 0.6`,175},
     {1.11697466155946`, 3.54`, 0.000023849`, 0.6`,180},
     {1.10700167350982`, 3.537`, 0.000017825`, 0.6`,185},
     {1.09720519852301`, 3.535`, 0.000013488`, 0.6`,190},
     {1.08758059151842`, 3.532`, 9.0718`*^-6, 0.6`,200},
     {1.07812336898348`, 3.53`, 6.223`*^-6, 0.6`,210},
     {1.06882920200948`, 3.528`, 3.877`*^-6, 0.6`,230},
     {1.05969390968461`, 3.526`, 2.0483`*^-6, 0.6`,260},
     {1.05071345282288`, 3.524`, 6.1036`*^-7, 0.6`,320},
     {1.04188392800924`, 3.522`, 3.4091`*^-7, 0.6`,345},
     {1.0332015619425`, 3.52`, 2.1008`*^-7, 0.6`,355},
     {1.02466270605868`, 3.518`, 1.2518`*^-7, 0.6`,380},
     {1.01626383141885`, 3.517`, 7.9609`*^-8, 0.6`,390},
     {1.00800152384634`, 3.515`, 4.6004`*^-8, 0.6`,405},
     {0.999872479299191`, 3.513`, 2.3682`*^-8, 0.6`,410},
     {0.991873499464798`, 3.511`, 9.9472`*^-9, 0.6`,430},
     {0.984001487564283`, 3.509`, 3.6096`*^-9, 0.6`,440},
     {0.976253444355116`, 3.508`, 2.0213`*^-9, 0.6`,455},
     {0.968626464321092`, 3.506`, 1.2223`*^-9, 0.6`,470},
     {0.961117732039533`, 3.505`, 7.2885`*^-10, 0.6`,500},
     {0.953724518716152`, 3.503`, 4.6553`*^-10, 0.6`,525},
     {0.946444178878624`, 3.502`, 2.8147`*^-10, 0.6`,550},
     {0.939274147220452`, 3.5`, 1.6807`*^-10, 0.6`,580},
     {0.932211935587216`, 3.499`, 8.467`*^-11, 0.6`,610},
     {0.925255130097759`, 3.497`, 3.7322`*^-11, 0.6`,650},
     {0.918401388393331`, 3.496`, 1.8263`*^-11, 0.6`,670},
     {0.911648437008086`, 3.495`, 1.0281`*^-11, 0.6`,675},
     {0.904994068854742`, 3.494`, 6.5413`*^-12, 0.6`,680},
     {0.898436140819563`, 3.492`, 4.173`*^-12, 0.6`,685},
     {0.891972571461149`, 3.491`, 2.5441`*^-12, 0.6`,690},
     {0.885601338807855`, 3.49`, 1.5597`*^-12, 0.6`,700},
     {0.879320478248934`, 3.489`, 9.5374`*^-13, 0.6`,710},
     {0.873128080514787`, 3.488`, 5.65`*^-13, 0.6`,720},
     {0.867022289741956`, 3.487`, 2.8449`*^-13, 0.6`,730},
     {0.861001301618748`, 3.486`, 2.0626`*^-13, 0.6`,740},
     {0.855063361607584`, 3.485`, 1.3846`*^-13, 0.6`, 750}};
   
   evF = SetPrecision[coeffs[[All, 1]],wp];
   nF = SetPrecision[coeffs[[All, 2]],wp];
   kF = SetPrecision[coeffs[[All, 3]],wp];
   cn = SetPrecision[coeffs[[All, 4]],wp];
   ck = SetPrecision[coeffs[[All, 5]],wp];
   
   nFInterp = Interpolation[
     Table[
      {evF[[i]]/evEh, nF[[i]]}, {i, Length[evF]}],
      InterpolationOrder->in];
   
   kFInterp = Interpolation[
     Table[
      {evF[[i]]/evEh, kF[[i]]}, {i, Length[evF]}],
      InterpolationOrder->ik];
   
   cnInterp = Interpolation[
     Table[
      {evF[[i]]/evEh, cn[[i]]}, {i, Length[evF]}],
      InterpolationOrder->iCn];
   
   ckInterp = Interpolation[
     Table[
      {evF[[i]]/evEh, ck[[i]]}, {i, Length[evF]}],
      InterpolationOrder->iCk];
   
   nInterp = 
    nFInterp[\[Omega]]*(T/rmTmp)^(cnInterp[\[Omega]]*rmTmp*10^-4);
   
   kInterp = 
    kFInterp[\[Omega]]*(T/rmTmp)^(ckInterp[\[Omega]]*rmTmp*10^-4);
   
   epsInterp = (nInterp + I*kInterp)^2;
   
   Return[ SetPrecision[epsInterp, wp]]]
                            
casimirPolderSi[
	temperature_,
	zValue_?NumericQ,
	OptionsPattern[{
		workingPrecision -> 32,
		precisionGoal -> MachinePrecision,
		element -> "helium"}]] :=
	Module[{
	alpha, Hfun, intgrnd, p, coeff,
	c, \[HBar], \[Omega]prime, pprime, ex,
	eps, \[Omega],
	z = zValue,
	T = temperature,
	wp = OptionValue[workingPrecision],
	pg = OptionValue[precisionGoal],
	el = OptionValue[element]},
	
	\[HBar] = 1;
	c = SetPrecision[137.13086704788267`,wp];
	(* The dynamic polarizabitliy as defined in the function above *)
	alpha = SetPrecision[dynPol[I*\[Omega],element -> el, workingPrecision -> wp],wp];
	
	(* Here we define the H-function *)
	Hfun = SetPrecision[hFun[T, I*\[Omega], p, workingPrecision -> wp], wp];
	(* The exponential term in the integrand *)
	ex = SetPrecision[Exp[-(2*p*\[Omega]*z)/c], wp];
	
	(* The full integrand *)
	intgrnd =
	SetPrecision[Re[-\[HBar]/(2*\[Pi])*alpha*\[Omega]^3/c^3*Hfun*ex], wp];
	
	NIntegrate[
	intgrnd,
	{\[Omega],SetPrecision[0,wp],\[Infinity]},
	{p,SetPrecision[1,wp],\[Infinity]},
	WorkingPrecision->wp,
	PrecisionGoal->pg,
	Method->{
	"GlobalAdaptive",
	"MaxErrorIncreases"->10000,
	Method->"GaussKronrodRule"}]]
       
f0[
   temperature_,
   zValue_,
   	OptionsPattern[{
     		workingPrecision -> 32,
     		element -> "helium",
     		outputUnits->"atom"}]] :=
     Module[{
     fz,
     epZro, alphZro, Hfun, wTH, coeff, c, \[HBar], kb,
     z = zValue,
     T = temperature,
     wp = OptionValue[workingPrecision],
     el = OptionValue[element],
     ou = OptionValue[outputUnits]},
    
    \[HBar] = 1;
    c = SetPrecision[137.13086704788267`, wp];
    kb =  SetPrecision[3.1668115634556072660807*10^-6, wp];
    
    (* The dielectric function *)
    epZro = SetPrecision[diFunSiCM[T, 0, workingPrecision -> wp], wp];
    
    (* The dynamic polarizabitliy as defined in the function above *)
    alphZro = SetPrecision[dynPol[0,element -> el, workingPrecision -> wp],wp];

    fz=-(kb*T*alphZro)/(4*z^3)*(epZro - 1)/(epZro + 1);
    If[ou!="eV",fz,fz*EheV*aoAngst^3]];
          
CPintegrand[
      numbTerms_?NumericQ,
  	temperature_,
  	zValue_?NumericQ,
  	OptionsPattern[{
    		workingPrecision -> MachinePrecision,
    		precisionGoal -> Automatic,
    		element -> "helium"}]] :=
 	Module[{
    	alpha, Hfun, intgrnd, p, coeff,
    	c, \[HBar], \[Omega]prime, pprime, ex,
    	eps, \[Omega], kb, omTerms, coefs,
    	n = numbTerms,
    	z = zValue,
    	T = temperature,
    	wp = OptionValue[workingPrecision],
    	pg = OptionValue[precisionGoal],
    	el = OptionValue[element]},
    	
    	\[HBar] = hbaratom;
    	c = SetPrecision[catom, wp];
        kb = kbatom;
        \[Omega] = 2*\[Pi]*n*kb*T/\[HBar];
   	(* The dynamic polarizabitliy as defined in the function above *)
   	
   	alpha = SetPrecision[dynPol[I*\[Omega],
   	element -> el,
       workingPrecision -> wp], wp];
   	
   	(* Here we define the H-function *)
   	Hfun = SetPrecision[hFun[T, I*\[Omega], p,
       workingPrecision -> wp], wp];
   	(* The exponential term in the integrand *)
   	ex = SetPrecision[Exp[-(2*p*\[Omega]*z)/c], wp];
   	
   	(* integrand for p-integral *)
   	intgrnd =
    	SetPrecision[Hfun*ex, wp];
    	
       omTerms =
    	Chop@Re@SetPrecision[alpha*\[Omega]^3, wp];
   
       coefs =
    	Re@SetPrecision[-(kb*T)/c^3, wp];
       
       coefs*omTerms*
     	 NIntegrate[
      	SetPrecision[intgrnd, wp],
      	{p, 1, \[Infinity]},
      	MaxRecursion -> 100000,
      	WorkingPrecision -> wp,
      	PrecisionGoal -> pg,
      	Method -> {
        	"GlobalAdaptive","MaxErrorIncreases" -> 100000,
        	Method -> "GaussKronrodRule"}] // Quiet] // Quiet
        	
CPfull[
  	temperature_,
  	zValue_?NumericQ,
  	OptionsPattern[{
    		workingPrecision -> MachinePrecision,
    		precisionGoal -> Automatic,
    		element -> "helium",
    		nSumTerms -> 50}]] :=
 	Module[{
    	intgrnd,
    	T = temperature,
    	z = zValue,
    	wp = OptionValue[workingPrecision],
    	pg = OptionValue[precisionGoal],
    	el = OptionValue[element],
    	nt = OptionValue[nSumTerms]},
   
   	NSum[CPintegrand[n, T, z],
   	{n, 1, \[Infinity]},
   	NSumTerms->nt,
   	WorkingPrecision->wp,
   	Method -> "EulerMaclaurin"] + f0[T, z] // Quiet] // Quiet
   	


End[]
EndPackage[]
