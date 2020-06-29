# A procedure for optimising the timing of loan recovery across portfolios
A novel procedure is presented for the objective comparison and evaluation of a bank's decision rules in optimising the timing of loan recovery. This procedure is based on finding a delinquency threshold at which the financial loss of a loan portfolio (or segment therein) is minimised, using a dynamic programming approach. The method incorporates the time value of money, costs, and the fundamental trade-off between accumulating arrears versus forsaking future interest revenue. Moreover, the procedure's formulation is measure-agnostic, thereby allowing an indirect comparison of alternative delinquency measures, other than payments in arrears. We demonstrate the procedure across a range of credit risk scenarios and portfolio compositions. The computational results show that threshold optima can exist across the risk spectrum, barring the extremely low/high ranges. In addition, the procedure is sensitive to more exotic portfolios, including systemic defaults and portfolios with a significant degree of episodic delinquency (cycles of curing and re-defaulting). In optimising a portfolio's recovery decision, our procedure can better inform the quantitative aspects of a bank's collection policies than relying on arbitrary discretion alone.

## Structure
This R-codebase can be run sequentially using the file numbering itself as a structure. Delinquency measures are algorithmically defined in **DelinqM.R** as data-driven functions, which may be valuable to the practitioner outside of the study's current scope. Note that scripts 3.2 and 3.3 are interactive scripts wherein the loss optimisation procedure is repeatedly run by rerunning the script with different settings, as set out in the comments. Each independent run produces results that are saved for graphing later on.

## Data
This R-codebase creates designed portfolios as constrained by the framework described in the main text, with adequate commentary within the scripts.


## Copyright
All code and scripts are hereby released under an [MIT](https://opensource.org/licenses/MIT) license. Similarly, all graphs produced by relevant scripts as well as those published here, are hereby released under a Creative Commons Attribution ([CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/)) licence.
