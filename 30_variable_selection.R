"To control the frequency with which interaction tests are carried out,
we puta Bonferroni-corrected significance threshold on each set of tests and
carry out the interaction tests only if all main effects are not significant.
Letχ2_ν,α denote the upper-αquantile of the chi-squared distribution with ν d.f.
The algorithm for variable selection can now be stated as follows"

"ALGORITHM 2.1  (Variable  selection).Let K>1  be  the  number  of  non-constant
predictor  variables  in  the  node.  Defineα=0.05/Kandβ=0.05/{K(K−1)}:
1.  Use Procedure2.1to findWM(Xi)fori=1,2,...,K.
2.  If maxiWM(Xi)>χ21,α, select the variable with the largest value of WM(Xi) and exit.
3.  Otherwise,  use  Procedure2.2 to  findWI(Xi,Xj) for  each  pair  of  predictor variables:
(a)  If  maxi=jWI(Xi,Xj)>χ21,β,
select  the  pair  with  the  largest  value  of WI(Xi,Xj) and exit.
(b)  Otherwise, select theXiwith the largest value of WM(Xi)."
