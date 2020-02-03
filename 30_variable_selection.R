"To control the frequency with which interaction tests are carried out,
we puta Bonferroni-corrected significance threshold on each set of tests and
carry out the interaction tests only if all main effects are not significant.
Letχ2_ν,α denote the upper-αquantile of the chi-squared distribution with ν d.f.
The algorithm for variable selection can now be stated as follows"

"ALGORITHM 2.1  (Variable  selection).
Let K > 1  be  the  number  of  non-constant
predictor  variables  in  the  node.

Define α=0.05/K and β=0.05/{K(K−1)}:

1.  Use Procedure 2.1 to find W_M(X_i) for i=1,2,...,K.
2.  If max_i W_M(X_i) > χ2_(1,α),
    select the variable with the largest value of W_M(X_i) and exit.
3.  Otherwise,  use  Procedure 2.2 to  find W_I(X_i,X_j)
    for  each  pair  of  predictor variables:
    (a)  If  max_(i!=j) W_I(X_i,X_j) >χ2_(1,β), select  the
         pair  with  the  largest  value  of W_I(X_i,X_j) and exit.
    (b)  Otherwise, select the X_i with the largest value of W_M(X_i)."

K <- ncol(mammograph_data) - 1 (response) #TODO automate this via formula
alpha <- 0.05/K

# 1.  Use Procedure 2.1 to find W_M(X_i) for i=1,2,...,K.

list_main_effect <- c()
j <- 1
for (i in 1:ncol(mammograph_data)) {
  curr_pred <- mammograph_data[i]
  if (names(curr_pred) != "ME") {
    chi_sq <- main_effect_chi_stat(mammograph_data$ME,
                                   curr_pred[[1]], mammograph_data)
    list_main_effect[[j]] <- c(names(curr_pred), chi_sq)
    j = j + 1
  }
}
