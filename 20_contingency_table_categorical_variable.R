# http://pages.stat.wisc.edu/~loh/treeprogs/guide/aoas260.pdf

# PROCEDURE 2.1.Main effect chi-squared statistic for X at node t: pg 6

# If X is a categorical variable
# a) Form a contingency table with the class labels as rows and
# the categories of X as columns
contingency_table <-
  table(mammograph_data$ME, mammograph_data$SYMPT)

# b) Let ν be the degrees of freedom of the table after deleting
# any rows and columns with no observations.
# Compute the chi-squared statistic χ2_ν for testing independence.
# If ν >1, use the Wilson–Hilferty approximation twice to
# convert χ2_ν to the 1-d.f. chi-squared

wilson_hilferty_approx <- function(chi_value, v) {
  inner <- (chi_value / v) ^ (1 / 3) - 1 + (2 / (9 * v))
  return(max(0, (7 / 9 + sqrt(v) * inner) ^ 3))
}

v <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)

chi_test <- chisq.test(mammograph_data$ME, mammograph_data$SYMPT)
chi_value <- chi_test$statistic
if (v > 1) {
  chi_value <- wilson_hilferty_approx(chi_value, v)
  chi_value <- wilson_hilferty_approx(chi_value, v)
}

pvalue <- pchisq(chi_value, df = 1, lower.tail = FALSE)
