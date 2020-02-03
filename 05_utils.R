wilson_hilferty_approx <- function(chi_value, v) {
  inner <- (chi_value / v) ^ (1 / 3) - 1 + (2 / (9 * v))
  return(max(0, (7 / 9 + sqrt(v) * inner) ^ 3))
}
