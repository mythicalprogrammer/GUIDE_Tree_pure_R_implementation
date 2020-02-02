# We apply the same idea to assess the local interaction effects of each
# pair of variables, using Cartesian products of sets of values for the
# columns of the chi-squared table.

# PROCEDURE 2.2.Interaction chi-squared statistic for a pair of variables
# X1 and X2 at node t:

# 1.  If Xi(i=1,2) is noncategorical, split its range into two intervals
# (Ai1,Ai2) at the sample mean x_mean if N(t) < 45J_t,
# or three intervals (Ai1,Ai2,Ai3) at the points x_mean±s√3/3,
# if N(t) ≥ 45J_t.
# If Xi(i=1,2) is categorical, let A_ik denote the singleton set
# containing its kth value.

# J_t = the number of classes in node t
# N(t) = denote the corresponding sample sizes in t


N_t <- nrow(mammograph_data) #TODO CHANGE to node specific
J_t <- nlevels(mammograph_data$ME) #TODO CHANGE to node specific
x1 <- mammograph_data$PB #TODO int
x2 <- mammograph_data$HIST #TODO bool/logical

if (is.numeric(x1)) {
#If Xi(i=1,2) is noncategorical, split its range into two intervals
# (Ai1,Ai2) at the sample mean x_mean if N(t) < 45J_t,
  if (N_t < 45 * J_t) {
    tmp_interval <- x1
    tmp_mean <- mean(tmp_interval)
    first_boundary <- min(tmp_interval)
    last_boundary <- max(tmp_interval)
    # [min] ----  [mean]  ----- [max]
    #interval_1 <- first_boundary:tmp_mean
    #interval_2 <- tmp_mean:last_boundary
    tmp_vec <- c()
    for (i in 1:length(x1)) {
      curr_item <- x1[i]
      if ((curr_item >= first_boundary)
          && (curr_item < tmp_mean)) {
        tmp_vec[i] <- 1
      } else if ((curr_item >= tmp_mean)
                 && (curr_item <= last_boundary)) {
        tmp_vec[i] <- 2
      }
    }
  } else if (
    N_t >= 45 * J_t
  ) {
    # or three intervals (Ai1,Ai2,Ai3) at the points x_mean±s√3/3,
    # if N(t) ≥ 45J_t.
    tmp_interval <- x1
    tmp_mean <- mean(tmp_interval)
    tmp_sd <- sd(tmp_interval)
    first_boundary <- min(tmp_interval)
    min_boundary <- tmp_mean - tmp_sd * sqrt(3) / 3
    max_boundary <- tmp_mean + tmp_sd * sqrt(3) / 3
    last_boundary <- max(tmp_interval)
    # [min] ----  [min_boundary] ------ [max_boundary]  ----- [max]
    if ((curr_item >= first_boundary)
      && (curr_item < min_boundary)
    ) {
      tmp_vec[i] <- 1
    } else if ((curr_item >= min_boundary)
      && (curr_item < max_boundary)
    ) {
      tmp_vec[i] <- 2
    } else if ((curr_item >= max_boundary)
      && (curr_item <= last_boundary)
    ) {
      tmp_vec[i] <- 3
    }
  }
}
x1_cleaned <- tmp_vec


# If Xi(i=1,2) is categorical, let A_ik denote the singleton set
# containing its kth value.

# leave x2 as is.
if (is.factor(x2) || is.logical(x2)) {
  x2_cleaned <- x2
}



# 2.  Divide the (X1,X2)-space into sets B_k,m = {(x1,x2):x1∈A1k,x2∈A2m},
# fork, m=1,2,....
B_km <- paste0(as.character(x1_cleaned), as.character(x2_cleaned))

# 3.  Form a contingency table with the class labels as
# rows and{Bk,m}as columns. Compute its chi-squared statistic and
# use (2.1) to transform it to a 1-d.f. chi-squared valueWI(X1,X2).
contingency_table <-
  table(mammograph_data$ME, B_km)

wilson_hilferty_approx <- function(chi_value, v) {
  inner <- (chi_value / v) ^ (1 / 3) - 1 + (2 / (9 * v))
  return(max(0, (7 / 9 + sqrt(v) * inner) ^ 3))
}

v <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)

chi_test <- chisq.test(mammograph_data$ME, as.factor(B_km))
chi_value <- chi_test$statistic
if (v > 1) {
  chi_value <- wilson_hilferty_approx(chi_value, v)
  chi_value <- wilson_hilferty_approx(chi_value, v)
}

pvalue <- pchisq(chi_value, df = 1, lower.tail = FALSE)

# TODO:
#WARNING: CHECK as.character for factor vectors make sure it work as intended
# currently only does logical and numerical
