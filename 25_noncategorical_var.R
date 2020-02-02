#2.  If X is a noncategorical variable:
# (a)  Compute mean of x and s of x, the mean and standard deviation,
# respectively, of the values of X in t.

x_mean <- mean(mammograph_data$PB)
x_sd <- sd(mammograph_data$PB)

#(b)  If N(t) ≥ 20J_t, divide the range of X into four intervals
# with boundary values x_mean and x_mean±s√3/2. Otherwise,
# if N(t) < 20J_t, divide the range of X into three intervals with
# boundary values x_mean±s√3/3. If X has a uniform distribution,
# these boundaries will yield intervals with roughly equal numbers of
# observations.
# J_t = the number of classes in node t
# N(t) = denote the corresponding sample sizes in t

N_t <- nrow(mammograph_data) #TODO CHANGE to node specific
J_t <- nlevels(mammograph_data$ME) #TODO CHANGE to node specific

if (N_t >= 20 * J_t) {
  first_boundary <- min(mammograph_data$PB)
  min_boundary <- x_mean - x_sd * sqrt(3) / 2
  max_boundary <- x_mean + x_sd * sqrt(3) / 2
  last_boundary <- max(mammograph_data$PB)
  # [min] ---- [mean - sd] ----- [mean] ---- [mean + sd] ----- [max]

  #interval_1 <- first_boundary:min_boundary
  #interval_2 <- min_boundary:x_mean
  #interval_3 <- x_mean:max_boundary
  #interval_4 <- max_boundary:last_boundary

  tmp_vec <- c()
  for (i in 1:length(mammograph_data$PB)) {
    curr_item <- mammograph_data$PB[i]
    if ((curr_item >= first_boundary)
        && (curr_item < min_boundary)) {
      tmp_vec[i] <- 1
    } else if ((curr_item >= min_boundary)
               && (curr_item < x_mean)) {
      tmp_vec[i] <- 2
    } else if ((curr_item >= x_mean)
               && (curr_item < max_boundary)) {
      tmp_vec[i] <- 3
    } else if ((curr_item >= max_boundary)
               && (curr_item <= last_boundary)) {
      tmp_vec[i] <- 4
    }
  }

} else if (N_t < 20 * J_t) {
  first_boundary <- min(mammograph_data$PB)
  min_boundary <- x_mean - x_sd * sqrt(3) / 3
  max_boundary <- x_mean + x_sd * sqrt(3) / 3
  last_boundary <- max(mammograph_data$PB)
  # [min] ---- [mean - sd] --------- [mean + sd] ----- [max]
  #interval_1 <- first_boundary:min_boundary
  #interval_2 <- min_boundary:max_boundary
  #interval_3 <- max_boundary:last_boundary
  tmp_vec <- c()
  for (i in 1:length(mammograph_data$PB)) {
    curr_item <- mammograph_data$PB[i]
    if ((curr_item >= first_boundary)
        && (curr_item < min_boundary)) {
      tmp_vec[i] <- 1
    } else if ((curr_item >= min_boundary)
               && (curr_item < max_boundary)) {
      tmp_vec[i] <- 2
    } else if ((curr_item >= max_boundary)
               && (curr_item < last_boundary)) {
      tmp_vec[i] <- 3
    }
  }
}
mammograph_data$PB_interval <- tmp_vec
mammograph_data$PB_interval <- factor(mammograph_data$PB_interval, ordered = TRUE)

# (c)  Form a contingency table with the class labels as rows and the intervals
# as columns.

contingency_table <-
  table(mammograph_data$ME, mammograph_data$PB_interval)

# (d)  Follow step 1(b) to obtain WM(X).

wilson_hilferty_approx <- function(chi_value, v) {
  inner <- (chi_value / v) ^ (1 / 3) - 1 + (2 / (9 * v))
  return(max(0, (7 / 9 + sqrt(v) * inner) ^ 3))
}

v <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)

chi_test <- chisq.test(mammograph_data$ME, mammograph_data$PB_interval)
chi_value <- chi_test$statistic
if (v > 1) {
  chi_value <- wilson_hilferty_approx(chi_value, v)
  chi_value <- wilson_hilferty_approx(chi_value, v)
}

pvalue <- pchisq(chi_value, df = 1, lower.tail = FALSE)

