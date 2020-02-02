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


N_t <- nrow(mammograph_data) #TODO CHANGE to node specific
J_t <- nlevels(mammograph_data$ME) #TODO CHANGE to node specific
x1 <- mammograph_data$PB #TODO
x2 <- mammograph_data$PB #TODO
if (is.numeric(x1) && is.numeric(x2)) {
#If Xi(i=1,2) is noncategorical, split its range into two intervals
# (Ai1,Ai2) at the sample mean x_mean if N(t) < 45J_t,
  if (N_t < 45 * J_t) {
    tmp_interval <- c(x1, x2)
    tmp_mean <- mean(tmp_interval)
    first_boundary <- min(tmp_interval)
    last_boundary <- max(tmp_interval)
    # [min] ----  [mean]  ----- [max]
    #interval_1 <- first_boundary:tmp_mean
    #interval_2 <- tmp_mean:last_boundary
    tmp_vec <- c()
    for (i in 1:length(mammograph_data$PB)) {
      curr_item <- mammograph_data$PB[i]
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
    tmp_interval <- c(x1, x2)
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

x1 <- mammograph_data$SYMPT #TODO
x2 <- mammograph_data$HIST #TODO

# If Xi(i=1,2) is categorical, let A_ik denote the singleton set
# containing its kth value.
if ((is.factor(x1) || is.logical(x1))
    && (is.factor(x2) || is.logical(x2))
) {

}




# 2.  Divide the (X1,X2)-space into sets B_k,m = {(x1,x2):x1∈A1k,x2∈A2m},
# fork, m=1,2,....

# 3.  Form a contingency table with the class labels as
# rows and{Bk,m}as columns. Compute its chi-squared statistic and
# use (2.1) to transform it to a 1-d.f. chi-squared valueWI(X1,X2).
