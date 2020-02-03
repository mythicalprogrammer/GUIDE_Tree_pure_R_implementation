main_effect_chi_stat_aux <- function(response, predictor) {
  # If X is a categorical variable
  # a) Form a contingency table with the class labels as rows and
  # the categories of X as columns
  contingency_table <- table(response, predictor)

  # b) Let ν be the degrees of freedom of the table after deleting
  # any rows and columns with no observations.
  # Compute the chi-squared statistic χ2_ν for testing independence.
  # If ν >1, use the Wilson–Hilferty approximation twice to
  # convert χ2_ν to the 1-d.f. chi-squared

  v <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)

  chi_test <- chisq.test(response, predictor)
  chi_value <- chi_test$statistic

  if (v > 1) {
    chi_value <- wilson_hilferty_approx(chi_value, v)
    chi_value <- wilson_hilferty_approx(chi_value, v)
  }

  #pvalue <- pchisq(chi_value, df = 1, lower.tail = FALSE)
  return(chi_value)
}

main_effect_chi_stat_noncat_aux <- function(response, predictor, node_data) {
  x_mean <- mean(predictor)
  x_sd <- sd(predictor)
  N_t <- nrow(node_data)
  J_t <- nlevels(response) #TODO CHANGE to node specific

  if (N_t >= 20 * J_t) {
    first_boundary <- min(predictor)
    min_boundary <- x_mean - x_sd * sqrt(3) / 2
    max_boundary <- x_mean + x_sd * sqrt(3) / 2
    last_boundary <- max(predictor)
    # [min] ---- [mean - sd] ----- [mean] ---- [mean + sd] ----- [max]

    #interval_1 <- first_boundary:min_boundary
    #interval_2 <- min_boundary:x_mean
    #interval_3 <- x_mean:max_boundary
    #interval_4 <- max_boundary:last_boundary

    tmp_vec <- c()
    for (i in 1:length(predictor)) {
      curr_item <- predictor[i]
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
    first_boundary <- min(predictor)
    min_boundary <- x_mean - x_sd * sqrt(3) / 3
    max_boundary <- x_mean + x_sd * sqrt(3) / 3
    last_boundary <- max(predictor)
    # [min] ---- [mean - sd] --------- [mean + sd] ----- [max]
    #interval_1 <- first_boundary:min_boundary
    #interval_2 <- min_boundary:max_boundary
    #interval_3 <- max_boundary:last_boundary
    tmp_vec <- c()
    for (i in 1:length(predictor)) {
      curr_item <- predictor[i]
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
  tmp_vec <- factor(tmp_vec, ordered = TRUE)

  # (c)  Form a contingency table with the class labels as rows and the intervals
  # as columns.
 return(main_effect_chi_stat_aux(response, tmp_vec))
}

# PROCEDURE 2.1.Main effect chi-squared statistic for X at node t: pg 6
main_effect_chi_stat <- function(response, predictor, node_data) {

  if (is.factor(predictor) || is.logical(predictor)) {
    main_effect_chi_stat_aux(response, predictor)
  } else {
    main_effect_chi_stat_noncat_aux(response, predictor, node_data)
  }
}



