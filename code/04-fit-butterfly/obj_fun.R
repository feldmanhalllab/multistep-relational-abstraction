this_obj_fun <- function(
  param_values, param_names,
  learning_data, choice_data,
  mem_rep, fit_data
) {
  
  #### Conditional pipe evaluation ####
  # Source: https://community.rstudio.com/t/conditional-pipelines/6076/2
  pipe_if <- function(data, condition, call) {
    if (rlang::eval_tidy(enquo(condition), data)) {
      rlang::eval_tidy(rlang::quo_squash(quo(data %>% !!enquo(call))))
    } else data
  }
  
  #### Unpack parameters ####
  
  # Assign variable names/values automatically
  for (j in 1:length(param_names)) {
    assign(param_names[j], param_values[j])
  }
  
  # Bound SR gamma to [0, 1)
  if ("sr_gamma" %in% param_names) {
    sr_gamma <- logistic_general(
      x = sr_gamma,
      lower_bound = 0,
      # SR becomes undefined when gamma = 1
      upper_bound = 0.99
    )
  }
  
  #### Build SR ####
  
  if ("sr_gamma" %in% param_names) {
    sr_rep <- build_rep_sr(
      learning_data = learning_data,
      # Assume learning rate instead of estimating
      this_alpha = 0.1,
      this_gamma = sr_gamma
    ) %>%
      # Make into edgelist
      filter(from != to) %>%
      mutate(
        from_sorted = if_else(from < to, from, to),
        to_sorted = if_else(from < to, to, from),
        from = from_sorted,
        to = to_sorted
      ) %>%
      select(-c(from_sorted, to_sorted)) %>%
      # Find averaged values across the diagonal
      group_by(from, to) %>%
      summarise(
        across(.cols = contains("_value"), .fns = mean),
        .groups = "drop"
      )
  }
  
  #### Calculate likelihood ####
  
  scaling_constant <- 10
  regularization_sd <- 5
  
  eval_fit <- choice_data %>%
    # Make sure choice data is non-directed
    mutate(
      from_sorted = if_else(from < to, from, to),
      to_sorted = if_else(from < to, to, from),
      from = from_sorted,
      to = to_sorted
    ) %>%
    select(-c(from_sorted, to_sorted)) %>%
    pipe_if(
      "w_mem" %in% param_names,
      left_join(mem_rep, by = c("from", "to"))
    ) %>%
    pipe_if(
      "sr_gamma" %in% param_names,
      left_join(sr_rep, by = c("from", "to"))
    ) %>%
    #### Construct linear combination `x` for logistic regression
    mutate(x = w_naught * scaling_constant) %>%
    pipe_if(
      "w_mem" %in% param_names,
      mutate(x = x + (w_mem * memorize_value * scaling_constant))
    ) %>%
    pipe_if(
      "w_sr" %in% param_names,
      mutate(x = x + (w_sr * sr_value * scaling_constant))
    ) %>%
    # Compute probability of saying "yes they're friends" given representation
    mutate(p_yes = logistic_standard(x)) %>%
    # Trust prediction task had responses ranging from $0-10
    mutate(predicted_amount_trusted = p_yes * 10) %>%
    # Compute likelihood for memory data
    pipe_if(
      fit_data == "memory",
      mutate(
        likelihood = if_else(choice == 1, p_yes, 1 - p_yes),
        neg_ll = neg_loglik_logistic(likelihood)
      )
    ) %>%
    # Compute likelihood for trust data
    pipe_if(
      fit_data == "trust",
      mutate(
        error = choice - predicted_amount_trusted,
        squared_error = error^2
      )
    )
  
  # What are we trying to minimize?
  if (fit_data == "memory") {
    # Maximize likelihood by minimizing negative log likelihood
    thing_to_be_minimized <- sum(eval_fit$neg_ll)
  } else if (fit_data == "trust") {
    # Minimize sum of squared error
    thing_to_be_minimized <- sum(eval_fit$squared_error)
  }
  
  # Regularization: L2 norm, Gaussian distribution
  lambda_weight <- 1/(regularization_sd^2)
  regularization_value <- 0
  for (j in 1:length(param_names)) {
    # Regularize weights only, not SR gamma
    if (str_detect(param_names[j], "w_")) {
      # Sum of squared weights; pulls values closer to mean of 0
      regularization_value <- regularization_value + (param_values[j]^2)
    }
  }
  
  return( thing_to_be_minimized + (lambda_weight * regularization_value) )
}
