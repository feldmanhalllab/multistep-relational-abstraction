#### Choice rules & logistic functions ####

softmax <- function(option_values, option_chosen, temperature = NULL) {
  
  if (is.null(temperature) | !is.numeric(temperature)) {
    temperature <- 1
  }
  
  # Standard Gibbs / Boltzmann distribution
  numerator <- exp(option_values[option_chosen] / temperature)
  denominator <- sum(exp(option_values / temperature))
  
  return (numerator / denominator)
}

logistic_standard <- function(x) {
  return ( 1 / (1 + exp(-x)) )
}

logistic_general <- function(x, lower_bound=0, upper_bound=1) {
  lower_bound + (upper_bound - lower_bound) / (1 + exp(-x))
}


#### Likelihood ####

neg_loglik_logistic <- function(likelihood) {
  if (any(is.nan(likelihood))) {
    warning("Some likelihoods originally NaN, returning NA")
    likelihood <- replace(likelihood, is.nan(likelihood), NA_real_)
  }
  
  if (any(is.infinite(log(likelihood)))) {
    warning("Some likelihoods are too close to 0, returning NA")
    likelihood <- replace(likelihood, is.infinite(log(likelihood)), NA_real_)
  }
  
  if (all(is.na(likelihood))) {
    warning("All likelihoods NA. Likely, the softmax temp is near-zero.")
  } else if (any(likelihood <= 0 | likelihood > 1, na.rm = TRUE)) {
    stop("Some likelihoods out of range (0, 1]. Check for bugs.")
  }
  
  return( -1 * log(likelihood) )
}

calculate_bic <- function(n_params, n_datapoints, neg_loglik) {
  return ( (n_params * log(n_datapoints)) - (2 * -neg_loglik) )
}


#### Tidy optimization ####

run_optim <- function(max_iter_per_run, objective_function,
                      param_guesses, ...) {
  ### Note: I write my objective functions so that `param_names` is a required
  #   named argument. This presents a bit of a headache because I also want to
  #   use the parameter names as an argument for `optim_to_tibble`. So the
  #   unholy compromise is for the user to treat param_names as a named argument
  #   for `run_optim` when in fact it is not formally.
  
  optim_to_tibble <- function(optim_output, param_names) {
    tibble(
      param_name = param_names,
      param_value = optim_output$par
    ) %>%
      mutate(
        optim_value = optim_output$value,
        convergence = case_when(
          optim_output$convergence == 0 ~ "converged",
          optim_output$convergence == 1 ~ "maxit reached",
          optim_output$convergence == 10 ~ "simplex degeneracy",
          TRUE ~ "unknown problem"
        )
      )
  }
  
  param_names_copy <- list(...)$param_names
  
  return (
    optim(
      par = param_guesses,
      fn = objective_function,
      # Usually, ... contains the data argument(s) for the objective function
      ... = ...,
      control = list(maxit = max_iter_per_run)
    ) %>%
      optim_to_tibble(param_names_copy)
  )
}

best_optim_run <- function(optim_output, return_as = c("dataframe", "vector")) {
  
  if (length(return_as) > 1) {
    stop("Define `return_as`.")
  } else if (!return_as %in% c("dataframe", "vector")) {
    stop("The argument `return_as` must either be `dataframe` or `vector`")
  }
  
  best_params <- optim_output %>%
    filter(convergence == "converged") %>%
    filter(optim_value == min(optim_value)) %>%
    filter(optimizer_run == min(optimizer_run))
  
  if (return_as == "vector") {
    return( best_params %>% select(parameter, value) %>% deframe() )
  } else {
    return( best_params )
  }
}
