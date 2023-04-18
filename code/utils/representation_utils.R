#### SR utils ####

build_successor <- function(
    successor_matrix, alpha, gamma,
    observations, sr_value_col_name,
    bidirectional = TRUE) {
  
  sr_update <- function (
    input_matrix, alpha, gamma,
    previous_state, current_state, 
    bidirectional = TRUE
  ) {
    forward_onehot <- input_matrix[previous_state, ] * 0
    forward_onehot[current_state] <- 1
    forward_delta <- (
      forward_onehot +
        (gamma * input_matrix[current_state, ]) -
        input_matrix[previous_state, ]
    )
    
    output <- input_matrix
    output[previous_state, ] <- (
      input_matrix[previous_state, ] + (alpha * forward_delta)
    )
    
    if (bidirectional) {
      backward_onehot <- input_matrix[current_state, ] * 0
      backward_onehot[previous_state] <- 1
      backward_delta <- (
        backward_onehot +
          (gamma * input_matrix[previous_state, ]) -
          input_matrix[current_state, ]
      )
      output[current_state, ] <- (
        input_matrix[current_state, ] + (alpha * backward_delta)
      )
    }
    return(output)
  }
  
  obs_matrix <- as.matrix(select(observations, from, to))
  for (j in 1:nrow(observations)) {
    previous_state <- obs_matrix[j, 1]
    current_state <- obs_matrix[j, 2]
    successor_matrix <- sr_update(
      successor_matrix,
      alpha,
      gamma,
      previous_state,
      current_state,
      bidirectional = TRUE
    )
  }
  
  return(
    successor_matrix %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(
        cols = -from,
        names_to = "to",
        values_to = sr_value_col_name
      ) %>%
      mutate(
        to = str_remove(to, "V"),
        to = as.numeric(to)
      ) %>%
      mutate(
        alpha = alpha,
        gamma = gamma
      )
  )
}

# Use the analytical method to compute the asymptotic SR
build_successor_analytically <- function(
  transition_matrix, lookahead_steps=NA, successor_horizon=NA, normalize=TRUE
) {
  if (!is.matrix(transition_matrix)) {
    stop("Transition matrix must be provided.")
  }
  
  if (length(dim(transition_matrix)) != 2) {
    stop("Two-dimensional transition matrix must be provided.")
  }
  
  if (dim(transition_matrix)[1] != dim(transition_matrix)[2]) {
    stop("Transition matrix must be square.")
  }
  
  if (
    (is.na(lookahead_steps) & is.na(successor_horizon)) |
    (!is.na(lookahead_steps) & !is.na(successor_horizon))
  ) {
    stop(
      "Specify EITHER the number of lookahead steps, OR the successor horizon."
    )
  }
  
  if (
    (!is.na(successor_horizon)) &
    ((successor_horizon < 0) | (successor_horizon > 1))
  ) {
    stop(
      "The successor horizon MUST fall in the range [0, 1]."
    )
  }
  
  if (!is.na(lookahead_steps)) {
    successor_horizon <- 1 - (1/lookahead_steps)
  }
  
  if (!is.na(successor_horizon)) {
    lookahead_steps <- 1 / (1-successor_horizon)
  }
  
  if (lookahead_steps == 1) {
    sr_matrix <- transition_matrix
  } else {
    identity_matrix <- diag(dim(transition_matrix)[1])
    sr_matrix <- solve(identity_matrix - successor_horizon*transition_matrix)
  }
  
  if (normalize) {
    sr_matrix <- sr_matrix / lookahead_steps
  }
  
  return(
    sr_matrix %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(
        cols = -from,
        names_to = "to"
      ) %>%
      mutate(
        to = str_remove(to, "V"),
        to = as.numeric(to)
      ) %>%
      mutate(gamma = successor_horizon)
  )
}


#### Triad utils ####

find_triads <- function(this_adjlist) {
  n_nodes <- with(this_adjlist, max(from, to))
  
  expand_grid(
    a = 1:n_nodes,
    b = 1:n_nodes,
    c = 1:n_nodes
  ) %>%
    # Are A and B friends?
    left_join(
      this_adjlist %>% rename(a = from, b = to, a2b = edge),
      by = c("a", "b")
    ) %>%
    # Are B and C friends?
    left_join(
      this_adjlist %>% rename(b = from, c = to, b2c = edge),
      by = c("b", "c")
    ) %>%
    # Are A and C friends?
    left_join(
      this_adjlist %>% rename(a = from, c = to, a2c = edge),
      by = c("a", "c")
    ) %>%
    # Is there an open triad?
    filter(a2b == 1) %>%
    filter(b2c == 1) %>%
    filter(a2c == 0) %>%
    filter(a != b) %>%
    filter(a != c) %>%
    filter(b != c) %>%
    select(from = a, to = c) %>%
    distinct() %>%
    mutate(triad = 1) %>%
    # Now create an adjlist
    right_join(
      expand_grid(from = 1:n_nodes, to = 1:n_nodes),
      by = c("from", "to")
    ) %>%
    mutate(triad = if_else(is.na(triad), 0, triad)) %>%
    arrange(from, to)
}


#### Build representations ####

build_rep_sr <- function(
    learning_data, this_alpha, this_gamma, bidirectional = TRUE
) {
  
  these_cols <- colnames(learning_data)
  
  if (!any(these_cols == "from") | !any(these_cols == "to")) {
    stop("Your dataframe does not contain the columns `from` and `to`.")
  } else if (
    !is.numeric(learning_data$from) |
    !is.numeric(learning_data$to)
  ) {
    stop("One or both of the columns `from` and `to` contain non-numbers.")
  }
  
  n_nodes <- with(learning_data, max(from, to))
  
  sr_rep <- matrix(0, n_nodes, n_nodes) %>%
    build_successor(
      alpha = this_alpha,
      gamma = this_gamma,
      observations = learning_data,
      sr_value_col_name = "sr_value",
      bidirectional = bidirectional
    ) %>%
    # Normalize expected-counts into transition matrix
    mutate(sr_value = sr_value * (1 - this_gamma)) %>%
    select(from, to, sr_value) %>%
    # Make sure datatypes are consistent
    mutate(across(c(from, to), as.integer))
  
  return ( sr_rep )
}


build_rep_triad <- function(this_adjlist, edge_col_name = "edge") {
  
  these_cols <- colnames(this_adjlist)
  
  if (!any(these_cols == "from") | !any(these_cols == "to")) {
    stop("Your dataframe does not contain the columns `from` and `to`.")
  } else if (
    !is.numeric(this_adjlist$from) |
    !is.numeric(this_adjlist$to)
  ) {
    stop("One or both of the columns `from` and `to` contain non-numbers.")
  } else if (!any(these_cols == edge_col_name)) {
    stop(str_c("Edge column not found. ",
               "It is either missing from your dataframe, or else you've ",
               "incorrectly specified the argument for `edge_col_name`."))
  }
  
  triad_rep <- this_adjlist %>%
    find_triads() %>%
    rename(triad_value = triad) %>%
    # Add real edges
    left_join(this_adjlist, by = c("from", "to")) %>%
    mutate(triad_value = as.numeric(triad_value == 1 | edge == 1)) %>%
    select(-edge) %>%
    # Turn adjacency into expected transition
    group_by(from) %>%
    mutate(
      transition_scaling = sum(triad_value > 0, na.rm = TRUE),
      triad_value = triad_value / transition_scaling
    ) %>%
    ungroup()
  
  return ( triad_rep )
}


build_rep_community <- function(this_adjlist, edge_col_name = "edge") {
  
  these_cols <- colnames(this_adjlist)
  
  if (!any(these_cols == "from") | !any(these_cols == "to")) {
    stop("Your dataframe does not contain the columns `from` and `to`.")
  } else if (
    !is.numeric(this_adjlist$from) |
    !is.numeric(this_adjlist$to)
  ) {
    stop("One or both of the columns `from` and `to` contain non-numbers.")
  } else if (!any(these_cols == edge_col_name)) {
    stop(
      str_c(
        "Edge column not found. ",
        "It is either missing from your dataframe, or else you've ",
        "incorrectly specified the argument for `edge_col_name`."
      )
    )
  }
  
  n_nodes <- with(this_adjlist, max(from, to))
  
  this_edgelist <- this_adjlist %>%
    filter(edge == 1) %>%
    mutate(
      from_sorted = if_else(from < to, from, to),
      to_sorted = if_else(from < to, to, from)
    ) %>%
    select(from = from_sorted, to = to_sorted) %>%
    distinct()
  
  if (
    this_edgelist %>%
    count(from, to) %>%
    mutate(test = n > 1) %>%
    select(test) %>%
    deframe() %>%
    any()
  ) {
    stop("This function is meant to be used with undirected networks only.")
  }
  
  nodelist_community <- tbl_graph(
    nodes = tibble(name = 1:n_nodes),
    edges = this_edgelist,
    directed = FALSE
  ) %>%
    mutate(community = group_edge_betweenness()) %>%
    as_tibble()
  
  comm_rep <- this_adjlist %>%
    # Make sure that the join is done consistently if from/to are not presorted
    mutate(
      from_sorted = if_else(from < to, from, to),
      to_sorted = if_else(from < to, to, from)
    ) %>%
    rename(
      from_original = from,
      to_original = to,
      from = from_sorted,
      to = to_sorted
    ) %>%
    join_nodelist(
      nodelist = nodelist_community,
      node_id_col = "name"
    ) %>%
    # Nodes from the same community?
    mutate(comm_value = as.numeric(from_community == to_community)) %>%
    # Standard output format
    select(
      from = from_original,
      to = to_original,
      comm_value
    ) %>%
    # Turn adjacency into expected transition
    group_by(from) %>%
    mutate(
      transition_scaling = sum(comm_value > 0, na.rm = TRUE),
      comm_value = comm_value / transition_scaling
    ) %>%
    ungroup()
  
  return ( comm_rep )
}
