join_nodelist <- function(
    relational_df, nodelist, node_id_col, join_by_vars = c()
) {
  relational_df %>%
    left_join(
      nodelist %>% rename(from = {{node_id_col}}),
      by = c(join_by_vars, "from")
    ) %>%
    left_join(
      nodelist %>% rename(to = {{node_id_col}}),
      by = c(join_by_vars, "to"),
      suffix = c(".from", ".to")
    ) %>%
    rename_with(.fn = ~str_c("from_", .x), .cols = ends_with(".from")) %>%
    rename_with(.fn = ~str_c("to_", .x), .cols = ends_with(".to")) %>%
    rename_with(.fn = ~str_remove(.x, "\\.from|\\.to"))
}

graph_to_adjlist <- function(input_tidygraph) {
  
  # Check whether the graph is directed or undirected
  directed_graph <- with_graph(input_tidygraph, graph_is_directed())
  
  # Get the edgelist
  edgelist <- input_tidygraph %>%
    activate("edges") %>%
    as_tibble() %>%
    select(from, to) %>%
    mutate(edge = 1)
  
  # Make symmetric if network is undirected
  if (!directed_graph) {
    edgelist <- bind_rows(
      # Original
      edgelist,
      # Flipped
      edgelist %>% select(from = to, to = from, edge)
    )
  }
  
  # Create a dataframe with all possible pairwise relations
  output <- expand_grid(from = 1:vcount(input_tidygraph), to = from) %>%
    # Add info about what edges exist in the network
    left_join(edgelist, by = c("from", "to")) %>%
    # Mark "missing" edges as non-existent edges
    mutate(edge = replace_na(edge, 0))
  
  return (output)
}
