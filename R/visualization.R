visualize_workflow <- function(workflow_info) {
  # Initialize lists for nodes and edges
  nodes_list <- list()
  edges_list <- list()

  node_counter <- 0

  for (script_name in names(workflow_info)) {
    info <- workflow_info[[script_name]]

    # Add script node
    node_counter <- node_counter + 1
    script_id <- node_counter
    nodes_list[[length(nodes_list) + 1]] <- list(
      id = script_id,
      type = "script",
      label = script_name,
      shape = "rectangle",
      values = 1  # This value can be adjusted as needed
    )

    # Add input nodes and edges
    for (input in info$inputs) {
      node_counter <- node_counter + 1
      input_id <- node_counter
      nodes_list[[length(nodes_list) + 1]] <- list(
        id = input_id,
        type = "input",
        label = input,
        shape = "circle",
        values = 1  # This value can be adjusted as needed
      )
      edges_list[[length(edges_list) + 1]] <- list(
        from = input_id,
        to = script_id
      )
    }

    # Add output nodes and edges
    for (output in info$outputs) {
      node_counter <- node_counter + 1
      output_id <- node_counter
      nodes_list[[length(nodes_list) + 1]] <- list(
        id = output_id,
        type = "output",
        label = output,
        shape = "circle",
        values = 1  # This value can be adjusted as needed
      )
      edges_list[[length(edges_list) + 1]] <- list(
        from = script_id,
        to = output_id
      )
    }
  }

  # Convert lists to data frames
  nodes <- do.call(rbind, lapply(nodes_list, as.data.frame))
  edges <- do.call(rbind, lapply(edges_list, as.data.frame))

  # Ensure correct data types for all columns
  nodes$id <- as.integer(nodes$id)
  nodes$type <- as.character(nodes$type)
  nodes$label <- as.character(nodes$label)
  nodes$shape <- as.character(nodes$shape)
  nodes$values <- as.numeric(nodes$values)

  edges$from <- as.integer(edges$from)
  edges$to <- as.integer(edges$to)

  # Create graph object
  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges
  )

  # Add global graph attributes
  graph <- graph %>%
    DiagrammeR::add_global_graph_attrs(
      attr = "rankdir",
      value = "LR",
      attr_type = "graph"
    ) %>%
    DiagrammeR::set_node_attrs(
      node_attr = "fillcolor",
      values = ifelse(nodes$type == "input", "lightblue",
                      ifelse(nodes$type == "output", "lightgreen", "white"))
    ) %>%
    DiagrammeR::set_node_attrs(
      node_attr = "style",
      values = "filled"
    )

  return(graph)
}
