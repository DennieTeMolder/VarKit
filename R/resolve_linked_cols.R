# NOTE Leiden provides faster clustering, good quality clustering, but the solution is not stable
##' @export
cluster_fast <- function(graph) {
  igraph::groups(igraph::cluster_leiden(graph, objective_function = "modularity", n_iterations = 1))
}

##' @export
cluster_stable <- function(graph) {
  igraph::groups(igraph::cluster_fast_greedy(graph, merges = FALSE, modularity = FALSE))
}

##' @export
resolve_links <- function(links,
                          strategy = c("safe", "greedy"),
                          cluster_fn = cluster_fast,
                          cores = 1
                          ) {
  strategy <- strategy[1]

  if (is.data.frame(links) || !is.list(links)) {
    results <- .resolve_links_internal(links, strategy = strategy, cluster_fn = cluster_fn)
    return(varkit_linkage_graph(results, origin = get_origin(links)))
  }

  cores <- min(floor(cores), length(links))

  # If links is a list we can solve each element individually (and in parallel)
  if (cores < 2) {
    res <- purrr::map(
      links, .resolve_links_internal,
      strategy = strategy, cluster_fn = cluster_fn,
      .progress = TRUE
    )
  } else {
    # Optimal ordering for core utilisation
    size_order <- order(sapply(links, nrow), decreasing = TRUE)
    size_order <- .split_load_balance(size_order, cores = cores)
    chunk_size <- length(size_order[[1]])
    size_order <- unlist(size_order)

    cl <- parallel::makeCluster(cores, type = "PSOCK")
    on.exit(tryCatch(parallel::stopCluster(cl), error = function(e) NULL))

    res <- parallel::parLapplyLB(cl,
      links[size_order], .resolve_links_internal,
      strategy = strategy, cluster_fn = cluster_fn,
      chunk.size = chunk_size
    )
    parallel::stopCluster(cl)
  }

  res <- do.call(igraph::disjoint_union, res)
  varkit_linkage_graph(res, origin = get_origin(links))
}

.resolve_links_internal <- function(links, strategy, cluster_fn) {
  stopifnot(is.data.frame(links) || is.matrix(links))
  stopifnot(!is.null(colnames(links)), colnames(links)[1:2] == c("row", "col"))

  # Simple cases
  if (nrow(links) < 1) {
    return(NULL)
  } else if (nrow(links) == 1) {
    return(igraph::graph_from_data_frame(links))
  }

  # Assume third column to contain weights (higher == stronger connection)
  if (ncol(links) > 2) {
    colnames(links)[3] <- "weight"
  }

  # Handle more complex cases with a network approach
  if (strategy == "greedy")
    return(.resolve_links_greedy(links, cluster_fn = cluster_fn))
  .resolve_links_safe(links, cluster_fn = cluster_fn)
}

.resolve_links_greedy <- function(links, cluster_fn) {
  graph <- igraph::graph_from_data_frame(links, directed = FALSE)
  stopifnot(igraph::is_named(graph))

  clusters <- cluster_fn(graph)
  names(clusters) <- NULL

  compute_degree <- if (igraph::is_weighted(graph)) igraph::strength else igraph::degree

  res <- lapply(clusters, function(cl) {
    len <- length(cl)
    if (len == 1)
      return(NULL)
    if (len == 2)
      return(data.frame(from = cl[1], to = cl[2]))
    deg <- compute_degree(igraph::induced_subgraph(graph, cl))
    cl_keep <- names(which.max(deg))
    data.frame(from = cl_keep, to = setdiff(cl, cl_keep))
  })
  res <- do.call(rbind, res)

  igraph::graph_from_data_frame(res)
}

.resolve_links_safe <- function(links, cluster_fn) {
  graph <- igraph::graph_from_data_frame(links, directed = FALSE)
  stopifnot(igraph::is_named(graph))

  # Loop over each clusters, take node with the highest degree (whilst being sure
  # to protect nodes that represent others) and drop its neighbours to ensure we
  # are not creating links that fall below the correlation threshold
  protected <- NULL
  .resolve_links_safe_internal <- function(graph) {
    clusters <- cluster_fn(graph)
    names(clusters) <- NULL

    compute_degree <- if (igraph::is_weighted(graph)) igraph::strength else igraph::degree

    # Iterate through each unique cluster membership
    res <- lapply(clusters, function(cl) {
      len <- length(cl)
      if (len == 1)
        return(NULL)

      cl_protected <- if (is.null(protected)) NULL else cl %in% protected
      if (!is.null(cl_protected) && all(cl_protected))
        return(NULL)

      if (len == 2) {
        if (!is.null(cl_protected) && any(cl_protected)) {
          return(setNames(list(cl[!cl_protected]), cl[cl_protected]))
        } else {
          return(setNames(list(cl[2]), cl[1]))
        }
      }

      sg <- igraph::induced_subgraph(graph, cl)

      if (is.null(cl_protected) || !any(cl_protected)) {
        deg <- compute_degree(sg)
        cl_keep <- names(which.max(deg))
      } else if (sum(cl_protected) == 1) {
        cl_keep <- cl[cl_protected]
      } else {
        deg <- compute_degree(sg, cl[cl_protected])
        cl_keep <- names(which.max(deg))
      }

      setNames(list(igraph::neighbors(sg, cl_keep)$name), cl_keep)
    })
    unlist(res, recursive = FALSE)
  }

  # Create a directed graph to store the result
  results <- igraph::make_empty_graph(igraph::vcount(graph), directed = TRUE)
  igraph::V(results)$name <- igraph::V(graph)$name

  while (TRUE) {
    new <- .resolve_links_safe_internal(graph)

    new_edges <- mapply(names(new), new, FUN = function(from, to) {
      as.vector(rbind(from, to))
    })
    results <- igraph::add_edges(results, edges = unlist(new_edges, use.names = FALSE))
    protected <- c(protected, names(new))

    graph <- igraph::delete_vertices(graph, unlist(new, use.names = FALSE))
    v_is_unconnected <- setNames(igraph::degree(graph), NULL) == 0
    if (any(v_is_unconnected))
      graph <- igraph::delete_vertices(graph, which(v_is_unconnected))

    if (igraph::ecount(graph) == 0 || all(igraph::V(graph)$name %in% protected))
      break()
  }

  results
}

# REVIEW this currently not used due to high computational cost
.propagate_strong_links <- function(graph, cutoff = 0.999) {
  stopifnot(igraph::is_named(graph))
  stopifnot(igraph::is_weighted(graph))
  w_range <- range(igraph::edge_attr(graph, "weight"))
  if (w_range[1] < 0 || (!all.equal(1, w_range[2]) && w_range[2] > 1))
    stop("Edge weights are outside the 0 to 1 range, they should represent absolute correlation values!")

  # Prevent floating point comparison errors
  which_above <- which(igraph::edge_attr(graph, "weight") > 1)
  if (length(which_above) > 0)
    igraph::edge_attr(graph, "weight")[which_above] <- 1

  strong_links <- which(igraph::edge_attr(graph, "weight") > cutoff)
  strong_links <- igraph::subgraph.edges(graph, strong_links)

  comps <- igraph::components(strong_links)$membership
  comps <- split(names(comps), comps)

  new_links <- lapply(comps, function(comp) {
    # Get nodes and edges that are connected with components (omit links between neighbours)
    # NOTE this 10x faster then calling incident_edges() on the entire graph
    sg <- igraph::neighborhood(graph, order = 1, nodes = comp)
    sg <- igraph::induced_subgraph(graph, unlist(lapply(sg, as.integer)))
    sg <- igraph::subgraph.edges(sg, do.call(c, igraph::incident_edges(sg, comp)))

    # Link component nodes to their neighbourhood (but do not link neighbours)
    g <- igraph::simplify(igraph::graph_from_data_frame(
      expand.grid(row = comp, col = igraph::as_ids(igraph::V(sg)), stringsAsFactors = FALSE),
      directed = FALSE
    ))

    # Only return edges/links that were not already present in the graph
    g <- igraph::difference(g, graph)

    if (igraph::ecount(g) < 1)
      return(NULL)
    g <- igraph::as_data_frame(g)

    # Convert correlation to a distance metric
    igraph::edge_attr(sg, "weight") <- 1 - igraph::edge_attr(sg, "weight")

    # Compute weights for new edges based on length of shortest path
    new_weights <- lapply(split(g, ~from), function(current) {
      1 - igraph::distances(sg, v = current$from[1], to = current$to, algorithm = "dijkstra")[1, ]
    })
    g$weight <- unlist(new_weights, use.names = FALSE)

    g
  })
  new_links <- do.call(rbind, new_links)

  if (is.null(new_links))
    return(graph)

  # The 1 - distance could theoretically generate negative weights, exclude them
  keep <- new_links$weight > 0
  if (!all(keep))
    new_links <- new_links[keep, ]

  igraph::add_edges(graph,
    edges = as.vector(t(new_links[1:2])),
    attr = list(weight = new_links$weight)
  )
}
