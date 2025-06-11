##' @export
new_drm <- function(links = NULL, labels = NULL, nodes = NULL) {
  if (is.null(links)) links <- list()
  if (is.null(labels)) labels <- character()
  if (is.null(nodes)) nodes <- list()
  env <- list2env(
    list(links = links, labels = labels, nodes = nodes),
    parent = rlang::empty_env()
  )
  structure(env, class = c("data_relation_model", "environment"))
}

##' @export
is_drm <- function(x) {
  inherits(x, "data_relation_model")
}

##' @export
validate_drm <- function(x) {
  # Requirements for empty models
  stopifnot(is_drm(x))
  stopifnot(c("links", "labels", "nodes") %in% names(x))
  stopifnot(is.list(x$links))
  stopifnot(is.character(x$labels))
  stopifnot(is.list(x$nodes))

  # Requirements for populated models
  if (length(x[["links"]]) > 0L) {
    # drm$links
    stopifnot(rlang::is_named(x$links))
    stopifnot(purrr::map_lgl(x$links, is.character))
    stopifnot(!purrr::map_lgl(x$links, anyNA))

    # drm$nodes
    stopifnot(rlang::is_named(x$nodes))
    missing <- setdiff(
      unique(purrr::list_c(x[["links"]])),
      c(names(x$nodes), "")
    )
    if (length(missing) > 0L)
      stop("The following 'dim_links' have no node: ", .quote_collapse(missing))

    purrr::walk(x$nodes, function(node) {
      # drm$node$length
      stopifnot("length" %in% names(node))
      .stopifnotsingle(node$length, what = "integer")

      # drm$node$select_which
      if (!is.null(node[["select_which"]]))
        stopifnot(is.integer(node$select_which), !anyNA(node$select_which), length(node$select_which) > 0L)

      # drm$node$match_on
      match_on <- node[["match_on"]]
      if (!is.null(match_on)) {
        if (!is.data.frame(match_on))
          stop("Match keys should be a data frame, not: ", .quote_collapse(class(match_on)))
        if (nrow(match_on) != node[["length"]]) {
          expected <- .frmt_big(node[["length"]])
          observed <- .frmt_big(nrow(match_on))
          stop("Expected match keys to have length ", expected, ", but found ", observed)
        }
      }
    })
  }
  invisible(x)
}

##' @export
drm_create <- function(links = NULL, labels = NULL, nodes = NULL) {
  validate_drm(new_drm(links = links, labels = labels, nodes = nodes))
}

##' @export
print.data_relation_model <- function(x, ...) {
  .msg_h1("Data relation model <", rlang::env_label(x), ">")
  if (length(x[["nodes"]]) == 0L) {
    message("Empty model...")
  } else {
    .msg_h2("Links")
    if (length(x[["links"]]) > 0L) {
      print(tibble::tibble(
        label = x[["labels"]][names(x[["links"]])],
        dim_links = sprintf("[%s]", purrr::map_chr(x[["links"]], .collapse)),
        uuid = names(x[["links"]])
      ))
    } else {
      print(tibble::tibble(
        label = character(),
        dim_links = character(),
        uuid = character()
      ))
    }
    cat("\n")

    .msg_h2("Nodes")
    purrr::iwalk(x[["nodes"]], function(sset, name) {
      if (is.null(sset[["select_which"]])) {
        msg <- paste("use all", .frmt_big(sset[["length"]]), "elements")
      } else {
        msg <- sprintf(
          "use %s%% of elements (%s/%s)",
          round(length(sset[["select_which"]]) / sset[["length"]] * 100, digits = 1),
          .frmt_big(length(sset[["select_which"]])),
          .frmt_big(sset[["length"]])
        )
      }

      if (is.null(sset[["match_on"]])) {
        keys <- NULL
      } else {
        keys <- sprintf("{%s}", .collapse(names(sset[["match_on"]]), delim = ","))
      }

      cli::cat_bullet(paste0(name, keys, ": ", msg))
    })
  }

  invisible(x)
}

##' @export
summary.data_relation_model <- function(object, ...) {
  tibble::tibble(
    drm_label = rlang::env_label(object),
    n_links = length(object[["links"]]),
    n_nodes = length(object[["nodes"]])
  )
}

##' @export
drm_copy <- function(drmodel) {
  stopifnot(is_drm(drmodel))
  # Only copy nodes, other elements should be added using: x <- drm_add(drm, x)
  new_drm(nodes = drmodel[["nodes"]])
}

##' @export
drm_do_attach <- function(x, drmodel, dim_links, label = NULL) {
  if (is_drm(x))
    stop("'x' cannot be a data relation model itself!")
  validate_drm(drmodel)
  stopifnot(is.vector(dim_links), is.character(dim_links))
  if (is.null(label)) {
    label <- deparse(substitute(x))
  } else {
    .stopifnotsingle(label, what = "character")
  }

  # Check DIM_LINKS, convert NA to empty string
  dim_links[is.na(dim_links)] <- ""
  if (.ndim(x) != length(dim_links))
    stop("The length of 'dim_links' should match the number of dimensions in 'x' (", .ndim(x), ")!")
  if (all(dim_links == ""))
    stop("Not all 'dim_links' can be empty/NA!")

  # Reuse UUID if the object already has one
  uuid <- attr(x, "data_relation_uuid", exact = TRUE)
  if (is.null(uuid) || !uuid %in% names(drmodel[["links"]]))
    uuid <- uuid::UUIDgenerate()

  # Add DIM_LINKS to DRMODEL nodes
  x_dim <- .dim(x)
  for (i in seq_along(dim_links)[dim_links != ""]) {
    id <- dim_links[i]
    curr_node <- drmodel[["nodes"]][[id]]
    if (is.null(curr_node)) {
      drmodel[["nodes"]][[id]] <- list(length = x_dim[i])
    } else if (x_dim[i] != curr_node[["length"]]) {
      expected <- .frmt_big(curr_node[["length"]])
      observed <- .frmt_big(x_dim[i])
      warning("At dimension ", i, "(", id, "): expected length ", expected, ", but got ", observed, "!")
    }
  }

  # Add X and DIM_LINKS to DRMODEL links
  drmodel[["links"]][[uuid]] <- dim_links
  drmodel[["labels"]][uuid] <- label

  # Attach DRMODEL to X
  attr(x, "data_relation_model") <- drmodel
  attr(x, "data_relation_uuid") <- uuid

  x
}

##' @export
drm_do_detach <- function(x) {
  # Recurse over lists/dataframes
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[[i]] <- drm_do_detach(x[[i]])
    }
  }

  attr(x, "data_relation_model") <- NULL
  attr(x, "data_relation_uuid") <- NULL
  x
}

##' @export
drm_get <- function(x) {
  if (is_drm(x))
    return(x)
  attr(x, "data_relation_model", exact = TRUE)
}

##' @export
has_drm <- function(x) {
  !is.null(drm_get(x))
}

##' @export
drm_contains <- function(x, ...) {
  rlang::check_dots_unnamed()

  x_drm <- drm_get(x)

  purrr::map_lgl(list(...), function(dot) {
    dot_drm <- attr(dot, "data_relation_model", exact = TRUE)
    if (is.null(dot_drm))
      return(FALSE)
    if (!identical(dot_drm, x_drm))
      return(FALSE)
    any(names(x_drm[["links"]]) == attr(dot, "data_relation_uuid", exact = TRUE))
  })
}

##' @export
drm_delete <- function(drmodel, ...) {
  validate_drm(drmodel)
  if (...length() == 0L)
    stop("Please provide the nodes to delete as additional arguments!")
  rlang::check_dots_unnamed()

  # ... contains the names of the nodes to delete
  dots <- as.character(substitute(list(...))[-1L])
  if (any(dots == ""))
    stop("Arguments cannot be empty/zero characters!")

  # Links cannot reference the deleted nodes, find those who do se they can be removed
  link_has_dots <- purrr::map_lgl(drmodel[["links"]], function(link) any(link %in% dots))
  keep_uuid <- names(which(!link_has_dots))

  # Remove nodes and any links referencing them
  drmodel[["links"]] <- drmodel[["links"]][keep_uuid]
  drmodel[["labels"]] <- drmodel[["labels"]][keep_uuid]
  drmodel[["nodes"]][dots] <- NULL

  drmodel
}

##' @export
drm_trim <- function(drmodel, ..., .trim_nodes = TRUE) {
  validate_drm(drmodel)
  .stopifnotsingle(.trim_nodes, what = "logical")
  rlang::check_dots_unnamed()

  # Trim drmodel$links to contain only ... if provided
  if (...length() != 0L) {
    stopifnot(drm_contains(drmodel, ...))

    uuids <- purrr::map_chr(list(...), function(dot) {
      attr(dot, "data_relation_uuid")
    })

    drmodel[["links"]] <- drmodel[["links"]][uuids]
  }

  # Trim drmodel$labels to match drmodel$links
  drmodel[["labels"]] <- drmodel[["labels"]][names(drmodel[["links"]])]

  # Trim drmodel$nodes to match drmodel$links
  if (.trim_nodes) {
    links <- unlist(drmodel[["links"]], use.names = FALSE)
    links <- unique(links[links != ""])
    drmodel[["nodes"]] <- drmodel[["nodes"]][links]
  }

  drmodel
}

##' @export
drm_preselect <- function(x, ...) {
  drmodel <- validate_drm(drm_get(x))
  if (...length() == 0L)
    stop("Please provide the nodes to delete as additional arguments!")

  dots <- list(...)
  if (!rlang::is_named(dots))
    stop("All additional arguments should be named!")

  # Check elements
  for (current in names(dots)) {
    curr_dots <- dots[[current]]
    curr_node <- drmodel[["nodes"]][[current]]
    if (is.null(curr_node))
      stop("At '", current, "': entry not found in data relation model!")
    if (!is.logical(curr_dots))
      stop("At '", current, "': should be a vector of logical values!")

    check_len <- c(length(curr_node[["select_which"]]), curr_node[["length"]])
    check_len <- check_len[check_len > 0L]
    if (!any(length(curr_dots) == check_len)) {
      stop("At '", current, "': node should have a length of ",
           .collapse(.frmt_big(check_len), delim = " or "),
           ", but got ", length(curr_dots))
    }

    if (!any(curr_dots))
      stop("At '", current, "': no elements remaining after applying selection!")
  }

  # Apply selections
  for (current in names(dots)) {
    curr_dots <- dots[[current]]
    if (length(curr_dots) == drmodel[["nodes"]][[current]][["length"]]) {
      if (all(curr_dots)) {
        drmodel[["nodes"]][[current]][["select_which"]] <- NULL
      } else {
        drmodel[["nodes"]][[current]][["select_which"]] <- which(curr_dots)
      }
    } else {
      drmodel$nodes[[current]]$select_which <- drmodel$nodes[[current]]$select_which[curr_dots]
    }
  }

  x
}

##' @export
drm_forget <- function(x, ..., .forget_select = TRUE, .forget_match = TRUE) {
  drmodel <- validate_drm(drm_get(x))
  rlang::check_dots_unnamed()
  .stopifnotsingle(.forget_select, what = "logical")
  .stopifnotsingle(.forget_match, what = "logical")

  # Parse ... for nodes to clear, if empty clear all nodes
  if (...length() > 0L) {
    dots <- as.character(substitute(list(...))[-1L])
    missing <- setdiff(dots, names(drmodel[["nodes"]]))
    if (length(missing) > 0L)
      stop("The following nodes are not recognised: ", .quote_collapse(missing))
  } else {
    dots <- names(drmodel[["nodes"]])
  }

  # Clear nodes
  for (name in dots) {
    if (.forget_select)
      drmodel[["nodes"]][[name]][["select_which"]] <- NULL
    if (.forget_match)
      drmodel[["nodes"]][[name]][["match_on"]] <- NULL
  }

  x
}

##' @export
drm_deselect <- function(x, ...) {
  drm_forget(x, ..., .forget_select = TRUE, .forget_match = FALSE)
}

##' @export
drm_which <- function(x, node) {
  x_drm <- drm_get(x)
  if (!any(names(x_drm[["nodes"]]) == node))
    stop("No node entry found in data relation model for: ", node)

  select_which <- x_drm[["nodes"]][[node]][["select_which"]]
  if (is.null(select_which)) {
    rlang::missing_arg()
  } else {
    select_which
  }
}

##' @export
drm_do_subset <- function(x, ..., .drop = FALSE) {
  if (is_drm(x))
    stop("Please provide the object to subset, not the data relation model itself!")
  rlang::check_dots_unnamed()

  # Allow for additional user provided indices, these take precedence
  dots <- rlang::dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

  drmodel <- validate_drm(drm_get(x))
  if (!drm_contains(drmodel, x))
    stop("The associated data relation model no longer contains '", substitute(x), "'!")

  links <- drmodel[["links"]][[attr(x, "data_relation_uuid")]]
  x_dim <- .dim(x)
  if (length(links) != length(x_dim))
    stop("Data relation model expects ", length(links), "dimensions, but got ", length(x_dim), "!")
  if (length(dots) > length(x_dim))
    stop("Provided more additional arguments than the number of dimensions in '", substitute(x), "'!")

  # Extent ... to match the dimensions of X
  dots <- c(dots, rep(list(rlang::missing_arg()), length(x_dim) - length(dots)))

  # Pull node indices from relation data model
  for (i in seq_along(x_dim)) {
    if (!rlang::is_missing(dots[[i]]) || links[[i]] == "")
      next()

    curr_node <- drmodel[["nodes"]][[links[[i]]]]
    if (x_dim[i] != curr_node[["length"]]) {
      stop("At dimension ", i, ": '", links[i], "' expects length ",
          curr_node[["length"]], ", but got ", x_dim[i])
    }

    if (!is.null(curr_node[["select_which"]]))
      dots[[i]] <- curr_node[["select_which"]]
  }

  # Apply subsets
  x <- do.call(`[`, args = c(list(x), dots, list(drop = .drop)))

  # Remove DRM attributes as size no longer matches the original object
  drm_do_detach(x)
}

##' @export
drm_do_match <- function(x, ..., .override = FALSE) {
  if (is_drm(x))
    stop("Please provide the object to match, not the data relation model itself!")
  drmodel <- validate_drm(drm_get(x))
  if (!drm_contains(drmodel, x))
    stop("The associated data relation model no longer contains '", substitute(x), "'!")
  if (...length() == 0L)
    stop("Please provide the keys to match on as additional arguments!")

  dots <- list(...)
  if (!rlang::is_named(dots))
    stop("All additional arguments should be named by the nodes to match!")

  missing <- setdiff(names(dots), names(drmodel[["nodes"]]))
  if (length(missing) != 0L)
    stop("The following arguments do not match any node: ", .quote_collapse(missing))

  links <- drmodel[["links"]][[attr(x, "data_relation_uuid")]]
  missing <- setdiff(names(dots), links)
  if (length(missing) != 0L)
    stop("The following arguments do not match a dimension: ", .quote_collapse(missing))

  # Validate arguments and cast lists to tibbles
  dots <- purrr::map(dots, function(dot) {
    if (!is.list(dot)) {
      stop("All additional arguments should be named lists!")
    } else if (!rlang::is_named(dot)) {
      stop("All list elements should be named!")
    }
    dot <- tibble::as_tibble(dot)
    if (nrow(dot) != dplyr::n_distinct(dot))
      stop("Provided combination of match keys are not unique!")
    dot
  })

  # If a node has no stored keys or if .OVERRIDE, store the provided keys
  # Else match the provided keys to the stored ones to match the order of both
  dots <- purrr::imap(dots, function(dot, name) {
    keys <- if (!.override) drmodel[["nodes"]][[name]][["match_on"]]
    if (is.null(keys)) {
      drmodel[["nodes"]][[name]][["match_on"]] <- drm_do_detach(dot)
      return(rlang::missing_arg())
    }

    missing <- setdiff(names(keys), names(dot))
    if (length(missing) != 0L)
      stop("The following match keys are expected but missing: ", .quote_collapse(missing))
    if (nrow(dot) != nrow(keys))
      stop("Expected keys to have length ", .frmt_big(nrow(keys)), ", but got ", .frmt_big(nrow(dot)), "!")

    # Drop unused columns
    dot <- dot[names(keys)]

    # Check if sorting is needed else return an empty argument to skip indexing
    if (isTRUE(all.equal(keys, dot, check.class = FALSE, check.attributes = FALSE)))
      return(rlang::missing_arg())

    # This approach is 10x faster, but produces invalid results when there is >1 key
    if (ncol(keys) == 1L) {
      res <- match(dot[[1L]], keys[[1L]])
      if (anyNA(res))
        stop("Failed to match all key values!")
      return(order(res))
    }

    # Add indices and join to match the order
    dot[[length(dot) + 1L]] <- seq_len(nrow(dot))
    join_result <- dplyr::left_join(
      x = keys,
      y = dot,
      by = names(keys),
      unmatched = "error",
      relationship = "one-to-one"
    )
    join_result[[length(join_result)]]
  })

  # Match arguments to 'dim_links' of X
  args <- rlang::rep_named(links, list(rlang::missing_arg()))
  args[names(dots)] <- dots
  names(args) <- NULL

  # Apply re-oredering
  do.call(`[`, args = c(list(x), args, list(drop = FALSE)))
}

##' @export
has_join <- function(x, y, by = NULL, ...) {
  stopifnot(is.data.frame(x))
  stopifnot(is.data.frame(y))

  if (is.null(by)) {
    by_x <- by_y <- intersect(names(x), names(y))
  } else {
    if (inherits(by, "dplyr_join_by")) {
      by_x <- by$x
      by_y <- by$y
    } else if (is.character(by)) {
      by_x <- if (rlang::is_named(by)) names(by) else by
      by_y <- unname(by)
    } else {
      stop("The 'by' argument should be a character vector or `dplyr::join_by()`!")
    }
  }

  # Subset X and Y to save on memory
  x <- x[by_x]
  y <- y[by_y]

  # Add a dummy column for checking if the join was successful
  y[[length(y) + 1L]] <- TRUE
  join_result <- dplyr::left_join(
    x = x,
    y = y,
    by = by,
    multiple = "any",
    suffix = c(".x", ""),
    ...
  )

  # Unsuccessful joins return NA
  !is.na(join_result[[length(join_result)]])
}
