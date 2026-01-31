new_linked_data <- function(x = NULL, slots = NULL, indices = NULL, keys = NULL) {
  x <- if (is.null(x)) list() else as.list(x)
  if (is.null(slots)) slots <- list()
  if (is.null(indices)) indices <- list()
  if (is.null(keys)) keys <- list()
  stopifnot(is.list(slots), is.list(indices), is.list(keys))

  structure(
    x,
    slots = slots,
    indices = indices,
    keys = keys,
    class = c("linked_data", "list")
  )
}

is_linked_data <- function(x) {
  inherits(x, "linked_data")
}

validate_linked_data <- function(x) {
  stopifnot(is_linked_data(x), is.list(x))

  stopifnot(c("slots", "indices", "keys") %in% names(attributes(x)))
  stopifnot(is.list(attr(x, "slots")), rlang::is_named2(attr(x, "slots")))
  stopifnot(is.list(attr(x, "indices")), rlang::is_named2(attr(x, "indices")))
  stopifnot(is.list(attr(x, "keys")), rlang::is_named2(attr(x, "keys")))

  invisible(x)
}

linked_data <- function(x = NULL, ...) {
  if (!is.null(x))
    stopifnot(is.list(x), rlang::is_named(x))

  out <- new_linked_data()
  if (...length() > 0L)
    out <- linked_data_register(out, ...)
  if (length(x) > 0L)
    out[names(x)] <- x

  out
}

linked_data_unclass <- function(x) {
  x <- unclass(x)
  attributes(x)[c("slots", "indices", "keys")] <- NULL
  x
}

# REVIEW unused?
linked_data_restore <- function(x, attr_from) {
  stopifnot(is.list(x))
  validate_linked_data(attr_from)

  copy_attr <- c("slots", "indices", "keys")
  attributes(x)[copy_attr] <- attributes(attr_from)[copy_attr]

  add_class <- c("linked_data", "list")
  class(x) <- c(setdiff(class(x), add_class), add_class)

  x
}

##' @export
`[<-.linked_data` <- function(x, i, ..., value) {
  validate_linked_data(x)
  result <- NextMethod("[")

  if (is.null(value))
    return(result)

  if (!is.character(i)) {
    if (length(result) != length(x))
      stop("New values must be assigned by name!")
    i <- names(x)[i]
  }

  # Only recycle VALUE if it is of length 1
  value <- as.list(value)
  if (length(value) == 1L && length(i) > 1L) {
    value <- rep(value, length(i))
  } else if (length(i) != length(value)) {
    stop("Number of items to replace should match replacement length!")
  }

  # Check if assigning to a defined slot
  slots <- attr(x, "slots")
  no_slot <- setdiff(i, names(slots))
  if (length(no_slot) > 0L)
    stop("Register slots before assigning: ", .quote_collapse(no_slot), "!")

  # Check if the new dimensions match with existing indices, creating them if missing
  indices <- attr(x, "indices")
  for (ii in seq_along(i)) {
    current <- i[ii]
    use_indices <- slots[[current]]
    if (is.null(use_indices) || is.null(value[[ii]]))
      next()

    dims <- .dim(value[[ii]])
    if (length(dims) != length(use_indices))
      stop("Expected ", length(use_indices), " dimensions, but got ", length(dims), "!")

    names(dims) <- use_indices
    dims <- dims[use_indices != ""]

    for (idx in names(dims)) {
      if (is.null(indices[[idx]])) {
        indices[[idx]] <- rep(TRUE, dims[[idx]])
      } else if (dims[idx] != length(indices[[idx]])) {
        stop("At dim ", which(use_indices == idx), " of `$", current,
             "`: expected length ", .frmt_big(length(indices[[idx]])),
             ", but got ", .frmt_big(dims[idx]), "!")
      }
    }
  }

  attr(result, "indices") <- indices
  result
}

##' @export
`[[<-.linked_data` <- function(x, i, value) {
  stopifnot(length(i) == 1L)
  x[i] <- if (!is.null(value)) list(value)
  x
}

##' @export
`$<-.linked_data` <- function(x, name, value) {
  stopifnot(is.character(name), length(name) == 1L)
  x[name] <- if (!is.null(value)) list(value)
  x
}

##' @export
`[.linked_data` <- function(x, i, ...) {
  validate_linked_data(x)
  if (missing(i)) {
    i <- names(x)
  } else if (!is.character(i)) {
    i <- names(x)[i]
  }

  result <- purrr::map(i, function(ii) linked_data_get(x, ii))
  names(result) <- i

  # Also subset the attributes to keep the new object valid
  slots <- attr(x, "slots")[names(result)]
  keep_indices <- setdiff(purrr::list_c(slots), "")
  names(keep_indices) <- keep_indices

  indices <- purrr::map(keep_indices, function(name) {
    rep(TRUE, sum(attr(x, "indices")[[name]]))
  })

  keep_keys <- intersect(names(attr(x, "keys")), keep_indices)
  keys <- purrr::map(keep_keys, function(name) {
    key <- attr(x, "keys")[[name]]
    if (!is.null(key))
      key[attr(x, "indices")[[name]], , drop = FALSE]
  })

  result <- new_linked_data(result, slots = slots, indices = indices, keys = keys)

  # Restore remaining attributes
  copy_attr <- c("class", setdiff(names(attributes(x)), names(attributes(result))))
  attributes(result)[copy_attr] <- attributes(x)[copy_attr]

  result
}

##' @export
`[[.linked_data` <- function(x, i, ..., exact = TRUE) {
  # NOTE we always use exact name matching
  if (isFALSE(exact))
    warning("'exact' name matching cannot be disabled")
  linked_data_get(x, i = i)
}

##' @export
`$.linked_data` <- function(x, name) {
  linked_data_get(x, i = name)
}

##' @export
print.linked_data <- function(x, ...) {
  .msg_h1("Linked data object")
  .msg_h2("Slots")
  .print_slots(x, ...)
  .msg_h2("Indices")
  .print_indices(x)
}

.print_slots <- function(x, ..., .exclude = NULL) {
  slots <- attr(x, "slots", exact = TRUE)
  if (!is.null(.exclude))
    slots <- slots[!names(slots) %in% .exclude]

  if (length(slots) > 0L) {
    tbl <- tibble::tibble(
      slot = paste0("$", names(slots)),
      indices = paste0("[", purrr::map_chr(slots, .collapse), "]"),
      value = unclass(x)[names(slots)]
    )
    cat(format(tbl, ...)[-c(1L, 3L)], "", sep = "\n")
  }

  invisible(x)
}

.print_indices <- function(x, show_keys = TRUE) {
  indices <- attr(x, "indices", exact = TRUE)
  keys <- attr(x, "keys", exact = TRUE)

  if (length(indices) > 0L) {
    purrr::iwalk(indices, function(idx, name) {
      if (all(idx)) {
        bullet <- "record"
        msg <- paste("use all", .frmt_big(length(idx)), "elements")
      } else {
        bullet <- "info"
        n_use <- sum(idx)
        msg <- sprintf(
          "use %s%% of elements (%s/%s)",
          round(n_use / length(idx) * 100, digits = 1),
          .frmt_big(n_use), .frmt_big(length(idx))
        )
      }

      key <- keys[[name]]
      if (isTRUE(show_keys) && !is.null(key))
        key <- sprintf("{%s}", .collapse(names(key), delim = ","))

      cli::cat_bullet(paste0(name, key, ": ", msg), bullet = bullet)
    })
    cat("\n")
  }

  invisible(x)
}

##' @export
str.linked_data <- function(object, ..., give.attr = FALSE, no.list = FALSE) {
  if (!isTRUE(no.list))
    cat("Linked data object <list of ", length(object), ">:\n", sep = "")
  utils::str(unclass(object), give.attr = give.attr, no.list = TRUE, ...)
}

##' @export
summary.linked_data <- function(object, ...) {
  validate_linked_data(object)
  idx <- attr(object, "indices")

  tibble::tibble(
    indices = if (length(idx) > 0L) names(idx) else character(),
    length = purrr::map_int(idx, length),
    n_use = purrr::map_int(idx, sum),
    frac_use = .data$n_use / .data$length,
    keys = attr(object, "keys")[.data$indices]
  )
}

linked_data_register <- function(x, ...) {
  validate_linked_data(x)
  rlang::check_dots_unnamed()

  if (...length() == 0L)
    stop("Please provide the slots to add as additional arguments!")

  dots <- purrr::list_flatten(purrr::map(rlang::enexprs(...), function(exp) {
    if (rlang::is_symbol(exp)) {
      exp <- as.character(exp)
    } else if (rlang::is_call(exp, name = "[")) {
      exp <- as.character(rlang::call_args(exp))
    } else {
      stop("Invalid argument: ", rlang::expr_label(exp))
    }
    setNames(list(if (length(exp) > 1L) exp[-1L]), nm = exp[1L])
  }))

  occupied <- intersect(names(dots), names(x))
  if (length(occupied) > 0L)
    stop("Provided slot are already occupied: ", .quote_collapse(occupied), "!")

  attr(x, "slots")[names(dots)] <- dots
  x
}

linked_data_add <- function(x, slot, data, ..., .override = FALSE) {
  validate_linked_data(x)
  .stopifnotsingle(slot, what = "character")
  .stopifnotsingle(.override, what = "logical")

  slots <- attr(x, "slots")
  keys <- attr(x, "keys")

  if (!any(names(slots) == slot))
    stop("No slot registered for: '", slot, "'!")

  # Return early if there is nothing to match
  if (...length() == 0L) {
    x[[slot]] <- data
    return(x)
  }

  # Ensure data has the expected number of dimensions for the slot
  use_indices <- slots[[slot]]
  if (!is.null(use_indices) && .ndim(data) != length(use_indices))
    stop("Expected data to have ", length(use_indices), " dimensions, but got ", .ndim(data), "!")

  dots <- rlang::enquos(...)
  if (!rlang::is_named(dots))
    stop("All additional arguments should be named!")

  dots <- .dots_compute_keys(dots, data = data, use_indices = use_indices)

  # Add missing keys
  if (.override) {
    add_keys <- names(dots)
  } else {
    add_keys <- names(dots)[purrr::map_lgl(keys[names(dots)], is.null)]
  }
  attr(x, "keys")[add_keys] <- dots[add_keys]

  # Match the keys that have not been added/overwritten
  match_keys <- setdiff(names(dots), add_keys)
  dots <- purrr::map2(dots[match_keys], keys[match_keys], .dot_match_key)

  # If no reordering is needed we can short circuit
  if (all(purrr::map_lgl(dots, isTRUE))) {
    x[[slot]] <- data
    return(x)
  }

  # Match arguments to indices
  args <- rlang::rep_named(use_indices, list(TRUE))
  args[names(dots)] <- dots
  names(args) <- NULL

  # Apply re-oredering and store value
  x[[slot]] <- do.call("[", args = c(list(data), args, list(drop = FALSE)))

  x
}

linked_data_get <- function(x, i, ..., .drop = FALSE) {
  validate_linked_data(x)

  x <- unclass(x)
  xi <- x[[i]]
  if (is.null(xi))
    return(xi)

  # Slots are accessed by name, so we might need to convert
  if (isTRUE(is.numeric(i)))
    i <- names(x)[[i]]

  # Pull the indices associated to slot i if any
  indices <- attr(x, "indices")
  use_indices <- attr(x, "slots")[[i]]
  if (is.null(use_indices)) {
    idx_val <- rep(list(TRUE), .ndim(x))
  } else {
    missing <- setdiff(use_indices, c(names(indices), ""))
    if (length(missing) > 0L)
      stop("Missing indices for: ", .quote_collapse(missing), "!")

    idx_val <- indices[use_indices]
    idx_val[use_indices == ""] <- TRUE
  }

  # Allow for additional user provided indices, these take precedence
  if (...length() > 0L) {
    dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)

    # DOTS can target indices by name
    have_name <- rlang::have_name(dots)

    # If unnamed arguments are present, the number of dimensions should match
    if (!all(have_name) && length(dots) != length(idx_val)) {
      stop("The number of additional arguments (", length(dots),
           ") should match the dimensions of '", i, "' (", length(idx_val), ")!")
    }

    # Put named arguments in the correct positions
    if (any(have_name)) {
      named_dots <- dots[have_name]
      dots[have_name] <- list(NULL)

      missing <- setdiff(names(named_dots), use_indices)
      if (length(missing) > 0L)
        stop("The following arguments do not match any indices: ", .quote_collapse(missing))

      dots_match_idx <- match(names(named_dots), use_indices)

      # Ensure named and unnamed arguments don't reference the same position
      overlap <- sort(intersect(dots_match_idx, which(!have_name)))
      if (length(overlap) > 0L) {
        stop("Positional and named arguments collide: ",
             paste0(overlap, " & '", use_indices[overlap], "'", collapse = ", "), "!")
      }

      # Match size first so named dots can be given the correct position
      dots <- c(dots, rep(list(NULL), length(idx_val) - length(dots)))
      dots[dots_match_idx] <- named_dots
    }

    # Only replace index values if not null/missing
    which_use <- which(!purrr::map_lgl(dots, function(dot) is.null(dot) || rlang::is_missing(dot)))
    idx_val[which_use] <- dots[which_use]
  }

  # Short circuit if there is nothing to select
  if (all(purrr::map_lgl(idx_val, function(idx) is.logical(idx) && all(idx))))
    return(xi)

  do.call("[", args = c(list(xi), unname(idx_val), list(drop = .drop)))
}

linked_data_filter <- function(x, ...) {
  validate_linked_data(x)
  if (...length() == 0L)
    stop("Please provide indices to subset as additional arguments!")

  dots <- purrr::map(rlang::enquos(...), function(quo) {
    rlang::eval_tidy(quo, data = list(. = x))
  })
  if (!rlang::is_named(dots))
    stop("All additional arguments should be named!")

  indices <- attr(x, "indices")

  # Check elements
  for (current in names(dots)) {
    curr_dots <- dots[[current]]
    curr_idx <- indices[[current]]

    if (is.null(curr_idx))
      stop("At argument '", current, "': no matching indices found!")
    if (!is.logical(curr_dots))
      stop("At argument '", current, "': should be a TRUE/FALSE vector!")
    if (length(curr_dots) != sum(curr_idx)) {
      stop("At '", current, "': expected length ", .frmt_big(sum(curr_idx)),
           ", but got ", .frmt_big(length(curr_dots)), "!")
    }

    if (all(curr_dots))
      next()

    which_mask <- which(curr_idx)[!curr_dots]
    indices[[current]][which_mask] <- FALSE
  }

  attr(x, "indices") <- indices
  x
}

linked_data_reset <- function(x, indices = NULL) {
  validate_linked_data(x)
  attr_ind <- attr(x, "indices")

  # Reset all indices if NULL
  if (is.null(indices)) {
    indices <- names(attr_ind)
  } else {
    missing <- setdiff(indices, names(attr_ind))
    if (length(missing) > 0L)
      stop("The following indices were not recognized: ", .quote_collapse(missing))
  }

  attr(x, "indices")[indices] <- purrr::map(attr_ind[indices], function(idx) {
    if (isTRUE(all(idx))) {
      idx
    } else {
      rep(TRUE, length(idx))
    }
  })

  x
}

linked_data_trim <- function(x, indices = NULL) {
  validate_linked_data(x)
  slots <- attr(x, "slots")
  attr_ind <- attr(x, "indices")

  # Trim all indices if NULL
  if (is.null(indices)) {
    indices <- names(attr_ind)
  } else {
    stopifnot(is.character(indices))
    missing <- setdiff(indices, names(attr_ind))
    if (length(missing) > 0L)
      stop("The following indices were not recognized: ", .quote_collapse(missing))
  }

  # Find indices only associated with empty slots
  used_slots <- names(x)[!purrr::map_lgl(x, is.null)]
  remove_indices <- setdiff(indices, purrr::list_c(slots[used_slots]))
  if (length(remove_indices) > 0L)
    attr(x, "indices") <- attr_ind[setdiff(names(attr_ind), remove_indices)]

  x
}

linked_data_indices_get <- function(x) {
  validate_linked_data(x)
  attr(x, "indices")
}

linked_data_indices_set <- function(x, indices, add = FALSE) {
  validate_linked_data(x)
  stopifnot(is.list(indices), rlang::is_named(indices))
  .stopifnotsingle(add, what = "logical")

  indices <- purrr::imap(indices, function(curr, idx) {
    if (!(is.vector(curr) && is.logical(curr)))
      stop("Expected a vector of type logical!")

    idx <- .indices(x)[[idx]]
    if (is.null(idx))
      return(curr)

    if (length(curr) != length(idx))
      stop("Expected length ", length(idx), ", but got ", length(curr), "!")

    if (add) {
      curr & idx
    } else {
      curr
    }
  })

  .indices(x)[names(indices)] <- indices
  x
}

.indices <- function(x) {
  attr(x, "indices", exact = TRUE)
}

`.indices<-` <- function(x, value) {
  attr(x, "indices") <- value
  x
}

.quo_replace_fn <- function(quo, fn) {
  args <- rlang::call_args(rlang::quo_get_expr(quo))
  rlang::new_quosure(
    rlang::call2(fn, !!!args),
    env = rlang::quo_get_env(quo)
  )
}

.dots_compute_keys <- function(dots_quos, data, use_indices) {
  dims <- .dim(data)
  names(dims) <- use_indices

  purrr::imap(dots_quos, function(dot, name) {
    if (!any(use_indices == name))
      stop("Argument '", name, "' does not match any indices!", call. = FALSE)

    # Unpack `node = c(key1, key2, ...)` style expressions
    if (rlang::quo_is_call(dot, "c"))
      dot <- .quo_replace_fn(dot, fn = tibble::tibble)

    res <- rlang::eval_tidy(dot, data = c(list(. = data), if (is.list(data)) data))
    if (!is.list(res))
      stop("All additional arguments should evaluate to a list/dataframe!", call. = FALSE)
    if (!rlang::is_named(res))
      stop("All elements should be named!", call. = FALSE)

    res <- tibble::as_tibble(res)
    if (nrow(res) != dims[[name]]) {
      stop("Data size (", .frmt_big(dims[[name]]), "), doesn't match keys (",
           .frmt_big(nrow(res)), ")!", call. = FALSE)
    } else if (nrow(res) != dplyr::n_distinct(res)) {
      stop("Provided combination of match keys are not unique!", call. = FALSE)
    }

    res
  })
}

.dot_match_key <- function(dot, curr_key) {
  missing <- setdiff(names(curr_key), names(dot))
  if (length(missing) != 0L)
    stop("The following keys are expected but missing: ", .quote_collapse(missing), call. = FALSE)
  if (nrow(dot) != nrow(curr_key))
    stop("Expected keys to have length ", .frmt_big(nrow(curr_key)), ", but got ", .frmt_big(nrow(dot)), "!", call. = FALSE)

  # Drop unused columns
  dot <- dot[names(curr_key)]

  # Check if sorting is needed else return a single TRUE to skip indexing later
  if (isTRUE(all.equal(curr_key, dot, check.class = FALSE, check.attributes = FALSE)))
    return(TRUE)

  # This approach is 10x faster, but produces invalid results when there is >1 key
  if (ncol(curr_key) == 1L) {
    res <- match(dot[[1L]], curr_key[[1L]])
    if (anyNA(res))
      stop("Failed to match all key values!", call. = FALSE)
    return(order(res))
  }

  # Add indices as last column
  dot[[length(dot) + 1L]] <- seq_len(nrow(dot))

  # Use join to match the order
  join_result <- dplyr::left_join(
    x = curr_key,
    y = dot,
    by = names(curr_key),
    unmatched = "error",
    relationship = "one-to-one"
  )

  # Retrieve indices to obtain the reordering
  join_result[[length(join_result)]]
}
