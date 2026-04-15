##' @export
new_related_data <- function(x = NULL, slots = NULL, indices = NULL, keys = NULL) {
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
    class = c("related_data", "list")
  )
}

##' @export
is_related_data <- function(x) {
  inherits(x, "related_data")
}

##' @export
validate_related_data <- function(x) {
  stopifnot(is_related_data(x), is.list(x))

  stopifnot(c("slots", "indices", "keys") %in% names(attributes(x)))
  stopifnot(is.list(attr(x, "slots")), rlang::is_named2(attr(x, "slots")))
  stopifnot(is.list(attr(x, "indices")), rlang::is_named2(attr(x, "indices")))
  stopifnot(is.list(attr(x, "keys")), rlang::is_named2(attr(x, "keys")))

  x
}

##' @export
related_data <- function(x = NULL, ...) {
  if (!is.null(x))
    stopifnot(is.list(x), rlang::is_named(x))

  out <- new_related_data()
  if (...length() > 0L)
    out <- rd_register(out, ...)
  if (length(x) > 0L)
    out[names(x)] <- x

  out
}

##' @export
rd_unclass <- function(x) {
  x <- unclass(x)
  attributes(x)[c("slots", "indices", "keys")] <- NULL
  x
}

# REVIEW unused?
##' @export
rd_restore <- function(x, attr_from) {
  stopifnot(is.list(x))
  validate_related_data(attr_from)

  copy_attr <- c("slots", "indices", "keys")
  attributes(x)[copy_attr] <- attributes(attr_from)[copy_attr]

  add_class <- c("related_data", "list")
  class(x) <- c(setdiff(class(x), add_class), add_class)

  x
}

##' @export
`[<-.related_data` <- function(x, i, ..., value) {
  validate_related_data(x)
  if (is.null(value)) {
    value <- list(NULL)
  } else {
    value <- as.list(value)
  }

  # Only recycle VALUE if it is of length 1
  if (length(value) == 1L && length(i) > 1L) {
    value <- rep(value, length(i))
  } else if (length(i) != length(value)) {
    stop("Number of items to replace should match replacement length!")
  }

  rd_set_nomatch(x, i = i, value = value, validate = FALSE)
}

##' @export
`[[<-.related_data` <- function(x, i, value) {
  stopifnot(length(i) == 1L)
  rd_set_nomatch(x, i = i, value = list(value))
}

##' @export
`$<-.related_data` <- function(x, name, value) {
  stopifnot(is.character(name), length(name) == 1L)
  rd_set_nomatch(x, i = name, value = list(value))
}

##' @export
`[.related_data` <- function(x, i, ...) {
  validate_related_data(x)
  if (missing(i)) {
    i <- names(x)
  } else if (!is.character(i)) {
    i <- names(x)[i]
  }

  result <- purrr::map(i, function(ii) rd_get(x, ii, .validate = FALSE))
  names(result) <- i

  # Also subset the attributes to keep the new object valid
  slots <- attr(x, "slots")[i]

  keep_indices <- setdiff(purrr::list_c(slots), "")
  indices <- purrr::map(keep_indices, function(name) {
    rep(TRUE, sum(attr(x, "indices")[[name]]))
  })
  names(indices) <- keep_indices

  keep_keys <- intersect(names(attr(x, "keys")), keep_indices)
  keys <- purrr::map(keep_keys, function(name) {
    key <- attr(x, "keys")[[name]]
    if (!is.null(key))
      key[attr(x, "indices")[[name]], , drop = FALSE]
  })
  names(keys) <- keep_keys

  result <- new_related_data(result, slots = slots, indices = indices, keys = keys)

  # Restore remaining attributes
  copy_attr <- c("class", setdiff(names(attributes(x)), names(attributes(result))))
  attributes(result)[copy_attr] <- attributes(x)[copy_attr]

  result
}

##' @export
`[[.related_data` <- function(x, i, ..., exact = TRUE) {
  # NOTE always use exact name matching
  if (isFALSE(exact))
    warning("'exact' name matching cannot be disabled")
  rd_get(x, i = i)
}

##' @export
`$.related_data` <- function(x, name) {
  rd_get(x, i = name)
}

##' @export
print.related_data <- function(x, ...) {
  .msg_h1("Related data object")
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
        bullet <- "bullet"
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
str.related_data <- function(object, ..., give.attr = FALSE, no.list = FALSE) {
  if (!isTRUE(no.list))
    cat("Related data object <list of ", length(object), ">:\n", sep = "")
  utils::str(unclass(object), give.attr = give.attr, no.list = TRUE, ...)
}

##' @export
summary.related_data <- function(object, ...) {
  validate_related_data(object)
  slots <- attr(object, "slots")
  idx <- attr(object, "indices")

  list(
    slots = tibble::tibble(
        slot = names(slots),
        indices = slots,
        value = unclass(object)[names(slots)]
    ),
    indices = tibble::tibble(
        indices = if (length(idx) > 0L) names(idx) else character(),
        length = purrr::map_int(idx, length),
        n_use = purrr::map_int(idx, sum),
        frac_use = .data$n_use / .data$length,
        keys = attr(object, "keys")[.data$indices]
    )
  )
}

##' @export
rd_register <- function(x, ...) {
  validate_related_data(x)
  rlang::check_dots_unnamed()

  if (...length() == 0L)
    stop("Please provide the slots to add as additional arguments!")

  dots <- purrr::list_flatten(purrr::map(rlang::enexprs(...), function(exp) {
    if (is.character(exp) && !is.na(exp)) {
      # Do nothing
    } else if (rlang::is_symbol(exp)) {
      exp <- as.character(exp)
    } else if (rlang::is_call(exp, name = "[")) {
      exp <- as.character(rlang::call_args(exp))
    } else {
      stop("Invalid argument: ", rlang::expr_label(exp))
    }
    setNames(list(if (length(exp) > 1L) exp[-1L]), nm = exp[1L])
  }))

  # TODO re-validate slots instead
  occupied <- intersect(names(dots), names(x))
  if (length(occupied) > 0L)
    stop("Provided slot are already occupied: ", .quote_collapse(occupied), "!")

  attr(x, "slots")[names(dots)] <- dots
  x
}

# REVIEW prefix arguments with a dot to prevent collisions with indices?
##' @export
match_rd <- function(data, rd, slot, ..., .return_unused = FALSE) {
  validate_related_data(rd)
  .stopifnotsingle(slot, what = "character")
  .stopifnotsingle(.return_unused, what = "logical")

  # Dots represent keys used for sorting, named list elements can be referred to directly
  dots <- rlang::enquos(...)
  if (!rlang::is_named2(dots))
    stop("All additional arguments should be named!")

  slots <- attr(rd, "slots")
  keys <- attr(rd, "keys")

  if (!slot %in% names(slots))
    stop("No slot registered for: '", slot, "'!")
  curr_slot <- slots[[slot]]

  nomatch <- setdiff(names(dots), curr_slot)
  if (length(nomatch) > 0L)
    stop("The following arguments don't match any indices: ", .quote_collapse(nomatch), call. = FALSE)

  # Warn about indices with known keys that were not provided by the user
  omitted <- setdiff(intersect(curr_slot, names(keys)), names(dots))
  if (length(omitted) > 0L)
    warning("Some indices with were keys not provided as arguments: ", .quote_collapse(omitted))

  if (length(dots) == 0L)
    return(data)

  if (is.null(curr_slot))
    stop("No indices registered for slot: '", slot, "'!")

  # Ensure data has the expected number of dimensions for the slot
  if (.ndim(data) != length(curr_slot))
    stop("Expected data to have ", length(curr_slot), " dimensions, but got ", .ndim(data), "!")

  dots <- .dots_compute_keys(dots, data = data, slot = curr_slot)

  # Match on intersecting indices
  match_indices <- names(dots)[!purrr::map_lgl(keys[names(dots)], is.null)]

  if (.return_unused)
    unused <- dots[setdiff(names(dots), match_indices)]

  dots <- purrr::map2(dots[match_indices], keys[match_indices], .dot_match_key)

  # Reorder if needed
  if (!all(purrr::map_lgl(dots, isTRUE))) {
    # Match arguments to indices to construct slicing arguments
    args <- rlang::rep_named(curr_slot, list(TRUE))
    args[names(dots)] <- dots
    names(args) <- NULL

    # Apply re-ordering & return
    data <- do.call("[", args = c(list(data), args, list(drop = FALSE)))
  }

  if (.return_unused)
    attr(data, "unused_keys") <- unused
  data
}

##' @export
rd_set_nomatch <- function(x, i, value, validate = TRUE) {
  if (!isFALSE(validate))
    validate_related_data(x)
  stopifnot(is.list(value))
  stopifnot(length(i) == length(value))

  # Attempt to convert I to character using names(X)
  if (!is.character(i)) {
    i <- names(x)[i]
    if (is.null(i) || isTRUE(all(nchar(i) > 0L)))
      stop("New values must be assigned by name!")
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

  # Unclass to use primitive and restore after
  class_x <- class(x)
  x <- unclass(x)
  x[i] <- value
  class(x) <- class_x

  # Update indices and return
  attr(x, "indices") <- indices
  x
}

##' @export
rd_set <- function(x, slot, data, ..., .override = FALSE) {
  .stopifnotsingle(.override, what = "logical")

  keys <- attr(x, "keys")
  if (.override)
    attr(x, "keys") <- list()

  data <- match_rd(data, rd = x, slot = slot, ..., .return_unused = TRUE)

  unused <- attr(data, "unused_keys")
  attr(data, "unused_keys") <- NULL

  if (length(unused) > 0L)
    keys[names(unused)] <- unused
  attr(x, "keys") <- keys

  rd_set_nomatch(x, i = slot, value = list(data), validate = FALSE)
}

##' @export
rd_get <- function(x, i, ..., .drop = FALSE, .validate = TRUE) {
  if (!isFALSE(.validate))
    validate_related_data(x)

  if (is.na(i[[1L]]))
    stop("The value of 'i' cannot be NA!")
  if (is.character(i) && !i[[1L]] %in% names(attr(x, "slots")))
    stop("No slot registered for: '", i[[1L]], "'!")

  x <- unclass(x)
  xi <- x[[i]]

  if (is.null(xi))
    return(NULL)

  # Slots are accessed by name, convert
  if (!is.character(i))
    i <- names(x)[[i]]

  # Pull the index values associated to slot I if known
  use_indices <- attr(x, "slots")[[i]]
  if (is.null(use_indices)) {
    use_indices <- character(.ndim(xi))
    idx_val <- rep(list(TRUE), length(use_indices))
  } else {
    indices <- attr(x, "indices")

    missing <- setdiff(use_indices, c(names(indices), ""))
    if (length(missing) > 0L)
      stop("Missing indices for: ", .quote_collapse(missing), "!")

    idx_val <- indices[use_indices]
    idx_val[use_indices == ""] <- TRUE
  }

  # Allow for additional user provided indices, these take precedence
  # Additional arguments can be named, positional, or a mix of both
  if (...length() > 0L) {
    dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)

    # Match names of DOTS to USE_INDICES to order based on the target dimension
    dots <- .dots_match_indices(dots, use_indices = use_indices)

    # Only replace index values if not null
    has_value <- !purrr::map_lgl(dots, is.null)
    idx_val[has_value] <- dots[has_value]
  }

  # Check if subsetting is actually needed to improve performance
  for (idx in idx_val) {
    if (is.logical(idx) && !anyNA(idx) && all(idx))
      next()
    return(do.call("[", args = c(list(xi), unname(idx_val), list(drop = .drop))))
  }

  xi
}

##' @export
rd_filter <- function(x, ...) {
  validate_related_data(x)
  if (...length() == 0L)
    stop("Please provide indices to subset as additional arguments!")

  dots <- purrr::map(rlang::enquos(...), function(quo) {
    with_rd(x, expr = !!quo)
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

##' @export
rd_reset <- function(x, indices = NULL) {
  validate_related_data(x)
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

##' @export
rd_trim <- function(x, indices = NULL) {
  validate_related_data(x)
  slots <- attr(x, "slots")
  keys <- attr(x, "keys")
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

  # Remove indices only associated with empty slots
  used_slots <- names(x)[!purrr::map_lgl(unclass(x), is.null)]
  remove_indices <- setdiff(indices, purrr::list_c(slots[used_slots]))
  if (length(remove_indices) > 0L)
    attr(x, "indices") <- attr_ind[setdiff(names(attr_ind), remove_indices)]

  # Remove keys not associated with any indices
  attr(x, "keys") <- keys[intersect(names(keys), names(attr(x, "indices")))]

  x
}

##' @export
rd_dim <- function(x, i, use_indices = TRUE, validate = TRUE) {
  if (!isFALSE(validate))
    validate_related_data(x)
  .stopifnotsingle(use_indices, what = "logical")
  x <- unclass(x)

  # Slots are accessed by name, so we might need to convert
  if (is.numeric(i))
    i <- names(x)[[i]]

  res <- setNames(.dim(x[[i]]), nm = attr(x, "slots")[[i]])
  if (!(use_indices && any(rlang::have_name(res))))
    return(res)

  # Subset dims using indices if element is named
  purrr::imap_int(res, function(len, idx) {
    if (is.na(idx) || idx == "")
      return(len)
    sum(attr(x, "indices")[[idx]])
  })
}

##' @export
rd_dimnames <- function(x, i, use_indices = TRUE, validate = TRUE) {
  if (!isFALSE(validate))
    validate_related_data(x)
  .stopifnotsingle(use_indices, what = "logical")
  x <- unclass(x)

  # Slots are accessed by name, so we might need to convert
  if (is.numeric(i))
    i <- names(x)[[i]]

  res <- setNames(.dimnames(x[[i]]), nm = attr(x, "slots")[[i]])
  if (!(use_indices && any(rlang::have_name(res))))
    return(res)

  # Subset dimnames using indices if element is named
  purrr::imap(res, function(dname, idx) {
    if (is.null(dname) || is.na(idx) || idx == "")
      return(dname)
    dname[attr(x, "indices")[[idx]]]
  })
}

##' @export
rd_indices_set <- function(x, indices, add = FALSE) {
  validate_related_data(x)
  stopifnot(is.list(indices), rlang::is_named(indices))
  .stopifnotsingle(add, what = "logical")

  indices <- purrr::imap(unclass(indices), function(curr, idx) {
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

##' @export
rd_indices_get <- function(x) {
  validate_related_data(x)
  attr(x, "indices")
}

##' @export
with_rd <- function(x, expr) {
  stopifnot(is_related_data(x))
  if (missing(expr))
    stop("The 'expr' argument is empty!")
  expr <- rlang::enquo(expr)

  # The related data object itself can be referenced with '.'
  eval_env <- rlang::env(. = x)

  # Lazily bind elements of X so subsetting is only applied if needed
  bind_vars <- setNames(nm = names(x))
  bind_exprs <- lapply(bind_vars, function(var) {
    rlang::expr(rd_get(x, i = !!var))
  })
  rlang::env_bind_lazy(eval_env, !!!bind_exprs)

  # Create data mask and add .data pronoun for tidyverse parity
  eval_env <- rlang::new_data_mask(eval_env)
  eval_env$.data <- rlang::as_data_pronoun(eval_env)

  rlang::eval_tidy(expr, data = eval_env)
}

`.indices<-` <- function(x, value) {
  attr(x, "indices") <- value
  x
}

.indices <- function(x) {
  attr(x, "indices", exact = TRUE)
}

.quo_replace_fn <- function(quo, fn) {
  args <- rlang::call_args(rlang::quo_get_expr(quo))
  rlang::new_quosure(
    rlang::call2(fn, !!!args),
    env = rlang::quo_get_env(quo)
  )
}

.dots_compute_keys <- function(dots_quos, data, slot) {
  dims <- .dim(data)
  names(dims) <- slot

  purrr::imap(dots_quos, function(dot, name) {
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
    stop("The following match keys are missing: ", .quote_collapse(missing), call. = FALSE)

  # Drop unused columns
  dot <- dot[names(curr_key)]

  # Check if sorting is needed else return a single TRUE to skip indexing later
  if (isTRUE(all.equal(curr_key, dot, check.class = FALSE, check.attributes = FALSE)))
    return(TRUE)

  # match() is 10x faster, but produces invalid results when there is >1 key
  if (ncol(curr_key) == 1L) {
    res <- match(dot[[1L]], curr_key[[1L]])
    return(order(res, na.last = NA))
  }

  # Add indices as last column
  dot[[length(dot) + 1L]] <- seq_len(nrow(dot))

  # Use join to match the order
  join_result <- dplyr::inner_join(
    x = curr_key,
    y = dot,
    by = names(curr_key),
    relationship = "one-to-one"
  )

  # Retrieve indices to obtain the reordering
  join_result[[length(join_result)]]
}

.dots_match_indices <- function(dots, use_indices) {
  # Convert missing arguments to NULL
  dots <- purrr::map(dots, function(dot) if (!rlang::is_missing(dot)) dot)

  # DOTS can target indices by name
  have_name <- rlang::have_name(dots)

  # If unnamed arguments are present, the number of dimensions should match
  if (!all(have_name) && length(dots) != length(use_indices)) {
    stop("The number of additional arguments (", length(dots),
         ") should match the dimensions of `i` (", length(use_indices), ")!")
  }

  # Unnamed arguments don't need any matching so we return early
  if (!any(have_name))
    return(dots)

  # Put named arguments in the correct positions
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
  dots <- c(dots, rep(list(NULL), length(use_indices) - length(dots)))
  dots[dots_match_idx] <- named_dots

  dots
}
