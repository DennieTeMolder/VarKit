##' @export
new_drm <- function(links = NULL, shortnames = NULL, selectors = NULL) {
  if (is.null(links)) links <- list()
  if (is.null(shortnames)) shortnames <- character()
  if (is.null(selectors)) selectors <- list()
  env <- list2env(
    list(links = links, shortnames = shortnames, selectors = selectors),
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
  stopifnot(c("links", "shortnames", "selectors") %in% names(x))
  stopifnot(is.list(x$links))
  stopifnot(is.character(x$shortnames))
  stopifnot(is.list(x$selectors))

  # Requirements for populated models
  if (length(x[["links"]]) > 0L) {
    # drm$links
    stopifnot(rlang::is_named(x$links))
    stopifnot(purrr::map_lgl(x$links, is.character))
    stopifnot(!purrr::map_lgl(x$links, anyNA))

    # drm$selector
    stopifnot(rlang::is_named(x$selectors))
    missing <- setdiff(
      unique(purrr::list_c(x[["links"]])),
      c(names(x$selectors), "")
    )
    if (length(missing) > 0L)
      stop("The following 'dim_links' have no selector: ", .quote_collapse(missing))

    purrr::walk(x$selectors, function(selector) {
      # drm$selector$length
      stopifnot("length" %in% names(selector))
      .stopifnotsingle(selector$length, what = "integer")

      # drm$selector$select_which
      if (!is.null(selector[["select_which"]]))
        stopifnot(is.integer(selector$select_which), !anyNA(selector$select_which))

      # drm$selector$match_on
      match_on <- selector[["match_on"]]
      if (!is.null(match_on)) {
        if (is.data.frame(match_on)) {
          match_len <- nrow(match_on)
        } else if (is.atomic(match_on)) {
          match_len <- length(match_on)
        } else {
          stop("Match keys should be an atomic vector or data frame, not ", class(match_on))
        }
        if (match_len != selector[["length"]]) {
          expected <- .frmt_big(selector[["length"]])
          observed <- .frmt_big(match_len)
          stop("Expected match keys to have length ", expected, ", but found ", observed)
        }
      }
    })
  }
  invisible(x)
}

##' @export
drm_create <- function(links = NULL, shortnames = NULL, selectors = NULL) {
  validate_drm(new_drm(links = links, shortnames = shortnames, selectors = selectors))
}

##' @export
print.data_relation_model <- function(x, ...) {
  .msg_h1("Data relation model <", rlang::env_label(x), ">")
  if (length(x[["selectors"]]) == 0L) {
    message("Empty model...")
  } else {
    .msg_h2("Links")
    print(tibble::tibble(
      name = x[["shortnames"]][names(x[["links"]])],
      dim_links = vapply(x[["links"]], .collapse, character(1L)),
      uuid = names(x[["links"]])
    ))
    cat("\n")

    .msg_h2("Selectors [match_on]")
    purrr::iwalk(x[["selectors"]], function(sset, name) {
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
        if (is.atomic(sset[["match_on"]])) {
          keys <- "*"
        } else {
          keys <- .collapse(names(sset[["match_on"]]), delim = ",")
        }
        keys <- paste0("[", keys, "]")
      }
      cli::cat_bullet(paste0(name, keys, ": ", msg))
    })
  }

  invisible(x)
}

##' @export
drm_add <- function(x, drmodel, dim_links, shortname = NULL) {
  validate_drm(drmodel)
  stopifnot(is.vector(dim_links), is.character(dim_links))
  if (is.null(shortname)) {
    shortname <- deparse(substitute(x))
  } else {
    .stopifnotsingle(shortname, what = "character")
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

  # Add DIM_LINKS to DRMODEL selectors
  x_dim <- .dim(x)
  for (i in seq_along(dim_links)[dim_links != ""]) {
    id <- dim_links[i]
    curr_sel <- drmodel[["selectors"]][[id]]
    if (is.null(curr_sel)) {
      drmodel[["selectors"]][[id]] <- list(length = x_dim[i])
    } else if (x_dim[i] != curr_sel[["length"]]) {
      expected <- .frmt_big(curr_sel[["length"]])
      observed <- .frmt_big(x_dim[i])
      warning("At dimension ", i, "(", id, "): expected length ", expected, ", but got ", observed, "!")
    }
  }

  # Add X and DIM_LINKS to DRMODEL links
  drmodel[["links"]][[uuid]] <- dim_links
  drmodel[["shortnames"]][uuid] <- shortname

  # Add DRMODEL to X
  attr(x, "data_relation_model") <- drmodel
  attr(x, "data_relation_uuid") <- uuid

  x
}

##' @export
drm_remove <- function(x) {
  # Recurse over lists/dataframes
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[[i]] <- drm_remove(x[[i]])
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
drm_preselect <- function(x, ...) {
  drmodel <- validate_drm(drm_get(x))
  if (is.null(drmodel))
    stop(substitute(x), " has no associated data relation model!")
  rlang::check_dots_used()

  dots <- list(...)
  if (!rlang::is_named(dots))
    stop("All additional arguments should be named!")

  # Check elements
  for (current in names(dots)) {
    curr_dots <- dots[[current]]
    curr_sel <- drmodel[["selectors"]][[current]]
    if (is.null(curr_sel))
      stop("At '", current, "': entry not found in data relation model!")
    if (!is.logical(curr_dots))
      stop("At '", current, "': should be a vector of logical values!")

    check_len <- c(length(curr_sel[["select_which"]]), curr_sel[["length"]])
    check_len <- check_len[check_len > 0L]
    if (!any(length(curr_dots) == check_len)) {
      stop("At '", current, "': selector should have a length of ",
           .collapse(.frmt_big(check_len), delim = " or "),
           ", but got ", length(curr_dots))
    }

    if (!any(curr_dots))
      stop("At '", current, "': no elements remaining after applying selector!")
  }

  # Apply selectors
  for (current in names(dots)) {
    curr_dots <- dots[[current]]
    if (length(curr_dots) == drmodel[["selectors"]][[current]][["length"]]) {
      if (all(curr_dots)) {
        drmodel[["selectors"]][[current]][["select_which"]] <- NULL
      } else {
        drmodel[["selectors"]][[current]][["select_which"]] <- which(curr_dots)
      }
    } else {
      drmodel$selectors[[current]]$select_which <- drmodel$selectors[[current]]$select_which[curr_dots]
    }
  }

  x
}

##' @export
drm_forget <- function(x, ..., .forget_select = TRUE, .forget_match = TRUE) {
  drmodel <- validate_drm(drm_get(x))
  if (is.null(drmodel))
    stop(substitute(x), " has no associated data relation model!")
  rlang::check_dots_unnamed()
  .stopifnotsingle(.forget_select, what = "logical")
  .stopifnotsingle(.forget_match, what = "logical")

  # Parse ... for selectors to clear, if empty clear all selectors
  if (...length() > 0L) {
    dots <- as.character(substitute(list(...))[-1L])
    missing <- setdiff(dots, names(drmodel[["selectors"]]))
    if (length(missing) > 0L)
      stop("The following selectors are not recognised: ", .quote_collapse(missing))
  } else {
    dots <- names(drmodel[["selectors"]])
  }

  # Clear selectors
  for (name in dots) {
    if (.forget_select)
      drmodel[["selectors"]][[name]][["select_which"]] <- NULL
    if (.forget_match)
      drmodel[["selectors"]][[name]][["match_on"]] <- NULL
  }

  x
}

##' @export
drm_deselect <- function(x, ...) {
  drm_forget(x, ..., .forget_select = TRUE, .forget_match = FALSE)
}

##' @export
drm_pull <- function(x, ..., drop = FALSE) {
  if (!has_drm(x))
    stop(substitute(x), "has no associated data relation model!")
  drmodel <- validate_drm(drm_get(x))

  # Allow for additional user provided indices, these take precedence
  rlang::check_dots_unnamed()
  dots <- rlang::dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

  links <- drmodel[["links"]][[attr(x, "data_relation_uuid")]]
  if (is.null(links))
    stop("No links found for UUID: ", attr(x, "data_relation_uuid"))

  x_dim <- .dim(x)
  if (length(links) != length(x_dim))
    stop("Data relation model expects ", length(links), "dimensions, but got ", length(x_dim), "!")

  # Extent ... to match the dimensions of X
  dots <- c(dots, rep(list(rlang::missing_arg()), length(x_dim) - length(dots)))

  # Pull selector indices from relation data model
  for (i in seq_along(x_dim)) {
    if (!rlang::is_missing(dots[[i]]) || links[[i]] == "")
      next()

    curr_sel <- drmodel[["selectors"]][[links[[i]]]]
    if (x_dim[i] != curr_sel[["length"]]) {
      stop("At dimension ", i, ": '", links[i], "' expects length ",
          curr_sel[["length"]], ", but got ", x_dim[i])
    }

    if (!is.null(curr_sel[["select_which"]]))
      dots[[i]] <- curr_sel[["select_which"]]
  }

  x <- do.call(`[`, args = c(list(x), dots, list(drop = drop)))

  # Remove DRM attributes as size no longer matches the original object
  drm_remove(x)
}

# REVIEW drop support for atomic arguments? Use single column df instead.
##' @export
drm_match <- function(x, ..., .override = FALSE) {
  if (!has_drm(x))
    stop(substitute(x), "has no associated data relation model!")
  drmodel <- validate_drm(drm_get(x))
  rlang::check_dots_used()

  dots <- list(...)
  if (!rlang::is_named(dots))
    stop("All additional arguments should be named!")

  missing <- setdiff(names(dots), names(drmodel[["selectors"]]))
  if (length(missing) != 0L)
    stop("The following arguments do not match a selector: ", .quote_collapse(missing))

  links <- drmodel[["links"]][[attr(x, "data_relation_uuid")]]
  if (is.null(links))
    stop("No links found for UUID: ", attr(x, "data_relation_uuid"))

  missing <- setdiff(names(dots), links)
  if (length(missing) != 0L)
    stop("The following arguments do not match a dimension: ", .quote_collapse(missing))

  # Validate arguments and cast lists to tibbles
  dots <- purrr::map(dots, function(dot) {
    if (is.list(dot)) {
      if (!rlang::is_named(dot))
        stop("When providing a list, all elements should be named!")
      dot <- tibble::as_tibble(dot)
      is_distinct <- nrow(dot) == nrow(dplyr::distinct(dot))
    } else if (is.atomic(dot)) {
      is_distinct <- anyDuplicated(dot, nmax = length(dot)) == 0L
    } else {
        stop("Additional arguments can only be atomic vectors or lists!")
    }
    if (!is_distinct)
      stop("Provided combination of match keys are not unique!")
    dot
  })

  # If a selector has no stored keys or if .OVERRIDE, store the provided keys
  # Else match the provided keys to the stored ones to match the order
  dots <- purrr::imap(dots, function(dot, name) {
    keys <- if (!.override) drmodel[["selectors"]][[name]][["match_on"]]
    if (is.null(keys)) {
      drmodel[["selectors"]][[name]][["match_on"]] <- drm_remove(dot)
      return(rlang::missing_arg())
    }

    if (typeof(dot) != typeof(keys))
      stop("Data relation model expects a ", typeof(keys), ", but got a ", typeof(dot), "!")
    if (is.data.frame(dot)) {
      missing <- setdiff(names(keys), names(dot))
      if (length(missing) != 0L)
        stop("The following match keys are expected but missing: ", .quote_collapse(missing))
    }

    .match_key <- function(y, name = NULL) {
      key <- if (is.null(name)) keys else keys[[name]]
      res <- match(y, table = key)
      if (anyNA(res))
        stop("Unable to match all key values!")
      res
    }

    if (!is.data.frame(dot)) {
      dot <- .match_key(dot)
    } else {
      dot <- purrr::imap(dot, .match_key)
    }

    do.call(order, args = dot)
  })

  # Match arguments to 'dim_links' of X
  args <- rlang::rep_named(links, list(rlang::missing_arg()))
  args[names(dots)] <- dots
  names(args) <- NULL

  # Apply re-oredering
  do.call(`[`, args = c(list(x), args, list(drop = FALSE)))
}
