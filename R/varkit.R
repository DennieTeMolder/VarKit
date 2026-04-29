##' @export
varkit <- function(site, geno, meta = NULL, use_filter = TRUE, sample_keys = TRUE, site_keys = TRUE) {
  stopifnot(is.data.frame(site))
  stopifnot(is.matrix(geno))
  if (nrow(site) != ncol(geno))
    stop("The rows of 'site' (", nrow(site), ") should match 'geno' (", ncol(geno), ")!")
  if (isTRUE(all(nchar(rownames(geno)) > 0L)))
  .stopifnotsingle(use_filter, what = "logical")

  # Initial mask based on the FILTER column
  site_indices <- if (use_filter && "filter" %in% names(site)) {
    site$filter == "PASS" | is.na(site$filter)
  }

  # Add default keys match_rd(), TRUE values indicate the default value should be used
  if (isTRUE(sample_keys) && .valid_names(rownames(geno), check_dup = TRUE)) {
    sample_keys <- tibble::tibble(name = rownames(geno))
  } else {
    sample_keys <- NULL
  }

  if (isTRUE(site_keys)) {
    if ("id" %in% colnames(site) && .valid_names(site$id, check_dup = TRUE)) {
      site_keys <- "id"
    } else if (all(c("chrom", "pos") %in% colnames(site))) {
      site_keys <- c("chrom", "pos")
    }
  }
  if (is.character(site_keys))
    site_keys <- site[site_keys]
  if (!is.data.frame(site_keys) || dplyr::n_distinct(site_keys, na.rm = TRUE) != nrow(site))
    site_keys <- NULL

  validate_varkit(new_varkit(
    site = site,
    geno = geno,
    meta = meta,
    site_indices = site_indices,
    sample_keys = sample_keys,
    site_keys = site_keys
  ))
}

##' @export
new_varkit <- function(site = data.frame(chrom = character(), pos = integer()),
                       geno = varkit_geno(),
                       meta = data.frame(name = character(), value = character()),
                       sample_indices = logical(),
                       site_indices = logical(),
                       sample_keys = NULL,
                       site_keys = NULL
                       ) {
  stopifnot(is.data.frame(site))
  stopifnot(is.matrix(geno))
  if (missing(meta) || is.null(meta)) {
    meta <- data.frame(name = character(), value = character())
  } else {
    stopifnot(is.data.frame(meta))
  }
  if (missing(site_indices) || is.null(site_indices)) {
    site_indices <- rep(TRUE, ncol(geno))
  } else {
    stopifnot(is.logical(site_indices))
  }
  if (missing(sample_indices) || is.null(sample_indices)) {
    sample_indices <- rep(TRUE, nrow(geno))
  } else {
    stopifnot(is.logical(sample_indices))
  }
  stopifnot(nrow(site) == ncol(geno))
  stopifnot(nrow(site) == length(site_indices))
  stopifnot(nrow(geno) == length(sample_indices))

  keys <- list()
  if (!is.null(sample_keys)) {
    stopifnot(is.data.frame(sample_keys), nrow(sample_keys) == nrow(geno))
    keys$samples <- tibble::as_tibble(sample_keys)
  }
  if (!is.null(site_keys)) {
    stopifnot(is.data.frame(site_keys), nrow(site_keys) == nrow(site))
    keys$sites <- tibble::as_tibble(site_keys)
  }

  obj <- new_related_data(
    x = list(
      meta = tibble::as_tibble(meta),
      site = tibble::as_tibble(site),
      geno = varkit_geno(geno)
    ),
    slots = list(
      meta = c("", ""),
      site = c("sites", ""),
      geno = c("samples", "sites")
    ),
    indices = list(
      samples = sample_indices,
      sites = site_indices
    ),
    keys = keys
  )

  class(obj) <- c("varkit", class(obj))
  obj
}

##' @export
is_varkit <- function(x) {
  inherits(x, "varkit")
}

##' @export
validate_varkit <- function(x, check_info = FALSE, check_format = FALSE) {
  .stopifnotsingle(check_info, what = "logical")
  .stopifnotsingle(check_format, what = "logical")
  stopifnot(is_varkit(x))

  old_x <- validate_related_data(x)
  x <- rd_unclass(x)

  .validate_varkit_meta(x$meta)
  .validate_varkit_site(x$site, check_info = check_info, check_format = check_format)
  .validate_varkit_geno(x$geno)

  old_x
}

.validate_varkit_site <- function(x, check_info, check_format) {
  if (is.null(x))
    stop("The '$site' slot is missing!")
  if (!tibble::is_tibble(x))
    stop("The '$site' slot should be a tibble!")
  if (!any(colnames(x) == "chrom"))
    stop("The '$site$chrom' column is missing!")
  if (!is.character(x$chrom))
    stop("The '$site$chrom' column should be of type character!")
  if (.any_na(x$chrom))
    stop("The '$site$chrom' column contains NAs!")
  if (!any(colnames(x) == "pos"))
    stop("The '$site$pos' column is missing!")
  if (!is.numeric(x$pos))
    stop("The '$site$pos' column should be numeric!")
  if (.any_na(x$pos))
    stop("The '$site$pos' column contains NAs!")

  if (check_info && !any(colnames(x) == "info"))
    stop("The '$site$info' column is missing!")

  if (check_format) {
    if (!any(colnames(x) == "format"))
      stop("The '$site$format' column is missing!")
    if (.any_na(x$format))
      stop("The '$site$format' column contains NAs!")
  }

  x
}

.validate_varkit_geno <- function(x) {
  if (is.null(x))
    stop("The '$geno' slot is missing!")
  if (!is_mtrx(x))
    stop("The '$geno' slot should be of class 'mtrx'!")
  if (!isTRUE(all(nchar(rownames(x)) > 0L)))
    stop("The '$geno' slot cannot have missing/zero-length rownames!")

  x
}

##' @export
`[<-.varkit` <- function(x, i, ..., value) {
  validate_related_data(x)

  # Attempt to convert I to character using names(X)
  if (!is.character(i)) {
    i <- names(x)[i]
    if (is.null(i) || isTRUE(all(nchar(i) > 0L)))
      stop("New values must be assigned by name!")
  }

  # Only recycle VALUE if it is of length 1
  value <- as.list(value)
  if (length(value) == 1L && length(i) > 1L) {
    value <- rep(value, length(i))
  } else if (length(i) != length(value)) {
    stop("Number of items to replace should match replacement length!")
  }
  names(value) <- i

  # Validate an modify reserved slots
  if (any(names(value) == "meta")) {
    if (!is.data.frame(value$meta))
      stop("The '$meta' slot must be a data frame!")
    value$meta <- tibble::as_tibble(value$meta)
  }

  if (any(names(value) == "site")) {
    if (!is.data.frame(value$site))
      stop("The '$site' slot must be a data frame!")
    value$site <- tibble::as_tibble(value$site)
  }

  if (any(names(value) == "geno")) {
    geno_old <- rd_unclass(x)$geno

    # Cast equal sized vectors to matrix because some operations drop the matrix class
    if (is.vector(value$geno)) {
      if (!is.null(geno_old) && length(value$geno) == length(geno_old)) {
        value$geno <- mtrx(
          value$geno,
          nrow = nrow(geno_old),
          ncol = ncol(geno_old),
          dimnames = dimnames(geno_old)
        )
      }
    } else if (!is.matrix(value$geno)) {
      stop("The '$geno' slot must be a matrix!")
    }

    value$geno <- varkit_geno(value$geno)

    # Support dropping samples by modifying $geno, this requires freeing the 'samples' index
    if (any(dim(value$geno) != dim(geno_old))) {
      # If no other slots are using the 'samples' indices, clear them so they can be modified
      slots_to_check <- attr(x, "slots")
      exclude_slots <- union("geno", names(which(purrr::map_lgl(rd_unclass(x), is.null))))
      slots_to_check[exclude_slots] <- NULL
      if (!any("samples" == purrr::list_c(slots_to_check)))
        attr(x, "indices")$samples <- NULL
    }
  }

  validate_varkit(rd_set_nomatch(x, i = i, value = value, validate = FALSE))
}

##' @export
`[[<-.varkit` <- function(x, i, value) {
  stopifnot(length(i) == 1L)
  `[<-.varkit`(x, i, value = list(value))
}

##' @export
`$<-.varkit` <- function(x, name, value) {
  stopifnot(is.character(name), length(name) == 1L)
  `[<-.varkit`(x, name, value = list(value))
}

##' @export
print.varkit <- function(x, ..., n = NULL, n_samples = NULL) {
  # Disable tooltips as they refer to the wrong arguments
  rlang::local_options(
    pillar.advice = FALSE,
    mtrx.advice = FALSE
  )

  # Fetch slots to print
  which_print <- c("meta", "site", "geno")
  x_print <- rd_unclass(x[which_print])

  # Number of samples to print
  if (is.null(n_samples))
    n_samples <- 10L
  n_samples <- min(n_samples, nrow(x_print$geno))
  sample_trunc <- n_samples < nrow(x_print$geno)

  # Number of sites to print
  if (is.null(n))
    n <- min(10L, .max_print_col(x_print$geno, row_idx = seq_len(n_samples)))
  site_trunc <- n < ncol(x_print$geno)

  .msg_h1("VarKit Variant Object")

  .msg_h2("$meta, Metadata header")
  print(x_print$meta, ...)
  cat("\n")

  .msg_h2("$site, Site information")
  print(x_print$site, ..., n = n)
  if (site_trunc) .msg_info("Use `print(n = ...)` to see more sites")
  cat("\n")

  .msg_h2("$geno, Transposed genotype matrix")
  print(x_print$geno, n = n_samples, n_col = n)
  if (site_trunc && sample_trunc) {
    .msg_info("Use `print(n = ..., n_samples = ...)` to see more sites and samples respectively")
  } else if (site_trunc) {
    .msg_info("Use `print(n = ...)` to see more sites")
  } else if (sample_trunc) {
    .msg_info("Use `print(n_samples = ...)` to see more samples")
  }
  cat("\n")

  # Summarize slots not printed, if any
  if (!all(names(attr(x, "slots")) %in% which_print)) {
    .msg_h2("Additional slots")
    .print_slots(x, .exclude = which_print)
  }

  .msg_h2("Soft-mask status, `summary(...)`")
  .print_indices(x)

  invisible(x)
}

##' @export
summary.varkit <- function(object, ...) {
  validate_varkit(object)
  class(object) <- c("related_data", "list")
  summary(object)
}

##' @export
as.data.frame.varkit <- function(x, row.names = NULL, optional = FALSE, ...) {
  cbind(
    as.data.frame(x$site, row.names = row.names, optional = optional, ...),
    as.data.frame.list(asplit(x$geno, MARGIN = 1), optional = optional)
  )
}

##' @exportS3Method tibble::as_tibble
as_tibble.varkit <- function(x, ..., optional = TRUE) {
  tibble::as_tibble(as.data.frame.varkit(x, optional = optional), ...)
}

##' @export
split.varkit <- function(x, f, drop = FALSE, ...) {
  if (missing(drop))
    drop <- TRUE
  x <- rd_unclass(x)

  # Add indexes so we can retrace the splits
  x$site$idx <- seq_len(nrow(x$site))

  # Split sites and determine grouping using the index
  split_site <- splitm(x$site, f = f, drop = drop, margin = 1, keep_f = TRUE, ...)
  groups <- purrr::list_rbind(lapply(split_site, function(xx) xx[, "idx"]), names_to = "label")
  groups <- groups$label[order(groups$idx)]

  # Split the other parts of the varkit object in the same way
  split_geno <- splitm(x$geno, f = groups, margin = 2)
  split_mask <- export_mask.varkit(x)
  split_mask$sites <- split(split_mask$sites, f = groups)

  # Recreate the varkit object for each split
  # TODO split keys
  lapply(setNames(nm = names(split_geno)), function(name) {
    rd_filter(
      new_varkit(meta = x$meta, site = split_site[[name]], geno = split_geno[[name]]),
      samples = .env$split_mask$samples,
      sites = .env$split_mask$sites[[name]])
  })
}
