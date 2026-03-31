##' @export
varkit <- function(site, geno, meta = NULL, use_filter = TRUE) {
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

  validate_varkit(new_varkit(
    site = site,
    geno = geno,
    meta = meta,
    site_indices = site_indices
  ))
}

##' @export
new_varkit <- function(site = data.frame(chrom = character(), pos = integer()),
                       geno = varkit_geno(),
                       meta = data.frame(name = character(), value = character()),
                       sample_indices = logical(),
                       site_indices = logical()
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

  obj <- new_linked_data(
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
    )
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

  old_x <- validate_linked_data(x)
  x <- linked_data_unclass(x)

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
  validate_linked_data(x)

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

  # Validate an modify reserved slots
  i_meta <- match("meta", i, nomatch = 0L)
  if (i_meta > 0L) {
    if (!is.data.frame(value[[i_meta]]))
      stop("The '$meta' slot must be a data frame!")
    value[[i_meta]] <- tibble::as_tibble(value[[i_meta]])
  }

  i_site <- match("site", i, nomatch = 0L)
  if (i_site > 0L) {
    if (!is.data.frame(value[[i_site]]))
      stop("The '$site' slot must be a data frame!")
    value[[i_site]] <- tibble::as_tibble(value[[i_site]])
    # REVIEW warn about mask being active if length mismatches?
  }

  i_geno <- match("geno", i, nomatch = 0L)
  if (i_geno > 0L) {
    # Cast equal sized vectors to matrix because some operation drop the matrix class
    if (is.vector(value[[i_geno]])) {
      xgeno <- linked_data_unclass(x)$geno
      if (length(value) == length(xgeno)) {
        warning("Converting '$meta' to matrix, check if the values are in the right row & column.")
        value <- mtrx(value, nrow = nrow(xgeno), ncol = ncol(xgeno), dimnames = dimnames(xgeno))
      }
    } else if (!is.matrix(value[[i_geno]])) {
      stop("The '$geno' slot must be a matrix!")
    }

    value[[i_geno]] <- varkit_geno(value[[i_geno]])
    # REVIEW warn about mask being active if length mismatches?

    # Remove the old value, and attempt to free the 'samples' index so it can be modified
    x <- linked_data_set_nomatch(x, i = "geno", value = list(NULL), validate = FALSE)
    x <- linked_data_trim(x, indices = "samples")
  }

  validate_varkit(linked_data_set_nomatch(x, i = i, value = value, validate = FALSE))
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
  x_print <- linked_data_unclass(x[which_print])

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
  if (!all(names(x) %in% which_print)) {
    .msg_h2("Additional slots")
    .print_slots(x, .exclude = which_print)
  }

  .msg_h2("Soft-mask status, `summary(...)`")
  .print_indices(x)

  invisible(x)
}

##' @export
summary.varkit <- function(object, ...) {
  tibble::tibble(
    samples_visible = n_samples.varkit(object, mask = TRUE),
    samples_total = n_samples.varkit(object, mask = FALSE),
    sites_visible = n_sites.varkit(object, mask = TRUE),
    sites_total = n_sites.varkit(object, mask = FALSE),
    samples_frac = .data$samples_visible / .data$samples_total,
    sites_frac = .data$sites_visible / .data$sites_total
  )
}

##' @export
as.data.frame.varkit <- function(x, row.names = NULL, optional = FALSE, ...) {
  cbind(
    as.data.frame(site.varkit(x), row.names = row.names, optional = optional, ...),
    as.data.frame(t(geno.varkit(x)), optional = optional)
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
  x <- linked_data_unclass(x)

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
  lapply(setNames(nm = names(split_geno)), function(name) {
    linked_data_filter(
      new_varkit(meta = x$meta, site = split_site[[name]], geno = split_geno[[name]]),
      samples = .env$split_mask$samples,
      sites = .env$split_mask$sites[[name]])
  })
}
