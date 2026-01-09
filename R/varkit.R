##' @export
varkit <- function(site, geno, meta = NULL, use_filter = TRUE) {
  stopifnot(is.data.frame(site), nrow(site) > 0L)
  if (is.data.frame(geno)) {
    stopifnot(nrow(site) == nrow(geno))
  } else {
    stopifnot(nrow(site) == ncol(geno))
  }
  .stopifnotsingle(use_filter, what = "logical")

  vcf <- validate_varkit(new_varkit(
    site = site,
    geno = geno,
    meta = meta
  ))

  # Apply initial mask based on the FILTER column
  if (use_filter && any(colnames(vcf$site) == "filter")) {
    mask.varkit(vcf, .data$filter != "PASS", na_mask = FALSE)
  } else {
    vcf
  }
}

##' @export
new_varkit <- function(site = data.frame(), geno = varkit_geno(), meta = NULL) {
  stopifnot(is.data.frame(site))
  stopifnot(is.matrix(geno) || is.data.frame(geno))
  stopifnot(is.null(meta) || is.data.frame(meta))

  obj <- linked_data(
    meta[, ], site[sites, ], geno[samples, sites],
    x = list(
      meta = if (!is.null(meta)) tibble::as_tibble(meta),
      site = tibble::as_tibble(site),
      geno = varkit_geno(geno)
    )
  )

  class(obj) <- c("varkit", class(obj))
  obj
}

##' @export
validate_varkit <- function(x, check_info = FALSE, check_format = FALSE) {
  .stopifnotsingle(check_info, what = "logical")
  .stopifnotsingle(check_format, what = "logical")

  old_x <- validate_linked_data(x)
  x <- unclass(x)

  .validate_varkit_meta(x$meta)
  .validate_varkit_site(x$site, check_info = check_info, check_format = check_format)
  .validate_varkit_geno(x$geno)

  old_x
}

.validate_varkit_site <- function(x, check_info, check_format) {
  if (is.null(x))
    stop("The 'site()' slot is missing!")
  if (!tibble::is_tibble(x))
    stop("The 'site()' slot should be a tibble!")
  if (!any(colnames(x) == "chrom"))
    stop("The 'site()' slot should contain the 'chrom' column!")
  if (!is.character(x$chrom))
    stop("The 'chrom' column in the 'site()' slot should be of type character!")
  if (.any_na(x$chrom))
    stop("The 'chrom' column in the 'site()' slot contains NAs!")
  if (!any(colnames(x) == "pos"))
    stop("The 'site()' slot should contain the 'pos' column!")
  if (!is.numeric(x$pos))
    stop("The 'pos' column in the 'site()' slot should be numeric!")
  if (.any_na(x$pos))
    stop("The 'pos' column in the 'site()' slot contains NAs!")
  if (check_info && !any(colnames(x) == "info"))
    stop("The 'site()' slot is missing the 'info' column!")
  if (check_format) {
    if (!any(colnames(x) == "format"))
      stop("The 'site()' slot is missing the 'format' column!")
    if (.any_na(x$format))
      stop("The 'format' column in the 'site()' slot contains NAs!")
  }

  x
}

.validate_varkit_geno <- function(x) {
  if (is.null(x))
    stop("The 'geno()' slot is missing!")
  if (!is_varkit_geno(x))
    stop("The 'geno()' slot should be of class 'varkit_geno'!")
  if (nrow(x) < 1L)
    stop("The 'geno()' slot should contain at least a single sample/row!")
  if (is.null(rownames(x)) || any(nchar(rownames(x)) == 0L))
    stop("The 'geno()' slot cannot have missing/zero-length rownames!")

  x
}

##' @export
is_varkit <- function(x) {
  inherits(x, "varkit")
}

##' @export
print.varkit <- function(x, ..., n = NULL, n_samples = NULL) {
  if (is.null(n_samples))
    n_samples <- 10L
  n_samples <- min(n_samples, n_samples.varkit(x))

  x_geno <- geno.varkit(x)
  if (is.null(n))
    n <- min(10L, .max_print_col(x_geno, row_idx = seq_len(n_samples)))

  .msg_h1("VarKit Variant Object")
  if (!is.null(x$meta)) {
    .msg_h2("Header metadata, `meta(vcf)`")
    print(meta.varkit(x), ...)
    cat("\n")
  }

  .msg_h2("Sample names, `samples(vcf)`")
  .msg_vec(samples.varkit(x))
  cat("\n")

  .msg_h2("Site information, `site(vcf)`")
  print(site.varkit(x), ..., n = n)
  cat("\n")

  .msg_h2("Transposed genotype matrix, `geno(vcf)`")
  print.mtrx(varkit_geno.default(x_geno), n = n_samples, n_col = n, tooltip = FALSE)
  cat("\n")

  .msg_h2("Data masks, `summary(vcf)`")

  current <- .indices(x)$samples
  total <- length(current)
  current <- sum(current)
  samples_masked <- total != current
  if (samples_masked) {
    cli::cli_alert_warning(sprintf(
      "Samples: using %s%% of total (%s/%s)",
      round(current / total * 100, digits = 1), .frmt_big(current), .frmt_big(total)
    ))
  } else {
    cli::cli_alert_info("Samples: using all { .frmt_big(total)} samples")
  }

  current <- .indices(x)$sites
  total <- length(current)
  current <- sum(current)
  sites_masked <- total != current
  if (sites_masked) {
    cli::cli_alert_warning(sprintf(
      "Sites: using %s%% of total (%s/%s)",
      round(current / total * 100, digits = 1), .frmt_big(current), .frmt_big(total)
    ))
  } else {
    cli::cli_alert_info("Sites: using all { .frmt_big(total)} sites")
  }

  if (sites_masked || samples_masked) {
    cli::cli_alert_warning("A mask is active!")
  } else {
    cli::cli_alert_success("No mask is active.")
  }
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
    drm_preselect(
      new_varkit(meta = x$meta, site = split_site[[name]], geno = split_geno[[name]]),
      samples = split_mask$samples,
      sites = split_mask$sites[[name]])
  })
}
