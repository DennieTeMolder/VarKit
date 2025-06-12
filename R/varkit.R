##' @export
varkit <- function(site, geno, meta = NULL, drmodel = NULL) {
  stopifnot(nrow(site) > 0L)
  stopifnot(ncol(geno) == nrow(site))
  if (is.null(drmodel)) {
    drmodel <- drm_create()
  } else {
    stopifnot(is_drm(drmodel))
  }

  vcf <- validate_varkit(new_varkit(
    meta = meta,
    site = site,
    geno = geno,
    drmodel = drmodel
  ))

  # Apply initial mask based on the FILTER column
  use_site <- FALSE
  if (any(colnames(site) == "filter"))
    use_site <- site$filter == "PASS" | is.na(site$filter)
  if (!any(use_site))
    use_site <- rep(TRUE, nrow(site))

  drm_preselect(drmodel, sites = use_site)
  vcf
}

##' @export
new_varkit <- function(site = data.frame(), geno = varkit_geno(), meta = NULL, drmodel = drm_create()) {
  stopifnot(is.null(meta) || is.data.frame(meta))
  stopifnot(is.data.frame(site))
  stopifnot(is.matrix(geno))
  stopifnot(is_drm(drmodel))

  site <- drm_do_attach(site, drmodel = drmodel, dim_links = c("sites", NA), label = "site()")
  geno <- drm_do_attach(geno, drmodel = drmodel, dim_links = c("samples", "sites"), label = "geno()")

  structure(
    list(
      meta = if (is.null(meta)) NULL else tibble::as_tibble(meta),
      site = tibble::as_tibble(site),
      geno = varkit_geno(geno)
    ),
    class = "varkit",
    data_relation_model = drmodel
  )
}

##' @export
validate_varkit <- function(x, check_info = FALSE, check_format = FALSE) {
  # Site
  if (is.null(x$site))
    stop("The 'site()' slot is missing!")
  if (!tibble::is_tibble(x$site))
    stop("The 'site()' slot should be a tibble!")
  if (!has_drm(x$site))
    stop("The 'site()' slot has no associated data relation model!")
  if (!drm_contains(drm_get(x$site), x$site))
    stop("The associated data relation model no longer contains 'site()'!")
  if (!any(colnames(x$site) == "chrom"))
    stop("The 'site()' slot should contain the 'chrom' column!")
  if (!is.character(x$site$chrom))
    stop("The 'chrom' column in the 'site()' slot should be of type character!")
  if (anyNA(x$site$chrom))
    stop("The 'chrom' column in the 'site()' slot contains NAs!")
  if (!any(colnames(x$site) == "pos"))
    stop("The 'site()' slot should contain the 'pos' column!")
  if (!is.numeric(x$site$pos))
    stop("The 'pos' column in the 'site()' slot should be numeric!")
  if (anyNA(x$site$pos))
    stop("The 'pos' column in the 'site()' slot contains NAs!")
  if (check_info && !any(colnames(x$site) == "info"))
    stop("The 'site()' slot is missing the 'info' column!")
  if (check_format) {
    if (!any(colnames(x$site) == "format"))
      stop("The 'site()' slot is missing the 'format' column!")
    if (anyNA(x$site$format))
      stop("The 'format' column in the 'site()' slot contains NAs!")
  }

  # Meta
  if (!is.null(x$meta)) {
    .validate_varkit_meta(x$meta)
  }

  # Geno
  if (is.null(x$geno))
    stop("The 'geno()' slot is missing!")
  if (!is_varkit_geno(x$geno))
    stop("The 'geno()' slot should be of class 'varkit_geno'!")
  if (!has_drm(x$geno))
    stop("The 'geno()' slot has no associated data relation model!")
  if (!drm_contains(drm_get(x$geno), x$geno))
    stop("The associated data relation model no longer contains 'geno()'!")
  if (nrow(x$geno) < 1L)
    stop("The 'geno()' slot should contain at least a single sample/row!")
  if (is.null(rownames(x$geno)) || any(nchar(rownames(x$geno)) == 0L))
    stop("The 'geno()' slot cannot have missing/zero-length rownames!")

  # Data relation model
  if (is.null(drm_get(x)))
    stop("The 'varkit' object has no data relation model!")
  if (!identical(drm_get(x$site), drm_get(x)))
    stop("The 'site()' slot is associated with the wrong data relation model!")
  if (!identical(drm_get(x$geno), drm_get(x)))
    stop("The 'geno()' slot is associated with the wrong data relation model!")

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
  print(x_geno, n = n_samples, n_col = n)
  cat("\n")

  .msg_h2("Data masks, `summary(vcf)`")

  total <- drm_n_total(x$geno, "samples")
  current <- drm_n_selected(x$geno, "samples")
  samples_masked <- total != current
  if (samples_masked) {
    cli::cli_alert_warning(sprintf(
      "Samples: using %s%% of total (%s/%s)",
      round(current / total * 100, digits = 1), .frmt_big(current), .frmt_big(total)
    ))
  } else {
    cli::cli_alert_info("Samples: Using all { .frmt_big(total)} samples")
  }

  total <- drm_n_total(x$geno, "sites")
  current <- drm_n_selected(x$geno, "sites")
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
  validate_varkit(x)
  cbind(
    as.data.frame(site.varkit(x), row.names = row.names, optional = optional, ...),
    as.data.frame(t.varkit_geno(geno.varkit(x)), optional = optional)
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
