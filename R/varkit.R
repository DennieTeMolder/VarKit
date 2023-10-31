##' @export
varkit <- function(site, geno, meta = NULL) {
  site <- tibble::as_tibble(site)
  stopifnot(nrow(site) > 0L)
  geno <- varkit_geno(geno)
  stopifnot(ncol(geno) == nrow(site))
  if (!is.null(meta))
    meta <- tibble::as_tibble(meta)

  # Apply initial mask based on the FILTER column
  use_site <- FALSE
  if ("filter" %in% rownames(site))
    use_site <- site$filter == "PASS" | is.na(site$filter)
  if (!any(use_site))
    use_site <- rep(TRUE, nrow(site))

  vcf <- new_varkit(meta = meta, site = site, geno = geno, use_site = use_site)
  validate_varkit(vcf)
}

new_varkit <- function(meta = data.frame(), site = data.frame(), geno = matrix(), use_site = logical()) {
  stopifnot(is.data.frame(meta))
  stopifnot(is.data.frame(site))
  stopifnot(is.matrix(geno))
  stopifnot(is.logical(use_site))
  structure(
    list(
      meta = meta,
      site = site,
      geno = geno,
      use_site = use_site
    ),
    class = c("varkit", "varkit_header")
  )
}

validate_varkit <- function(x, check_info = FALSE, check_format = FALSE) {
  # Site
  if (is.null(x$site))
    stop("The 'site' slot is missing!")
  if (!tibble::is_tibble(x$site))
    stop("The 'site' slot should be a tibble!")
  if (!any(colnames(x$site) == "chrom"))
    stop("The 'site' slot should contain the 'chrom' column!")
  if (!is.character(x$site$chrom))
    stop("The 'chrom' column in the 'site' slot should be of type character!")
  if (!any(colnames(x$site) == "pos"))
    stop("The 'site' slot should contain the 'pos' column!")
  if (!is.numeric(x$site$pos))
    stop("The 'pos' column in the 'site' slot should be numeric!")
  if (check_info && !any(colnames(site) == "info"))
    stop("The 'site' slot is missing the 'info' column!")
  if (check_format && !any(colnames(site) == "format"))
    stop("The 'site' slot is missing the 'format' column!")

  # Meta
  if (!is.null(x$meta)) {
    .validate_varkit_meta(x$meta)
    if (check_info && !"INFO" %in% meta$name)
      warning("The 'meta' slot has no INFO entry, functionality is impaired.")
    if (check_format &&!"FORMAT" %in% meta$name)
      warning("The 'meta' slot has no FORMAT entry, functionality is impaired.")
  } else if (check_info || check_format) {
    warning("The 'meta' slot is empty, functionality is impaired")
  }

  # Geno
  if (is.null(x$geno))
    stop("The 'geno' slot is missing!")
  if (!is.matrix(x$geno))
    stop("The 'geno' slot should be a matrix!")
  if (nrow(x$geno) < 1L)
    stop("The 'geno' slot should contain at least a single sample/row!")
  if (is.null(rownames(x$geno)) || any(nchar(rownames(x$geno)) == 0L))
    stop("The 'geno' slot cannot have missing/zero-length rownames!")

  # Use_site
  if (is.null(x$use_site))
    stop("The 'use_site' slot is missing!")
  if (!is.vector(x$use_site) || !is.logical(x$use_site))
    stop("The 'use_site' slot should be a boolean vector!")
  if (all(!x$use_site))
    stop("All elements of slot 'use_site' are FALSE!")

  # Equal dimensions
  if (nrow(x$site) != ncol(x$geno))
    stop("The number of rows in slot 'site' should match the number of column in 'geno'!")
  if (nrow(x$site) != length(x$use_site))
    stop("The length of slot 'use_site' should match the number of rows of 'site'!")

  x
}

##' @export
is_varkit <- function(x) {
  inherits(x, "varkit")
}

##' @export
print.varkit <- function(x, ..., n = NULL, n_samples = 10) {
  n_samples <- min(n_samples, length(samples(x)))
  if (is.null(n)) {
    width <- getOption("width")
    which_show <- utils::head(which(x$use_site), n = max(1, floor(width / 2) - 1))
    n <- .max_print_col(x$geno[seq_len(n_samples), which_show, drop = FALSE], width = width)
  }

  .msg_h1("VarKit Variant Object")
  if (!is.null(x$meta)) {
    .msg_h2("Header metadata, `meta(vcf)`")
    print(meta(x), ..., verbose = FALSE)
    cat("\n")
  }

  .msg_h2("Sample names, `samples(vcf)`")
  samp <- cli::cli_vec(samples(x), style = list("vec-sep" = ", ", "vec-last" = ", ", "vec-trunc" = 100))
  cli::cli_text("{samp}")
  cat("\n")

  .msg_h2("Site information, `site(vcf)`")
  print(site(x), ..., n = n)
  cat("\n")

  .msg_h2("Transposed genotype matrix, `geno(vcf)`")
  print(geno(x), ..., n = n, n_samples = n_samples)
  cat("\n")

  if (all(x$use_site)) {
    total <- .frmt_big(length(x$use_site))
    cli::cli_alert_success("No mask is active, using all {total} sites. See `summary(vcf)`")
  } else {
    total <- length(x$use_site)
    current <- sum(x$use_site)
    cli::cli_alert_warning(sprintf(
      "A mask is active, using %s%% of all sites (%s/%s). See `summary(vcf)`",
      round(current / total * 100, digits = 1), .frmt_big(current), .frmt_big(total)
    ))
  }
}

##' @export
summary.varkit <- function(object, ...) {
  tibble::tibble(
    samples = length(samples(object)),
    sites_visible = sum(object$use_site),
    sites_total = length(object$use_site),
    visible_frac = mean(object$use_site)
  )
}

##' @export
split.varkit <- function(x, f, drop = FALSE, keep_f = FALSE, ...) {
  if (missing(drop))
    drop <- TRUE

  # Add indexes so we can retrace the splits
  x$site$idx <- seq_len(nrow(x$site))

  # Split sites and determine grouping using the index
  split_site <- splitm(x$site, f = f, drop = drop, margin = 1, keep_f = keep_f, ...)
  groups <- purrr::list_rbind(lapply(split_site, function(xx) xx[, "idx"]), names_to = "label")
  groups <- groups$label[order(groups$idx)]

  # Split the other parts of the varkit object in the same way
  split_geno <- splitm(x$geno, f = groups, margin = 2)
  split_use <- split(x$use_site, f = groups)

  # Recreate the varkit object for each split
  splits <- lapply(names(split_geno), function(name) {
    new_varkit(meta = x$meta, site = split_site[[name]], geno = split_geno[[name]], use_site = split_use[[name]])
  })
  setNames(splits, nm = names(split_geno))
}
