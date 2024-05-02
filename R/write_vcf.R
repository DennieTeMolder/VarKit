##' @export
write_vcf <- function(x,
                      file,
                      append = NULL,
                      replace_filter = FALSE,
                      na_geno = NULL,
                      timestamp = TRUE,
                      compression = 5) {
  UseMethod("write_vcf", x)
}

##' @export
write_vcf.varkit <- function(x,
                             file,
                             append = NULL,
                             replace_filter = FALSE,
                             na_geno = NULL,
                             timestamp = TRUE,
                             compression = 5) {
  if (!is.null(na_geno))
    stopifnot(is.character(na_geno), length(na_geno) == 1)
  validate_varkit(x)

  # If FILE is a connection we only append if at start of file (unless specified otherwise)
  if (is.null(append)) {
    if (.is_connection(file)) {
      if (!isSeekable(file))
        stop("`append` cannot be NULL if `file` is a connection & `isSeekable(file)` is FALSE!")
      append <- isTRUE(seek(file) > 0L)
    } else {
      append <- FALSE
    }
  }

  # Open a connection for writing, unless we want to append
  if (!.is_connection(file)) {
    file <- file_con(file, open = if (append) "a" else "w", compression = compression)
    on.exit(close(file))
  }

  # Ensure all columns & values adhere to VCF spec
  x <- standardize_cols.varkit(x)
  x <- .replace_na_geno(x, na_geno)

  # Check if filter column reflects current state, replace if indicated
  x$site$filter <- .check_filter(x$site$filter, use_site = x$use_site, replace_filter = replace_filter)

  # Make sure we write all sites, the FILTER column will reflect if a site should be masked
  x <- unmask_all.varkit(x)

  if (!append) {
    .msg_step("Writing VCF header")
    write_vcf_header.varkit(x, file = file, timestamp = timestamp, compression = compression)
    cli::cli_progress_done()
  }

  .msg_step("Writing VCF body")

  # Convert all sites to a data frame by unmasking
  x <- as.data.frame.varkit(x, optional = TRUE)

  write_tbl(
    x,
    file = file,
    append = append,
    col_names = FALSE,
    row_names = FALSE,
    na = "."
  )
}

.replace_na_geno <- function(x, na_geno) {
  if (!anyNA(x$geno))
    return(x)

  if (is.null(na_geno)) {
    .msg_step("Determining VCF compliant NA format for geno()")

    # Build an index of possible NA replacements, one for each value of FORMAT
    frmt <- x$site[["format"]]
    if (is.null(frmt))
      stop("Conversion of NA values requires the FORMAT column to be present!", call. = FALSE)
    if (anyNA(frmt))
      stop("The FORMAT column contains missing values!", call. = FALSE)

    uniq <- as.character(unique(frmt))
    match_uniq <- match(uniq, frmt)

    non_na <- apply(x$geno[, match_uniq, drop = FALSE], 2, function(site) {
      site[match(FALSE, is.na(site))]
    })

    if (anyNA(non_na)) {
      stop("\nError at index: ", .collapse(match_uniq[is.na(non_na)]),
           "\nCannot determine replacement as all sites are NA!", call. = FALSE)
    }

    na_geno <- stringr::str_replace_all(non_na, "[^|/:;,]+", ".")
    names(na_geno) <- uniq
  }

  if (length(unique(na_geno)) > 1L) {
    # Complex case with multiple replacement values (based on column index of NA value)
    is_na <- which(is.na(x$geno), arr.ind = TRUE, useNames = FALSE)
    x$geno[is_na] <- unname(na_geno)[match(frmt[is_na[, 2L]], names(na_geno))]
  } else if (na_geno[[1L]] != ".") {
    # Simple case, na_geno == "." is handled by write_tbl()
    x$geno[is.na(x$geno)] <- na_geno[[1L]]
  }

  x
}

.check_filter <- function(fltr, use_site, replace_filter) {
  is_na <- is.na(fltr)
  if (all(is_na))
    return(ifelse(use_site, "PASS", "MASKED"))

  if (any(is_na))
    fltr[is_na] <- "PASS"

  pass_filter <- fltr == "PASS"
  if (any(pass_filter != use_site)) {
    if (replace_filter) {
      fltr[use_site] <- "PASS"
      return(ifelse(pass_filter & !use_site, "MASKED", fltr))
    }
    cli::cli_alert_warning("FILTER column does not reflect currently masked sites!")
    cli::cli_alert_warning("Use `replace_filter = TRUE` if this is desired.")
  }
  fltr
}
