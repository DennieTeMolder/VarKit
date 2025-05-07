##' @export
write_vcf <- function(x,
                      file,
                      append = NULL,
                      replace_filter = FALSE,
                      na_geno = NULL,
                      timestamp = TRUE,
                      digits = 6,
                      nsignif = getOption("digits", 7L),
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
                             digits = 6,
                             nsignif = getOption("digits", 7L),
                             compression = 5) {
  validate_varkit(x)
  if (!is.null(na_geno))
    stopifnot(is.character(na_geno), length(na_geno) == 1L || length(na_geno) == n_sites(x))

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
    .defer(close(file))
  }

  # Ensure all columns & values adhere to VCF spec
  x <- standardize_cols.varkit(x)
  x$geno <- .round(x$geno, digits = digits)
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

  # Convert body to a data frame
  x <- as.data.frame.varkit(x, optional = TRUE)

  write_tbl(
    x,
    file = file,
    append = append,
    col_names = FALSE,
    row_names = FALSE,
    na = ".",
    digits = digits,
    nsignif = nsignif
  )
}

.replace_na_geno <- function(x, na_geno) {
  if (!anyNA(x$geno))
    return(x)

  is_na <- which(is.na(x$geno), arr.ind = TRUE, useNames = FALSE)

  if (is.null(na_geno)) {
    .msg_step("Determining VCF compliant NA format for geno()")

    # Find all columns with NAs to save on computation time
    na_col <- unique(is_na[, 2L])
    na_geno <- get_first_non_na(x$geno[, na_col], margin = 2)

    # Convert to VCF spec
    na_geno <- stringr::str_replace_all(na_geno, "[^|/:;,]+", ".")
    if (anyNA(na_geno))
      na_geno[is.na(na_geno)] <- "."

    # Expand to a vector that matches the number of columns
    na_geno <- `[<-`(NA_character_, is_na[, 2L], na_geno[match(is_na[, 2L], na_col)])
  }

  # NOTE this step can appear slow for non-character matrices as they need to be
  # cast before replacing, but this time is saved when writing to file.
  .msg_step("Replacing NA geno() values")
  if (has_no_variation(na_geno, na.rm = TRUE)) {
    # Simple case with a single replacement value
    x$geno[is_na] <- na_geno[[1L]]
  } else {
    # Complex case with multiple replacement values (based on column index of NA value)
    x$geno[is_na] <- na_geno[is_na[, 2L]]
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
