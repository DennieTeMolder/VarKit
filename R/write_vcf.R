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

# REVIEW sample masking information is currently not stored, should we?
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
  if (!is.null(append))
    .stopifnotsingle(append, what = "logical")
  .stopifnotsingle(replace_filter, what = "logical")
  if (!is.null(na_geno))
    stopifnot(is.character(na_geno), length(na_geno) == 1L || length(na_geno) == n_sites(x))
  .stopifnotsingle(timestamp, what = "logical")

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

  # Ensure required columns are present and in the right order
  x <- standardize_cols.varkit(x)

  if (!append) {
    .msg_step("Writing VCF header")
    write_vcf_header.varkit(x, file = file, timestamp = timestamp, compression = compression)
    cli::cli_progress_done()
  }

  # Unclass to prevent data masking
  x <- linked_data_unclass(x)

  # Check if filter column reflects current state, replace if indicated
  x <- .check_filter(x, replace_filter = replace_filter, use_site = .indices(x)$sites)

  # Apply DIGITS argument
  x$site <- .round(x$site, digits = digits)
  x$geno <- .round(x$geno, digits = digits)

  # Infer the correct NA format for the geno fields and replace
  x <- .replace_na_geno(x, na_geno = na_geno)

  # Convert body to a data frame
  x <- cbind(x$site, t(x$geno))

  .msg_step("Writing VCF body")

  write_tbl(
    x,
    file = file,
    append = append,
    col_names = FALSE,
    row_names = FALSE,
    na = ".",
    digits = NULL,
    nsignif = nsignif
  )
}

.replace_na_geno <- function(x, na_geno) {
  which_na <- which(is.na(x$geno), arr.ind = TRUE, useNames = FALSE)
  if (length(which_na) == 0L)
    return(x)

  if (is.null(na_geno)) {
    .msg_step("Determining VCF compliant NA format for geno()")

    # Find all columns with NAs to save on computation time
    na_col <- unique(which_na[, 2L])
    na_geno <- get_first_non_na(x$geno[, na_col], margin = 2)

    # Convert to VCF spec
    na_geno <- stringr::str_replace_all(na_geno, "[^|/:;,]+", ".")
    if (.any_na(na_geno))
      na_geno[is.na(na_geno)] <- "."

    # Expand to a vector that matches the number of columns
    na_geno <- `[<-`(NA_character_, which_na[, 2L], na_geno[match(which_na[, 2L], na_col)])
  }

  # NOTE this step can appear slow for non-character matrices as they need to be
  # cast before replacing, but this time is saved when writing to file.
  .msg_step("Replacing NA geno() values")
  if (has_no_variation(na_geno, na.rm = TRUE)) {
    # Simple case with a single replacement value
    x$geno[which_na] <- na_geno[[1L]]
  } else {
    # Complex case with multiple replacement values (based on column index of NA value)
    x$geno[which_na] <- na_geno[which_na[, 2L]]
  }

  x
}

.check_filter <- function(x, replace_filter, use_site) {
  curr_fltr <- getElement(x$site, "filter")
  is_na <- is.na(curr_fltr)
  if (all(is_na)) {
    x$site$filter <- .if_else(use_site, "PASS", "MASKED")
    return(x)
  }

  # If the state of the filter column matches the current state, return X
  is_pass <- curr_fltr == "PASS" | is_na
  if (identical(is_pass, use_site))
    return(x)

  if (replace_filter) {
    x$site$filter[is_pass] <- "MASKED"
    x$site$filter[use_site] <- "PASS"
  } else {
    cli::cli_alert_warning("FILTER column does not reflect currently masked sites!")
    cli::cli_alert_warning("Use `replace_filter = TRUE` if this is desired.")
  }

  x
}
