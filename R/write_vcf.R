##' @export
write_vcf <- function(x, file, append = FALSE, replace_filter = FALSE, compression = NULL) {
  UseMethod("write_vcf", x)
}

##' @export
write_vcf.varkit <- function(x, file, append = FALSE, replace_filter = FALSE, compression = NULL) {
  validate_varkit(x)

  # Open a connection for writing, unless we want to append
  if (.is_connection(file)) {
    if (isSeekable(file) && seek(file) == 0L)
      append <- FALSE
  } else {
    file <- file_con(file, open = if (append) "a" else "w", compression = compression)
    on.exit(close(file))
    append <- FALSE
  }

  # Ensure all columns & values adhere to VCF spec
  x <- standardize_cols.varkit(x)
  x <- .replace_na_geno(x)

  # Check if filter column reflects current state, replace if indicated
  x$site$filter <- .check_filter(x$site$filter, use_site = x$use_site, replace_filter = replace_filter)

  if (!append) {
    cli::cli_progress_step("Writing VCF header")
    write_vcf_header.varkit(x, file = file)
    cli::cli_progress_done()
  }

  cli::cli_progress_step("Writing VCF body")

  # Convert all sites to a data frame by unmasking
  x <- as.data.frame.varkit(unmask_all.varkit(x), optional = TRUE)

  write_tbl(
    x,
    file = file,
    append = append,
    col_names = FALSE,
    row_names = FALSE,
    na = ".",
  )
}

.replace_na_geno <- function(x) {
  if (!anyNA(x$geno))
    return(x)

  cli::cli_progress_step("Reformatting missing values according to VCF spec")

  # Build an index of possible NA replacements, one for each value of FORMAT
  frmt <- x$site$format
  if (is.null(frmt))
    stop("Conversion of NA values requires the FORMAT column to be present!", call. = FALSE)
  if (anyNA(frmt))
    stop("The FORMAT column contains missing values!", call. = FALSE)

  uniq <- as.character(unique(frmt))
  match_uniq <- match(uniq, frmt)

  non_na <- apply(x$geno[, match_uniq, drop = FALSE], 2, function(site) {
    is_na <- is.na(site)
    if (all(is_na))
      return(NA)
    site[which.min(is_na)]
  })

  if (any(is.na(non_na))) {
    stop("\nError at index: ", .collapse(match_uniq[is.na(non_na)]),
         "\nCannot determine replacement as all sites are NA!", call. = FALSE)
  }

  replace_na <- stringr::str_replace_all(non_na, "[^|/:;,]+", ".")
  names(replace_na) <- uniq

  if (length(unique(replace_na)) == 1) {
    x$geno[is.na(x$geno)] <- replace_na[[1]]
  } else {
    replacements <- replace_na[frmt]
    i <- 0
    x$geno <- apply_sites(x$geno, function(site) {
      i <<- i + 1
      site[is.na(site)] <- replacements[[i]]
      site
    })
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
