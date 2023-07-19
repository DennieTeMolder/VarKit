##' @export
write_vcf <- function(x, file, append = FALSE, replace_filter = FALSE, na_geno = NULL) {
  UseMethod("write_vcf", x)
}

##' @export
write_vcf.varkit <- function(x, file, append = FALSE, replace_filter = FALSE, na_geno = NULL) {
  if (!is.null(na_geno))
    .stopifnotsingle(na_geno, class = "character")

  ## Fixed fields ------
  x <- standardize_cols(x)
  x$site$filter <- .check_filter(x$site$filter, x$use_site, replace_filter)

  ## Genotype fields ------
  is_na <- is.na(x$geno)
  if (all(is_na)) {
    if (is.null(na_geno)) {
      stop("All genotypes are NA, please provide 'na_geno'!")
    } else {
      cli::cli_alert_danger("All genotypes are NA!")
    }
  }

  if (is.null(na_geno)) {
    non_na <- x$geno[which.min(is_na)]
    na_geno <- stringr::str_replace_all(non_na, "[^|/:]+", ".")
    cli::cli_alert_info(c("Representing missing genotypes as: '", na_geno, "'"))
  }
  x$geno[is_na] <- na_geno

  ## Write ------
  if (!append || !file.exists(file)) {
    cli::cli_progress_step("Writing VCF header")
    write_vcf_header(x, file)
    cli::cli_progress_done()
  }

  cli::cli_progress_step("Writing VCF body")
  readr::write_tsv(to_tibble.varkit(x), file, append = TRUE, col_names = FALSE, na = ".")
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
