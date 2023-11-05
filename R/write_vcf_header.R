##' @export
write_vcf_header <- function(x, file, compression = NULL) {
  UseMethod("write_vcf_header", x)
}

##' @export
write_vcf_header.varkit_header <- function(x, file, compression = NULL) {
  # Warn users about the use of .gz(ip) compressed files
  description <- if (.is_connection(file)) summary.connection(file)$description else file
  if (any(tools::file_ext(description) == c("gz", "gzip"))) {
    cli::cli_alert_warning("Varkit writes regular non-block g-zipped files, which often cannot be indexed by external tools.")
    message("  If you need this functionality, write to plain '.vcf' and compress using an external tool like 'bgzip'.")
  }

  header <- meta.varkit_header(x)

  # Replace fileformat and add info
  header <- rbind(
    data.frame(name = "fileformat", value = "VCFv4.2"),
    header[header$name != "fileformat", ],
    data.frame(
      name = "VarKit",
      value = paste0("Modified and written to file by 'write_vcf()';Date=", date())
    )
  )

  # Standardise the column names of the fixed fields and convert to upper
  x <- standardize_cols.varkit_header(x)
  which_to_upper <- seq_along(.fixed_columns())
  x$col_names[which_to_upper] <- toupper(x$col_names[which_to_upper])

  header <- c(
    paste0("##", header$name, "=", header$value),
    paste0("#", .collapse(x$col_names, "\t"))
  )

  write_lines2(header, file = file, compression = compression)
}

##' @export
write_vcf_header.varkit <- function(x, file, compression = NULL) {
  write_vcf_header.varkit_header(
    new_varkit_header(
      meta = meta.varkit(x),
      col_names = c(colnames(x$site), samples.varkit(x))
    ),
    file = file,
    compression = compression
  )
}
