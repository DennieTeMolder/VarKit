##' @export
write_vcf_header <- function(x, file, timestamp = TRUE, compression = NULL) {
  UseMethod("write_vcf_header", x)
}

##' @export
write_vcf_header.varkit_header <- function(x, file, timestamp = TRUE, compression = NULL) {
  .stopifnotsingle(timestamp, what = "logical")

  header <- x$meta

  if (timestamp) {
    stamp <- data.frame(
      name = "VarKit",
      value = paste0("Written to file by 'varkit::write_vcf()';Version=",
                     utils::packageVersion("varkit"), ";Date=", date())
    )
  } else {
    stamp <- NULL
  }

  # Replace fileformat and add info
  header <- rbind(
    data.frame(name = "fileformat", value = "VCFv4.2"),
    header[header$name != "fileformat", ],
    stamp
  )

  # Standardize the column names of the fixed fields and convert to upper
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
write_vcf_header.varkit <- function(x, file, timestamp = TRUE, compression = NULL) {
  x <- rd_unclass(x)
  write_vcf_header.varkit_header(
    new_varkit_header(
      meta = x$meta,
      col_names = c(colnames(x$site), rownames(x$geno))
    ),
    file = file,
    timestamp = timestamp,
    compression = compression
  )
}
