.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.parse_col_names <- function(x) {
  col_names <- stringr::str_remove(x, "#")
  col_names <- stringr::str_split(col_names, "\t")[[1]]

  if (length(col_names) < 5)
    stop("The number of columns is too low, make sure the file is TAB delimited.")

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- na.omit(match(fixed_names, tolower(col_names)))
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]

  if (!all(c("chrom", "pos") %in% col_names))
    warning("The VCF header is missing the CHROM and/or POS column(s)!")

  col_names
}

.parse_meta_lines <- function(x) {
  x <- stringr::str_remove(x, "##")
  x <- stringr::str_split_fixed(x, "=", n = 2)
  colnames(x) <- c("name", "value")
  tibble::as_tibble(x)
}

.parse_meta_type <- function(x, type) {
  x <- dplyr::filter(x, name == type)

  if (nrow(x) == 0)
    return(NA)

  # These types can be extended with quoted value pairs, handle them below
  if (!type %in% c("INFO", "FORMAT", "FILTER", "ALT"))
    return(.match_key_value(x$value, pattern = "<?([^=]+)=([^,]+)(?:,|>)"))

  # Split line based on transition from unquoted to quoted values
  x <- stringr::str_match(x$value, "<(.*[^\"]),([^=]+=\".*)>")

  # Parse unquoted and quoted key-value pairs separately
  unquoted <- .match_key_value(
    x[, 2],
    pattern = "([^=]+)=([^,]+)(?:,|$)"
  )
  quoted <- .match_key_value(
    x[, 3],
    pattern = "([^=]+)=\"(.+?)\"(?:,|$)"
  )

  dplyr::bind_cols(unquoted, quoted)
}

.match_key_value <- function(x, pattern) {
  # Match
  x <- stringr::str_match_all(x, pattern)
  # Convert
  x <- t(sapply(x, function(x) x[, -1]))
  # First half == colnames; second half == values
  i <- seq(1, ncol(x) / 2)
  df <- as.data.frame(x[, -i])
  colnames(df) <- x[1, i]
  # Return as tibble
  tibble::tibble(df)
}
