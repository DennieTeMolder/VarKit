##' @export
read_tbl <- function(file,
                     col_select = NULL,
                     sep = "\t",
                     col_names = NULL,
                     col_types = NA,
                     row_names = NULL,
                     header = is.null(col_names),
                     as.is = TRUE,
                     na.strings = "NA",
                     nrows = -1,
                     skip = 0,
                     strip.white = TRUE,
                     ...) {
  # Check args
  stopifnot(is.numeric(skip), length(skip) == 1)
  if (skip < 0)
    skip <- 0
  if (is.null(col_names)) {
    if (!header)
      stop("If 'header = FALSE' then 'col_names' must be provided!")
  } else {
    if (anyNA(col_names))
      stop("'col_names' contains missing values!")
    stopifnot(is.character(col_names))
    if (any(nchar(col_names) < 1))
      stop("'col_names' contains empty strings!")

    # We will always provide 'header = FALSE', so we must adjust 'skip' accordingly
    if (header)
      skip <- skip + 1
  }

  # Open the file if needed
  if (inherits(file, "connection")) {
    if (!is_open(file))
      stop("The provided 'file' connection is invalid!")
    con <- file
  } else {
    con <- file(file, "r")
    on.exit(close(con))
  }

  # Detect column names
  if (is.null(col_names)) {
    # NOTE as we are passing ..., we must 'catch' all arguments below in read_tbl()
    col_names <- unlist(utils::read.table(
      file = con, header = FALSE, sep = sep, as.is = TRUE, na.strings = "",
      colClasses = "character", nrows = 1, strip.white = TRUE, ...
    ), use.names = FALSE)
    if (anyNA(col_names))
      stop("\nSome of the header fields are NA, detected fields:\n", .quote_collapse(col_names))
  }

  # The result is always a named character vector, but possibly zero length
  col_types <- .standardize_col_types(col_types, col_names = col_names)

  # Instruct read.delim to ignore cols that are not selected
  col_types[.removed_cols(col_names, col_select = {{ col_select }})] <- "NULL"
  if (all(col_names %in% names(col_types)) && isTRUE(all(col_types == "NULL")))
    stop("All columns will be excluded!")

  if (length(col_types) == 0)
    col_types <- NA

  tibble::as_tibble(utils::read.table(
    file = con, header = FALSE, sep = sep, col.names = col_names, colClasses = col_types,
    row.names = row_names, as.is = as.is, na.strings = na.strings, nrows = nrows,
    skip = skip, strip.white = strip.white, ...
  ))
}

.standardize_col_types <- function(col_types, col_names) {
  if (is.null(col_types) || all(is.na(col_types)))
    return(character())

  if (!(is.vector(col_types) && is.character(col_types)))
    stop("'col_types' must be a character vector!", call. = FALSE)

  if (is.null(names(col_types))) {
    # A single unnamed string might be interpreted as readr::read_* style shorthand
    if (length(col_types) == 1 && .is_expandable(col_types))
      col_types <- strsplit(col_types, "", fixed = TRUE)[[1]]
    if (length(col_types) > length(col_names))
      stop("There are more 'col_types' (", length(col_types), ") then column names (", length(col_names) ,")!")

    # Recycle single length unnamed vectors (>1 is never recycled)
    if (length(col_types) == 1)
      col_types <- rep(col_types, length(col_names))
    names(col_types) <- col_names[seq_along(col_types)]

  } else {
    if (anyDuplicated(names(col_types)))
      stop("The names of 'col_types' are not unique!", call. = FALSE)

    # All named elements should match with col_names
    not_found <- !names(col_types) %in% col_names
    if (any(not_found)) {
      stop("\nThe following 'col_types':\n\t",
           .quote_collapse(names(col_types)[not_found]),
           "\ndid not match with any column names:\n\t",
           .quote_collapse(col_names),
           call. = FALSE)
    }

  }

  # Expand readr::read_* style single character col_type abbreviations
  col_types <- .expand_col_types(col_types)

  # The name '.default' can be used to specify a default type
  is_default <- names(col_types) == ".default"
  if (all(is_default)) {
    col_types <- rep(col_types[1], length(col_names))
    names(col_types) <- col_names
  } else if (any(is_default)) {
    default_type <- col_types[which(is_default)[1]]
    col_types[is_default] <- NULL
    col_types[!col_names %in% names(col_types)] <- default_type
  }

  col_types
}

.is_expandable <- function(col_types) {
  stringr::str_detect(col_types, "^[cindlfDTt?_]+$")
}

# Should be FALSE:
## any(.is_expandable(c("NULL", "logical", "integer", "numeric", "complex", "character", "raw", "factor", "Date", "POSIXct")))

.expand_col_types <- function(col_types) {
  try_expand <- nchar(col_types) == 1
  if (!any(try_expand))
    return(col_types)

  expand_me <- col_types[try_expand]
  expanded <- dplyr::case_match(
    expand_me,
    "c" ~ "character",
    "i" ~ "integer",
    c("n", "d") ~ "numeric",
    "l" ~ "logical",
    "f" ~ "factor",
    "D" ~ "Date",
    c("T", "t") ~ "POSIXct",
    "?" ~ "guess",
    "_" ~ "NULL",
    .ptype = character()
  )

  if (anyNA(expanded)) {
    stop("\nThe following 'col_types' are not recognised:\n\t",
         .quote_collapse(expand_me[is.na(expanded)]), call. = FALSE)
  }

  expanded[expanded == "guess"] <- NA

  col_types[try_expand] <- expanded
  col_types
}

.removed_cols <- function(col_names, col_select = NULL) {
  col_select <- rlang::enquo(col_select)
  if (rlang::quo_is_null(col_select))
    return(character())

  names_df <- matrix(nrow = 0, ncol = length(col_names), dimnames = list(NULL, col_names))
  names_df <- tibble::as_tibble(names_df)

  keep <- colnames(dplyr::select(names_df, !!col_select))
  setdiff(col_names, keep)
}
