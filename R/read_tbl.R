## This is a wrapper of utils::read.table() that supports Readr style
## 'col_select' & 'col_types' arguments. In contrast to Readr it does not
## extract zipped files to the tempdir(), leaves passed connections open,
## and is more memory efficient (Vroom seems to leaks memory, mainly when
## using *_chunked() functions). Unlike read.table's colClasses, col_types
## are not recycled, instead c(.default = ...) can be specified.
##' @export
read_tbl <- function(file,
                     col_select = NULL,
                     sep = "\t",
                     col_names = NULL,
                     col_types = NA,
                     header = is.null(col_names),
                     as.is = TRUE,
                     na.strings = "NA",
                     nrows = -1,
                     skip = 0,
                     strip.white = TRUE,
                     ...) {
  # Check args
  if (!is.null(col_names)) {
    stopifnot(is.character(col_names))
    if (anyNA(col_names))
      stop("'col_names' contains missing values!")
    if (any(nchar(col_names) < 1L))
      stop("'col_names' contains empty strings!")
    if (anyDuplicated(col_names) != 0L)
      stop("'col_names' contains duplicated values!")
  }
  if (is.null(col_types) || length(col_types) == 0L || all(is.na(col_types))) {
    col_types <- NULL
  } else {
    stopifnot(is.vector(col_types), is.character(col_types))
    if (.col_names_required(col_types) && !header && is.null(col_names))
        stop("If 'col_types' is named, either 'header' should be TRUE or 'col_names' should be provided!")
  }

  # Open the file if needed
  if (inherits(file, "connection")) {
    if (!is_open.connection(file))
      stop("The provided 'file' connection is invalid!")
    con <- file
  } else {
    con <- file(file, "r")
    on.exit(close(con))
  }

  # Detect column names
  if (header) {
    if (is.null(col_names)) {
      # NOTE as we are passing ..., we must 'catch' all arguments below in read_tbl()
      col_names <- unlist(utils::read.table(
        file = con, header = FALSE, sep = sep, as.is = TRUE, na.strings = "",
        colClasses = "character", nrows = 1, strip.white = TRUE, ...
      ), use.names = FALSE)
      if (anyNA(col_names))
        stop("\nSome of the header fields are NA, detected fields:\n", .quote_collapse(col_names))
    } else {
      # NOTE We want the header arg to be missing in read.table() to correctly handle rownames,
      # Therefore we must adjust 'skip' accordingly
      skip <- max(0, skip) + 1
    }
  }

  # If col_names is not NULL col_types will be named, can be zero length 
  col_types <- .standardize_col_types(col_types, col_names = col_names)

  # Instruct read.delim to ignore cols that are not selected
  col_types[.removed_cols(col_names, col_select = {{ col_select }})] <- "NULL"

  if (length(col_types) == 0)
    col_types <- NA

  # HACK read.table() needs col.names to be missing not NULL
  # Other then that the rest these statements should be identical
  if (is.null(col_names)) {
    tbl <- utils::read.table(
      file = con, sep = sep,                        colClasses = col_types,
      as.is = as.is, na.strings = na.strings, nrows = nrows, skip = skip,
      strip.white = strip.white, ...
    )
  } else {
    tbl <- utils::read.table(
      file = con, sep = sep, col.names = col_names, colClasses = col_types,
      as.is = as.is, na.strings = na.strings, nrows = nrows, skip = skip,
      strip.white = strip.white, ...
    )
  }
  tibble::as_tibble(tbl)
}

.col_names_required <- function(col_types) {
  rlang::is_named(col_types) && !identical(names(col_types), ".default")
}

.standardize_col_types <- function(col_types, col_names) {
  if (is.null(col_types))
    return(character())

  # This is a special case in which col_types is 'recycled' to the table size
  if (identical(names(col_types), ".default")) {
    if (is.null(col_names)) {
      names(col_types) <- NULL
    } else {
      col_types <- rep(col_types, length(col_names))
      names(col_types) <- col_names
    }

    # Expand readr::read_* style single character col_type abbreviations
    return(.expand_col_types(col_types))
  }

  # The name '.default' can be used to specify a default type, only works when colnames are known
  if (".default" %in% names(col_types)) {
    if (is.null(col_names)) stop("An unexpected error occurred!")
    default_type <- col_types[".default"]
    col_types <- col_types[names(col_types) != ".default"]
  } else {
    default_type <- NULL
  }

  # When not recycling, the result is always named
  if (rlang::is_named(col_types)) {
    # For named vectors we do some validation to provide more informative errors then read.table()
    if (is.null(col_names)) stop("An unexpected error occurred!")
    if (anyDuplicated(names(col_types)) != 0L)
      stop("The names of 'col_types' are not unique!")

    # All named elements should match with col_names
    not_found <- !names(col_types) %in% col_names
    if (any(not_found)) {
      warning("\nThe following 'col_types':\n\t",
              .quote_collapse(names(col_types)[not_found]),
              "\ndid not match with any column names:\n\t",
              .quote_collapse(col_names), call. = FALSE)
      col_types <- col_types[!not_found]
    }
  } else {
    # For unnamed vectors we support a readr::read_* style shorthand
    if (length(col_types) == 1 && .is_expandable(col_types))
      col_types <- strsplit(col_types, "", fixed = TRUE)[[1]]

    # Add names to the vector, this inhibits recycling by read.table()
    if (is.null(col_names)) {
      names(col_types) <- paste0("V", seq_along(col_types))
    } else {
      if (length(col_types) > length(col_names)) {
        warning("There are more 'col_types' (", length(col_types), ") then column names (", length(col_names) ,")!")
        col_types <- col_types[seq_along(col_names)]
      }
      names(col_types) <- col_names[seq_along(col_types)]
    }
  }

  # Add default col_type for all col_names that are not in names(col_types)
  if (!is.null(default_type))
    col_types[setdiff(col_names, names(col_types))] <- default_type

  # Expand readr::read_* style single character col_type abbreviations
  .expand_col_types(col_types)
}

.expand_col_types <- function(col_types) {
  should_expand <- nchar(col_types) == 1
  if (!any(should_expand))
    return(col_types)

  expand_me <- col_types[should_expand]
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

  col_types[should_expand] <- expanded
  col_types
}

.is_expandable <- function(col_types) {
  stringr::str_detect(col_types, "^[cindlfDTt?_]+$")
}

# Should be FALSE:
## any(.is_expandable(c("NULL", "logical", "integer", "numeric", "complex", "character", "raw", "factor", "Date", "POSIXct")))

.removed_cols <- function(col_names, col_select) {
  col_select <- rlang::enquo(col_select)
  if (rlang::quo_is_null(col_select))
    return(character())

  if (is.null(col_names))
    stop("When using 'col_select', either 'header' should be TRUE or 'col_names' should be provided!", call. = FALSE)

  names_df <- matrix(nrow = 0, ncol = length(col_names), dimnames = list(NULL, col_names))
  names_df <- tibble::as_tibble(names_df)

  keep <- colnames(dplyr::select(names_df, !!col_select))
  setdiff(col_names, keep)
}
