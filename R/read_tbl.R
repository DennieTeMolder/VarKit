## This is a wrapper of utils::read.table() that supports Readr style
## 'col.select' & 'colClasses' arguments. In contrast to Readr it does not
## extract zipped files to the tempdir(), leaves passed connections open,
## and is more memory efficient (Vroom seems to leaks memory, mainly when
## using *_chunked() functions). Unlike utils::read.table(), colClasses
## are not recycled, instead c(.default = ...) can be specified.
##' @export
read_tbl <- function(file,
                     col.select = NULL,
                     sep = "\t",
                     col.names = NULL,
                     colClasses = NA,
                     header = is.null(col.names),
                     nrows = -1,
                     skip = 0,
                     guess.max = 1000,
                     ...) {
  # Check args
  if (!is.null(col.names))
    stopifnot(is.character(col.names), length(col.names) > 0)
  stopifnot(is.numeric(nrows), length(nrows) == 1)
  stopifnot(is.numeric(guess.max), length(guess.max) == 1)
  col.select <- rlang::enquo(col.select)

  # Disable guessing if fewer rows are requested
  if (nrows > 0L && guess.max >= nrows)
    guess.max <- -1L

  # if col.names is a named vector it doubles as the colClasses argument
  if (!is.null(names(col.names))) {
    if (!rlang::is_named(col.names))
      stop("Either all or none of the values in 'col.names' should be named!")
    if (any(names(col.names) == ".default"))
      stop("The use of '.default' is not allowed in 'col.names'! Use 'colClasses' instead")
    if (!identical(colClasses, NA))
      stop("The use of a named vector/list for 'col.names' is incompatible with 'colClasses'!")

    colClasses <- col.names
    col.names <- names(col.names)
  }

  # We handle SKIP here to save us from figuring out which of the functions below gets to skip
  if (skip > 0L) {
    if (!.is_connection(file)) {
      file <- file(file, "r")
      on.exit(close(file))
    }
    .skip_lines(file, n = skip, progress = FALSE)
  }

  # If missing extract the col.names from the start of the file
  if (is.null(col.names)) {
    col.names <- names(guess_col_classes(
      file = file, nmax = header + 1L, sep = sep, header = header, colClasses = "character", ...
    ))

    # If no col.names are found the file must be an invalid
    if (!is.character(col.names))
      return(NULL)
  }

  # Expand colClasses using readr::cols() style shorthands and store the ".default" value
  default <- NA_character_
  if (length(colClasses) > 0L) {
    colClasses <- .expand_col_classes(colClasses)
    if (any(names(colClasses) == ".default")) {
      default <- colClasses[".default"]
      colClasses <- colClasses[names(colClasses) != ".default"]
    }

    if (rlang::is_named(colClasses)) {
      # Ensure colClasses matches with col.names
      colClasses <- .check_col_classes(colClasses, col.names = col.names)
    } else {
      # read.table will recycle unnamed colClasses vectors which is not desired
      names(colClasses) <- col.names[seq_along(colClasses)]
    }
  }

  # Incorporate the ".default" value in a way that read.table() recognises
  missing <- setdiff(col.names, names(colClasses))
  colClasses <- c(colClasses, rlang::rep_named(missing, default))[col.names]

  # Apply col.select to colClasses & allow columns to be renamed
  colClasses <- .col_select_classes(colClasses, col.select = col.select)
  col.names <- names(colClasses)

  # If any col.names have an unknown class we guess their classes based on guess.max
  # Unknown colClasses are read as character, which inflates the string cache
  # Using a smaller portion of the file improves memory consumption & is up to 33% faster
  if (guess.max >= 1L && anyNA(colClasses)) {
    skip_cols <- names(colClasses)[!is.na(colClasses)]
    skip_cols <- rlang::rep_named(skip_cols, "NULL")
    guessed_classes <- guess_col_classes(
      file = file, nmax = guess.max, sep = sep, header = header,
      col.names = col.names, colClasses = skip_cols, ...
    )
    colClasses[names(guessed_classes)] <- guessed_classes
  }

  tibble::as_tibble(
    .read_tbl_internal(file, col.names = col.names, colClasses = colClasses,
                       sep = sep, header = header, nrows = nrows, ...),
    .name_repair = "minimal"
  )
}

.expand_col_classes <- function(colClasses) {
  stopifnot(is.vector(colClasses), length(colClasses) > 0L)
  # Convert to character but keep colnames
  storage.mode(colClasses) <- "character"

  non_default <- colClasses[rlang::names2(colClasses) != ".default"]
  if (.is_shorthand(non_default)) {
    colClasses <- c(
      strsplit(non_default, "", fixed = TRUE)[[1L]],
      colClasses[match(".default", rlang::names2(colClasses), 0L)]
    )
  }

  try_expand <- nchar(colClasses) == 1L & !is.na(colClasses)
  if (!any(try_expand))
    return(colClasses)

  expand_me <- colClasses[try_expand]
  expanded <- dplyr::case_match(
    expand_me,
    "c" ~ "character",
    "i" ~ "integer",
    c("n", "d") ~ "numeric",
    "l" ~ "logical",
    "f" ~ "factor",
    "D" ~ "Date",
    c("T", "t") ~ "POSIXct",
    c("?", NA) ~ "guess",
    "_" ~ "NULL",
    .ptype = character()
  )

  if (anyNA(expanded)) {
    stop("\nThe following 'colClasses' are not recognised:\n\t",
         .quote_collapse(expand_me[is.na(expanded)]), call. = FALSE)
  }

  expanded[expanded == "guess"] <- NA

  colClasses[try_expand] <- expanded
  colClasses
}

.is_expandable <- function(colClasses) {
  if (!is.character(colClasses))
    return(rep(FALSE, length(colClasses)))
  stringr::str_detect(colClasses, "^[cindlfDTt?_]+$") & !is.na(colClasses)
}

# Should be FALSE:
## any(.is_expandable(c("NULL", "logical", "integer", "numeric", "complex", "character", "raw", "factor", "Date", "POSIXct")))

.is_shorthand <- function(colClasses) {
  length(colClasses) == 1L         &&
    !rlang::is_named(colClasses)   &&
    isTRUE(nchar(colClasses) > 1L) &&
    .is_expandable(colClasses)
}

.check_col_classes <- function(colClasses, col.names) {
  if (length(colClasses) > length(col.names)) {
    warning(sprintf(
      "There are more 'colClasses' (%i) then column names (%i)",
      length(colClasses), length(col.names)
    ))
  }

  not_found <- !names(colClasses) %in% col.names
  if (any(not_found)) {
    warning("\nThe following 'colClasses':\n\t",
            .quote_collapse(names(colClasses)[not_found]),
            "\ndid not match with any column names:\n\t",
            .quote_collapse(col.names), call. = FALSE)
    colClasses <- colClasses[!not_found]
  }

  colClasses
}

.col_select_classes <- function(colClasses, col.select) {
  if (rlang::quo_is_null(col.select))
    return(colClasses)

  if (is.null(names(colClasses)))
    stop("Unexpected!")

  .data <- colClasses
  loc <- tidyselect::eval_select(col.select, data = .data)

  colClasses[-loc] <- "NULL"
  names(colClasses)[loc] <- names(loc)

  colClasses
}

.read_tbl_internal <- function(file, ...) {
  tryCatch({
    utils::read.table(file = file, ...)
  }, error = function(e) {
    if (startsWith(e$message, "scan()")) {
      msg <- stringr::str_remove(e$message, stringr::fixed("scan()"))
      stop("Value in 'file' did not match 'colClasses':", msg,
           "\nCheck the value of 'na.strings' or increase 'guess.max'.", call. = FALSE)
    }
    stop(e)
  })
}
