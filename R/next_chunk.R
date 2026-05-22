##' @export
next_chunk <- function(x, ...) {
  UseMethod("next_chunk", x)
}

##' @export
next_chunk.varkit_index <- function(x, col_select = NULL, ...) {
  args <- list(...)
  if (!rlang::is_named2(args))
    stop("All additional arguments must be named!")

  # Open index to ensure it is valid
  open.varkit_index(x)
  next_idx <- .seek_next(x)

  # Fetch arguments stored on the index & merge with ...
  if (!is.null(read_vcf_args(x)))
    args <- purrr::list_assign(read_vcf_args(x), !!!args)

  if (is.null(args[["header"]]))
    stop("Could not detect a VCF header! Please supply the 'header' argument.")
  if (!is.null(args[["n_max"]]) && !(is.numeric(args$n_max) && length(args$n_max) == 1L))
    stop("Please provide a single numeric value for the 'n_max' argument!")

  if (!missing(col_select))
    args$col_select <- rlang::enquo(col_select)

  # Ignore negative N_MAX
  if (!isTRUE(args$n_max >= 1L))
    args$n_max <- NULL

  # Stay below N_MAX whilst never reading more than one chunk
  args$n_max <- min(as.integer(args$n_max), next_idx$n_lines, na.rm = TRUE)
  args$file <- .env(x)$con

  vcf <- do.call(read_vcf, args)
  if (is.null(vcf))
    return(NULL)

  n <- n_sites.varkit(vcf, mask = FALSE)

  # Check if we got the expected number of sites as long as we are within the index's range
  if (n != args$n_max && .env(x)$curr_line <= x$linum[nrow(x)]) {
    warning("At index row ", rownames(seek(x)), ": expected ", .frmt_big(args$n_max),
            " sites, but got ", .frmt_big(n), " sites.")
    n <- args$n_max
  }

  # Increment index
  .env(x)$curr_line <- .env(x)$curr_line + n

  vcf
}

.seek_next <- function(index, skip_header = TRUE) {
  pos <- seek.varkit_index(index)

  # If the pointer is outside of an index element, move to the next if possible
  if (is.na(pos$id) && (skip_header || rownames(pos) != "0")) {
    seek.varkit_index(index, pos$linum + max(0L, pos$n_lines))
    pos <- seek.varkit_index(index)
  }

  # NOTE Keep repeating the last element when past the index end.
  # This allows for "open ended" indexes like from index_vcf_header()
  if (pos$n_lines < 1L) {
    pos$n_lines <- index$n_lines[nrow(index)]
  }

  pos
}
