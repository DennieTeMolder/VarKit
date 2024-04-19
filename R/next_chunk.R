##' @export
next_chunk <- function(x, ...) {
  UseMethod("next_chunk", x)
}

##' @export
next_chunk.varkit_index <- function(x, col_select = NULL, ...) {
  args <- list(...)
  if (!rlang::is_named2(args))
    stop("All additional arguments must be named!")

  open.varkit_index(x)
  n_max <- .seek_next(x)$n_lines

  # Fetch arguments stored on the index & merge with ...
  if (exists("read_vcf_args", envir = env(x)))
    args <- purrr::list_assign(env(x)$read_vcf_args, !!!args)

  if (is.null(args[["header"]]))
    stop("Could not detect a VCF header! Please supply the 'header' argument.")

  # Arguments 'col_select' (when provided), 'file', and 'n_max' are always overwritten
  if (!missing(col_select))
    args$col_select <- rlang::enquo(col_select)
  args <- purrr::list_assign(args, file = env(x)$con, n_max = n_max)

  vcf <- do.call(read_vcf, args)
  if (is.null(vcf))
    return(NULL)

  # Increment index
  env(x)$curr_line <- env(x)$curr_line + n_sites.varkit(vcf)

  vcf
}

.seek_next <- function(index, skip_header = TRUE) {
  stopifnot(is_varkit_index(index))

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
