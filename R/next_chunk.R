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

  # Use seek to process the VCF header if before start of index
  if (env(x)$curr_line < x$linum[1L])
    seek.varkit_index(x, where = x$linum[1L])

  # Fetch arguments stored on the index & merge with ...
  if (exists("read_vcf_args", envir = env(x)))
    args <- purrr::list_assign(env(x)$read_vcf_args, !!!args)

  if (is.null(args[["header"]]))
    stop("Could not detect a VCF header! Please supply the 'header' argument.")

  # NOTE Keep repeating the last element when outside of the index
  # This allows for "open ended" indexes like from index_vcf_header()
  n_max <- seek.varkit_index(x)$n_lines
  if (n_max < 1L)
    n_max <- utils::tail(x$n_lines, n = 1L)

  # Arguments 'col_select' (when provided), 'file', and 'n_max' are always overwritten
  if (!missing(col_select))
    args$col_select <- rlang::enquo(col_select)
  args <- purrr::list_assign(args, file = env(x)$con, n_max = n_max)

  vcf <- do.call(read_vcf, args)
  if (is.null(vcf))
    return(vcf)

  # Increment index
  env(x)$curr_line <- env(x)$curr_line + n_sites.varkit(vcf)

  vcf
}
