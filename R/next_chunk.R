##' @export
next_chunk <- function(x, ...) {
  UseMethod("next_chunk", x)
}

##' @export
next_chunk.varkit_index <- function(x, ...) {
  open.varkit_index(x)

  # Read_vcf excludes the header from n_max, adjust for this behaviour
  if (env(x)$curr_line < x$linum[1]) {
    header_line <- find_header_line(env(x)$file)
    if (x$linum[1] != header_line + 1)
      stop("The index should start right below the header line! Expected start: ", header_line)
    env(x)$curr_line <- x$linum[1]
  }

  # NOTE Keep repeating the last element when outside of the index
  # This allows for "open ended" indexes like from index_vcf_partial()
  n_max <- seek.varkit_index(x)$n_lines
  if (n_max < 1)
    n_max <- utils::tail(x$n_lines, n = 1)

  # Fetch arguments stored on the index & merge with computed arguments
  if (is.null(env(x)$read_vcf_args))
    env(x)$read_vcf_args <- list()
  args <- utils::modifyList(env(x)$read_vcf_args, list(...))
  args <- utils::modifyList(args, list(file = env(x)$con, n_max = n_max))

  vcf <- do.call(read_vcf, args)
  if (is.null(vcf))
    return(vcf)

  # Increment index
  env(x)$curr_line <- env(x)$curr_line + n_max

  # Store header for further reading
  if (is.null(args$header))
    env(x)$read_vcf_args$header <- new_varkit_header(meta = meta(vcf), col_names = col_names(vcf))

  vcf
}
