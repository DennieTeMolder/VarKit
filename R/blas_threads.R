##' @export
blas_threads <- function(threads = NULL) {
  old <- RhpcBLASctl::blas_get_num_procs()
  if (is.null(threads))
    return(old)
  threads <- as.integer(threads)
  RhpcBLASctl::blas_set_num_threads(threads)
  RhpcBLASctl::omp_set_num_threads(threads)
  invisible(old)
}
