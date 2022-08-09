##' @export
varkit_help <- function(x) {
  # Retrieve and format
  x <- sapply(class(x), function(class, name) {
    methods <- methods(class = class)
    methods <- stringr::str_remove(
      methods,
      pattern = paste0("\\.", class, "$")
    )
    paste0(methods, "(", name, ")")
  }, name = substitute(x))
  x <- sort(unique(unlist(x, use.names = FALSE)))
  # Print
  cat("Available methods:\n")
  print(x)
  cat("Use `?function_name` for more info (e.g. ?samples).\n")
  invisible(x)
}
