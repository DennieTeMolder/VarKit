##' @export
varkit_help <- function(x) {
  # Strip non-varkit classes
  classes <- class(x)
  is_varkit_class <- stringr::str_starts(classes, "varkit(_|$)")
  if (any(is_varkit_class))
    classes <- classes[is_varkit_class]
  # Retrieve and format
  methods <- sapply(classes, function(class) {
    stringr::str_remove(
      methods(class = class),
      pattern = paste0("\\.", class, "$")
    )
  })
  methods <- sort(unique(unlist(methods, use.names = FALSE)))
  methods <- paste0(methods, "(", substitute(x), ")")
  # Print
  cat("Available methods:\n")
  print(methods)
  cat("Use `?function_name` for more info (e.g. ?samples).\n")
  invisible(methods)
}
