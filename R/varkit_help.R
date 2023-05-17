##' @export
varkit_help <- function(x, varkit_only = TRUE) {
  classes <- class(x)
  if (varkit_only) {
    is_varkit_class <- stringr::str_starts(classes, "varkit(_|$)")
    if (!any(is_varkit_class))
      stop("'x' is not a varkit class. To get help use `varkit_only = FALSE`.")
    classes <- classes[is_varkit_class]
  }
  # Retrieve and format
  methods <- sapply(classes, function(class) {
    stringr::str_remove(
      methods(class = class),
      pattern = paste0("\\.", class, "$")
    )
  })
  methods <- sort(unique(unlist(methods, use.names = FALSE)))
  if (length(methods) == 0)
    stop("There are no methods associated with 'x'.")
  # Print
  cat("# Available methods:\n")
  cli::cat_bullet(paste0(methods, "(", substitute(x), ")"))
  cat("# Use `?function_name` for more info (e.g. ?samples).\n")
  invisible(methods)
}
