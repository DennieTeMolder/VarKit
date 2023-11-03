##' @export
varkit_help <- function(x, varkit_only = TRUE) {
  classes <- class(x)
  if (varkit_only) {
    is_varkit_class <- stringr::str_starts(classes, "varkit(_|$)")
    if (!any(is_varkit_class))
      stop("'", substitute(x), "' is not a VarKit class! To get help use `varkit_only = FALSE`.")
    classes <- classes[is_varkit_class]
  }

  # Retrieve and format
  methods <- lapply(classes, function(class) {
    stringr::str_remove(
      methods(class = class),
      pattern = paste0("\\.", class, "$")
    )
  })
  methods <- sort(unique(do.call(c, methods)))

  if (length(methods) == 0L)
    stop("There are no methods associated with 'x'.")

  # Print
  .msg_h2("Available methods")
  cli::cat_bullet(paste0(methods, "(", substitute(x), ")"))
  .msg_info("Use `?function_name` for more info (e.g. ?samples)")
  invisible(methods)
}
