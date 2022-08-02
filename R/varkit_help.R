##' @export
varkit_help <- function(x) {
  x <- sapply(class(x), function(my_class) {
    class_methods <- methods(class = my_class)
    class_methods <- stringr::str_remove(
      class_methods,
      pattern = paste0("\\.", my_class, "$")
    )
    paste0(class_methods, "(x)")
  })
  x <- sort(unique(unlist(x, use.names = FALSE)))
  cat("Available methods:\n")
  print(x)
  invisible(x)
}
