##' @export
write_link_list.link_list <- function(x, path) {
  UseMethod("write_link_list", x)
}

##' @export
write_link_list <- function(x, path) {
  if (!stringr::str_ends(path, "\\.json"))
    stop("Only paths ending in '.json' are supported!")
  x$translation <- attr(x, "translation")
  write(rjson::toJSON(x), path)
}
