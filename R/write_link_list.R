##' @export
write_link_list.link_list <- function(x, path) {
  UseMethod("write_link_list", x)
}

##' @export
write_link_list <- function(x, path) {
  x$translation <- attr(x, "translation")
  write(rjson::toJSON(x), path)
}
