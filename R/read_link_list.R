##' @export
read_link_list <- function(file) {
  llist <- rjson::fromJSON(file = file)
  if (is.list(llist)) {
    trans <- llist$translation
    attr(llist, "translation") <- trans
    llist$translation <- NULL
  }
  class(llist) <- "link_list"
  llist
}
