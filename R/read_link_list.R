##' @export
read_link_list <- function(file) {
  llist <- rjson::fromJSON(file = file)
  class(llist) <- "link_list"
  trans <- llist$translation
  if (!is.null(trans))
    attr(llist, "translation") <- trans
  llist$translation <- NULL
  llist
}
