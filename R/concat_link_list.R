concat_link_lists <- function(llist_list) {
  # Collect all translations and prepare llist_list for concatenating
  n <- NULL
  trans_list <- vector(mode = "list", length = length(llist_list))
  for (i in seq_along(llist_list)) {
    cur_len <- length(llist_list[[i]])

    # Store translations
    cur_trans <- attr(llist_list[[i]], "translation")
    if (is.null(cur_trans)) {
      trans_list[[i]] <- 1:cur_len
    } else {
      trans_list[[i]] <- cur_trans
    }

    # First llist does not need to be adjusted
    if (i == 1) {
      n <- cur_len
      next()
    }

    # Adjust entries that contain links
    needs_adjust <- contains_link(llist_list[[i]])
    for (j in which(needs_adjust)) {
      llist_list[[i]][[j]] <- llist_list[[i]][[j]] + n
    }

    n <- n + cur_len
  }

  # Perform the concatenation
  llist <- unlist(llist_list, recursive = FALSE, use.names = FALSE)
  rm(llist_list)
  trans <- unlist(trans_list)
  rm(trans_list)

  if (any(duplicated(trans)))
    stop("Duplicated translations detected, cannot concat!")

  # Add attributes
  if (!all(trans == seq_along(llist)))
    attr(llist, "translation") <- trans
  class(llist) <- "link_list"

  llist
}
