##' @export
check_window <- function(vcf, windows, cols_each, probs = c(0, 0.025, 0.5, 0.975, 1)) {
  stopifnot(is_varkit(vcf))
  .stopifnotsingle(windows, what = is.numeric)
  .stopifnotsingle(cols_each, what = is.numeric)
  stopifnot(cols_each > 0)

  locs <- split(site(vcf, "pos"), site(vcf, "chrom"))
  locs <- lapply(locs, sort)

  # Check number of observations per chromosome
  n <- sapply(locs, length)
  min_n <- cols_each * (windows + 1)
  if (windows < 0 || max(n) <= min_n) {
    # Only report statistics for the largest chromosome
    locs <- locs[which.max(n)]
    dist <- diff(range(locs[[which.max(n)]]))
  } else {
    # Only use chromosomes of a sufficient length
    locs <- locs[n >= min_n]
    dist <- lapply(locs, function(loc) {
      loc <- tibble::tibble(pos = loc)
      loc <- splitm(loc, size = cols_each)
      loc <- loc[sapply(loc, nrow) == cols_each]

      # Only compare the extremes
      comps <- tibble::tibble(mid = seq_along(loc), left = .data$mid - windows, right = .data$mid + windows)

      ind_max <- length(loc)
      res <- lapply_rowwise(comps, function(mid, left, right) {
        skip_right <- left == right
        mid <- loc[[mid]]$pos
        a <- b <- NULL
        if (left >= 1) {
          left <- loc[[left]]$pos
          a <- c(mid - left[1], utils::tail(mid, n = 1) - left)
        }
        if (!skip_right && right <= ind_max) {
          right <- loc[[right]]$pos
          b <- c(utils::tail(right, n = 1) - mid, right - mid[1])
        }
        c(a, b)
      })
      unlist(res)
    })
    dist <- unlist(dist, use.names = FALSE)
  }

  res <- c(mean = mean(dist), quantile(dist, probs = probs))
  res <- .frmt_big(round(res))
  data.frame(quantile = names(res), bp_size = unname(res))
}
