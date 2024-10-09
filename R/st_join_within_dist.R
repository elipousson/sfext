#' Join sf objects within specified distance
#'
#' @noRd
st_join_within_dist <- function(x,
                                y,
                                dist = NULL,
                                ...) {
  x <- map(
    vctrs::vec_chop(x),
    function(feat) {
      feat_y <- y

      if (!is.null(dist)) {
        if (has_length(dist, 1)) {
          feat <- st_buffer_ext(feat, dist = dist, ...)
        } else {
          feat <- st_erase(
            st_buffer_ext(feat, dist = max(dist), ...),
            st_buffer_ext(feat, dist = min(dist), ...)
          )
        }

        feat_y <- st_filter_ext(y, feat, crs = x)
      }

      if (nrow(feat_y) == 0) {
        return(feat)
      }

      st_join(
        x = feat,
        y = feat_y,
        join = st_nearest_feature
      )
    }
  )

  sf_list_rbind(x)
}
