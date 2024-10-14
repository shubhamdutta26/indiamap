#' Join district or state level data to India map data
#'
#' @inheritParams india_map
#' @param data The data that should be joined to a India map. This
#'   parameter should be a data frame consisting of two columns,
#'   a code11 (2 characters for state, 3 characters for district)
#'   and the value that should be associated with that region. The
#'   columns of \code{data} \emph{must} be \code{code11} or \code{state} and
#'   the value of the `values` parameter. If both \code{code11} and \code{state}
#'   are provided, this function uses the \code{code11}.
#' @param values The name of the column that contains the values to be associated
#'   with a given region. The default is \code{"values"}.
#' @param na The value to be inserted for states or counties that don't have
#'   a value in \code{data}. This value must be of the same type as the \code{value}
#'   column of \code{data}.
#'
#' @return A data frame composed of the map data frame (from [india_map()]) except
#'   an extra column containing the values in \code{data} is included.
#'
#'   The result can be plotted using [ggplot2::ggplot()] or [plot_map()].
#'
#' @seealso [plot_map()]
#'
#' @examples
#' data_01 <- data.frame(code11 = c("01", "02", "04"), values = c(1, 5, 8))
#' df <- map_with_data(data_01, na = 0)
#'
#' data_02 <- data.frame(state = c("AP", "WB", "Tamil Nadu"), values = c(6, 9, 3))
#' df <- map_with_data(data_02, na = 0)
#'
#' @export
map_with_data <- function(data,
                          values = "values",
                          include = c(),
                          exclude = c(),
                          na = NA) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }

  if (nrow(data) == 0) {
    if (length(include) == 0) {
      region_type <- "state"
    } else {
      region_type <- ifelse(nchar(include[1]) == 2, "state", "district")
    }

    warning(paste("`data` is empty, returning basic", region_type, "India map data frame"))
    return(indiamap::india_map(regions = region_type, include = include, exclude = exclude))
  }

  if (!(values %in% names(data))) {
    stop(paste0("\"", values, "\" column not found in `data`."))
  }

  if ("code11" %in% names(data)) {
    # do nothing
  } else if ("state" %in% names(data)) {
    # convert to code11
    data$code11 <- indiamap::code11(data$state)
  } else {
    # error
    stop("`data` must be a data.frame containing either a `stname` or `code11` column.")
  }

  data$code11 <- as.character(data$code11)

  region_type <- ifelse(nchar(data$code11[1]) <= 2, "state", "district")
  map_df <- indiamap::india_map(regions = region_type, include = include, exclude = exclude)

  # Remove columns in data that are already in map_df
  data$abbr <- NULL
  data$stname <- NULL
  data$dtname <- NULL
  data$geom <- NULL

  padding <- ifelse(region_type == "state", 2, 5)
  data$code11 <- sprintf(paste0("%0", padding, "d"), as.numeric(data$code11))

  result <- merge(map_df, data, by = "code11", all.x = TRUE, sort = FALSE)
  result[is.na(result[, values]), values] <- na

  result <- result[, c(setdiff(names(result), names(data)), names(data))]

  if (region_type == "state") {
    result <- result[order(result$stname), ]
  } else {
    result <- result[order(result$stname, result$dtname), ]
  }

  result
}
