#' Conveniently plot basic India map
#'
#' @inheritParams india_map
#' @param data A data frame containing values to plot on the map. This
#'   parameter should be a data frame consisting of two columns,
#'   a code (2 characters for state, 3 characters for district)
#'   and the value that should be associated with that region. The
#'   columns of \code{data} \emph{must} be \code{code} or \code{state} and
#'   the value of the `values` parameter.
#' @param values The name of the column that contains the values to be associated
#'   with a given region. The default is \code{"value"}.
#' @param theme The theme that should be used for plotting the map. The default
#'   is \code{theme_map} from \href{https://github.com/jrnold/ggthemes}{ggthemes}.
#' @param labels Whether or not to display labels on the map. Labels are not displayed
#'   by default.
#' @param label_color The color of the labels to display. Corresponds to the \code{color}
#'   option in the [ggplot2::aes()] mapping. The default is \code{"black"}.
#' @param label_size The size of the labels to display. Corresponds to the \code{size}
#'   option in the [ggplot2::aes()] mapping. The default is \code{4}.
#'   for more color options.
#' @param ... Other arguments to pass to [ggplot2::aes()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like \code{color = "red"}
#'   or \code{linewidth = 3}. They affect the appearance of the polygons used to render
#'   the map (for example fill color, line color, line thickness, etc.). If any of
#'   \code{color}/\code{colour}, \code{fill}, or \code{linewidth} are not specified they
#'   are set to their default values of \code{color="black"}, \code{fill="white"},
#'   and \code{linewidth=0.4}.
#'
#' @return A [ggplot2::ggplot] object that contains a basic
#'   US map with the described parameters. Since the result is a \code{ggplot}
#'   object, it can be extended with more [ggplot2::Geom] layers, scales, labels,
#'   themes, etc.
#'
#' @seealso [indiamap], [ggplot2::theme()]
#'
#' @examples
#' plot_map()
#' plot_map(regions = "states")
#' plot_map(regions = "districts")
#' plot_map(regions = "state")
#' plot_map(regions = "district")
#'
#' # Output is ggplot object so it can be extended
#' # with any number of ggplot layers
#' library(ggplot2)
#' plot_map(include = c("GJ", "MH", "MP", "UP")) +
#'   labs(title = "Indian States")
#'
#'
#' @importFrom rlang .data
#' @export
plot_map <- function(regions = c("states", "state", "districts", "district"),
                     include = c(),
                     exclude = c(),
                     data = data.frame(),
                     values = "values",
                     theme = theme_map(),
                     labels = FALSE,
                     label_color = "black",
                     label_size = 4,
                     ...) {

  # check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`ggplot2` must be installed to use `plot_usmap`.
         Use: install.packages(\"ggplot2\") and try again.")
  }

  .data <- ggplot2::.data

  # parse parameters
  regions <- match.arg(regions)
  geom_args <- list(...)

  # set geom_polygon defaults
  if (is.null(geom_args[["colour"]]) && is.null(geom_args[["color"]])) {
    geom_args[["color"]] <- "black"
  }

  if (is.null(geom_args[["linewidth"]])) {
    geom_args[["linewidth"]] <- 0.3
  }

  # set default "fill" if data is not included
  if (is.null(geom_args[["fill"]]) && nrow(data) == 0) {
    geom_args[["fill"]] <- "white"
  }

  # create polygon layer
  if (nrow(data) == 0) {
    map_df <- indiamap::india_map(regions = regions, include = include, exclude = exclude)
    geom_args[["mapping"]] <- ggplot2::aes()
  } else {
    map_df <- indiamap::map_with_data(data, values = values, include = include, exclude = exclude)

    if (!is.null(map_df$district)) regions <- "districts"
    geom_args[["mapping"]] <- ggplot2::aes(fill = .data[[values]])
  }

  polygon_layer <- do.call(ggplot2::geom_sf, geom_args)

  # create label layer
  if (labels) {
    if (regions == "state") regions <- "states"
    else if (regions == "district") regions <- "districts"

    centroid_labels <- indiamapdata::centroid_labels(regions)

    if (length(include) > 0) {
      centroid_labels <- centroid_labels[
        centroid_labels$stname %in% include |
          centroid_labels$abbr %in% include |
          centroid_labels$code11 %in% include,
      ]
    }

    if (length(exclude) > 0) {
      centroid_labels <- centroid_labels[!(
        centroid_labels$stname %in% exclude |
          centroid_labels$abbr %in% exclude |
          centroid_labels$code11 %in% exclude |
          substr(centroid_labels$code11, 1, 2) %in% exclude
      ), ]
    }

    if (regions == "district" || regions == "districts") {
      label_layer <- ggplot2::geom_sf_text(
        data = centroid_labels,
        ggplot2::aes(label = sub(" District", "", .data$dtname)),
        color = label_color, size = label_size
      )
    } else {
      label_layer <- ggplot2::geom_sf_text(
        data = centroid_labels,
        ggplot2::aes(label = .data$abbr), color = label_color,
        size = label_size
      )
    }
  } else {
    label_layer <- ggplot2::geom_blank()
  }

  # construct final plot
  ggplot2::ggplot(data = map_df) + polygon_layer + label_layer + theme
}

#' Convenient theme map
#'
#' @description
#' This creates a nice map theme for use in [plot_usmap()].
#' It originated from the `ggthemes` package located at this repository:
#'   \url{https://github.com/jrnold/ggthemes}.
#'
#' This function was manually rewritten here to avoid the need for
#'  another package import.
#'
#' @keywords internal
theme_map <- function(base_size = 9, base_family = "") {
  element_blank <- ggplot2::element_blank()
  `%+replace%` <- ggplot2::`%+replace%` # nolint: object_name_linter
  unit <- ggplot2::unit

  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = element_blank,
                   axis.text = element_blank,
                   axis.ticks = element_blank,
                   axis.title = element_blank,
                   panel.background = element_blank,
                   panel.border = element_blank,
                   panel.grid = element_blank,
                   panel.spacing = unit(0, "lines"),
                   plot.background = element_blank,
                   legend.position = "inside",
                   legend.justification.inside = c(0, 0))
}
