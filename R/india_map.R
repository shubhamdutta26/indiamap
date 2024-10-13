#' Retrieve India map data
#'
#' @param regions The region breakdown for the map, can be one of
#'   (\code{"states"}, \code{"state"}, \code{"districts"}, \code{"district"}).
#'   The default is \code{"states"}.
#' @param include The regions to include in the resulting map. If \code{regions} is
#'  \code{"states"}/\code{"state"}, the value can be either a state name, abbreviation or code.
#'  For districts, the district codes must be provided as there can be multiple districts with the
#'  same name. If states are provided in the districts map, only districts in the included states
#'  will be returned.
#' @param exclude The regions to exclude in the resulting map. If \code{regions} is
#'  \code{"states"}/\code{"state"}, the value can be either a state name, abbreviation or code.
#'  For districts, the district codes must be provided as there can be multiple districts with the
#'  same name. The regions listed in the \code{include} parameter are applied first and the
#'  \code{exclude} regions are then removed from the resulting map. Any excluded regions
#'  not present in the included regions will be ignored.
#'
#' @seealso [indiamapdata::india_map()] of which this function is a wrapper for.
#'
#' @return A data frame of India map coordinates divided by the desired \code{regions}.
#'
#' @examples
#' str(india_map())
#'
#' df <- india_map(regions = "districts")
#' states <- india_map(include = c("WB", "NCT", "AP"))
#'
#' @export
india_map <- function(regions = c("states", "state", "districts", "district"),
                      include = c(),
                      exclude = c()) {
  indiamapdata::india_map(regions = regions, include = include, exclude = exclude)
}
