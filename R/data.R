#' Indian Population (census and projections) by states
#'
#' @description Indian Population (census and projections) by states. \cr\cr
#'   The data is formatted for easy merging with output from [indiamap::india_map()].
#'
#' @usage data(statepop)
#'
#' @details
#' \itemize{
#'   \item \code{code11} The 2-digit state code.
#'   \item \code{abbr} The 2-letter state abbreviation.
#'   \item \code{state} The full state name.
#'   \item \code{pop_1901} The 1901 population estimate (in number of people).
#'   \item \code{pop_1951} The 1951 population estimate (in number of people).
#'   \item \code{pop_2011} The 2011 population estimate (in number of people).
#'   \item \code{pop_2023} The 2023 population estimate (in number of people).
#' }
#'
#' @name statepop
#' @format A data frame with 36 rows and 8 variables.
#' @docType data
#' @keywords data
"statepop"

#' West Bengal population, sex-ratio, and literacy data (2011)
#'
#' @description West Bengal population, sex-ratio, and literacy data for 2011. \cr\cr
#'   The data is formatted for easy merging with output from [indiamap::india_map()].
#'
#' @usage data(wb_2011)
#'
#' @details
#' \itemize{
#'   \item \code{code11} The 5-digit code corresponding to the district.
#'   \item \code{abbr} The 2-letter state abbreviation.
#'   \item \code{district} The full district name.
#'   \item \code{pop_2011} The 2011 population estimate (in number of people).
#'   \item \code{pop_increase_2011} The 2011 population increase (percent of people).
#'   \item \code{sex_ratio_2011} The 2011 sex ratio (females per 100 males).
#'   \item \code{literacy_per_2011} The 2011 literacy percentage.
#'   \item \code{pop_2011} The 2011 population density (unknown units).
#' }
#'
#' @name wb_2011
#' @format A data frame with 23 rows and 8 variables.
#' @docType data
#' @keywords data
"wb_2011"
