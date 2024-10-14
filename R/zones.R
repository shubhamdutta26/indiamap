#' Northern Zonal Council
#'
#' @description
#' Northern Zonal Council comprises of Chandigarh, Delhi, Haryana, Himachal
#' Pradesh, Jammu and Kashmir, Ladakh, Punjab, and Rajasthan.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/Northern_Zonal_Council}
#'
#' @examples
#' plot_map(include = .north, labels = TRUE)
#' @export
.north <- c("CH", "NCT", "HR", "HP", "J&K", "LA", "PB", "RJ")

#' Northestern Zonal Council
#'
#' @description
#' North Eastern Council comprises of Assam, Arunachal Pradesh, Manipur,
#' Meghalaya, Mizoram, Nagaland and Tripura.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/North_Eastern_Council}
#'
#' @examples
#' plot_map(include = .northeast, labels = TRUE)
#' @export
.northeast <- c("AS", "AR", "MN", "ML", "MZ", "NL", "TR")

#' Central Zonal Council
#'
#' @description
#' Central Zonal Council comprises of Chhattisgarh, Madhya Pradesh,
#' Uttarakhand and Uttar Pradesh.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/Central_Zonal_Council}
#'
#' @examples
#' plot_map(include = .central, labels = TRUE)
#' @export
.central <- c("CG", "MP", "UK", "UP")

#' Eastern Zonal Council
#'
#' @description
#' Eastern Zonal Council comprises of Bihar, Jharkhand, Odisha, and West Bengal.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/Eastern_Zonal_Council}
#'
#' @examples
#' plot_map(include = .east, labels = TRUE)
#' @export
.east <- c("BR", "JH", "OR", "WB")

#' Western Zonal Council
#'
#' @description
#' Western Zonal Council comprises of Dadra and Nagar Haveli and Daman and Diu,
#' Goa, Gujarat, and Maharashtra.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/Western_Zonal_Council}
#'
#' @examples
#' plot_map(include = .west, labels = TRUE)
#' @export
.west <- c("DN", "DD", "GA", "GJ", "MH")

#' Southern Zonal Council (w/o Special invitees)
#'
#' @description
#' Southern Zonal Council comprises of Andhra Pradesh, Karnataka, Kerala,
#' Puducherry, Tamil Nadu, and Telangana.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/Southern_Zonal_Council}
#'
#' @examples
#' plot_map(include = .south, labels = TRUE)
#' @export
.south <- c("AP", "KA", "KL", "PY", "TN", "TG")


#' Southern Zonal Council (w/ Special invitees)
#'
#' @description
#' Southern Zonal Council comprises of Andhra Pradesh, Karnataka, Kerala,
#' Puducherry, Tamil Nadu, and Telangana. Andaman and Nicobar and Lakshadweep
#' are special invitees of the Southern Zonal Council.
#'
#' @details
#' See
#' \url{https://en.wikipedia.org/wiki/Southern_Zonal_Council}
#'
#' @examples
#' plot_map(include = .southsp, labels = TRUE)
#' @export
.southsp <- c("AP", "KA", "KL", "PY", "TN", "TG", "AN", "LD")
