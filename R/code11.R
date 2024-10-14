#' Retrieve code11 for either a India state or district
#'
#' @description Each state and district has a unique two and five digit code11
#'   code respectively. Use this function to obtain the code11 code for a state or
#'   district.
#'
#' @param state The state(s) for which to obtain a code11 code(s).
#'  Can be entered as either a state abbreviation or full name (case-insensitive).
#'
#'  `state` can be entered as either a single state or a vector of states.
#'  If `state` is a vector, `district` must be omitted.
#'
#' @param district The county for which to obtain a code11 code.
#'  Can be entered with or without "district" (case-insensitive).
#'
#' @note A \code{state} must be included when searching for \code{district},
#'  otherwise multiple results may be returned for duplicate district names.
#'
#' @details State and district code11 are two and five digit codes, respectively.
#'   They uniquely identify all states and districts within India. They uniquely
#'   identify all states and districts within the United States. The state and
#'   district codes is merged into one code. The first two digits of the five
#'   digit district codes correspond to the state that the district belongs to.
#'
#' @return The code11 code(s) of given \code{state} or \code{district}.
#'
#' If only states are entered, a vector of length equal to the number of states
#' is returned. If any states are not found or are invalid, `NA` is returned in their place.
#'
#' If a state and district are entered, a single value with the code11 code
#' for the given district is returned. If the district is invalid for the given state,
#' an error is thrown.
#'
#' If both `state` and `district` are omitted, the entire list of available code11
#' codes is returned, sorted by the state's abbreviation.
#'
#' @seealso [code11_info()]
#'
#' @examples
#' code11()
#' code11("AP")
#' code11("Tamil Nadu")
#'
#' code11(c("AP", "WB", "TN"))
#'
#' code11("WB", district = "Kolkata")
#' code11(state = "Uttarakhand", district = "Nainital")
#' code11(state = "RJ", district = "Pratapgarh")
#' @export
code11 <- function(state, district = c()) {
  if (missing(state) && missing(district)) {
    return(indiamapdata::get_code11()$code11)
  }

  state_ <- tolower(state)
  district_ <- tolower(district)

  if (length(district_) == 0) {
    df <- indiamapdata::get_code11()
    abbr <- tolower(df$abbr)
    stname <- tolower(df$stname)
    code11_2 <- c(df$code11, df$code11)

    result <- code11_2[match(state_, c(abbr, stname))]
    result[result == "NA"] <- NA
    result
  } else {
    if (length(state_) > 1) {
      stop("`district` parameter cannot be used with multiple states.")
    }

    df <- indiamapdata::get_code11("districts")
    name <- tolower(df$dtname)
    state_abbr <- tolower(df$abbr)
    state_full <- tolower(df$stname)

    result <- c()

    for (district_i in district_) {
      result <- c(
        result,
        df$code11[which(
          (name %in% district_i | name %in% paste(district_i, "district")) &
            (state_abbr %in% state_ | state_full %in% state_)
        )]
      )
    }

    if (length(result) == 0) {
      if (length(district) == 1) {
        stop(paste0(district, " is not a valid district in ", state, ".\n"))
      } else {
        stop(paste0(district, " are not valid districts in ", state, ".\n"))
      }
    } else {
      result
    }
  }
}

#' Retrieve states or counties using FIPS codes
#'
#' @param code11 A one to three digit, either \code{numeric}
#'  or \code{character}, vector of FIPS codes for which to look up states or counties.
#'  States have a two digit FIPS code and counties have a five digit FIPS
#'  code (where the first 2 numbers pertain to the state).
#'
#' @param sortAndRemoveDuplicates Whether or not to sort the output and remove
#'  duplicates. By default, the output will be returned in the order of
#'  the values provided to the \code{fips} parameter. Set this parameter to \code{TRUE}
#'  to return the output sorted by FIPS with a single instance of each FIPS.
#'
#' @return A data frame with the states or counties and the associated
#'  FIPS codes.
#'
#'  If `code11` is omitted, the data frame containing all available states is
#'  returned.
#'
#' @seealso [code11()]
#'
#' @examples
#' code11_info(2)
#' code11_info("2")
#' code11_info(c("02", "03", "04"))
#'
#' code11_info(2016)
#' code11_info(c("02016", "02017"), sortAndRemoveDuplicates = TRUE)
#'
#' @rdname code11_info
#' @export
code11_info <- function(code11, sortAndRemoveDuplicates = FALSE) {
  if (missing(code11)) {
    code11_info.character(indiamap::code11())
  } else {
    UseMethod("code11_info", code11)
  }
}

#' @rdname code11_info
#' @export
code11_info.numeric <- function(code11, sortAndRemoveDuplicates = FALSE) {
  if (all(code11 >= 1001 & code11 <= 39496)) {
    code11_ <- sprintf("%05d", code11)
  } else if (all(code11 >= 1 & code11 <= 38)) {
    code11_ <- sprintf("%02d", code11)
  } else {
    stop("Invalid Code11 code(s), must be either 2 digit (states) or 5 digit (districts), but not both.")
  }

  get_code11_info(code11_, sortAndRemoveDuplicates)
}

#' @rdname code11_info
#' @export
code11_info.character <- function(code11, sortAndRemoveDuplicates = FALSE) {
  if (all(nchar(code11) %in% 4:5)) {
    code11_ <- sprintf("%05s", code11)
  } else if (all(nchar(code11) %in% 1:2)) {
    code11_ <- sprintf("%02s", code11)
  } else {
    stop("Invalid Code11 code, must be either 2 digit (states) or 5 digit (districts), but not both.")
  }

  get_code11_info(code11_, sortAndRemoveDuplicates)
}

#' Gets code11 info for either states or districts depending on input.
#' Helper function for S3 method [code11_info()].
#' @keywords internal
get_code11_info <- function(code11, sortAndRemoveDuplicates) {
  if (all(nchar(code11) == 2)) {
    df <- indiamapdata::get_code11()
    columns <- c("abbr", "code11", "stname")
  } else if (all(nchar(code11) == 5)) {
    df <- indiamapdata::get_code11("districts")
    columns <- c("stname", "abbr", "dtname", "code11")
  }

  if (sortAndRemoveDuplicates) {
    result <- df[df$code11 %in% code11, ]
  } else {
    result <- static_merge(data.frame(code11 = code11), df)
  }

  if (nrow(result) == 0) {
    # Present warning if no results found.
    warning(paste("Code11 code(s)", toString(code11), "not found, returned 0 results."))
  } else if (!all(code11 %in% result$code11)) {
    # Present warning if any FIPS codes included are not found.
    excluded_code11 <- code11[which(!code11 %in% result$code11)]
    warning(paste("Code11 code(s)", toString(excluded_code11), "not found"))
  }

  rownames(result) <- NULL
  result[, columns]
}

#' Merge while maintaining original sort order
#'
#' Internal function used by [code11_info()].
#'
#' @seealso \url{https://stackoverflow.com/a/61560405/7264964}
#' @keywords internal
static_merge <- function(x, y, ...) {
  x$join_id_ <- seq_len(nrow(x))
  joined <- merge(x = x, y = y, sort = FALSE, ...)
  joined[order(joined$join_id), colnames(joined) != "join_id_"]
}
