#' US Presidential Elections 1948-2004
#'
#' A small data set containing information of incumbent vote shares,
#' approval ratings, and economic growth rates in US Presidential Elections
#' 1948-2004.
#'
#' @docType data
#'
#' @usage data(us)
#'
#' @format An object of class \code{"tbl_df"}. Contains 8 variables for 15 observations.
#' \describe{
#'  \item{year}{Election year}
#'  \item{vote}{Incumbent vote share in percent}
#'  \item{party}{Incumbent's party}
#'  \item{incumbent}{Name of the incumbent president}
#'  \item{approval}{Incumbent's approval rating}
#'  \item{approval_above_mean}{A binary indicator that equals 1 if the approval rating is above the sample average}
#'  \item{centered_approval}{A mean-centered version of incumbent's approval rating}
#' }
#' @keywords datasets
#' @examples
#' data(us)
#' head(us)
#'
"us"
