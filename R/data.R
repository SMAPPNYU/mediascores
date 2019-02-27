#' News Media Domains Tweeted by Members of the 115th US Congress
#'
#' The counts of news media domains tweeted or retweeted by Members of
#' the 115th US Congress (among other political actors).
#'
#' @usage data(MOC115)
#' 
#' @format A data frame with 590 rows and 151 variables:
#' \describe{
#'   \item{name}{name of politician (or organization)}
#'   \item{nominate_name}{name of Members of Congress as specified on \url{voteview.com}}
#'   \item{party}{political party with whom each actor is associated}
#'   \item{group}{integer party ID of each actor \{1 = Democrat, 2 = Republican\}}
#'   \item{role}{political role of each actor \{Senate, House, Governor, Delegate, Other\}}
#'   \item{...}{remaining columns (145) in the data frame indicate the counts of each news domain tweeted or retweeted by each political actor}
#' }
#' @source Collected by author in late 2018.
"MOC115"