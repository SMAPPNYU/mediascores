#' News Media Domains Tweeted by Members of the 115th US Congress and
#' other politicians
#'
#' @description The counts of news media stories (URL domains) tweeted or
#' retweeted by Members of the 115th US Congress and other political actors.
#'
#' \describe{
#'   \item{name}{name of politician (or organization)}
#'   \item{nominate_name}{name of Members of Congress as specified by \url{voteview.com}}
#'   \item{party}{political party of politician (or organization)}
#'   \item{group}{integer party ID of politician (or organization) \{1 = Democrat, 2 = Republican\}}
#'   \item{role}{political role of politician (or organization) \{Senate, House, Governor, Delegate, Other\}}
#'   \item{remaining variables}{remaining variables indicate the count for each news domain shared by each political actor}
#' }
#' 
#' @docType data
#' @name MOC115
#' @usage data(MOC115)
#' @format A data frame with 590 rows and 151 variables
NULL