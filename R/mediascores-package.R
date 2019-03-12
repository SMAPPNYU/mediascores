#' mediascores: News-sharing Ideology from Social Media Link Data
#' 
#' @description
#' This library enables the examination of political communication by
#' politicians and ordinary users on social media. It uses the news stories
#' shared by users to estimate the news-sharing ideology of users and
#' the ideology of the content that they share.
#' 
#' @section
#' This library provides three sets of functions for the following tasks:
#' (1) model estimation, (2) model diagnostics, and (3) model summary.
#' 
#' @section Model estimation:
#' 
#' The core estimation function, \code{\link{mediascores}}, takes as an input a
#' matrix or data.frame where the rows represent social media users and the
#' columns represent the news media domains shared by each user. Each cell of
#' the matrix represents the count of each domain shared by each user. If, for
#' example, a user shares 10 news stories from the New York Times, the value of
#' the cell indicated by the row of that user and the column of the New York
#' Times would be a 10.
#' 
#' @section Model diagnostics:
#' 
#' The object outputted from the estimation function \code{\link{mediascores}}
#' is a \code{\link{stanfit-class}} object from the
#' \href{https://mc-stan.org}{Stan} Bayesian inference engine. All methods for
#' the analysis of Stan objects are therefore useable for those outputted by the
#' \code{\link{mediascores}} function. A helper function \code{\link{rhat}} is
#' provided to give a quick summary of R-hat values to assess the most common
#' convergence statistics for one's parameters of interest.
#' 
#' @section Model summary:
#' 
#' Point estimates and credible intervals for the quantities of interest
#' (i.e. the ideology of social media users and news organizations) are
#' accessible through the \code{\link{point_est}} function.
#' 
#' @name mediascores-package
#' @docType package
#' @useDynLib mediascores, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom rstan sampling
#' 
#' @references 
#' Stan Development Team (2018). RStan: the R interface to Stan. R package version 2.18.2. \url{https://mc-stan.org}
#' 
NULL
