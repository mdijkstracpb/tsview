#' @title Plotting Time Series Objects in Browser
#' @name tsview
#' @description Method for conveniently viewing objects inheriting from classes "\code{ts}", "\code{regts}" in your favorite web browser.
#' @param x time series object, usually inheriting from class "\code{ts}" or "\code{regts}".
#' @param plot.type for multivariate time series. \code{multiple} displays each series separately (with a common time axis), \code{single} displays all series in the same plot.
#' @examples
#' x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4) # 5 time series
#'
#' \dontrun{
#' tsview(x, "single")
#' tsview(x, "multiple")
#' }
#'
#' @seealso \code{\link{tsplot}, \link{ts}, \link{regts}, \link{grepl}}
#' @export

tsview = function(x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4), plot.type = "multiple")
{
  .install.if.not.installed("shiny")
  .install.if.not.installed("DT")
  shiny::runApp(list(server=.wrapped_server(x), ui=.ui), launch.browser = T)
}

