#' @title Plotting Time Series Objects
#' @name tsplot
#' @description Plotting method for objects inheriting from classes "\code{ts}" and "\code{regts}".
#' @param x time series object, usually inheriting from class "\code{ts}" or "\code{regts}".
#' @param plot.type for multivariate time series. \code{multiple} displays each series separately (with a common time axis), \code{single} displays all series in the same plot.
#' @examples
#' x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4) # 5 time series
#'
#' tsplot(x, "single")
#' tsplot(x, "multiple")
#'
#' @seealso \code{\link{tsview}, \link{ts}, \link{regts}, \link{grepl}}
#' @export

tsplot = function(x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4), plot.type = "multiple")
{
  tsview_plot(x, plot.type = plot.type)
}
