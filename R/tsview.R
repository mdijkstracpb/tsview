#' @title Plotting Time Series Objects (in Browser)
#' @name tsview
#' @description Plotting method for objects inheriting from class "\code{ts}".
#' @param x time series object, usually inheriting from class "\code{ts}".
#' @param plot.type for multivariate time series. \code{multiple} displays each series separately (with a common time axis), \code{single} displays all series in the same plot.
#' @param gui if gui = FALSE time series are shown in a regular plot. Else, if gui = TRUE, plots are shown in a gui (c.q. your default web browser).
#' @examples
#' x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4) # 5 time series
#'
#' tsview(x, "single")
#' tsview(x, "multiple")
#'
#' \dontrun{
#' tsview(x, gui = TRUE)
#' }
#' @seealso \code{\link{ts}, \link{grepl}}
#' @export

tsview = function(x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4), plot.type = "multiple", gui = FALSE)
{
  .install.if.not.installed("RColorBrewer")

  if (gui)
	{
    .install.if.not.installed("shiny")
    .install.if.not.installed("DT")

    shiny::runApp(list(server=.wrapped_server(x), ui=.ui), launch.browser = T)
	}
	else
	{
	  tsview_plot(x, plot.type = plot.type)
	}
}

