#' @title View time series in browser
#' @name tsview
#' @param x time series object, usually inheriting from class "ts".
#' @param plot.type for multivariate time series. \code{multiple} means each series has its own y-axis, \code{single} means all series share one and the same y-axis.
#' @param gui if gui == FALSE time series are shown in a regular plot. Else, if gui == TRUE, plots are shown in a gui.
#' @description Plotting method for objects inheriting from class "ts".
#' @examples
#' z = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4) # 5 time series
#' tsview(z, "single")
#' @import RColorBrewer
#' @import shiny
#' @export

tsview = function(x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4), plot.type = "multiple", gui = FALSE)
{
  install.and.source.dependency("RColorBrewer")

  if (gui)
	{
	  install.and.source.dependency("shiny")
    install.and.source.dependency("DT")

    # Make data publicly available (FIX: this shoudn't be done via global env)
    .tsview.shiny.x <<- x
    .tsview.shiny.x.names <<- colnames(.tsview.shiny.x)

    app.dir = system.file("shiny-app", package = "tsview")
    runApp(app.dir, launch.browser = T)
	}
	else
	{
	  tsview_plot(x, plot.type = plot.type)
	}
}
