tsview_plot = function (x, y = NULL, plot.type = c("multiple", "single"), lwd = 3, xy.labels,
											xy.lines, panel = lines, nc, yax.flip = FALSE,
											mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1),
											oma.multi = c(6, 0, 5, 0), cex.axis = 1.5, axes = TRUE, frame.plot = FALSE, ...)
{
  x.n = dim(as.matrix(x))[2] # number of time series in x

  if (x.n < 1) stop("Your input x contains no time series data.")

	# Our settings
	legend.name.cex	= 2
	n.dots.show.below = 10
	x.legend  = if (1 == x.n) "Time series" else colnames(x)

	show.xlab			= F
	axis.col			= "gray50"
	axis.cex			= 2
	axis.x.padj		= .5
	show.main			= F
	ts.col				= brewer.pal(12, "Paired")[1:x.n]
	name.col			= ts.col
	ts.lty				= 1:x.n
	mar.single		= c(2.5, 8, 1, 15)
	mar.multiple	= c(2.5, 6, 0, 15)
	legend.single.shift	= .9

	tsview_plot_internal <- function(x, y = NULL, plot.type = c("multiple",
																													 "single"), xy.labels, xy.lines, panel = lines, nc, xlabel,
																ylabel, type = "l", xlim = NULL, ylim = NULL, xlab = "Time",
																ylab, log = "", col = par("col"), bg = NA, pch = par("pch"),
																cex = par("cex"), lty = par("lty"), lwd = par("lwd"),
																axes = TRUE, frame.plot = axes, ann = par("ann"), cex.lab = par("cex.lab"),
																col.lab = par("col.lab"), font.lab = par("font.lab"),
																cex.axis = par("cex.axis"), col.axis = par("col.axis"),
																font.axis = par("font.axis"), main = NULL, ...) {
		plot.type <- match.arg(plot.type)
		nser <- NCOL(x)

		# Determine whether we want to show dots
		n.dots = nrow(as.matrix(x))
		if (n.dots < n.dots.show.below) type = 'b'

		#
		## Here starts plot.type == "multiple"
		#
		if (plot.type == "multiple" && 1 < nser) {
			addmain <- function(main, cex.main = par("cex.main"),
													font.main = par("font.main"), col.main = par("col.main"),
													...) mtext(main, side = 3, line = 3, cex = cex.main,
																		 font = font.main, col = col.main, ...)
			panel <- match.fun(panel)
			nser <- NCOL(x)
			if (nser > 10)
				stop("cannot plot more than 10 series as \"multiple\"")
			if (is.null(main))
				main <- xlabel
			nm <- colnames(x)
			if (is.null(nm))
				nm <- paste("Series", 1L:nser)
			if (missing(nc))
				nc <- if (nser > 4)
					2
			else 1
			nr <- ceiling(nser/nc)
			oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr,
																																nc))
			on.exit(par(oldpar))
			for (i in 1L:nser) {
				# PLOT MULTIPLE:
				plot.default(x[, i], axes = FALSE, xlab = "",
										 ylab = "", log = log, col = col, bg = bg, pch = pch,
										 ann = ann, type = "n", ...)
				panel(x[, i], col = ts.col[i], bg = bg, pch = pch,
							cex = cex, lwd = lwd, lty = lty, type = type,
							...)

				if (frame.plot)
					box(...)
				y.side <- if (i%%2 || !yax.flip)
					2
				else 4
				do.xax <- i%%nr == 0 || i == nser
				if (axes) {
					y.labels = axis(y.side, lwd=0,labels=NA)
					y.min = y.labels[1]
					y.max = y.labels[length(y.labels)]
					# axis(y.side, at = y.min, hadj = -1, xpd = NA, cex.axis = axis.cex, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
					axis(y.side, at = c(y.min, y.max), padj = c(0, 1), xpd = NA, cex.axis = axis.cex, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
					# axis(y.side, at = c(y.min, y.max), xpd = NA, cex.axis = axis.cex, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
					if (do.xax)
					{
						#
						## Code below adds Q's (for quarters) at right place
						#
						# TODO remove hardcoding (should only work in quarter data..):
						x.at = axis(1, xpd = NA, lwd = 0, labels = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
						if (!all(x.at == round(x.at)))
						{
							index = which((x.at - round(x.at)) %in% (0:4 / 4)[1:4]) # only select .0, .25, .5, .75 to display (in format yearQ{1,2,3,4})
							if (0 < length(index)) x.label	= prettyTime(x.at)
							if (0 == length(index))
							{
								axis(1, xpd = NA, lwd = 0, labels = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis, ...)
							}
							else
							{
								x.at	= x.at[index]
								x.label	= x.label[index]
								axis(1, at = x.at, labels = x.label, xpd = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
							}
						}
						else
						{ # normal x-axis (ie no Q's):
							axis(1, xpd = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
						}
					}
				}
				if (ann) {
					legend("topright", nm[i], cex = legend.name.cex, text.col = name.col[i], text.font = 3, bty = "n")
					#mtext(nm[i], y.side, line = 3, cex = cex.lab, col = col.lab, font = font.lab, ...) # MD: no longer outside
					if (show.xlab && do.xax) mtext(xlab, side = 1, line = 3, cex = cex.lab, col = col.lab, font = font.lab, ...)
				}
			}
			if (show.main && ann && !is.null(main)) {
				par(mfcol = c(1, 1))
				addmain(main, ...)
			}
			return(invisible())
		}

		#
		## MD: here starts plot.type == "single"
		#
		x <- as.ts(x)
		if (!is.null(y)) { # MD: this code is currently not used...
			y <- hasTsp(y)
			if (NCOL(x) > 1 || NCOL(y) > 1)
				stop("scatter plots only for univariate time series")
			if (is.ts(x) && is.ts(y)) {
				xy <- ts.intersect(x, y)
				xy <- xy.coords(xy[, 1], xy[, 2], xlabel, ylabel,
												log)
			}
			else xy <- xy.coords(x, y, xlabel, ylabel, log)
			xlab <- if (missing(xlab))
				xy$xlab
			else xlab
			ylab <- if (missing(ylab))
				xy$ylab
			else ylab
			xlim <- if (is.null(xlim))
				range(xy$x[is.finite(xy$x)])
			else xlim
			ylim <- if (is.null(ylim))
				range(xy$y[is.finite(xy$y)])
			else ylim
			n <- length(xy$x)
			if (missing(xy.labels))
				xy.labels <- (n <= 150)
			do.lab <- if (is.logical(xy.labels))
				xy.labels
			else {
				if (!is.character(xy.labels))
					stop("'xy.labels' must be logical or character")
				TRUE
			}
			ptype <- if (do.lab)
				"n"
			else if (missing(type))
				"p"
			else type
			dev.hold()
			on.exit(dev.flush())
			plot.default(xy, type = ptype, xlab = xlab, ylab = ylab,
									 xlim = xlim, ylim = ylim, log = log, col = col,
									 bg = bg, pch = pch, cex = cex, lty = lty, lwd = lwd,
									 axes = axes, frame.plot = frame.plot, ann = ann,
									 main = main, ...)
			if (missing(xy.lines))
				xy.lines <- do.lab
			if (do.lab)
				text(xy, labels = if (is.character(xy.labels))
					xy.labels
					else if (all(tsp(x) == tsp(y)))
						formatC(unclass(time(x)), width = 1)
					else seq_along(xy$x), col = col, cex = cex)
			if (xy.lines)
				lines(xy, col = col, lty = lty, lwd = lwd, type = if (do.lab)
					"c"
					else "l")
			return(invisible())
		}
		if (missing(ylab)) {
			ylab <- colnames(x)
			if (length(ylab) != 1L)
				ylab <- xlabel
		}
		if (is.matrix(x)) {
			k <- ncol(x)
			tx <- time(x)
			xy <- xy.coords(x = matrix(rep.int(tx, k), ncol = k),
											y = x, log = log)
			xy$x <- tx
		}
		else xy <- xy.coords(x, NULL, log = log)
		if (is.null(xlim))
			xlim <- range(xy$x)
		if (is.null(ylim))
			ylim <- range(xy$y[is.finite(xy$y)])

		par.mar.previous = par()$mar
		par(mar = mar.single)
		plot.new()
		plot.window(xlim, ylim, log, ...)
		if (is.matrix(x)) {
			for (i in seq_len(k)) lines.default(xy$x, x[, i],
																					col = ts.col[(i - 1L)%%length(ts.col) + 1L], lty = ts.lty[(i -
																																																		 	1L)%%length(ts.lty) + 1L], lwd = lwd[(i - 1L)%%length(lwd) +
																																																		 																			 	1L], bg = bg[(i - 1L)%%length(bg) + 1L], pch = pch[(i -
																																																		 																			 																												1L)%%length(pch) + 1L], cex = cex[(i - 1L)%%length(cex) +
																																																		 																			 																																														1L], type = type)
		}
		else {
			lines.default(xy$x, x, col = ts.col[1L], bg = bg, lty = ts.lty[1L],
										lwd = lwd[1L], pch = pch[1L], cex = cex[1L],
										type = type)
		}
		if (show.main && ann)
			title(main = main, xlab = xlab, ylab = ylab, ...)
		if (axes) {

			#
			## Code below adds Q's (for quarters) at right place
			#
			# TODO remove hardcoding (should work only with quarter data..):
			x.at = axis(1, lwd = 0, labels = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)

			if (!all(x.at == round(x.at)))
			{
				index = which((x.at - round(x.at)) %in% (0:4 / 4)[1:4]) # only select .0, .25, .5, .75 to display (in format yearQ{1,2,3,4})
				if (0 < length(index)) x.label	= prettyTime(x.at)
				if (0 == length(index))
				{
					axis(1, lwd = 0, labels = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis, ...)
				}
				else
				{
					x.at	= x.at[index]
					x.label	= x.label[index]
					axis(1, at = x.at, labels = x.label, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
				}
			}
			else
			{ # normal x-axis (ie no Q's):
				axis(1, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
			}


			axis(2, cex.axis = axis.cex,
					 col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
		}
		if (frame.plot)
			box(...)

		legend("topleft", legend = x.legend, inset=c(legend.single.shift,0), xpd = T, box.lwd = 0, cex = legend.name.cex, text.col = name.col, text.font = 3, col = ts.col, lty = ts.lty, lwd = rep(lwd, x.n))

		par(mar = par.mar.previous) # set old settings back
	}
	xlabel <- if (!missing(x))
		deparse(substitute(x))
	ylabel <- if (!missing(y))
		deparse(substitute(y))

	tsview_plot_internal(x = x, y = y, plot.type = plot.type, lwd = lwd, xy.labels = xy.labels,
										xy.lines = xy.lines, panel = panel, nc = nc, xlabel = xlabel,
										ylabel = ylabel, axes = axes, frame.plot = frame.plot, ...)
}

#.tsview_plot(x, plot.type = "single")
