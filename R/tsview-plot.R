#' @importFrom grDevices dev.flush dev.hold xy.coords
#' @importFrom graphics axis box legend lines lines.default mtext par plot.default plot.new plot.window text title
#' @importFrom stats as.ts hasTsp is.ts rnorm time ts ts.intersect tsp

tsview_plot = function (x, plot.type = c("multiple", "single"), show.names = NULL, show.versions = NULL, lwd = 3, time.range = NA, text.size = 1,
											xy.labels, xy.lines, panel = lines, nc, yax.flip = FALSE,
											mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1),
											oma.multi = c(6, 0, 5, 0), cex.axis = text.size, axes = TRUE, frame.plot = FALSE, ...)
{
	#.install.if.not.installed("RColorBrewer")

	if (!is.list(x)) x = list(x)
	stopifnot(length(x) < 10)

	n.versions	= length(x)
	x			= .tsview.lst.as.ts(lst = x)
	x.names		= unique( .tsview.get.ts.names(x) )

	# fill show.* if NULL
	if (is.null(show.names)) show.names = x.names
	if (is.null(show.versions)) show.versions = 1:n.versions

	n.show.names		= length(show.names)
	#n.show.versions	= length(show.versions)

	# zoom x-axis if not NA
	time.range = .adjust.time.range(x, time.range)
	x = suppressWarnings(window(x, start = c(time.range[1]), end = c(time.range[2])))

	# Our settings
	#legend.name.cex	= 2
	n.dots.show.below = 10

	# Create legend
	#x.legend  = if (1 == x.n) "Time series" else colnames(x)
	#if (is.null(colnames(x))) x.legend = paste("Series", 1:x.n)

	show.xlab			= F
	axis.col			= "gray50"
	#axis.cex			= 2
	axis.x.padj			= .5
	show.main			= F
	ts.col				= c("#1B9E77", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#A6CEE3", "#E6AB02", "gray80", "#E31A1C", "mediumorchid1", "seagreen1")[1:n.show.names]#RColorBrewer::brewer.pal(12, "Paired")[1+1:n.show.names]
	name.col			= if (plot.type == "multiple") rep(axis.col, n.show.names) else ts.col
	#ts.lty				= 1:x.n # lty == show.versions
	mar.single			= c(2.5, 6, 0, 7.5 * text.size)
	mar.multiple		= c(2.5, 6, 0, 15)
	legend.single.shift	= .9

	tsview_plot_internal = function(x, plot.type = c("multiple", "single"), xy.labels, xy.lines, panel = lines, nc, xlabel,
										ylabel, type = "l", xlim = NULL, ylim = NULL, xlab = "Time",
										ylab, log = "", col = par("col"), bg = NA, pch = par("pch"),
										cex = par("cex"), lty = par("lty"), lwd = par("lwd"),
										axes = TRUE, frame.plot = axes, ann = par("ann"), cex.lab = par("cex.lab"),
										col.lab = par("col.lab"), font.lab = par("font.lab"),
										cex.axis = par("cex.axis"), col.axis = par("col.axis"),
										font.axis = par("font.axis"), main = NULL, ...) {

		plot.x.axis = function()
		{
			x.at = axis(1, xpd = NA, lwd = 0, labels = NA, cex.axis = cex.axis, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
			if (!all(x.at == floor(x.at)))
			{
				# now we want Q's everywhere			
				index = which((x.at - floor(x.at)) %in% (0:3 / 4)) # only select .0, .25, .5, .75 to display (in format yearQ{1,2,3,4})
				if (0 < length(index)) x.label	= prettyTime(x.at)
				if (length(index) < 2)
				{
				 	x.at	= as.vector(time(x))
					x.label = prettyTime(x.at)
					# axis(1, xpd = NA, lwd = 0, labels = NA, cex.axis = axis.cex, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis, ...)
				}
				else
				{
					x.at	= x.at[index]
					x.label	= x.label[index]

					# add extra ticks for quarters
					x.ticks = as.vector(time(x))
					x.ticks.extra = x.ticks[which(!is.element(x.ticks, x.at) & x.ticks <= max(x.at) & min(x.at) < x.ticks)]
					if (0 < length(x.ticks.extra))
					{
						axis(1, at = x.ticks.extra, labels = F, xpd = NA, cex.axis = cex.axis, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, lwd = 0, lwd.ticks = 1, ...)
					}
				}
				axis(1, at = x.at, labels = x.label, xpd = NA, cex.axis = cex.axis, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
			}
			else
			{ # normal x-axis (ie no Q's):
				axis(1, xpd = NA, cex.axis = cex.axis, col.axis = axis.col, col = axis.col, padj = axis.x.padj, font.axis = font.axis, ...)
			}
		}

		plot.type <- match.arg(plot.type)
		#nser <- NCOL(x) # seems redundant to me

		# Determine whether we want to show dots
		n.dots = nrow(as.matrix(x))
		if ("l" == type && n.dots < n.dots.show.below) type = 'b'

		#
		## Here starts plot.type == "multiple"
		#
		if (plot.type == "multiple" && 1 < n.show.names) {
			addmain = function(main, cex.main = par("cex.main"), font.main = par("font.main"), col.main = par("col.main"), ...)
			{
				mtext(main, side = 3, line = 3, cex = cex.main, font = font.main, col = col.main, ...)
			}

			panel <- match.fun(panel)
			#nser <- NCOL(x)

			if (10 < n.show.names)
				stop("cannot plot more than 10 series as \"multiple\"")
			if (is.null(main))
				main <- xlabel
			if (missing(nc)) nc = if (4 < n.show.names) 2 else 1
			nr <- ceiling(n.show.names/nc)
			oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr, nc))
			on.exit(par(oldpar))

			for (i in 1L:n.show.names) {
				# now show all versions of i in same plot
				i.index			= .tsview.get.index.from.name(x, show.names[i]) # for these ts names we want to plot
				valid.y.values	= !all(is.na(x[,i.index]))
				if (valid.y.values)
				{
					i.ylim		= range(x[, i.index], na.rm = T) # keep y-axis same if user (de)selects versions
					i.version	= .tsview.get.version.from.index(x, i.index) # these versions we have

					# PLOT MULTIPLE:
					plot.default(x, axes = FALSE, xlab = "", ylab = "", log = log, col = col, bg = bg, pch = pch, ann = ann, type = "n", ylim = i.ylim, ...)
					for (j in 1:length(i.index))
					{
						# plot only if we want to show this version
						if (is.element(i.version[j], show.versions))
						{
							panel(x[, i.index[j]], col = .darker.col(ts.col[i], i.version[j], n.versions) , bg = bg, pch = pch, cex = cex, lwd = .smaller.lwd(lwd, i.version[j], n.versions), lty = lty, type = type, ...) # lty = i.version[j]
							
							# dash if version > 1
							if (1 < i.version[j])
							{
								panel(x[, i.index[j]], col = "white", bg = bg, pch = pch, cex = cex, lwd = .smaller.lwd(lwd, i.version[j], n.versions), lty = i.version[j], type = type, ...)
							}
						}
					}
				}
				else
				{
					y.min = 0
					y.max = 1
					plot.default(x, axes = FALSE, xlab = "", ylab = "", log = log, col = col, bg = bg, pch = pch, ann = ann, type = "n", ylim = c(y.min, y.max), ...)
				}
				if (frame.plot)
					box(...)
				y.side <- if (i%%2 || !yax.flip)
					2
				else 4
				do.xax <- i%%nr == 0 || i == n.show.names
				if (axes) {
					if (valid.y.values)
					{
						y.labels = axis(y.side, lwd=0,labels=NA)
						y.min = y.labels[1]
						y.max = y.labels[length(y.labels)]						
					}
					# axis(y.side, at = y.min, hadj = -1, xpd = NA, cex.axis = axis.cex, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
					axis(y.side, at = c(y.min, y.max), padj = c(0, 1), xpd = NA, cex.axis = cex.axis, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
					# axis(y.side, at = c(y.min, y.max), xpd = NA, cex.axis = axis.cex, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
					if (do.xax)
					{
						#
						## Code below adds Q's (for quarters) at right place
						#
						# TODO remove hardcoding (should only work in quarter data..):
						plot.x.axis()
					}
				}

				if (ann) {
					legend("topright", x.names[i], cex = text.size, text.col = name.col[i], text.font = 3, bty = "n")
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
		x.single		= x[, .tsview.get.index.from.name(x, show.names)]
		x.single.ylim	= if (all(is.na(x.single))) c(0,1) else range(x.single, na.rm = T)
		if (!all(is.finite(x.single.ylim))) x.single.ylim = c(0,1)
		par(mar = mar.single)
		plot.default(x.single, axes = FALSE, xlab = "", ylab = "", log = log, col = col, bg = bg, pch = pch, ann = ann, type = "n", ylim = x.single.ylim, ...)

		# add individual lines of versions
		for (i in 1L:n.show.names) {
			# now show all versions of i in same plot
			i.index			= .tsview.get.index.from.name(x, show.names[i]) # for these ts names we want to plot
			# valid.y.values	= !all(is.na(x[,i.index]))
			# if (valid.y.values)
			# {
				i.version	= .tsview.get.version.from.index(x, i.index) # these versions we have

				# PLOT MULTIPLE:
				for (j in 1:length(i.index))
				{
					# plot only if we want to show this version
					if (is.element(i.version[j], show.versions))
					{
						panel(x[, i.index[j]], col = .darker.col(ts.col[i], i.version[j], n.versions) , bg = bg, pch = pch, cex = cex, lwd = .smaller.lwd(lwd, i.version[j], n.versions), lty = lty, type = type, ...) # lty = i.version[j]
						if (1 < i.version[j])
						{
							panel(x[, i.index[j]], col = "white" , bg = bg, pch = pch, cex = cex, lwd = .smaller.lwd(lwd, i.version[j], n.versions), lty = i.version[j], type = type, ...)
						}						
					}
				}
			#}
		}
		
		if (axes) {
			plot.x.axis()
			axis(2, cex.axis = cex.axis, col.axis = axis.col, font.axis = font.axis, col = axis.col, las = 2, ...)
		}
		if (frame.plot)
			box(...)

		legend("topleft", legend = show.names, inset=c(legend.single.shift,0), xpd = T, box.lwd = 0, cex = text.size, text.col = name.col, text.font = 3, col = ts.col, lty = lty, lwd = rep(lwd, n.show.names))

	}


	tsview_plot_internal(x = x, plot.type = plot.type, lwd = lwd, xy.labels = xy.labels, cex.axis = cex.axis,
										xy.lines = xy.lines, panel = panel, nc = nc, xlabel = NULL,
										ylabel = NULL, axes = axes, frame.plot = frame.plot, ...)
}

#x = ts(matrix(rnorm(150), 30, 5), start = c(1961, 1), frequency = 4)
#l = list(a = x, b = x + 1)
#source('ts-helper.R')
#.install.if.not.installed = function(x) 1
# tsview_plot(l, plot.type = "multiple")
# tsview_plot(l, plot.type = "single", show.names = 'a')
# tsview_plot(l, plot.type = "single", show.names = paste("Series", 1:3), text.size = 1)
# tsview_plot(l, plot.type="single", show.names = "MS_HI___", time.range=c(2013,2013.25))