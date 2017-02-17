#' @importFrom utils install.packages installed.packages

getResource = function(file.name) paste0("shiny-app/www/", file.name)

.tsview.lst.as.ts = function(lst, version.prefix = " (", version.postfix = ")")
{
	stopifnot(is.list(lst))

	# Update colnames: 1. give name if NULL, 2. add version (v)
	x.names.version	= NULL
	for (i in 1:length(lst))
	{
		if (is.null(colnames(lst[[i]])))
		{
			colnames(lst[[i]]) = paste("Series", 1:ncol(lst[[i]])) # add missing colnames
		}

		colnames(lst[[i]]) = paste0(colnames(lst[[i]]), version.prefix, i, version.postfix) # add version
		x.names.version = c(x.names.version, colnames(lst[[i]])) # collect names (version)
	}

	# transform list to matrix
	x = NULL
	for (i in 1:length(lst))
	{
		x = ts.union(x, lst[[i]])
	}
	colnames(x) = x.names.version

	return(x)
}

# Define helper functions to work with multiple time series objects (in list form)
.tsview.get.ts.names = function(x, version.prefix = " (", version.postfix = ")")
{
	cn = colnames(x)
	cn = substr(cn, 1, nchar(cn) - nchar(version.prefix) - nchar(version.postfix) - 1)
	cn
}

.tsview.get.index.from.name = function(x, name, version.prefix = " (", version.postfix = ")")
{
	which(.tsview.get.ts.names(x, version.prefix, version.postfix) %in% name)
}

.tsview.get.version.from.index = function(x, index, version.prefix = " (", version.postfix = ")")
{
	name.full	= colnames(x)[index]
	as.numeric( substr(name.full, nchar(name.full) - nchar(version.postfix), nchar(name.full) - nchar(version.postfix)))
}

.darker.col = function(current.col, i.version, n.versions)
{
	n.versions = max(5, n.versions)
	paste0(colorRampPalette(c(current.col, "black"))(n.versions)[min(n.versions, i.version)],"FF")
}

.smaller.lwd = function(lwd.max, i.version, n.versions)
{
	n	= max(3, n.versions)
	i	= min(i.version, n)

	(lwd.max - 1) / (1 - n) * i + 1 - (lwd.max - 1) / (1 - n) * n
}

.adjust.time.range = function(x, time.range)
{
	if (is.na(time.range[1])) time.range[1] = min(time(x))
	if (is.na(time.range[2])) time.range[2] = max(time(x))

	delta.t = diff(time(x))[1]
	if (diff(time.range) < delta.t) time.range[2] = time.range[2] + delta.t

	time.range
}

prettyTime = function(time.label, format = F)
{
  time.label.floor = floor(time.label)

  q1.index = which(time.label - 0    == time.label.floor)
  q2.index = which(time.label -  .25 == time.label.floor)
  q3.index = which(time.label -  .50 == time.label.floor)
  q4.index = which(time.label -  .75 == time.label.floor)

  addQ = function(year, q)
  {
    paste0(if (format) "<B>", year, if (format) '</B><font face="verdana" color="green">', q, if (format) '</font>')
  }
  time.label.floor[q1.index] = addQ(time.label.floor[q1.index], "Q1")
  time.label.floor[q2.index] = addQ(time.label.floor[q2.index], "Q2")
  time.label.floor[q3.index] = addQ(time.label.floor[q3.index], "Q3")
  time.label.floor[q4.index] = addQ(time.label.floor[q4.index], "Q4")

  time.label.floor
}

.install.if.not.installed = function(this.package)
{
  new.packages = this.package[!(this.package %in% utils::installed.packages()[,"Package"])]

  if (length(new.packages))
  {
    utils::install.packages(new.packages)
  }
}
