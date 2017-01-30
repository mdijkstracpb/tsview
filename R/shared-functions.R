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