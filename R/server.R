.tsview.shiny.table.decimals = 3

getDataPath = function(file.name) paste0(system.file("shiny-app", package = "tsview"), "/data/", file.name)

.wrapped_server = function(x)
{
  shiny::addResourcePath("shiny-app", system.file("shiny-app", package = "tsview"))

  .server = function(input, output, session) {
		#     session$onSessionEnded(function() {
		# stopApp()
		#     })

observe({
	query = shiny::parseQueryString(session$clientData$url_search)		
	if (!is.null(query$file))
	{
		file.path = getDataPath(query$file)
		if (file.exists(file.path))	x = dget(file.path)
	}

	start.empty = is.null(x)
	
	# FIX: this is ugly
	if (!is.list(x))
	{
		x = list("Current" = x)
	}	

	if (start.empty)
	{
		x.names = NULL
		x.lst = x
	}
	else
	{	
		# If list elts have no name, give them names
		if (is.null(names(x)))
		{
			names(x) = letters[1:length(x)]
		}
		index.no.name = which(is.na(names(x)) | "" == names(x))
		if (0 < length(index.no.name))
		{
			names(x)[index.no.name] = letters[index.no.name]
		}

		x.lst = x
		x = .tsview.lst.as.ts(lst = x) # from now on x is matrix

		#x.versions	= unique( .tsview.get.index.from.name(x.names) )
	    #if (is.null(x.names)) x.names = paste("Series", 1:ncol(x))

		x.names		= unique( .tsview.get.ts.names(x) )
		timeRange	= timeRange.select = range(time(x))
		if (!is.null(query$x))
		{
			timeRange.select.test = as.numeric(unlist(strsplit(query$x,",")))
			if (2 == length(timeRange.select.test) && all(is.finite(timeRange.select.test))) timeRange.select = timeRange.select.test
		}
	    updateSliderInput(session, "timeRange", min = timeRange[1], max = timeRange[2], value = timeRange.select)
	}		

	add.prefix.to.version = function(x.lst)
	{
		paste0("(", 1:length(x.lst), ") ", names(x.lst))
	}

    NewPlot = function(A) renderPlot({
      plot.type = if (input$switch.plot.type) "single" else "multiple"
	  
	  if (1 < length(x.lst))
	  {
		  # we deal with versions
		  show.versions = which(add.prefix.to.version(x.lst) %in% input$versions)
	  }
	  else show.versions = NULL
		  

      if (0 < length(A))
      {
		  #a <<- tsview.shiny.x.zoom()
		  #b <<- A
		  tsview_plot(x.lst, plot.type = plot.type, lwd = 8, show.names = A, time.range = input$timeRange, show.versions = show.versions, text.size = 2)
      }
    })

    tsview.shiny.x.zoom = function()
    {
		time.range = .adjust.time.range(x, input$timeRange)
		suppressWarnings(window(x, start = time.range[1], end = time.range[2]))
    }

    NewTableDataFrame = function(A, format)
    {
		if (is.null(A)) return( data.frame() )

		# Filter right names
		index.col = which(.tsview.get.ts.names(x) %in% A)

		# Filter right versions
		if (1 < length(x.lst))
		{
			show.versions = which(add.prefix.to.version(x.lst) %in% input$versions)
			i.version	= .tsview.get.version.from.index(x, index.col) # these versions we have	
			index.col	= index.col[which(i.version %in% show.versions)]
			
			if (0 == length(index.col)) return( data.frame() )
		}  

		ts.table = tsview.shiny.x.zoom()[, index.col, drop = F]


		# round:
		time.label				= time(ts.table)
		ts.table				= format(round(ts.table, .tsview.shiny.table.decimals), .tsview.shiny.table.decimals)
		ts.table				= as.data.frame(ts.table)
		if (1 == length(x.lst)) colnames(ts.table)    = A
		rownames(ts.table) 	= prettyTime(time.label, format = format)

		ts.table
    }

    NewTable = function(A)
    {
      tdf	  = NewTableDataFrame(A, format = T)

      tdf.  = DT::datatable(tdf, escape = -1, extensions = 'FixedColumns',
                       options = list(
                         searching = T,
                         ordering = F,
                         scrollX = T,
                         scrollY = '50vh',
                         scrollCollapse = T,
                         paging = F,
                         dom = 't',
                         fixedColumns = TRUE
                       ),
                       class = 'cell-border stripe'
      )

      DT::renderDataTable(tdf.)
    }

	select.init = function()
	{
		if (is.null(query$sel))
		{
			this.selected = NULL
		}
		else
		{
			index = as.numeric(unlist(strsplit(query$sel,",")))			
			this.selected = x.names[index]
		}
		
		this.selected
	}

    output$ui.A = renderUI({
		selectInput("A", label = "2. Select to plot", choices = x.names, selected = select.init(), multiple = T, selectize = F, size = 31)
    })

    observeEvent(input$A, {
      output$plot = NewPlot(input$A)

      # Add helpTextPlot after first call to plot
      output$helpTextPlot <- renderUI({
        HTML("<BR/>Please note that your data points (x,y) are currently visualized \"as is\". Hence, the plot is correct only if x refers to a <B>time point</B>.")
      })

      output$table = NewTable(input$A)
    }, ignoreNULL = FALSE)

    observeEvent(input$filter.query, {
      ts.name.1 = x.names[grepl(input$filter.query, x.names)]
	  updateSelectInput(session, "A", choices = ts.name.1, selected = if (is.null(input$A)) select.init() else input$A)	
    })

    observeEvent(input$timeRange, {
      output$plot = NewPlot(input$A)
      output$table = NewTable(input$A)
    })

    if (1 < length(x.lst))
	{
		if (is.null(query$v))
		{
			index = 1:length(x.lst)
		}
		else
		{
			index = as.numeric(unlist(strsplit(query$v,",")))			
		}	
	    updateCheckboxGroupInput(session, "versions", choices = add.prefix.to.version(x.lst), selected = add.prefix.to.version(x.lst)[index], inline = T)
	}
	
    observeEvent(input$versions, {
        output$plot = NewPlot(input$A)
        output$table = NewTable(input$A)
    }, ignoreNULL = FALSE)

    observeEvent(input$switch.plot.type, {
      output$plot = NewPlot(input$A)
    })
	
	NewInfoTable = function()
	{
		x.name	= NULL
		x.lab	= NULL

		if (!start.empty && "regts" %in% rownames(installed.packages()))
		{
			for (i in 1:length(x.lst))
			{
				this.labels = regts::ts_labels(x.lst[[i]])
				if (!is.null(this.labels))
				{
					x.name = c(x.name, colnames(x.lst[[i]]))
					x.lab = c(x.lab, this.labels)
				}
			}
		}
		
		tdf = suppressWarnings(unique(data.frame(cbind("Time series" = x.name, "Description" = x.lab))))
		rownames(tdf) = NULL
		
		DT::renderDataTable(DT::datatable(tdf, options = list(autoWidth = F), rownames= FALSE))
	}
	
	output$info = NewInfoTable()

    output$downloadData <- downloadHandler(
      filename	= "ts.csv",
      content		= function(file) {
        write.csv(NewTableDataFrame(input$A, format = F), file)
      }
    )

	output$queryText <- shiny::renderText({
		query <- shiny::parseQueryString(session$clientData$url_search)
	    # Return a string with key-value pairs
	    paste(names(query), query, sep = "=", collapse=", ")
	})
	
    # output$helpTextTable <- renderUI({
    #   HTML("<BR/>Please note that values are rounded to", .tsview.shiny.table.format, "significant digits. Download the table as a CSV file to see all digits.")
    # })
	
})
  }

  return(.server)
}
