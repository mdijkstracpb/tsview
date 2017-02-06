.tsview.shiny.table.decimals = 3

.wrapped_server = function(x)
{
  shiny::addResourcePath("shiny-app", system.file("shiny-app", package = "tsview"))

  .server = function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })

    timeRange = range(time(x))
    x.names = colnames(x)
    if (is.null(x.names)) x.names = paste("Series", 1:ncol(x))

    updateSliderInput(session, "timeRange", min = timeRange[1], max = timeRange[2], value = timeRange)

    NewPlot = function(A) renderPlot({
  	plot.type = if (input$switch.plot.type) "single" else "multiple"
  	index = which(x.names %in% A)
      if (0 < length(index))
      {
        tsview:::tsview_plot(tsview.shiny.x.zoom()[, index], plot.type = plot.type)
      }
    })

    tsview.shiny.x.zoom = function()
    {
  	time.range = input$timeRange
      suppressWarnings(window(x, start = c(time.range[1],1), end = c(time.range[2],4)))
    }

    NewTableDataFrame = function(A, format)
    {
      if (is.null(A)) return( data.frame() )

      ts.table				= tsview.shiny.x.zoom()[, which(x.names %in% A), drop = F]

      # round:
      time.label			= time(ts.table)
      ts.table				= format(round(ts.table, .tsview.shiny.table.decimals), .tsview.shiny.table.decimals)
      ts.table				= as.data.frame(ts.table)
      colnames(ts.table)  = A
      rownames(ts.table) 	= tsview:::prettyTime(time.label, format = format)

      ts.table
    }

    NewTable = function(A)
    {
      tdf	  = NewTableDataFrame(A, format = T)

      tdf.  = DT::datatable(tdf, escape = -1, extensions = 'FixedColumns',
                       options = list(
                         searching = F,
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

    output$ui.A = renderUI({
      selectInput("A", label = "2. Select to plot", choices = x.names, multiple = T, selectize = F, size = 31)
    })

    observeEvent(input$A, {
      output$plot = NewPlot(input$A)

      # Add helpTextPlot after first call to plot
      output$helpTextPlot <- renderUI({
        HTML("<BR/>Please note that your data points (x,y) are visualized \"as is\". Hence, the plot is correct only if x refers to a <B>time point</B>.")
      })

      output$table = NewTable(input$A)
    })

    observeEvent(input$filter.query, {
      ts.name.1 = x.names[grepl(input$filter.query, x.names)]

      updateSelectInput(session, "A", choices = ts.name.1)
    })

    observeEvent(input$timeRange, {
      output$plot = NewPlot(input$A)
      output$table = NewTable(input$A)
    })

    observeEvent(input$switch.plot.type, {
      output$plot = NewPlot(input$A)
    })

    output$downloadData <- downloadHandler(
      filename	= "ts.csv",
      content		= function(file) {
        write.csv(NewTableDataFrame(input$A, format = F), file)
      }
    )

    # output$helpTextTable <- renderUI({
    #   HTML("<BR/>Please note that values are rounded to", .tsview.shiny.table.format, "significant digits. Download the table as a CSV file to see all digits.")
    # })
  }

  return(.server)
}
