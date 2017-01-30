library(shiny)

updateZoom = function(timeRange)
{
  .tsview.shiny.x.zoom <<- window(.tsview.shiny.x, start = c(timeRange[1],1), end = c(timeRange[2],4))
}

NewPlot = function(A) renderPlot({
  index = which(.tsview.shiny.x.names %in% A)
  if (0 < length(index))
  {
    tsview_plot(.tsview.shiny.x.zoom[, index], plot.type = plot.type)
  }
})

NewTableDataFrame = function(A, format)
{
  if (is.null(A)) return( data.frame() )

  ts.table				= .tsview.shiny.x.zoom[, which(.tsview.shiny.x.names %in% A)]

  # round:
  ts.table				= signif(ts.table, .tsview.shiny.signif.digits)
  time.label			= time(ts.table)
  ts.table				= as.data.frame(ts.table)
  colnames(ts.table) 		= A
  rownames(ts.table) 		= prettyTime(time.label, format = format)

  ts.table
}

NewTable = function(A)
{
  tdf		= NewTableDataFrame(A, format = T)
  tdf.	= datatable(tdf, escape = -1, extensions = 'FixedColumns',
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
  renderDataTable(tdf.)
}

shinyServer(function(input, output, session) {
  timeRange = range(time(.tsview.shiny.x))
  updateSliderInput(session, "timeRange", min = timeRange[1], max = timeRange[2], value = timeRange)

  output$ui.A = renderUI({
    selectInput("A", label = "2. Select to plot", choices = .tsview.shiny.x.names, multiple = T, selectize = F, size = 31)
  })

  observeEvent(input$A, {
    # updateSelectInput(session, "B", choices = input$A)
    output$plot = NewPlot(input$A)
    output$table = NewTable(input$A)

    # Add helpTextPlot after first call to plot
    output$helpTextPlot <- renderUI({
      HTML("<BR/>Please note that your data points (x,y) are visualized \"as is\". Hence, the plot is correct only if x refers to a <B>time point</B>.")
    })
  })

  observeEvent(input$filter.query, {
    ts.name.1 = .tsview.shiny.x.names[grepl(input$filter.query, .tsview.shiny.x.names)]

    updateSelectInput(session, "A", choices = ts.name.1)
  })

  observeEvent(input$timeRange, {
    updateZoom(input$timeRange)
    output$plot = NewPlot(input$A)
    output$table = NewTable(input$A)
  })

  observeEvent(input$switch.plot.type, {
    plot.type <<- if (input$switch.plot.type) "single" else "multiple"
    output$plot = NewPlot(input$A) #if (is.null(input$B)) NewPlot(input$A) else NewPlot(input$B)
  })

  output$downloadData <- downloadHandler(
    filename	= "ts.csv",
    content		= function(file) {
      write.csv(NewTableDataFrame(input$A, format = F), file)
    }
  )

  output$helpTextTable <- renderUI({
    HTML("<BR/>Please note that values are rounded to", .tsview.shiny.signif.digits, "significant digits. Download the table as a CSV file to see all digits.")
  })
})
