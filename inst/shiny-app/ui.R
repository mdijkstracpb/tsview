library(shiny)

shinyUI(fluidPage(theme = "button.css",
                  tags$head(tags$style(".rightAlign{float:right;}")),
                  titlePanel(title=div(img(src="cpb-logo.png"), "Time series")),
                  sidebarLayout(
                    sidebarPanel(
                      fluidRow(
                        textInput("filter.query", "1. Filter (optional)", ""),
                        uiOutput('ui.A'),
                        helpText("Ctrl-click or Shift-click to select multiple")
                      )
                    ),

                    mainPanel(
                      div(title="Slide to zoom x-axis", sliderInput("timeRange", "", min = NA, max = NA, value = c(NA, NA), animate = T, width = "70%", sep="", round = T), align = "middle"),
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot",
                                           switchButton(inputId = "switch.plot.type", label = "", value = F, col = "GB", type = "TF"),
                                           br(),
                                           plotOutput("plot"),
                                           htmlOutput("helpTextPlot"),
                                           tags$head(tags$style("#helpTextPlot{color: lightgray;}"))
                                  ),
                                  tabPanel("Table",
                                           div(downloadButton('downloadData', 'Download'), "(visible data only)", align = "right"),
                                           dataTableOutput("table"),
                                           htmlOutput("helpTextTable"),
                                           tags$head(tags$style("#helpTextTable{color: lightgray;}"))
                                  )
                      )

                    )
                  )
))
