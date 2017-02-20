.ui = shiny::fluidPage(theme = getResource("button.css"),
                  shiny::tags$head(shiny::tags$style(".rightAlign{float:right;}")),
                  shiny::titlePanel(title=shiny::div(shiny::img(src=getResource("cpb-logo.png")), "Time series viewer*")),
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      shiny::fluidRow(
                        shiny::tags$div(
							title="POSIX 1003.2 extended regular expressions (Note: you can't use brackets yet.)",
							shiny::textInput("filter.query", "1. Filter (optional)", "")
						),
						shiny::uiOutput('ui.A'),
						shiny::helpText("Ctrl-click or Shift-click to select multiple")
                      )
                    ),

						shiny::mainPanel(
						  shiny::div(title="Slide to zoom x-axis", shiny::sliderInput("timeRange", NULL, min = NA, max = NA, value = c(NA, NA), animate = T, width = "70%", sep="", step = .25), align = "middle"),
						  shiny::checkboxGroupInput("versions", NULL, NULL, inline = T),
						  shiny::tabsetPanel(type = "tabs",
						                     shiny::tabPanel("Plot",
                                           switchButton(inputId = "switch.plot.type", label = "", value = F, col = "GB", type = "TF"),
                                           shiny::br(),
                                           shiny::plotOutput("plot"),
                                           shiny::htmlOutput("helpTextPlot"),
                                           shiny::tags$head(shiny::tags$style("#helpTextPlot{color: lightgray;}"))
                                  ),
						                     shiny::tabPanel("Table",
#                                           div(downloadButton('downloadData', 'Download'), "(visible data only)", align = "right"),
                                           DT::dataTableOutput("table")
                                           # htmlOutput("helpTextTable")
#                                           tags$head(tags$style("#helpTextTable{color: lightgray;}"))
                                  ),
								  			shiny::tabPanel("Info",
								  			DT::dataTableOutput("info")
								  )
                      )

                    )
                  )
)
