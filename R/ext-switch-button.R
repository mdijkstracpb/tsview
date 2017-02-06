# Customised TRUE-FALSE switch button for Rshiny
# Only sing CSS3 code (No javascript)
#
# Sébastien Rochette
# http://statnmap.com/en/
# April 2016
#
# CSS3 code was found on https://proto.io/freebies/onoff/
# For CSS3 customisation, refer to this website.

switchButton <- function(inputId, label, value=FALSE, col = "GB", type="TF") {

  # color class
  if (col != "RG" & col != "GB") {
    stop("Please choose a color between \"RG\" (Red-Green)
      and \"GB\" (Grey-Blue).")
  }
  if (!type %in% c("OO", "TF", "YN")){
   warning("No known text type (\"OO\", \"TF\" or \"YN\") have been specified,
     button will be empty of text")
  }
  if(col == "RG"){colclass <- "RedGreen"}
  if(col == "GB"){colclass <- "GreyBlue"}
  if(type == "OO"){colclass <- paste(colclass,"OnOff")}
  if(type == "TF"){colclass <- paste(colclass,"TrueFalse")}
  if(type == "YN"){colclass <- paste(colclass,"YesNo")}

  # No javascript button - total CSS3
  # As there is no javascript, the "checked" value implies to
  # duplicate code for giving the possibility to choose default value

  if(value){
    tagList(
      tags$div(class = "form-group shiny-input-container",
        tags$div(class = colclass,
          tags$label(label, class = "control-label"),
          tags$div(class = "onoffswitch",
            tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
              id = inputId, checked = ""
            ),
            tags$label(class = "onoffswitch-label", `for` = inputId,
              tags$span(class = "onoffswitch-inner"),
              tags$span(class = "onoffswitch-switch")
            )
          )
        )
      )
    )
  } else {
    shiny::tagList(
      shiny::tags$div(class = "form-group shiny-input-container",
        shiny::tags$div(class = colclass,
                        shiny::tags$label(label, class = "control-label"),
                        shiny::tags$div(class = "onoffswitch",
                                        shiny::tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox", id = inputId),
                                        shiny::tags$label(class = "onoffswitch-label", `for` = inputId,
                                                          shiny::tags$span(class = "onoffswitch-inner"),
                                                          shiny::tags$span(class = "onoffswitch-switch")
            )
          )
        )
      )
    )
  }
}
