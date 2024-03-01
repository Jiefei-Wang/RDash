visNamespace <- "visualizer"
quaNamespace <- "quantifier"



#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # vheight <- 600
    # vwidth <- vheight*2694/1314
    visualizer <- sidebarLayout(
        sidebarPanel(sidebarUI(visNamespace), width = 2),
        mainPanel(
            fluidRow(
                panelUI("panel1", "Panel 1"),
                panelUI("panel2", "Panel 2")
            ),
            fluidRow(
                panelUI("panel3", "Panel 3"),
                panelUI("panel4", "Panel 4")
            ), 
            uiOutput("output"),
            width = 10
        )
    )

    quantifier <- sidebarLayout(
        sidebarPanel(sidebarUI(quaNamespace), width = 2),
        img(src="www/quantifier.png", style = "height: 80vh; width: auto;")
    )

    css <- 'li>a {
        font-size: 20px;
    }

    .navbar-brand {
        font-size: 25px;
    }'

    css2 <- '.navbar-brand {
        font-size: 25px;
    }'


  ui <- navbarPage(
      "ADRD",
      golem_add_external_resources(),
      tabPanel("Visualizer", visualizer, tags$style(HTML(css))),
      tabPanel("Quantifier", quantifier)
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "golex"
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
