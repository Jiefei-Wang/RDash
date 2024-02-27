visNamespace <- "visualizer"
quaNamespace <- "quantifier"



makeUI <- function(){
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
        useShinyjs(),
        tabPanel("Visualizer", visualizer, tags$style(HTML(css))),
        tabPanel("Quantifier", quantifier)
    )
}


