sidebarUI <- function(id){
        ns <- NS(id)
        tagList(
            selectInput(ns("state"),
                        label = "State:",
                        choices = c("Texas")),
            sliderInput(ns("ageRange"), label = h3("Age Range"), min = 0, 
                max = 100, value = c(40, 75)),
            tags$h4("Additional Filters:"),
            tags$ul(
                tags$li("Year in 2005-2015"),
                tags$li("Race is not missing")
            ),
            actionButtonStyled(ns("clear all"), "clear all", type = "danger",class="btn-xs")
        )
    } 



siderbarServer <- function(ns, filters, geoLevel){
    moduleServer(visNamespace, function(input, output, session){
        observeEvent(input$ageRange,{
            print(glue("ageRange: {input$ageRange}"))
            filters$age <- make_filter("age", "between", input$ageRange)
        })

        reactive({
            geoLevel(input$geoLevel)
        })
    })
}