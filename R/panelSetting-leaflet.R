################################################
## leaflet setting UI
################################################
leafletSettingUI <- function(session){
    ns <- session$ns
    tagList(
        hiddenDataElement(ns("id"), leafletId),
        selectizeInput(ns("data"), "Data", choices = NULL),
        selectizeInput(ns("var"), "Variable", choices = NULL),
        selectizeInput(ns("level"), "Level", choices = NULL),
        selectizeInput(ns("color"), "Color", choices = NULL),
        selectizeInput(ns("aggregate"), "Aggregate", choices=NULL),
        checkboxInput(ns("adjPopulation"), "Adjust by Population?", value = FALSE),
        checkboxInput(ns("overlayNursingHome"), "Overlay Nursing Home?", value = FALSE)
    )
}

################################################
## Setting server
################################################
leafletSettingServer <- function(panelConfig){
    defaultData <- "SDOH"
    moduleServer(leafletId, function(input, output, session){
        ns <- session$ns

        ## Monitor the change of the dataset
        observeEvent(input$data, {
            isolate(updateDataOptions(session, input, panelConfig, "data"))
        })

        observeEvent(input$data, {
            updateVariableOptions(session, input, panelConfig, "var", "data", numericFilter, defaultData)             
        })

        observeEvent(input$color, {
            choices <- c("Blues", "Greens", "Reds")
            updataSelectizeInput(session, input, panelConfig, "color", choices)
        })

        observeEvent(input$level, {
            choices <- c("county")
            updataSelectizeInput(session, input, panelConfig, "level", choices)
        })

        observeEvent(input$aggregate, {
            updataSelectizeInput(session, input, panelConfig, "aggregate", getAggregatorNames())
        })

        observeEvent(list(input$data, input$var), {
            if(identical(input$data, panelConfig$data)){
                if(identical(input$var, panelConfig$var)){
                    updateCheckboxInput(session, "adjPopulation", value = panelConfig$adjPopulation)
                    updateCheckboxInput(session, "overlayNursingHome", value = panelConfig$overlayNursingHome)
                }
            }
        })

        values <- reactive({
            list(
                data = input$data,
                var = input$var,
                level = input$level,
                color = input$color,
                aggregate = input$aggregate,
                adjPopulation = input$adjPopulation,
                overlayNursingHome = input$overlayNursingHome
            )
        })
        
        UI <- reactive(leafletSettingUI(session))
        
        list(
            UI= UI,
            values = values
        )
    })
}


