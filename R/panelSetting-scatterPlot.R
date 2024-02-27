################################################
## scatter plot setting UI
################################################
scatterPlotSettingUI <- function(session){
    ns <- session$ns
    tagList(
        hiddenDataElement(ns("id"), scatterPlotId),
        selectizeInput(ns("level"), "Level", choices = NULL),
        selectizeInput(ns("xDataName"), "X Dataset", choices = NULL),
        selectizeInput(ns("xName"), "X Variable", choices = NULL),
        selectizeInput(ns("xAggregate"), "X Aggregate", choices = NULL),
        checkboxInput(ns("adjXPopulation"), "Adjust X by Population?", value = FALSE),
        selectizeInput(ns("yDataName"), "Y Dataset", choices = NULL),
        selectizeInput(ns("yName"), "Y Variable", choices = NULL),
        selectizeInput(ns("yAggregate"), "Y Aggregate", choices = NULL),
        checkboxInput(ns("adjYPopulation"), "Adjust Y by Population?", value = FALSE),
        selectizeInput(ns("byGroup"), "By Group", choices = NULL)
    )
}


################################################
## Setting server
################################################
scatterPlotSettingServer <- function(panelConfig){
    defaultData <- "SDOH"
    moduleServer(scatterPlotId, function(input, output, session){
        ns <- session$ns

        availableDataChoices <- reactive({
            req(input$level)
            ## TODO: Dirty implementation
            dt <- availableDatasets()
            dtLevels <- sapply(dt, getDataLevel)
            if(input$level == "subject"){
                dt <- dt[dtLevels == "subject"]
            }
            dt
        })

        ## Monitor the change of the dataset for x axis
        observe({
            choices <- availableDataChoices()
            isolate(updataSelectizeInput(session, input, panelConfig, "xDataName", choices))
        })

        observeEvent(input$xDataName, {
            updateVariableOptions(session, input, panelConfig, "xName", "xDataName", numericFilter, defaultData)
        })

        ## Monitor the change of the dataset for y axis
        observe({
            choices <- availableDataChoices()
            isolate(updataSelectizeInput(session, input, panelConfig, "yDataName", choices))
        })

        observeEvent(input$yDataName, {
            updateVariableOptions(session, input, panelConfig, "yName", "yDataName", numericFilter, defaultData)
        })

        ## by group options
        observeEvent(list(input$xDataName,input$yDataName),{
            xDataName <- input$xDataName
            yDataName <- input$yDataName
            data <- unique(c(xDataName, yDataName))
            ## TODO:dirty
            choices <- c("None")
            if("SDOH" %in% data){
                choices <- c(choices, "is_rural")
            }
            if("ADRD" %in% data){
                choices <- c(choices, "sex")
            }
            if(length(choices)>0){
                shinyjs::show("byGroup")
                updataSelectizeInput(session, input, panelConfig, "byGroup", choices)
            }else{
                updateSelectizeInput(
                    session = session,
                    inputId = "byGroup",
                    selected = "None"
                )
                shinyjs::hide("byGroup")
            }
        })

        observeEvent(input$level, {
            choices <- c("county", "subject")
            isolate(updataSelectizeInput(session, input, panelConfig, "level", choices))
        })

        observeEvent(input$level, {
            req(input$level)
            toggle("adjXPopulation", condition = identical(input$level, "county"))
            toggle("adjYPopulation", condition = identical(input$level, "county"))
        })

        observe({
            req(input$level)
            req(input$xDataName)
            condition <- input$level == "county" && identical(getDataLevel(input$xDataName), "subject")
            toggle("xAggregate", condition = condition)
        })

        observe({
            req(input$level)
            req(input$yDataName)
            condition <- input$level == "county" && identical(getDataLevel(input$yDataName), "subject")
            toggle("yAggregate", condition = condition)
        })

        
        observeEvent(input$xAggregate, {
            updataSelectizeInput(session, input, panelConfig, "xAggregate", getAggregatorNames())
        })
        
        observeEvent(input$yAggregate, {
            updataSelectizeInput(session, input, panelConfig, "yAggregate", getAggregatorNames())
        })


        values <- reactive({
            list(
                xDataName = input$xDataName,
                xName = input$xName,
                yDataName = input$yDataName,
                yName = input$yName,
                xAggregate = input$xAggregate,
                yAggregate = input$yAggregate,
                level = input$level,
                byGroup = input$byGroup,
                adjXPopulation= input$adjXPopulation,
                adjYPopulation= input$adjYPopulation
            )
        })
        
        UI <- reactive(scatterPlotSettingUI(session))
        
        list(
            UI= UI,
            values = values
        )
    })
}



updateUIConfig <- function(panelConfig, values){
    args <- lapply(args, function(x) input[[x]])
    names(args) <- args
    args <- c(list(panelConfig=panelConfig), args)

    id <- input$id
    configFunc <- getPanelConfigFunc(id)
    do.call(configFunc, args)
}

updateScatterPlotUIConfig <- function(input, panelConfig){
    panelConfig$xDataName <- input$xDataName
    panelConfig$xName <- input$xName
    panelConfig$yDataName <- input$yDataName
    panelConfig$yName <- input$yName
    panelConfig
}
