panelSettingId <- "panelSetting"

settingNS <- function(ns){
    function(id){
        paste0(ns, "_", id)
    }
}

panelSettingServer <- function(panelConfig){
    moduleServer(panelSettingId, function(input, output, session){
        ns <- session$ns

        servers <- lapply(getPanelIds(), getPanelSettingServerFunc)
        servers <- lapply(servers, function(x) x(panelConfig))
        names(servers) <- getPanelIds()

        ## If user click the edit button, show the dialog
        observeEvent(input$edit, {
                selected <- panelConfig$id
                ## selection between different panels
                panelSelectionUI <- radioButtons(
                    inputId = ns("panelType"), 
                    label = h3("Panel Type:"),
                    choices = getPanelIds(),
                    selected = selected,
                    inline = TRUE
                )
                ## find the setting body for each panel type
                settingBody <- list()
                ids <- getPanelIds()
                for(i in seq_along(ids)){
                    settingBody[[i]] <- tabPanelBody(ids[[i]],servers[[i]]$UI())
                }
                
                
                args <- c(list(
                    id = ns("settingSwitcher"),
                    type = "hidden",
                    selected = selected
                    ),
                    settingBody
                )
                ## create diaglog UI
                settingBodySet <- do.call(tabsetPanel, args)
                
                diaglog <- modalDialog(
                    panelSelectionUI,
                    settingBodySet,
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("update"), "Update", class = "btn-primary")
                    )
                )

                ## show the dialog
                showModal(diaglog)
        })

        observeEvent(input$panelType,{
            updateTabsetPanel(session, "settingSwitcher", selected = input$panelType)
            setSelectedPanel(panelConfig, input$panelType)
        })

        observeEvent(input$update, {
            panelTypeId <- input$panelType
            values <- servers[[panelTypeId]]$values()
            configFunc <- getPanelConfigFunc(panelTypeId)
            do.call(configFunc, c(list(config = panelConfig), values))
            removeModal()
        })
    })
}

setSelectedPanel <- function(panelConfig, panelId){
    panelConfig$selectedPanelId <- panelId
}

getSelectedPanel <- function(panelConfig){
    panelConfig$selectedPanelId
}

isSameAsSelected <- function(panelConfig, panelId){
    value <- getSelectedPanel(panelConfig)
    if(is.null(value)){
        setSelectedPanel(panelConfig, panelConfig$id)
    }
    identical(getSelectedPanel(panelConfig), panelId)
}

## update the selectize input
## If the panel is active, the panel does not have value, and the current users have selected a value,
## it will be shown as the default, otherwise, the first value will be shown
updataSelectizeInput <- function(session, input, panelConfig, objectId, choices){
    ## TODO: Dirty
    if(isSameAsSelected(panelConfig, input$id)){
        inputObj <- input[[objectId]]
        configObj <- panelConfig[[objectId]]
        inputEmpty <- is.null(inputObj)|| inputObj==""
        inputValid <- !inputEmpty&&inputObj%in% choices
        configValid <- !is.null(configObj)&& configObj%in% choices
        
        if(inputEmpty&&configValid){
            selected <- configObj
        }else if(inputValid){
            selected <- input[[objectId]]
        }else{
            selected <- choices[1]
        }
    }else{
        selected <- choices[1]
    }
    updateSelectizeInput(
        session = session,
        inputId = objectId,
        choices = choices,
        selected = selected
    )
}





## List the available datasets
updateDataOptions <- function(session, input, panelConfig, dataNameId){
    choices <- availableDatasets()
    updataSelectizeInput(session, input, panelConfig, dataNameId, choices)
}

geoFilter <- function(dataName, data){
    geoColumns <- getGeoColumnNames(dataName)
    !names(data) %in% geoColumns
}

idFilter <- function(dataName, data){
    !names(data) %in% getIdColumnName(dataName)
}

commonFilter <- function(dataName, data){
    geoFilter(dataName, data)&idFilter(dataName, data)
}

numericFilter <- function(dataName, data){
    sapply(data, is.numeric)&commonFilter(dataName, data)
}

## update the variable options based on the selected dataset
## 1. The active panel is the same as the setting panel
##   - Use input dataset
##       - If the dataset is the same as the setting panel, use the selected variable
##       - Otherwise, use the first variable
## 2. The active panel is different from the setting panel
##   - Use the default dataset
##   - Use the first variable
updateVariableOptions <- function(session, input, panelConfig, varNameId, dataNameId, filter, defaultData){
    activePanel <- isSameAsSelected(panelConfig, input$id)
    if(activePanel){
        dataName <- input[[dataNameId]]
    }else{
        dataName <- defaultData
    }
    if(!isTruthy(dataName)){
        return()
    }
    ## Find out the available variables
    data <- getDataset(dataName)
    choices <- names(data)
    choices <- choices[filter(dataName, data)]

    if (activePanel&identical(dataName, panelConfig[[dataNameId]])){
        selected <- panelConfig[[varNameId]]
    } else {
        selected <- choices[1]
    }
    updateSelectizeInput(
        session = session,
        inputId = varNameId,
        choices = choices,
        selected = selected
    )
}
