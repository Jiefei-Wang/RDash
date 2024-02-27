################################################
##  All information regarding a panel is here
################################################
panelInfo <- new.env()

## Function to add panel information
addPanelInfo <- function(id, type, outputFunc, drawFunc, configFunc, settingServerFunc){
    if(existsPanel(id))
        stop(glue("Panel id {id} already exists"))
    panelInfo[[id]] <- list(
        id = id,
        type = type,
        outputFunc = outputFunc,
        drawFunc = drawFunc,
        configFunc = configFunc,
        settingServerFunc = settingServerFunc
    )
}

## Accessors 
getPanelIds <- function(){
    names(panelInfo)
}

existsPanel <- function(id){
    if(length(id)!=1)
        stop(glue("Panel id should be a single string, but the length of id is {length(id)}"))
    id %in% getPanelIds()
}

getPanelInfo <- function(id){
    if(!existsPanel(id)){
        stop(glue("Invalid panel id: {id}"))
    }
    panelInfo[[id]]
}


getPanelType <- function(id){
    pi <- getPanelInfo(id)
    pi$type
}

getPanelOutputFunc <- function(id){
    pi <- getPanelInfo(id)
    get(pi$outputFunc)
}

getPanelDrawFunc <- function(id){
    pi <- getPanelInfo(id)
    get(pi$drawFunc)
}

getPanelConfigFunc <- function(id){
    pi <- getPanelInfo(id)
    get(pi$configFunc)
}


getPanelSettingServerFunc <- function(id){
    pi <- getPanelInfo(id)
    get(pi$settingServerFunc)
}


################################################
##  Panel Config
################################################
updateConfig <- function(config, id, args, expectArgNames){
    if(!existsPanel(id)){
        stop(glue("Invalid panel id: {id}"))
    }
    
    config$id <- id
    config$type <- getPanelType(id)

    argsName <- names(args)
    idx <- argsName %in% expectArgNames
    if(!all(idx)){
        stop(glue("Invalid argument: {argsName[!idx]}"))
    }
    for(i in argsName){
        config[[i]] <- args[[i]]
    }
}



################################################
##  Register Panels
################################################
scatterPlotId <- "ScatterPlot"
addPanelInfo(id = scatterPlotId, 
             type = "plot", 
             outputFunc = "plotOutput", 
             drawFunc = "scatterPlotDraw", 
             configFunc = "scatterPlotConfig", 
             settingServerFunc = "scatterPlotSettingServer")

leafletId <- "Leaflet"
addPanelInfo(id = leafletId, 
            type = "leaflet", 
            outputFunc = "leafletOutput", 
            drawFunc = "leafletDraw", 
            configFunc = "leafletConfig", 
            settingServerFunc = "leafletSettingServer")

