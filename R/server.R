library(plyr)
library(ggplot2)


# Define server logic
dt_id <- 1

vheight <- 600
vwidth <- vheight*2694/1314


qheight <- 500
qwidth <- qheight*416/1090




server <- function(input, output) {
    datasets <- reactive({
        nms <- availableDatasets()
        dt <- lapply(nms, getDataset)
        names(dt) <- nms
        dt
    })

    geoLevel <- reactiveVal("county")

    filters <- reactiveValues(
        state = make_filter("state_name", "in", "Texas", visible = FALSE)
    )

    filteredData <- reactive({
        print("filteredData")
        currentFilters <- reactiveValuesToList(filters)
        lapply(datasets(), apply_filters, currentFilters)
    })


    panelConfigs <- list()
    for(i in 1:4){
        panelConfigs[[i]] <- reactiveValues()
    }

    
    leafletConfig(
        panelConfigs[[1]], 
        data = "ADRD",
        var = "ADRD_diag",
        color = "Reds",
        level = "county",
        aggregate = "sum",
        adjPopulation = TRUE,
        overlayNursingHome = TRUE
    )

    leafletConfig(
        panelConfigs[[2]], 
        data = "SDOH",
        var = "sdi",
        color = "Blues"
    )

    scatterPlotConfig(
        panelConfigs[[3]],
        xName = "sdi",
        xDataName = "SDOH",
        xAggregate = NULL,
        adjXPopulation = FALSE, 
        yName = "ADRD_diag",
        yDataName = "ADRD",
        yAggregate = "sum",
        adjYPopulation = TRUE,
        level = "county"
    )

    scatterPlotConfig(
        panelConfigs[[4]], 
        xName = "sdi",
        xDataName = "SDOH",
        xAggregate = NULL,
        adjXPopulation = FALSE, 
        yName = "ADRD_diag",
        yDataName = "ADRD",
        yAggregate = "sum",
        adjYPopulation = TRUE,
        level = "county",
        byGroup = "is_rural"
    )


    ## server to handle the sidebar filter
    siderbarServer(visNamespace, filters, geoLevel)

    # for(i in 4){
    #     scatterPlotConfig(
    #         panelConfigs[[i]], 
    #         xName = "sdi",
    #         xDataName = "SDOH",
    #         xAggregate = NULL,
    #         yName = "pct_Poverty_LT100",
    #         yDataName = "SDOH",
    #         yAggregate = NULL
    #     )
    # }

    lapply(1:4, function(i){
        id <- paste0("panel",i)
        output_server(id, panelConfig = panelConfigs[[i]], data = filteredData)
    })
}

