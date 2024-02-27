
leafletConfigArgs <- c("data", "var", "level", "color", "aggregate", "adjPopulation", "overlayNursingHome")

leafletConfig <- function(config, ...){
    id <- "Leaflet"
    args <- list(...)
    if(is.null(args$color))
        args$color <- "Blues"
    updateConfig(config, id, args, leafletConfigArgs)
}

leafletDraw <- function(output, panelConfig, data, ...){
    leafletData <- reactive({
        validate(need(panelConfig$id=="Leaflet", message=FALSE))

        var <- panelConfig$var
        dataName <- panelConfig$data
        level <- panelConfig$level
        aggregate <- panelConfig$aggregate
        adjPopulation <- panelConfig$adjPopulation
        
        ## TODO:dirty implementation
        if(getDataLevel(dataName) == "subject"){
            if(!isTruthy(aggregate)|| !isTruthy(level))
                stop("The data is at subject level, level and aggregate should be provided")
        }

        filteredData <- getData(data, panelConfig$data)|>
        select(county_fips, all_of(var))
        
        aggregatedData <- aggregateData(
            dataName, 
            filteredData, 
            level, 
            aggregate, 
            var)
            
        rn <- "val"
        names(rn) <- panelConfig$var
        counties |> 
        left_join(aggregatedData, by = c("county_fips" = "county_fips"))->
        dt

        dt[["val"]] <- dt[[var]]

        ######################################################
        ## TODO: Quick but dirty. Find a better way to do this
        ######################################################
        if(isTruthy(adjPopulation)){
            dt <- dt|> 
            left_join(SDOH, by = c("county_fips" = "county_fips"))|>
            mutate(val = val/County_population)
        }
        dt
    })



    output$leaflet <- renderLeaflet({
        validate(need(panelConfig$id=="Leaflet", message=FALSE))
        var <- panelConfig$var
        color <- panelConfig$color
        overlayNursingHome <- panelConfig$overlayNursingHome
        dt <- leafletData()

        ## center of the map
        lat <- mean(range(as.numeric(dt$INTPTLAT)))
        lng <- mean(range(as.numeric(dt$INTPTLON)))
        
        values <- dt$val[!is.na(dt$val)]
        pal <- colorNumeric(color,domain = range(dt$val,na.rm = T))
        plot <- suppressWarnings(
        leaflet(data = dt, options = leafletOptions(preferCanvas = TRUE)) |>
            addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Default Maptile",
            options =
                providerTileOptions(
                    noWrap = TRUE,
                    updateWhenZooming = FALSE,
                    updateWhenIdle = TRUE)
            ) |>
            addPolygons(fillColor = ~pal(val),
                        fillOpacity = 0.7,
                        color = "grey",
                        weight = 1,
                        highlight = highlightOptions(color = "blue",
                                                    weight = 2,
                                                    bringToFront = F,
                                                    opacity = 0.7),
                        layerId = ~COUNTYNS,
                        group = "counties")|>
            addLegend("bottomright", pal = pal, values = ~values,
                opacity = 1, title = var, na.label=""
            )|>
            setView(lng = lng, lat = lat, zoom = 5)
        )
        if(isTruthy(overlayNursingHome)){
            plot <- plot|>
            addMarkers(lng = nursingHome$LONGITUDE, lat = nursingHome$LATITUDE, clusterOptions = markerClusterOptions(),
            popup = "Nursing Home")
        }
        plot
    })
}

