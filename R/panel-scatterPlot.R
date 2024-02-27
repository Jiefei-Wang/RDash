scatterPlotConfigArgs <- c("level", "xName", "xDataName", "xAggregate", "yName", "yDataName", "yAggregate", "byGroup", "adjXPopulation", "adjYPopulation")

scatterPlotConfig <- function(config, ...){
    id <- "ScatterPlot"
    args <- list(...)
    updateConfig(config, id, args, scatterPlotConfigArgs)
}


################################################
## scatter plot UI
################################################

scatterPlotXData <- function(data, panelConfig){
    getData(data, panelConfig$xDataName)
}

scatterPlotYData <- function(data, panelConfig){
    getData(data, panelConfig$yDataName)
}

## TODO:dirty
makeData <- function(data, dataName, varName, level, aggregatorName, byGroup, adjPopulation){
    dt <-  getData(data, dataName)
    
    allVars <- varName
    if(isTruthy(byGroup)&&byGroup!="None"&&byGroup%in%colnames(dt)){
            allVars <- c(allVars, byGroup)
    }
    ## TODO: dirty implementation
    if(getDataLevel(dataName) == "subject"&& level == "county"){
        dt <- aggregateData(dataName, dt, "county", aggregatorName, allVars)
        dt$id <- dt$county_fips
    }else{
        id <- dt[[getIdColumnName(dataName)]]
        dt$id <- id
    }
    print(adjPopulation)
    if(isTruthy(adjPopulation)){
        tmp <- SDOH|> select(county_fips, County_population)

        dt <- dt|>left_join(tmp, by = c("county_fips" = "county_fips"))|>
        mutate_at(varName, ~./County_population)
    }

    dt|>select(id, all_of(allVars))
}


scatterPlotDraw <- function(output, panelConfig, data, ...){
    xData <- reactive({
        validate(need(panelConfig$id==scatterPlotId, message=FALSE))
        xDataName <- panelConfig$xDataName
        xName <- panelConfig$xName
        xAggregate <- panelConfig$xAggregate
        level <- panelConfig$level
        byGroup <- panelConfig$byGroup
        adjPopulation <- panelConfig$adjXPopulation
        makeData(data, xDataName, xName, level, xAggregate, byGroup,adjPopulation)
    })

    yData <- reactive({
        validate(need(panelConfig$id==scatterPlotId, message=FALSE))
        yDataName <- panelConfig$yDataName
        yName <- panelConfig$yName
        yAggregate <- panelConfig$yAggregate
        level <- panelConfig$level
        byGroup <- panelConfig$byGroup
        adjPopulation <- panelConfig$adjYPopulation
        makeData(data, yDataName, yName, level, yAggregate,byGroup, adjPopulation)
    })

    finalData <- reactive({
        validate(need(panelConfig$id==scatterPlotId, message=FALSE))
        xDt <- xData()
        yDt <- yData()
        byGroup <- panelConfig$byGroup
        if(isTruthy(byGroup)&&byGroup!="None"){
           if(byGroup%in% colnames(xDt) && byGroup%in% colnames(yDt)){
                xDt <- xDt|>select(-all_of(byGroup))
            }
        }
        left_join(xDt, yDt, by = "id")
    })


    output$plot <- renderPlot({
        validate(need(panelConfig$id==scatterPlotId, message=FALSE))
        xName <- panelConfig$xName
        yName <- panelConfig$yName
        byGroup <- panelConfig$byGroup
        dt <- finalData()
        
        
        if(isTruthy(byGroup)&&byGroup!="None"){
            ggplot(dt, aes(x = .data[[xName]], y = .data[[yName]], color = .data[[byGroup]])) +
            geom_point() +
            xlab(xName) +
            ylab(yName)
        }else{
            ggplot(dt, aes(x = .data[[xName]], y = .data[[yName]])) +
                geom_point() +
                xlab(xName) +
                ylab(yName)
        }
    })
}
