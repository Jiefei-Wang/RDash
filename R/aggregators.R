aggregators <- new.env()

addAggregator <- function(name, func){
    if(existsAggregator(name))
        stop(glue("Aggregator name {name} already exists"))
    aggregators[[name]] <- func
}

getAggregatorNames <- function(){
    names(aggregators)
}

existsAggregator <- function(name){
    if(length(name)!=1)
        stop(glue("Aggregator name should be a single string, but the length of name is {length(name)}"))
    name %in% getAggregatorNames()
}

getAggregator <- function(name){
    if(!existsAggregator(name)){
        stop(glue("Invalid aggregator name: {name}"))
    }
    aggregators[[name]]
}


# Add aggregators
# addAggregator("count", \(x)sum(!is.na(x)))
addAggregator("sum", \(x)sum(x, na.rm = TRUE))
addAggregator("mean", \(x)mean(x, na.rm = TRUE))
addAggregator("No Aggregation", \(x)x)
# addAggregator("median", \(x)median(x, na.rm = TRUE))
# addAggregator("min", \(x)min(x, na.rm = TRUE))
# addAggregator("max", \(x)max(x, na.rm = TRUE))


################################################
## Utils
################################################
aggregateData<- function(datasetName, filteredData, level, aggregatorName, variables){
    if(!isTruthy(level)){
        return(filteredData)
    }
    if(!isTruthy(aggregatorName)){
        return(filteredData)
    }
    geoNames <- getGeoColumnNames(datasetName)
    geoLevels <- sapply(geoNames, \(x)getGeoColumnProp(datasetName, x)$level)
    idx <- which(geoLevels == level)
    stopifnot(length(idx)==1)


    geoName <- geoNames[idx]
    isUnique <- getGeoColumnProp(datasetName, geoName)$unique
    if(isUnique){
        return(filteredData)
    }

    filteredData |> 
        group_by(.data[[geoName]]) |>
        summarise_at(variables, getAggregator(aggregatorName))
}

