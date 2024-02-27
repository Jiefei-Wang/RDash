columnLevels <- list(
    county_fips = "county"
)

geoLevels <- c("county", "subject")

geoColumnProp <- function(
    colName,
    level,
    unique = TRUE
){
    list(
        colName = colName,
        level = level,
        unique = unique
    )
}

geoColumnsProp <- function(colNames, uniques){
    res <- list()
    for(i in seq_along(colNames)){
        colName <- colNames[i]
        level <- columnLevels[[colName]]
        unique <- uniques[i]
        res[[colName]] <- geoColumnProp(colName, level, unique)
    }
    res
}

.getDatasetDefinition <- function(datasetName){
    stopifnot(existsDataset(datasetName))
    defaultDatasets[[datasetName]]
}

#############################################
## Accessors
#############################################
availableDatasets <- function(){
    names(defaultDatasets)
}

existsDataset <- function(datasetName){
    stopifnot(length(datasetName)==1)
    datasetName%in%availableDatasets()
}

getDataset <- function(datasetName){
    stopifnot(existsDataset(datasetName))
    .getDatasetDefinition(datasetName)$data
}

getGeoColumnNames <- function(datasetName){
    stopifnot(existsDataset(datasetName))
    dt <- .getDatasetDefinition(datasetName)$geoColumns
    sapply(dt, function(x)x$colName)
}

getGeoColumnProp <- function(datasetName, colName){
    stopifnot(existsDataset(datasetName))
    dt <- .getDatasetDefinition(datasetName)$geoColumns
    dt[[colName]]
}

getIdColumnName <- function(datasetName){
    if(datasetName == "ADRD")
        return("id")
    if(datasetName == "SDOH")
        return("county_fips")
    stop(glue("Invalid dataset name: {datasetName}"))
}

isColumnUnique <- function(datasetName, colName){
    stopifnot(existsDataset(datasetName))
    if(colName%in%getIdColumnNames(datasetName))
        return(TRUE)
    if(!colName%in%getGeoColumnNames(datasetName))
        return(FALSE)
    getGeoColumnProp(datasetName, colName)$unique
}

## TODO
getDataLevel<- function(datasetName){
    if(datasetName == "ADRD")
        return("subject")
    if(datasetName == "SDOH")
        return("county")
    stop(glue("Invalid dataset name: {datasetName}"))
}
