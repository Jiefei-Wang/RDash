available_ops <- tibble::tibble(
    op_id = c("equal", "not_equal", "less_than", "less_than_or_equal", "greater_than", "greater_than_or_equal", "in", "between"),
    op_name = c("Equal", "Not Equal", "Less Than", "Less Than or Equal", "Greater Than", "Greater Than or Equal", "In", "between"),
    op_symbol = c("==", "!=", "<", "<=", ">", ">=", "%in%", NA)
)

## Create a filter
## var: variable name
## op: operation
## value: value
## id: the unique id used to identify the filter
## visible: whether the filter will be shown on the filter list
make_filter <- function(var, op, value, id = NULL, visible = TRUE){
    list(var = var, op = op, value = value, id= id, visible = visible)
}

apply_filter <- function(dt, filter){
    ## if the variable is not in the data, do not apply anything
    if(!filter$var %in% names(dt)){
        return(TRUE)
    }
    ## special case for "between"
    if(filter$op == "between"){
        filter1 <- make_filter(filter$var, "greater_than_or_equal", filter$value[1])
        filter2 <- make_filter(filter$var, "less_than_or_equal", filter$value[2])
        return(apply_filter(dt, filter2)&apply_filter(dt, filter1))
    }
    
    left <- "dt[[filter$var]]"
    right <- "filter$value"
    symbol <- available_ops[available_ops$op_id == filter$op, "op_symbol"]
    cond <- eval(parse(text=paste0(left, symbol, right)))
    cond[is.na(cond)] <- FALSE
    cond
}


apply_filters <- function(dt, filters){
    if(length(filters)==0){
        return(dt)
    }
    conds <- map(filters, \(x) apply_filter(dt, x))
    dt[Reduce("&", conds), ,drop = FALSE]
}


if(FALSE){
    ## Test
    dt <- tibble(
        a = 1:10,
        b = 11:20
    )
    filter1 <- make_filter("a", "greater_than", 2)
    filter2 <- make_filter("b", "less_than", 18)
    apply_filters(dt, list(filter1, filter2))
}
