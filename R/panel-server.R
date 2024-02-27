################################################
##  Utils
################################################
getData <- function(data, name){
    data()[[name]]
}

################################################
##  UI
################################################
panelUI <- function(id, title = "Title"){
    ns <- NS(id)
    ns_button <- NS(c(id, panelSettingId))
    panels <- getConditionPanels(ns)
    button <- actionButton(ns_button("edit"), "Edit", class = "btn-xs", style = "float:right")
    panelBox(title, button, panels)
}

getConditionPanels <- function(ns){
    res <- list()
    for(i in getPanelIds()){
        type <- getPanelType(i)
        outputFunc <- getPanelOutputFunc(i)
        res[[i]] <- conditionalPanel(
            condition = glue("output.panelType == '{type}'"),
            outputFunc(ns(type)),
            ns = ns
        )
    }
    res
}

panelBox <- function(title, button, contents){
    div(class = "col-sm-6",
        div(style = "border-style: groove;border-radius: 5px; margin: 5px; padding: 5px",
            div(
                title,
                button
            ),
            div(
                contents
            )
        )
    )
}

################################################
##  Server
################################################
output_server <- function(id, panelConfig, data){
    moduleServer(id, function(input, output, session){
        stopifnot(is(panelConfig, "reactivevalues"))
        
        ## set the panel type at the user side
        output$panelType <- reactive({
            print(panelConfig$type)
            panelConfig$type
        })
        outputOptions(output, 'panelType', suspendWhenHidden = FALSE)

        ## edit button
        panelSettingServer(panelConfig)

        ## render the panel
        for(i in getPanelIds()){
            drawFunc <- getPanelDrawFunc(i)
            drawFunc(id = i, panelConfig = panelConfig, data = data,
                    input = input, output = output, session = session)
        }
    })

}
