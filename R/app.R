#' @export 
runDash <- function(...){
    ui <- CANDIS:::makeUI()
    server <-  CANDIS:::server
    shinyApp(ui, server)
}