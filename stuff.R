library(shiny)
library(sweidnumbr)
library(dplyr)

if(!("devtools" %in% installed.packages())) install.packages("devtools")

if(!try(require(sweidnumbrExtras))){
  devtools::install_github("janzzon/sweidnumbrExtras")
  library(sweidnumbrExtras)
}

# Wrapper function for sweidnumbr::pin_ctrl to set failing pin input to FALSE and valid to TRUE
# pin_ctrl2 <- Vectorize({
#   function(x) {
#     ifelse (!is.na(try(suppressWarnings(suppressMessages(pin_ctrl(x))),silent = TRUE)) &
#               (class(try(suppressWarnings(pin_ctrl(x)), silent =TRUE)) != "try-error")  ,
#             pin_ctrl(x), FALSE)
#   }
# })

# Wrapper function for sweidnumbr::oin_ctrl to set failing oin input to FALSE and valid to TRUE
# oin_ctrl2 <- Vectorize({
#   function(x) {
#     ifelse ((class(try(suppressWarnings(oin_ctrl(x)), silent =TRUE)) != "try-error"),
#             oin_ctrl(x), FALSE)
#   }
# })

# Generate a random but valid pin no from data set sweidnumbr::fake_pins
rnd_fake_pin <- function() {
  fake_pins_valid <- sweidnumbr::fake_pins$pin[pin_ctrl2(fake_pins$pin)]
  fake_pins_valid[sample(1:length(fake_pins_valid), 1)]
}
# rnd_fake_pin()


ui <- fluidPage(
  h3("Get info from Swedish identity numbers"),
  p("Provide a identity or organization number (or use the provided random faked identity)
    to get miscellaneous data about that identity."),
  textInput(inputId = "inmatat_pin", label = "Provide a swedish identity or organization number", value = rnd_fake_pin()),
  h5("Result:"),
  textOutput("result1"),
  textOutput("result2")
)

server <- function(input, output) {
  
  input_inmat_pin <- reactive(stringr::str_trim(input$inmatat_pin))
  output$inmatat_pin <- renderText(input$inmatat_pin)
  output$result1 <-
    renderText({
      paste0("You've provided number ", input_inmat_pin())
      
    })
  output$result2 <- renderText({
    if (oin_ctrl2(input_inmat_pin())){
      paste0("This is a swedish organization identiy number for an organization of type \"", oin_group(input_inmat_pin()), "\" ")
    } else {
    
    if (pin_ctrl2(input_inmat_pin())){
      
      paste0("This is a swedish personal number of a ",pin_age(input_inmat_pin())," year old " 
             ,tolower(pin_sex(input_inmat_pin())), 
             if (!(pin_birthplace(input_inmat_pin() ) %>% as.numeric() %>% .[] %in% c(27,28) )) {
               paste0(" born in ", pin_birthplace(input_inmat_pin() ) %>% as.character())
             } else { paste0("")}
             ,"."
             )
    } else {
    if (!(pin_ctrl2(input_inmat_pin()) && !oin_ctrl2(input_inmat_pin()))){
      paste0("This is not a valid swedish personal or organization number. 
      Input a valid number or refresh page to get a random personal number")
    }
    
    
  }}})

}

shinyApp(server = server, ui = ui)

# shiny::runApp()
# shinyapps::deployApp()
# setwd("./ddp_presentation/")
# slidify::publish(title = 'DDP Presentation', 'index.html', host = 'rpubs')
