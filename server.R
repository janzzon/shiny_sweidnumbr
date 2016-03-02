library(shiny)
library(sweidnumbr)
library(dplyr)

server <- function(input, output) {
  # Trim input string from leadin and trailing white space, assign to reactive function
  input_inmat_pin <- reactive(stringr::str_trim(input$inmatat_pin))
  output$inmatat_pin <- renderText(input$inmatat_pin)
  output$result1 <-
    renderText({
      paste0("You've provided number ", input_inmat_pin())
      
    })
  output$result2 <- renderText({
    if (oin_ctrl(input_inmat_pin(), force_logical = TRUE)){
      # Check if number is an organization number
      paste0("This is a swedish organization identiy number for an organization of type \"", oin_group(input_inmat_pin()), "\" ")
    } else {
      
      if (pin_ctrl(input_inmat_pin(), force_logical = TRUE)){
        # Check if number is a personal number and extract sex and age  
        paste0("This is a swedish personal number of a ",pin_age(input_inmat_pin())," year old " 
               ,tolower(pin_sex(input_inmat_pin())), 
               if (!(pin_birthplace(input_inmat_pin() ) %>% as.numeric() %>% .[] %in% c(27,28) )) {
                 # Check if birthplace data seems to be present, extract if so.
                 paste0(" born in ", pin_birthplace(input_inmat_pin() ) %>% as.character())
               } else { paste0("")}
               ,"."
        )
      } else {
        if (!(pin_ctrl(input_inmat_pin(), force_logical = TRUE) && !oin_ctrl(input_inmat_pin(), force_logical = TRUE))){
          # Show this text if no valid personal|organizatin number.
          paste0("This is not a valid swedish personal or organization number. 
      Input a valid number or refresh page to get a random personal number")
        }
      }}})
  
}

