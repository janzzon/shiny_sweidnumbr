library(shiny)
library(sweidnumbr)
library(dplyr)

# Wrapper function for sweidnumbr::pin_ctrl to set failing pin input to FALSE and valid to TRUE
# pin_ctrl2 <- Vectorize({
#   function(x) {
#     ifelse (!is.na(try(suppressWarnings(suppressMessages(pin_ctrl(x))),silent = TRUE)) &
#               (class(try(suppressWarnings(pin_ctrl(x)), silent =TRUE)) != "try-error")  ,
#             pin_ctrl(x), FALSE)
#   }
# })

# Generate a random but valid pin no from data set sweidnumbr::fake_pins
rnd_fake_pin <- function() {
  fake_pins_valid <- sweidnumbr::fake_pins$pin[pin_ctrl(fake_pins$pin, force_logical = TRUE)]
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
  #   p("Mer text"),
  #   p("en till rad")
  )