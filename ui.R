library(shiny)
library(sweidnumbr)
library(dplyr)

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
  )
