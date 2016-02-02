library(sweidnumbr)
library(dplyr)

x <- rnd_fake_pin()
x <- fake_pins$pin[2]

y <- sweidnumbr::as.pin(x) 

sweidnumbr::is.pin(y)

fake_pins

pin_ctrl(y)

all(TRUE, 1==1)

ex_pin <- c("196408233234", "196408233235")
pin_ctrl(ex_pin)

fake_pins

sweidnumbr::oin_group()

ex_oin <- c("556000-4615", "232100-0156", "802002-4280")
oin_group(ex_oin)


oin_group("556555-1073")
is.oin("556555-1073")
"556555-1073" %>% as.oin() %>% oin_group
 oin <- "556555-1073"
 pin <- "121212-1212"

sweidnumbr::oin_ctrl()

oin %>% oin_ctrl()
oin %>% oin_group()


oin %>% oin_ctrl() 
pin %>% oin_ctrl() 

"202100-6255 " %>% pin_ctrl()

pin_ctrl("202100-6255")

oin_ctrl("121212-1212")

oin_ctrl("202100-6255")

pin_ctrl("121212-1212")

pin_ctrl(1)
oin_ctrl(2)

"121212-1212" %>% pin_ctrl 
"121212-1212" %>% oin_ctrl
"111212-1212" %>% pin_ctrl 

oin_ctrl(x)
oin_ctrl
pin_ctrl

sessionInfo()
packageVersion("sweidnumbr")
# reprex::reprex()

fake_pins$pin[1:10]

# Sample values to check
num_to_check <- c("202100-6255","121212-1212","19121212-1212","121212+1212","1212121212",
                  "12121212","121212121212",1212121212, NA, Inf, TRUE, F, "foo", 123, 456L)

# Wrapper function for sweidnumbr::pin_ctrl to set failing pin input to FALSE and valid to TRUE
pin_ctrl2 <- Vectorize({
  function(x) {
    ifelse (!is.na(try(suppressWarnings(suppressMessages(pin_ctrl(x))),silent = TRUE)),
            pin_ctrl(x), FALSE)
  }
})

# Wrapper function for sweidnumbr::oin_ctrl to set failing oin input to FALSE and valid to TRUE
oin_ctrl2 <- Vectorize({
  function(x) {
    ifelse ((class(try(suppressWarnings(oin_ctrl(x)), silent =TRUE)) != "try-error"),
            oin_ctrl(x), FALSE)
  }
})

# Are values pin?
pin_ctrl2(num_to_check)

# Are values oin?
oin_ctrl2(num_to_check)



pin_ctrl(num_to_check)
oin_ctrl(num_to_check)

  pin_ctrl2(oin)

pin_ctrl(c("121212-1212","19121212-1212",oin,pin))
pin_ctrl2(c("121212-1212","19121212-1212",oin,pin))

oin_ctrl(c("121212-1212","19121212-1212",oin,pin))
oin_ctrl2(c("121212-1212","19121212-1212",oin,pin))


# 
# class(try(oin_ctrl(pin))) != "try-error"
# 
# try(oin_ctrl(oin)) 
# try(suppressWarnings(oin_ctrl(pin)), silent =TRUE)



my_pin <- "202100-6255"
!(is.na(pin_ctrl(my_pin)) || !pin_ctrl("202100-6255"))

my_oin <- "121212-1212"
!((try(oin_ctrl(my_oin)) != TRUE) || !oin_ctrl(my_oin))

is.null(NULL)

oin_ctrl2(NA)

svar <- try(oin_ctrl(my_oin))


pin_ctrl2(oin) == FALSE

oin_ctrl2(pin)
try(oin_ctrl(pin)) == TRUE
try(pin_ctrl(oin)) == TRUE

tryCatch(pin_ctrl(oin)) == TRUE


