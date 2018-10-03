# library(readxl)
# Daily_Collections_AR <- read_excel("Cash_Semana.xlsx", 
#                           sheet = "Dias")

library(readr)
Cash_Semana <- read_delim("Cash_Semana.csv", 
                          ";", escape_double = FALSE, col_types = cols(Chave = col_date(format = "%d/%m/%Y")), 
                          locale = locale(decimal_mark = ","),trim_ws = TRUE)

library(tsibble)
library(hts)
library(tidyverse)
library(furrr)
library(foreach)
library(xts)
library(thief)
library(tictoc)
library(furrr)
library(forecastHybrid)
library(lubridate)
library(hts)




Split <- Cash_Semana[,1]
Cash_Semana <-  Cash_Semana[,-1]

Cash_Semana[] <- lapply(Cash_Semana, function(x) as.numeric(as.character(x)))

Cash_Semana <- cbind(Split,Cash_Semana)


Tibble_Frame <- as.tsibble(Cash_Semana, index = Chave)

ped_gaps <- Tibble_Frame %>%
  fill_na(.)

Weeks <- head(ped_gaps,-61)

tail(Weeks)
tail(ped_gaps)

Weeks <-  tail(Weeks,- (nrow(ped_gaps) %% 7 )) #%>%

Primeiro_dia <- Weeks[1][1,1]
Primeiro_dia_string <- ymd(as.Date(unlist(Primeiro_dia)))
Primeiro_dia_decimal <- decimal_date(as.Date(Primeiro_dia_string))



test <- rep(1:100000, each = 7, len = nrow(Weeks))


Weeks <- head(ped_gaps,-32)
Wekks <-  tail(Weeks,- (nrow(ped_gaps) %% 7 ))

Fake_Weeks <- Wekks %>% 
  group_by(test) %>%
  summarise_all(funs(sum),na.rm = T)

ALL_Y <-  Fake_Weeks[,-1]
ALL_Y2  <- as.matrix(ALL_Y) 
ALL_Y2[ALL_Y2 <1] <- 1

library(lubridate)


#Y_Season<- msts(ALL_Y2, seasonal.periods=c(4.28,52.18), start = c(2011))
Y_Season <- ts(ALL_Y2,frequency = 365.25/7, start = Primeiro_dia_decimal)

x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))


h <- 4
ally <- aggts(x1)
ally1 <- ally

Testar <- as.list(ally1)
Testar<- Testar[1]

# Actual ------------------------------------------------------------------


Weeks <- head(ped_gaps,-31)

tail(Weeks)
tail(ped_gaps)

Weeks <-  tail(Weeks,- (nrow(ped_gaps) %% 7 ))


Primeiro_dia <- Weeks[1][1,1]
Primeiro_dia_string <- ymd(as.Date(unlist(Primeiro_dia)))
Primeiro_dia_decimal <- decimal_date(as.Date(Primeiro_dia_string))

#group_by(year_week = yearweek(Chave)) 


test <- rep(1:100000, each = 7, len = nrow(Weeks))

Wekks <- as.tibble(cbind(test,Weeks[,-1]))


Fake_Weeks <- Wekks %>% 
  group_by(test) %>%
  summarise_all(funs(sum),na.rm = T)

ALL_Y <-  Fake_Weeks[,-1]
ALL_Y2  <- as.matrix(ALL_Y) 
ALL_Y2[ALL_Y2 <1] <- 1

Y_Season <- ts(ALL_Y2,frequency = 365.25/7, start = Primeiro_dia_decimal)

x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))

h <- 4
ally <- aggts(x1)
ally1 <- ally

Actual <- as.list(ally1)
Actual <- Actual[1]




length(Testar$Total)
length(Actual$Total)

Cauda <- tail(Actual$Total,4)
Final <- end(Cauda)

TS_Result <- ts(SomethingTeste2$Total$result,frequency = 365.25/7,start = Final)

Erros <- as.vector(TS_Result)-Cauda
mean(Erros)
Mean_Error(error = Erros)
Mean_Absolute_Percentage_Error(y = Cauda,error = Erros)

mean((100*Erros)/Cauda)


accuracy(TS_Result,Cauda)
sum(SomethingTeste2$Total$result - Acctual)
sum(Acctual)