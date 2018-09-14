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


Weeks <- tail(ped_gaps,- (nrow(ped_gaps) %% 7 )) #%>%
#group_by(year_week = yearweek(Chave)) 

test <- rep(1:100000, each = 7, len = nrow(Weeks))

Wekks <- as.tibble(cbind(test,Weeks[,-1]))


Fake_Weeks <- Wekks %>% 
  group_by(test) %>%
  summarise_all(funs(sum),na.rm = T)

ALL_Y <-  Fake_Weeks[,-1]
ALL_Y2  <- as.matrix(ALL_Y) 
ALL_Y2[ALL_Y2 <1] <- 1

library(lubridate)

#Y_Season<- msts(ALL_Y2, seasonal.periods=c(4.28,52.18), start = c(2011))
Y_Season <- ts(ALL_Y2,frequency = 365.25/7, start =  decimal_date(as.Date("2011-09-02")))
x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))


h <- 4
ally <- aggts(x1)


#str(ally)

# allf <- matrix(NA,nrow = h,ncol = ncol(ally))
# 
Make_Forecast <- function(y,h) {
  forecast(stlm(y, lambda = 0),h)$mean
}

#Make_Forecast(ally[,3],4)

tic()
test <- lapply(ally,Make_Forecast,4)
toc()
allf <- as.data.frame(test)

sum(allf[,1])


# allf <- matrix(NA,nrow = h,ncol = ncol(ally))
# 
# Make_Forecast <- function(y,h) {
#   forecast(tbats(y),h)$mean
# }

# 
# foreach(i = 1:ncol(ally)) %dopar% {
#   allf[,i] <- forecast(tbats(ally[,i]),h = h)$mean
# }

#plan(multiprocess, workers = 3)


#allf <- as.ts(allf, start = 1000)

y.f <- combinef(allf, groups = get_groups(x1), keep ="all", algorithms = "lu")

sum(y.f[,1])
Teste2 <- forecast(
  ally,
  h = 4,
  method = "comb",
  algorithms = "lu",
  positive = T,
  FUN = function(x)
    tbats(x, use.parallel = FALSE)
)

tic()
Teste2 <- forecast(
  x1, h = 4, method = "comb", algorithms = "lu",
  FUN = function(x) hybridModel(x, weights = "cv.errors", cvHorizon = 4,), positive = TRUE
)
toc()


  Testes <- cvts(ally[,1], FUN = auto.arima,
     windowSize = 300, maxHorizon = 4)
  
sum(Teste2$bts)

tail(ally2)

y.f[,1]
plot(y.f)
## End(Not run)