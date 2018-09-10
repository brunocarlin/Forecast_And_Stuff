# library(readxl)
# Daily_Collections_AR <- read_excel("Cash_Semana.xlsx", 
#                           sheet = "Dias")

library(readr)
Cash_Semana <- read_delim("Cash_Semana.csv", 
                          ";", escape_double = FALSE, col_types = cols(Chave = col_date(format = "%d/%m/%Y")), 
                          trim_ws = TRUE)

library(tsibble)
library(hts)
library(tidyverse)
library(furrr)
library(foreach)
library(xts)
library(thief)






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

Y_Season<- msts(Test, seasonal.periods=c(7,365.25), start = c(2011))

x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))
frequency(x1)

h <- 12
ally <- aggts(x1)
frequency(x1) <- 2

str(ally)
allf <- matrix(NA,nrow = h,ncol = ncol(ally1))
foreach(i = 1:ncol(ally1)) %dopar% {
  allf[,i] <- forecast(tbats(ally1[,i]),h = h)$mean
}

Mean_Tbats <- function(y,h = 12) {
  forecast(tbats(y),h,lambda = 0)$mean
}

Mean_stlm <- function(y,h = 12) {
  forecast(thief(y),h)$mean
}
forecast(stlm(ally2),12,lambda ="auto")
ally2 <- ally[,1]
forecast(tbats(ally2),12)$mean

sum(Mean_Tbats(ally2,31))
sum(Mean_stlm(ally2,31))

T <- tbats(ally2)

map(ally2,Mean_Tbats)

teste <- lapply(ally1,Mean_Tbats)
allf <- as.ts(allf, start = 51)
y.f <- combinef(allf, groups = get_groups(x1), keep ="bottom", algorithms = "lu")

Teste2 <- forecast(
  ally, h = 10, method = "comb", algorithms = "lu",
  FUN = function(x) tbats(x, use.parallel = FALSE)
)

tail(ally2)

y.f[,1]
plot(y.f)
## End(Not run)
