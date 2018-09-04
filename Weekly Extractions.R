library(readxl)
Daily_Collections_AR <- read_excel("Cash_Semana.xlsx", 
                          sheet = "Dias")

library(tsibble)
library(hts)
library(tidyverse)
library(furrr)
library(foreach)
#Daily_Collections_AR$Column1 <- as.Date(Daily_Collections_AR$Column1, "%d/%m/%Y")

Tibble_Frame <- as.tsibble(Daily_Collections_AR, index = Chave)

ped_gaps <- Tibble_Frame %>%
  fill_na(.full = 0)

Weeks <- Tibble_Frame %>%
  index_by(year_week = yearweek(Chave)) %>%
  mutate_all(funs(as.numeric)) %>% 
  summarise_all(funs(sum),na.rm = T)




# Example 2 with two simple hierarchies (geography and product) to show the argument "characters"



x1 <- gts(Weeks[,-1], characters = list(c(5, 5), c(5, 5)))


h <- 12
ally <- aggts(x1)
ally1 <- msts(ally, seasonal.periods=c(4.35,52.18))
str(ally1)
str(ally)
allf <- matrix(NA,nrow = h,ncol = ncol(ally1))
foreach(i = 1:ncol(ally1)) %dopar% {
  allf[,i] <- forecast(tbats(ally1[,i]),h = h)$mean
}

Mean_Tbats <- function(y,h = 12) {
  forecast(tbats(y),h)$mean
}
forecast(tbats(ally2),12)$mean
Mean_Tbats(ally2)
map(ally2,Mean_Tbats)
ally2 <- ally1[,1]
teste <- lapply(ally1,Mean_Tbats)
allf <- as.ts(allf, start = 51)
y.f <- combinef(allf, groups = get_groups(x1), keep ="bottom", algorithms = "lu")


plot(y.f)
## End(Not run)
