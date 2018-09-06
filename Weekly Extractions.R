library(readxl)
Daily_Collections_AR <- read_excel("Cash_Semana.xlsx", 
                          sheet = "Dias")

library(tsibble)
library(hts)
library(tidyverse)
library(furrr)
library(foreach)
library(xts)
library(thief)
Daily_Collections_AR$Chave <- as.Date(Daily_Collections_AR$Chave, "%Y/%m/%d")

Tibble_Frame <- as.tsibble(Daily_Collections_AR, index = Chave)

ped_gaps <- Tibble_Frame %>%
  fill_na(.)

Weeks <- Tibble_Frame %>%
  group_by(year_week = yearweek(Chave)) %>%
  mutate_all(funs(as.numeric)) %>% 
  summarise_all(funs(sum),na.rm = T)

Weeks <- Tibble_Frame[,-1] %>%
  summarise_all(tile_dbl(., mean, .size = 7,na.rm = TRUE))



Weekly <- map_df(ped_gaps[,-1],function(col) tile_dbl(col,mean, .size = 7,na.rm = TRUE))

Teste_Sum <- Weekly[,1]
T
Teste_Sum2 <- ped_gaps[,2]%>% sum(na.rm = T)
head(Teste_Sum2,14)
Some <- Teste_Sum2[8:14,] %>% as.data.frame() %>% as.vector()
Some2 <- as.numeric(unlist(Some))
sum(Some2, na.rm = T)
tail(Teste_Sum2,7) %>% sum(na.rm = T)
# Example 2 with two simple hierarchies (geography and product) to show the argument "characters"

Test <- ped_gaps[,-1]

xts(x=Tibble_Frame[,-1], order.by=Tibble_Frame[,1])

Y_Season<- msts(Test, seasonal.periods=c(7,365.25), start = c(2011))

x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))
frequency(x1)



h <- 12
ally <- aggts(x1)
frequency(x1) <- 2


Testar <- as.tsibble(ally1)


str(ally1)
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
