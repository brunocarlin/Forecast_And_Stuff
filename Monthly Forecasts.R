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



Cash_Semana <- read_delim("Cash_Semana.csv", 
                          ";", escape_double = FALSE, col_types = cols(Chave = col_date(format = "%d/%m/%Y")), 
                          locale = locale(decimal_mark = ","),trim_ws = TRUE,guess_max = 10000)

Tibble_df <- as.tsibble(Cash_Semana, index = Chave)  %>% fill_na(.) %>% 
  index_by(year_month = yearmonth(Chave)) %>%  summarise_all(sum,na.rm = T)


First_Day <- Tibble_df[1][1, 1] %>%
  unlist %>%
  as.Date() %>%
  ymd() %>% 
  as.yearmon()

ALL_Y <-  Tibble_df[,-1] %>% 
  as.matrix()

ALL_Y[ALL_Y <1] <- 1



Y_Season <- ts(ALL_Y,frequency = 12, start = First_Day)
x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))

Train <- window(x1, end = c(2018,7))
Test <- window(x1, start = c(2017,2))

tic()
fcasts2.mo <- forecast(
  Train, h = 12, method = "comb", fmethod = "arima", keep.fitted = F, keep.resid = F,parallel = TRUE,
  num.cores = 3, weights = "mint",positive = TRUE
)
toc()
ally <- aggts(x1)
ally1 <- ally
Forecast_Training_Data <- as.list(ally1)
Forecast_Training_Data_Test <- Forecast_Training_Data[1] 



fcasts2.mo <- forecast(
  x1, h = 9, method = "mo", fmethod = "ets", level = 1,
  keep.fitted = TRUE, keep.resid = TRUE
)

ally <- aggts(x1)
ally1 <- ally
Forecast_Training_Data <- as.list(ally1)
Forecast_Training_Data_Test <- Forecast_Training_Data[1] 

plan(multisession)
tic("Whole Process")

Y_Pred <-  future_map(Forecast_Training_Data_Test, safely(function(u,h = 4) {
  
  y <- u
  
  
  # Use Forecast Functions --------------------------------------------------
  
  AR <- function(x, h) {
    forecast(auto.arima(x,
                        ic = "bic",
                        trace = F,
                        lambda = 0
    ), h = h)
  }
  
  TB <- function(x, h) {
    forecast(tbats(x), h = h)
  }
  
  ET <- function(x, h) {
    forecast(ets(x, lambda = 0), h = h)
  }
  
  NN <- function(x, h) {
    forecast(nnetar(x), h = h)
  }
  
  SA <- function(x, h) {
    forecast(stlm(x, method = "arima"), h = h)
  }
  
  SE <- function(x, h) {
    forecast(stlm(x, method = "ets"), h = h)
  }
  
  TH <- function(x, h) {
    forecast(thetaf(x), h = h)
  }
  
  RW <- function(x, h) {
    rwf(x, drift = TRUE, h = h)
  }
  
  SN <- function(x, h) {
    forecast(snaive(x), h = h)
  }
  
  TE <- function(x, h) {
    forecast(thief(x, usemodel = "arima"), h = h)
  }
  
  Forecast_Functions <- list(
    "Auto_Arima"      =     AR,
    "Tbats"           =     TB,
    "Thetha"          =     TH)
  
  
  
  # Besteira ----------------------------------------------------------------
  
  # h <- 4
  # ally <- aggts(x1)
  # 
  # y <- ally[,1]
  
  
  # pos ---------------------------------------------------------------------
  
  List_Forecasts <- Forecast_Saver(y,Forecast_Functions,h)
  
  
  Forecasts_Mean <- lapply(List_Forecasts, `[`, c('mean'))
  
  Mean_Forecasts <- matrix(unlist(Forecasts_Mean), nrow = length(Forecasts_Mean), byrow = TRUE)
  
  colnames(Mean_Forecasts) <- paste("h=", 1:length(Mean_Forecasts[1,]), sep = "")
  rownames(Mean_Forecasts) <- names(Forecast_Functions)
  
  
  List_of_Errors <- Cross_Calculate_Errors_CV(
    y = y,
    List_Functions = Forecast_Functions,
    h = h,
    #window = window,
    Min_Lenght = 24,
    Max_Fold = 5
  )
  
  
  Calculated_Errors <- Calculate_Errors(y,List_Errors = List_of_Errors)
  
  
  Inverted_Errors <- lapply(Calculated_Errors,lapply,Invert_List_Accuracy)
  
  Weight_Matrix <- lapply(Inverted_Errors,Create_Weight_Matrix)
  
  
  colSums(unlist(Weight_Matrix)  * as.matrix(Mean_Forecasts))
  
  
})
,.progress = T)

toc()

# Actual ------------------------------------------------------------------

Cut_df_Actual <- head(Tibble_df,delay_actual)


if (nrow(Cut_df_Actual) %% 7 != 0) {
  Cut_df_Actual <-  tail(Cut_df_Actual,- (nrow(Cut_df_Actual) %% 7 ))
}



First_Day <- Cut_df_Actual[1][1, 1] %>%
  unlist %>%
  as.Date() %>%
  ymd() %>% 
  as.Date() %>%
  decimal_date()

#group_by(year_week = yearweek(Chave)) 

Week_Index_Vector <- rep(1:100000, each = 7, len = nrow(Cut_df_Actual))


Week_df_Actual <- cbind(Week_Index_Vector,Cut_df_Actual[,-1]) %>%
  as.tibble() %>% 
  group_by(Week_Index_Vector) %>%
  summarise_all(funs(sum),na.rm = T)

ALL_Y <-  Week_df_Actual[,-1] %>% 
  as.matrix()


Y_Season_Actual <- ts(ALL_Y,frequency = 365.25/7, start = First_Day)

x1 <- gts(Y_Season_Actual, characters = list(c(5, 5), c(5, 5)))

h <- 4
ally <- aggts(x1)
ally1 <- ally

Actual <- as.list(ally1)
Actual <- Actual[1]
Actual <- tail(Actual$Total,4)

List_Results <- list(Act = Actual,Pred = Y_Pred)
return(List_Results)
}