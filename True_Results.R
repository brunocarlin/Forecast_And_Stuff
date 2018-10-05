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


Tester_PassadO <- function(dias_actual,dias_forecast) {
  
  Cash_Semana <- read_delim("Cash_Semana.csv", 
                            ";", escape_double = FALSE, col_types = cols(Chave = col_date(format = "%d/%m/%Y")), 
                            locale = locale(decimal_mark = ","),trim_ws = TRUE)
  
  Split <- Cash_Semana[,1]
  Cash_Semana <-  Cash_Semana[,-1]
  
  Cash_Semana[] <- lapply(Cash_Semana, function(x) as.numeric(as.character(x)))
  
  Cash_Semana <- cbind(Split,Cash_Semana)
  
  
  Tibble_Frame <- as.tsibble(Cash_Semana, index = Chave)
  
  ped_gaps <- Tibble_Frame %>%
    fill_na(.)
  
  Weeks <- head(ped_gaps,dias_forecast)
  Weeks <-  tail(Weeks,- (nrow(ped_gaps) %% 7 )) #%>%
  
  tail(Weeks)
  tail(ped_gaps)
  
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
  
  
  #Y_Season<- msts(ALL_Y2, seasonal.periods=c(4.28,52.18), start = c(2011))
  Y_Season <- ts(ALL_Y2,frequency = 365.25/7, start = Primeiro_dia_decimal)
  
  x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))
  
  
  h <- 4
  ally <- aggts(x1)
  ally1 <- ally
  
  Testar <- as.list(ally1)
  Testar<- Testar[1]
  
  # Actual ------------------------------------------------------------------
  
  
  Weeks <- head(ped_gaps,dias_actual)
  
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
  
  plan(multisession)
  tic("Whole Process")
  
  Maio_Teste <-  future_map(Testar, safely(function(u) {
    
    y <- u
    h <- 4
    
    
    # Use Forecast Functions --------------------------------------------------
    
    AR <- function(x, h) {
      forecast(auto.arima(x,
                          ic = "bic",
                          trace = T,
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
  
  length(Testar$Total)
  length(Actual$Total)
  
  Cauda <- tail(Actual$Total,4)
  Final <- end(Cauda)
  
  TS_Result <- ts(Maio_Teste$Total$result,frequency = 365.25/7,start = Final)
  
  Erros <- as.vector(TS_Result)-Cauda

  List_Results <- list(Cauda,TS_Result,Erros)
  return(List_Results)
}


Resultado_Teste <- Tester_PassadO(-62,-92)

Vector_Actual_Days <- c(-3,-33,-64,-94,-125,-153,-184,-153,-184,-215,-245,-276,-306,-337)


Vector_Forecast_Days <- c(-31,-61,-92,-122,-153,-181,-212,-181,-212,-243,-273,-304,-334,-365)

plan(multisession)
Insanity <- future_map2(Vector_Actual_Days,Vector_Forecast_Days,Tester_PassadO)
