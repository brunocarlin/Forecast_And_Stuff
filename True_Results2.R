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


Tester_PassadO <- function(delay_actual,delay_forecast) {
  
  Cash_Semana <- read_delim("Cash_Semana.csv", 
                            ";", escape_double = FALSE, col_types = cols(Chave = col_date(format = "%d/%m/%Y")), 
                            locale = locale(decimal_mark = ","),trim_ws = TRUE,guess_max = 10000)
  
  Tibble_df <- as.tsibble(Cash_Semana, index = Chave)  %>% fill_na(.)
  
  Cut_df_Forecast <- head(Tibble_df,delay_forecast)
  Cut_df_Forecast <-  tail(Cut_df_Forecast,- (nrow(Cut_df_Forecast) %% 7 ))
  
  First_Day <- Cut_df_Forecast[1][1, 1] %>%
    unlist %>%
    as.Date() %>%
    ymd() %>% 
    as.Date() %>%
    decimal_date()
  
  #Primeiro_dia_string <- ymd(as.Date(unlist(Primeiro_dia)))
  #Primeiro_dia_decimal <- decimal_date(as.Date(Primeiro_dia_string))
  
  Week_Index_Vector <- rep(1:100000, each = 7, len = nrow(Cut_df_Forecast))
  
  Week_df_Forecast <- cbind(Week_Index_Vector,Cut_df_Forecast[,-1]) %>%
    as.tibble() %>% 
    group_by(Week_Index_Vector) %>%
    summarise_all(funs(sum),na.rm = T)
  
  ALL_Y <-  Week_df_Forecast[,-1] %>% 
    as.matrix()
  
  ALL_Y[ALL_Y <1] <- 1
 
  #Y_Season<- msts(ALL_Y2, seasonal.periods=c(4.28,52.18), start = c(2011))
  Y_Season <- ts(ALL_Y,frequency = 365.25/7, start = First_Day)
  x1 <- gts(Y_Season, characters = list(c(5, 5), c(5, 5)))
  
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
  
  # Actual ------------------------------------------------------------------
  
  Cut_df_Actual <- head(Tibble_df,delay_actual)
  
  Cut_df_Actual <-  tail(Cut_df_Actual,- (nrow(Cut_df_Actual) %% 7 ))
  
  
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


Vector_Actual_Days <- c(-3,
                         -33,
                         -64,
                         -94,
                         -125,
                         -153,
                         -184,
                         -215,
                         -245,
                         -276,
                         -306,
                         -337
                        )


Vector_Forecast_Days <- Vector_Actual_Days - 28

plan(multisession, workers = 3L)
Insanity <- future_map2(Vector_Actual_Days,Vector_Forecast_Days,Tester_PassadO)

save(Insanity, file="fname.RData")


bad_lengths <- map_lgl(Insanity, ~is.null(.x$error) == F)
bad_techs <- Insanity %>%  discard(bad_lengths)

N_Testes <- 12
Ano_Passado <- bad_techs[1:N_Testes]
Temp <- lapply(Ano_Passado, `[[`, 1)

Act_y <- lapply(bad_techs, `[[`, 1)
Forecast_y <- lapply(bad_techs, `[[`, 2) %>% 
  lapply( `[[`, 1) %>% 
  lapply( `[[`, 1)

library(reshape2)
Acty_Matrix <- matrix(unlist(lapply(Act_y, as.vector)), nrow = 4)[,1:N_Testes]
Forc_Matrix <- matrix(unlist(Forecast_y),nrow = 4)[,1:N_Testes]
Erro_Matrix <- Acty_Matrix - Forc_Matrix

#apply(Erro_Matrix, 2, mean)
#mean(Erro_Matrix)

#mean(Erro_Matrix/Acty_Matrix)

Up_to_act <- apply(Acty_Matrix,2,sum)
Up_to_fct <- apply(Forc_Matrix,2,sum)

(Up_to_act- Up_to_fct)/Up_to_fct
mean(Up_to_act - Up_to_fct)

#ok <- tibble(unlist(rbind(lapply(Act_y, as.vector),lapply(Forecast_y, as.vector))))

#names(Temp) <- c("Julho18")
