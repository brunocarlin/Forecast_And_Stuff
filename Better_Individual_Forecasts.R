library(forecast)
library(thief)

AR <- function(x, h) {
  forecast(auto.arima(x,
                      #stepwise=FALSE,
                      #approximation=FALSE
  ), h = h)
}

TB <- function(x, h) {
  forecast(tbats(x), h = h)
}

ET <- function(x, h) {
  forecast(ets(x), h = h)
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

Forecast_Functions <- list(
  "Auto_Arima"      =     AR,
  "Tbats"           =     TB,
  "Thetha"          =     TH)



Better_CV_Fold <- function (y, forecastfunction, h = 1, window = NULL,Start= 1,Max_Fold = NULL,Min_Lenght = 0, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  tsp(e) <- tsp(y)
  First_Fold <- ifelse(is.null(Max_Fold), n%%h ,ifelse(n > h*Max_Fold,n - h*Max_Fold,n%%h))
  for (i in seq(First_Fold,n - 1,h)) {
    fc <- try(suppressWarnings(forecastfunction(subset(y, 
                                                       start = ifelse(i- Start >= 0L & i- Min_Lenght >= 0L, ifelse(is.null(window), 1L, ifelse(i - window >= 
                                                                                                                                                 0L, i - window + 1L, stop("small window"))),stop("Too Short")), 
                                                       end = i), h = h, ...)), silent = TRUE)
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  }
  else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}


Cross_Calculate_Errors_CV <- function(
  y,
  List_Functions,
  h,
  window = NULL,
  Start = 1,
  Min_Lenght = 0,
  Max_Fold = NULL
){
  Number_Functions <- length(List_Functions)
  Names <- vector("character",Number_Functions)
  MatrixErrors <- vector("list",Number_Functions)
  for (i in seq_len(Number_Functions)) {
    Names[i] <- names(List_Functions[i])
    MatrixErrors[[i]] <- Better_CV_Fold(
      y = y,
      forecastfunction = List_Functions[[i]],
      h = h,
      window = window,
      Start = Start,
      Min_Lenght = Min_Lenght,
      Max_Fold = Max_Fold
      
    )
  }
  names(MatrixErrors) <- Names
  return(MatrixErrors)
}


List_of_Errors <- Cross_Calculate_Errors_CV(
  y = ally[,1],
  List_Functions = Forecast_Functions,
  h = 4,
  #window = window,
  Min_Lenght = 24,
  Max_Fold = 5
)

Root_Mean_Squared_Error <- function(y = NULL, error) {
  sqrt(mean(error ^ 2, na.rm = TRUE))
}

Mean_Accuracy <- function(y, error) {
  Mean_Accracy_Results <- rbind(
    RMSE = Root_Mean_Squared_Error(y, error)
  )
  colnames(Mean_Accracy_Results) <- "Averaged_Time"
  return(Mean_Accracy_Results)
}


Calculate_Errors <-function(y,List_Errors) {
  
  Number_Models <- length(List_Errors)
  Error_Metrics_Mean <- vector("list",Number_Models)
  
  for(i in seq_len(Number_Models)) {
    Error_Metrics_Mean[[i]] <- Mean_Accuracy(y,List_Errors[[i]])
  }
  names(Error_Metrics_Mean) <- names(List_Errors)
  
  return(list(Mean_Error_Metrics =Error_Metrics_Mean))
  
  
}



Calculated_Errors <- Calculate_Errors(ally[,1],List_Errors = List_of_Errors)


Invert_List_Accuracy <- function(Accuracy) {
  
  ifelse(Accuracy !=0,1/Accuracy,100000L) 
  
}


Create_Weight_Matrix <- function(List_Accuracy) {
  
  RowNames <- names(List_Accuracy)
  
  ColNames <- colnames(List_Accuracy[[1]])
  
  ListNames <- rownames(List_Accuracy[[1]])
  
  Number_Models <- length(List_Accuracy)
  
  Number_Errors <- length(List_Accuracy[[1]][,1])
  
  Number_Predictions <- length( List_Accuracy[[1]][1,])
  
  
  Weight_Matrix <- vector("list",Number_Errors)
  Temporary_Matrix <-
    matrix(0, nrow = Number_Models, ncol = Number_Predictions)
  
  rownames(Temporary_Matrix) <- RowNames
  colnames(Temporary_Matrix) <- ColNames
  
  for(i in seq_len(Number_Errors)){
    
    
    for (j in seq_len(Number_Models)) {
      
      Temporary_Matrix[j,] <- List_Accuracy[[j]][i,1:Number_Predictions]
    }
    Temporary_Matrix <- sweep(Temporary_Matrix, 2, colSums(Temporary_Matrix), FUN = "/")
    
    Weight_Matrix[[i]] <- Temporary_Matrix
  }
  names(Weight_Matrix) <- ListNames
  return(Weight_Matrix)
}


Invert_List_Accuracy <- function(Accuracy) {
  
  ifelse(Accuracy !=0,1/Accuracy,100000L) 
  
}

Inverted_Errors <- lapply(Calculated_Errors,lapply,Invert_List_Accuracy)

Weight_Matrix <- lapply(Inverted_Errors,Create_Weight_Matrix)


Forecast_Saver <- function(x,List_Functions,h) {
  
  Number_Functions <- length(List_Functions)
  Names <- vector("character",Number_Functions)
  Forecass_Obeject <- vector("list",Number_Functions)
  
  for(i in seq_len(Number_Functions)) {
    Forecass_Obeject[[i]] <- List_Functions[[i]](x,h)
    Names[i] <- names(List_Functions[i])
    
    
  }
  names(Forecass_Obeject) <- Names
  return(Forecass_Obeject)
  
}

List_Forecasts <- Forecast_Saver(ally[,1],Forecast_Functions,4)

Forecasts_Mean <- lapply(List_Forecasts, `[`, c('mean'))

Mean_Forecasts <- matrix(unlist(Forecasts_Mean), nrow = length(Forecasts_Mean), byrow = TRUE)

colnames(Mean_Forecasts) <- paste("h=", 1:length(Mean_Forecasts[1,]), sep = "")
rownames(Mean_Forecasts) <- names(Forecast_Functions)



Multiplication_Forecast <- function(MatrixWeights,Forecast) {
  
  if( ncol(MatrixWeights) > 1) {
    Result <- diag(t(MatrixWeights) %*% Forecast,names = FALSE)
    
  } else {
    
    Result <-  t(MatrixWeights) %*% Forecast 
  }
  
  Result<- 1 %*% Result
  colnames(Result) <- colnames(Forecast)
  rownames(Result) <- "Teste"
  return(Result)
}


Multiplication_Forecast(as.matrix(Weight_Matrix),Mean_Forecasts)

Answers <- as.vector(unlist(Weight_Matrix)) * Mean_Forecasts
