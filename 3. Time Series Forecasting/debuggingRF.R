rm(list=ls())

tsCV_RandomForest <- function(my_xts, xreg = NULL,  forecastfunction, h = 1, window = NULL, 
    initial = 0, ...) {
    
    #print("ok") # CHECK
    
    n <- nrow(my_xts) # Lunghezza totale serie storica

    #Creo una matrice xts degli errori, con lo stesso numero di righe e indice della serie storica
    #ma con un numero di colonne h e vuota
    e <- xts(matrix(NA, nrow = nrow(my_xts), ncol = h), order.by = index(my_xts))
    e_percentage <- e
    y_estimate <- e
    y_groundtruth <- e
    
    if (initial >= n) 
        stop("initial period too long")
    
    # Aggiunge h righe di NA alla matrice di regressiri (quando presenti) e y, con lo scopo di avere h valori NA
    # Dopo la fine della serie storica, in modo da poter passare sempre h dati di test anche quando
    # La serie storica è finita
    if (!is.null(xreg)) {
        if (nrow(xreg) != nrow(my_xts)) 
            stop("xreg must be of the same size as y")
        
        # Creo una matrixe xts di NA, che parte dall'ultimo giorno + 1 della nostra serie storica
        na_df <- seq(from = end(my_xts) + 1, 
            to = end(my_xts) + h, 
            by = "day")
        # Stessa cosa per i regressori
        na_df_xreg <- seq(from = end(xreg) + 1, 
            to = end(xreg) + h, 
            by = "day")

        # Aggiungo la matrice dei NA alla fine della serie storica e della matrice dei regressori
        my_xts <- merge(my_xts, na_df)
        xreg <- merge(xreg, na_df_xreg)

        if (nrow(xreg) != nrow(my_xts)) 
            stop("xreg must be of the same size as y")

    } else {
        # Creo una matrice xts di NA, che parte dall'ultimo giorno + 1 della nostra serie storica
        na_df <- seq(from = end(my_xts) + 1, 
            to = end(my_xts) + h, 
            by = "day")

        # Aggiungo la matrice dei NA alla fine della serie storica
        my_xts <- merge(my_xts, na_df)
    }
    

    #print("ok1") # CHECK

    if (is.null(window)) {
        indx <- seq(1 + initial, n - 1L)
    } else {
        indx <- seq(window + initial, n - 1L, by = 1L)
    }

    #print("ok2") # CHECK

    # Iterazioni
    for (i in indx) {
        # print(i) # CHECK
        
        # Definizione stard e end di train
        start <- ifelse(is.null(window), 1L, ifelse(i - window >= 0L, i - window + 1L, stop("small window")))
        end <- i  # imposto come end i + h perchè così ho il train
        
        # Estraggo la sotto serie storica di train e di test
        y_subset <- my_xts[start:end,]
        y_test <- my_xts[(end + 1):(end + h),]

        # Costruzione della matrice dei ritardi
        #### NOTA BENE: a differenza di quanto accade nel codice di gian, qui converto y in xts.
        #### do lo stesso indice di y_subset privato delle prime 14 osservazioni (così come la matrice dei ritardi)
        y_subset <- xts(embed(y_subset, 15), order.by = index(tail(y_subset, n = -14)))
        colnames(y_subset) <- c("y", paste0("y_", 1:14))

        if (is.null(xreg)) {
            Y <- y_subset
            fc <- try(suppressWarnings(forecastfunction(Y = Y, 
                h = h)), silent = TRUE)
        } else {
            # Subset del train e del test dei regressori
            xreg_subset <- xreg[start:end,]
            xreg_future <- xreg[(end + 1):(end + h),]

            ## Combino i regressori di train con la matrice dei ritardi e y
            # Rimuovo le prime 14 osservazioni di xreg
            xreg_subset <- tail(xreg_subset, n = -14)
            # Combino
            Y <- cbind(y_subset, xreg_subset)
            names(Y) <- c(names(y_subset), names(xreg_subset))

            fc <- try(suppressWarnings(forecastfunction(Y = Y,
                newxreg = xreg_future, h = h)), silent = TRUE)
        }

        # Conversione del vettore delle predizioni in xts con le date del test set
        fc <- as.xts(fc, order.by = date(y_test))
        
        # Calcolo degli errori
        ## La posizione è i + 14 perchè i prim i 14 dati vanno persi a causa dei ritardi
        if (!is.element("try-error", class(fc))) {
            y_groundtruth[i, ] <- y_test[seq(h)]
            y_estimate[i, ] <- fc[seq(h)]

            e[i, ] <- y_test[seq(h)] - fc[seq(h)]
            # Trasposta per poter dividere per ogni riga
            e_percentage[i, ] <- 100*(t(e[i, ])/y_test[seq(h)])
        }
    }
    if (h == 1) {
        return(list(e = e[, 1L], 
        e_percentage = e_percentage[, 1L],
        y_groundtruth = y_groundtruth[, 1L],
        y_estimate = y_estimate[, 1L]))
    }
    else {
        colnames(e) <- paste("h=", 1:h, sep = "")
        colnames(e_percentage) <- paste("h=", 1:h, sep = "")
        colnames(y_groundtruth) <- paste("h=", 1:h, sep = "")
        colnames(y_estimate) <- paste("h=", 1:h, sep = "")
        
        return(list(e = e, 
        e_percentage = e_percentage,
        y_groundtruth = y_groundtruth,
        y_estimate = y_estimate))
    }   
}

set.seed(100)

# Setting librerie utili
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "ggplot2",
              "lubridate", "KFAS", "tseries", "xts", "randomForest") 

# Install packages if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Setting working directory
# working_dir = "C:/Users/marco/OneDrive/UNIMIB_DataScience/99-PROJECTS/DataScienceLab2022/Dati ristoranti"
# setwd(working_dir)

# MAPE 
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#MSE
rmse <- function(actual, pred){
  rmse <- sqrt(mean((actual - pred)^2))
  return (rmse)
}

# Significatività dei parametri
pars_test <- function(coef, var_coef){
  test <- (1-pnorm(abs(coef)/sqrt(diag(var_coef))))*2
  return(test)
}

# Grafico errore percentuale 
err_plot <- function(actual, pred){
  require(xts)
  err_perc <- ((actual - xts(pred, order.by = index(actual)))/(xts(actual, order.by = index(actual))))*100
  return(plot(err_perc, ylab="% errore", main="Errore percentuale di previsione"))

}



r1 <- read.csv("..\\Dati ristoranti\\pre-covid_r1.csv")
r1$data  <- parse_date(r1$data, "%Y-%m-%d", locale = locale("it"))

df_festivi <- readxl::read_xlsx("../Dati aggiuntivi/fest_precovid.xlsx")

f_mod1 <- function(Y, h, newxreg = NULL) {
  # Definizione e train del modello
  rf1 <- randomForest(y~., data = Y)
  
  # Definizione vettore vuoto che conterrà le previsioni
  y_hat <- numeric(h)
  
  ## Regressori per le previsioni
  # Prendo ultima riga della matrice dei regressori, elimino l'ultimo ritardo
  X <- Y[nrow(Y), -15, drop = FALSE] 
  colnames(X) <- colnames(Y)[-1] 
  
  # Itero la generazione dei regressori sulle h previsioni
  for (i in 1:h) {
    y_hat[i] <- predict(rf1, X, predict.all = TRUE)$individual |> median() # predict.all=True mantiene la stima data da tutti gli alberi nella foresta. Dopodichè si vanno a selezionare tutti e a prenderne la mediana (in alternativa, la media)
    # Produco lo shift in X
    # X viene aggionrato ogni volta, le stime entrano dei regressori
    X[1, 2:14] <- X[1, 1:13]
    X[1, 1] <- y_hat[i]
  }
  return(y_hat)
}

f_mod2 <- function(Y, h, newxreg = NULL) {
  # Definizione e train del modello
  rf1 <- randomForest(y~., data = Y)
  
  # Definizione vettore vuoto che conterrà le previsioni
  y_hat <- numeric(h)
  
  ## Regressori per le previsioni
  # Prendo ultima riga della matrice dei regressori, elimino l'ultimo ritardo
  X <- Y[nrow(Y), -15, drop = FALSE] 
  # Fix
  #X[1, c("Festivo", "Pioggia", "yday", "wday")] <- newxreg[1, c("Festivo", "Pioggia", "yday", "wday")]
  # Fine fix
  colnames(X) <- colnames(Y)[-1] 
  
  # Itero la generazione dei regressori sulle h previsioni
  for (i in 1:h) {
    # Aggiorniamo anche i regressori
    X[1, c("Festivo", "Pioggia", "yday", "wday")] <- newxreg[i, c("Festivo", "Pioggia", "yday", "wday")]
    y_hat[i] <- predict(rf1, X, predict.all = TRUE)$individual |> median() # predict.all=True mantiene la stima data da tutti gli alberi nella foresta. Dopodichè si vanno a selezionare tutti e a prenderne la mediana (in alternativa, la media)
    # Produco lo shift in X
    # X viene aggionrato ogni volta, le stime entrano dei regressori
    X[1, 2:14] <- X[1, 1:13]
    X[1, 1] <- y_hat[i]
  }
  return(y_hat)
}

f_mod3 <- function(Y, h, newxreg = NULL) {
  # Definizione e train del modello
  rf1 <- randomForest(y~., data = Y)
  
  # Definizione vettore vuoto che conterrà le previsioni
  y_hat <- numeric(h)
  
  ## Regressori per le previsioni
  # Prendo ultima riga della matrice dei regressori, elimino l'ultimo ritardo
  X <- Y[nrow(Y), -15, drop = FALSE] 
  colnames(X) <- colnames(Y)[-1] 
  
  # Itero la generazione dei regressori sulle h previsioni
  for (i in 1:h) {
    y_hat[i] <- predict(rf1, X, predict.all = TRUE)$individual |> median() # predict.all=True mantiene la stima data da tutti gli alberi nella foresta. Dopodichè si vanno a selezionare tutti e a prenderne la mediana (in alternativa, la media)
    # Produco lo shift in X
    # X viene aggionrato ogni volta, le STIME entrano dei regressori
    X[1, 2:14] <- X[1, 1:13]
    # Aggiorniamo anche i regressori con i VALORI VERI del validation
    X[1, 15:ncol(X)] <- newxreg[i, ]
    X[1, 1] <- y_hat[i]
  }
  return(y_hat)
}

h = 74 # 6 settimane
initial = 365 # Un anno 365
window = NULL # no moving window, si rolling origin

# Calcolo tempo di computazione
start.time <- Sys.time()
print(start.time)

#rf_r1 <- xts(r1[, "lordototale"], as.Date(as.character(r1$data), format = "%Y-%m-%d"))
rf_r1 <- xts(r1[, c("lordototale", "Festivo", "Pioggia")], as.Date(as.character(r1$data), format = "%Y-%m-%d"))

rf_r1$Festivo[rf_r1$Festivo=="False"] <- 0
rf_r1$Festivo[rf_r1$Festivo=="True"] <- 1
rf_r1$Pioggia[rf_r1$Pioggia==""] <- 0
rf_r1$Pioggia[rf_r1$Pioggia=="True"] <- 1
yday <- yday(time(rf_r1))
wday <- wday(time(rf_r1), week_start = getOption("lubridate.week.start", 1))
data <- cbind(rf_r1, yday, wday)

# CV su MODELLO 2

e_2 <- tsCV_RandomForest(my_xts = data[,1], xreg = data[,-1], forecastfunction = f_mod2, h=h, initial = initial, window = window)
e2 <- e_2$e
e2_percentage <- e_2$e_percentage
e2_estimate <- e_2$y_estimate
e2_groundtruth <- e_2$y_groundtruth

stime_shiftate <- c(as.list(e2_estimate["2019-09-25", 2:74]), NA)
#stime_shiftate <- as.list(e2_estimate["2019-10-25", ])
truth <- as.list(e2_groundtruth["2019-09-25", ])
result <- unlist(Map(function(x, y) x - y, truth, stime_shiftate))
sqrt(mean(result[1:43]^2))