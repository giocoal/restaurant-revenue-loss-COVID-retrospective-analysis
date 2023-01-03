tsCV_ARIMA <- function(y, forecastfunction, h = 1, window = NULL, xreg = NULL, 
    initial = 0, ...) {
    y <- as.ts(y) # Serie storica convertita in oggetto ts
    n <- length(y) # Lunghezza totale serie storica
    e <- ts(matrix(NA_real_, nrow = n, ncol = h)) # Inizializzazione matrice degli errori
    e_percentage <- e
    y_estimate <- e
    y_groundtruth <- e

    if (initial >= n) 
        stop("initial period too long")
    tsp(e) <- tsp(y) # Assegnazione tsp attribute di y alla matrice degli errori
    tsp(e_percentage) <- tsp(y)
    tsp(y_estimate) <- tsp(y)
    tsp(y_groundtruth) <- tsp(y)


    if (!is.null(xreg)) {
        xreg <- ts(as.matrix(xreg))
        if (NROW(xreg) != length(y)) 
            stop("xreg must be of the same size as y")
        xreg <- ts(rbind(xreg, matrix(NA, nrow = h, ncol = NCOL(xreg))), 
            start = start(y), frequency = frequency(y))
    }
    if (is.null(window)) 
        indx <- seq(1 + initial, n - 1L)
    else indx <- seq(window + initial, n - 1L, by = 1L)
    for (i in indx) {
        y_subset <- subset(y, start = ifelse(is.null(window), 
            1L, ifelse(i - window >= 0L, i - window + 1L, stop("small window"))), 
            end = i)
        if (is.null(xreg)) {
            fc <- try(suppressWarnings(forecastfunction(y_subset, 
                h = h, ...)), silent = TRUE)
        }
        else {
            xreg_subset <- subset(xreg, start = ifelse(is.null(window), 
                1L, ifelse(i - window >= 0L, i - window + 1L, 
                  stop("small window"))), end = i)
            xreg_future <- subset(xreg, start = i + 1, end = i + 
                h)
            fc <- try(suppressWarnings(forecastfunction(y_subset, 
                h = h, xreg = xreg_subset, newxreg = xreg_future)), 
                silent = TRUE)
        }
        if (!is.element("try-error", class(fc))) {
            y_groundtruth[i, ] <- y[i + seq(h)]
            y_estimate[i, ] <- fc$mean[seq(h)]

            e[i, ] <- y[i + seq(h)] - fc$mean[seq(h)]
            e_percentage[i, ] <- 100*(t(e[i, ])/y[i + seq(h)])
        }
    }
    if (h == 1) {
        return(list(e = e[, 1L], 
        e_percentage = e_percentage[, 1L]),
        y_estimate = y_estimate[, 1L],
        y_groundtruth = y_groundtruth[, 1L])
    }
    else {
        colnames(e) <- paste("h=", 1:h, sep = "")
        colnames(e_percentage) <- paste("h=", 1:h, sep = "")
        colnames(y_estimate) <- paste("h=", 1:h, sep = "")
        colnames(y_groundtruth) <- paste("h=", 1:h, sep = "")

        return(list(e = e, 
        e_percentage = e_percentage,
        y_estimate = y_estimate,
        y_groundtruth = y_groundtruth))
    }
}

tsCV_UCM <- function(my_xts,  forecastfunction, h = 1, window = NULL, 
    initial = 0, ...) {
    
    # print("ok") CHECK
    
    n <- nrow(my_xts) # Lunghezza totale serie storica
    #Creo una matrice xts degli errori, con lo stesso numero di righe e indice della serie storica
    #ma con un numero di colonne h e vuota
    e <- xts(matrix(NA, nrow = nrow(my_xts), ncol = h), order.by = index(my_xts))
    e_percentage <- e
    y_estimate <- e
    y_groundtruth <- e

    if (initial >= n) 
        stop("initial period too long")
    # tsp(e) <- tsp(y) # Assegnazione tsp attribute di y alla matrice degli errori
    
    # Aggiunge h righe di NA alla matrice di regressiri (quando presenti) e y, con lo scopo di avere h valori NA
    # Dopo la fine della serie storica, in modo da poter passare sempre h dati di test anche quando
    # La serie storica è finita

    # Creo una matrixe xts di NA, che parte dall'ultimo giorno + 1 della nostra serie storica
    na_df <- seq(from = end(my_xts) + 1, 
        to = end(my_xts) + h, 
        by = "day")

    # Aggiungo la matrice dei NA alla fine della serie storica
    my_xts <- merge(my_xts, na_df)

    # print("ok1") CHECK

    if (is.null(window)) 
        indx <- seq(1 + initial, n - 1L)
    else indx <- seq(window + initial, n - 1L, by = 1L)

    # print("ok2") CHECK

    # Iterazioni
    for (i in indx) {
        # print(i) CHECK
        # Qui cambio rispetto a tsCV, ricordiamo infatti che SSModel prende in pasto una
        # Serie storica di lunghezza train + test, dove i dati di test sono semplicemente NA
        # Quindi non si imposterà un orizzonte nel momento del fit, ma questo è implicito nella definizione
        
        # Definizione stard e end di train
        start <- ifelse(is.null(window), 1L, ifelse(i - window >= 0L, i - window + 1L, stop("small window")))
        end <- i + h # imposto come end i + h perchè così ho train + test
        
        # Imputazione dei valori di test a NA
        y <- my_xts[,1] # Estraggo la colonna di y in una variabile temporanea
        y[(i+1):(end)] <- NA # Imposto i valori di y di test come NA

        # Copio my_xts così da poterci lavorare sopra
        subset <- my_xts
        subset[,1] <- y # Sostituisco la colonna di y con quella modificata
        subset <- subset[start:end,] # Estraggo la sotto serie storica di train + test


        # Train e predizioni salvate in muhat
        muhat <- try(suppressWarnings(forecastfunction(subset)), 
            silent = TRUE)
        # muhat <- muhat # Esponenzio i valori predetti per ottenere i valori di y
        # Calcolo degli errori
        y_ground_truth <- my_xts[,1]
        if (!is.element("try-error", class(muhat))) {
            y_groundtruth[i, ] <- y_ground_truth[i + seq(h)]
            y_estimate[i, ] <- muhat[i + seq(h)]

            e[i, ] <- y_ground_truth[i + seq(h)] - muhat[i + seq(h)]
            e_percentage[i, ] <- 100*(t(e[i, ])/y_ground_truth[i + seq(h)])
        }
    }
    if (h == 1) {
        return(list(e = e[, 1L], 
        e_percentage = e_percentage[, 1L],
        y_estimate = y_estimate[, 1L],
        y_groundtruth = y_groundtruth[, 1L]))
    }
    else {
        colnames(e) <- paste("h=", 1:h, sep = "")
        colnames(e_percentage) <- paste("h=", 1:h, sep = "")
        colnames(y_estimate) <- paste("h=", 1:h, sep = "")
        colnames(y_groundtruth) <- paste("h=", 1:h, sep = "")

        return(list(e = e, 
        e_percentage = e_percentage,
        y_estimate = y_estimate,
        y_groundtruth = y_groundtruth))
    }   
}

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

pre_covid_accuracy_plot <- function(i_x, f_x, by_x, i_y, f_y, by_y, ylab, model_types, accuracy_metrics){
  
  accuracy_metrics <- accuracy_metrics[grep(model_types,names(accuracy_metrics))]
  
  name_map <- list(
    "ARIMA" = list(label = "ARIMA", col = "red"),
    "UCM" = list(label = "UCM", col = "darkgreen"),
    "RandomForest" = list(label = "RF", col = "blue")
  )
  
  suffix_map <- list(
    "SARIMA(1,0,0)(1,1,2)",
    "ARIMAX (2,1,3)-dummy",
    "SARIMAX(3,1,2)(1,0,1)-fourier",
    "SARIMAX(3,1,3)(1,0,0)-regressori",
    "ritardi",
    "regressori",
    "regressori+dummy festività",
    "ritardi",
    "regressori",
    "regressori+dummy festività"
  )
  
  # Inizializza il grafico
  #par(las = 0.5)
  
  #plot(1:74, NULL, type="l", col=1, xlab="horizon", ylab="RMSE", ylim = c(2000,10000), xgap.axis = 1)
  
  #png("highres.png", units = "in", width = 5, height = 4, res = 300)
  
  #png("highres.png", res = 300)
  
  #jpeg(file="saving_plot2.png", width=1280, height=720)
  
  par(oma=c(0, 0, 0, 8), asp=1)
  
  plot(x = NULL, y = NULL, type = "n", xlim = c(i_x, f_x), ylim = c(i_y, f_y),
       xlab="Forecast Horizon", ylab=ylab,
       xaxt = "n",
       yaxt = "n", grid = TRUE)
  
  
  int_seq_x <- append(seq.int(from = i_x, to = f_x, by = by_x), f_x)
  axis(side = 1, at = int_seq_x)
  int_seq_y <- append(seq.int(from = i_y, to = f_y, by = by_y), f_y)
  axis(side = 2, at = int_seq_y)
  
  abline(v = int_seq_x, lty = "dotted", col = "lightgray")
  abline(h = int_seq_y, lty = "dotted", col = "lightgray")
  
  # Crea un vettore di tratti per ogni etichetta
  lty_map <- list()
  for (label in names(name_map)) {
    lty_map[[label]] <- seq(1, length(grep(label, names(accuracy_metrics))))
  }
  
  # Plotta le linee per ogni vettore nella lista
  counter_map <- list()
  for(i in 1:length(accuracy_metrics)) {
    # Trova l'etichetta corrispondente al nome del vettore
    label <- NULL
    for (key in names(name_map)) {
      if (grepl(key, names(accuracy_metrics[i]))) {
        label <- key
        break
      }
    }
    
    if (!is.null(label)) {
      # Inizializza il contatore per il gruppo corrente, se non ancora presente
      if (!(label %in% names(counter_map))) {
        counter_map[[label]] <- 1
      }
      
      # Plotta la linea usando il colore e il tratto corrispondenti all'etichetta
      lines(1:f_x, accuracy_metrics[[i]], col = name_map[[label]]$col, lty = lty_map[[label]][counter_map[[label]]])
      
      # Incrementa il contatore per il gruppo corrente
      counter_map[[label]] <- counter_map[[label]] + 1
    }
  }
  
  # Aggiungi la legenda al grafico
  col <- c()
  lty <- c()
  labels <- c()
  for (key in names(name_map)) {
    col <- c(col, rep(name_map[[key]]$col, length(grep(key, names(accuracy_metrics)))))
    lty <- c(lty, lty_map[[key]])
    labels <- c(labels, 
                paste(rep(name_map[[key]]$label, length(grep(key, names(accuracy_metrics)))), suffix_map[grep(key, names(accuracy_metrics))], sep="-"))
  }
  #plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  #legend("topright", legend = labels, col = col, lty = lty)
  #legend(side = "topright", bty = "o", cex = 0.8, x.intersp = 1.5, legend = labels, col = col, lty = lty)
  
  legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
         legend = labels, col = col, lty = lty)
}