tsCV_ARIMA <- function(y, forecastfunction, h = 1, window = NULL, xreg = NULL, 
    initial = 0, ...) {
    y <- as.ts(y) # Serie storica convertita in oggetto ts
    n <- length(y) # Lunghezza totale serie storica
    e <- ts(matrix(NA_real_, nrow = n, ncol = h)) # Inizializzazione matrice degli errori
    if (initial >= n) 
        stop("initial period too long")
    tsp(e) <- tsp(y) # Assegnazione tsp attribute di y alla matrice degli errori
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
            e[i, ] <- y[i + seq(h)] - fc$mean[seq(h)]
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

tsCV_UCM <- function(my_xts,  forecastfunction, h = 1, window = NULL, 
    initial = 0, ...) {
    
    # print("ok") CHECK
    
    n <- nrow(my_xts) # Lunghezza totale serie storica
    #Creo una matrice xts degli errori, con lo stesso numero di righe e indice della serie storica
    #ma con un numero di colonne h e vuota
    e <- xts(matrix(NA, nrow = nrow(my_xts), ncol = h), order.by = index(my_xts))
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
        muhat <- exp(muhat) # Esponenzio i valori predetti per ottenere i valori di y
        # Calcolo degli errori
        y_ground_truth <- my_xts[,1]
        if (!is.element("try-error", class(muhat))) {
            e[i, ] <- y_ground_truth[i + seq(h)] - muhat[i + seq(h)]
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