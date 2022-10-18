# --- SETTING --- 

set.seed(100)

# Setting librerie utili
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "magrittr", "ggplot2",
              "forcats", "lubridate", "RQuantLib", "devtools", "patchwork", "KFAS",
              "caret", "tseries", "urca", "TSstudio", "gridExtra", "randomForest",
              "prophet", "xts", "corrplot", "rstan", "hydroTSM") 

# Install packages if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Setting working directory
# working_dir = percorso cartella dati
# working_dir = "~/GitHub/Data-Science-Lab"
# setwd(working_dir)

# Funzione utile 
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


# Caricamento datasets
ristorante1 <- read.csv("ristorante1.csv")

# Presenza di NaN
sum(is.na(ristorante1$scontrini)) # 300 NA
# which(is.na(ristorante1$scontrini))
subset(ristorante1[,c(2,6)], is.na(ristorante1$scontrini))

# Fino alla riga 243 la presenza di NaN è causata dal fatto che i dati sono
# aggregati mensilmente. Dopodichè c'è tutto il periodo COVID (da 802 a 857) e 
# alcune festività

### Metto a 0 i Na, per comodità

ristorante1$lordototale[is.na(ristorante1$lordototale)] <- 0
ristorante1$scontrini[is.na(ristorante1$scontrini)] <- 0  
ristorante1$Prezzo_medio_per_scontrino[is.na(ristorante1$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data

ristorante1$data <- parse_date(ristorante1$data, "%Y-%m-%d", locale = locale("it"))

# Creo una copia togliendo i dati aggregati mensilmente dei primi 8 mesi del 2018

copy_ristorante1 <- ristorante1[-c(1:243),]

# --- Analisi esplorativa Serie storica 


# Dataset contenente solo: data, giorno, lordo_totale, prezzo_medio_per_scontrino
copy_ristorante1_sub <- copy_ristorante1[,c('data', 'Giorno', 'scontrini', 'lordototale', 'Prezzo_medio_per_scontrino')]
copy_ristorante1_sub$Giorno <- as.factor(copy_ristorante1_sub$Giorno)

#daily Date object - helps my work on dates
inds <- seq(as.Date("2018-09-01"), as.Date("2022-04-30"), by = "day")

# Time series lordo totale
ts_lordototale <- ts(copy_ristorante1$lordototale, start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)

ts_decomposed_lordototale <- decompose(ts_lordototale, type = 'additive')
plot(ts_decomposed_lordototale)

ts_lordototale.stl <- stl(ts_lordototale, "periodic")
ts_lordototale_sa <- ts_lordototale - ts_decomposed_lordototale$seasonal
par(mfrow=c(2,1))
plot(ts_lordototale, type="l")  # original series
plot(ts_lordototale_sa, type="l")  # seasonal adjusted
