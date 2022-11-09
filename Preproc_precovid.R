# SETTING PROGETTO -------------------------------------------------------------

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
working_dir = "~/GitHub/Data-Science-Lab/Dati ristoranti"
setwd(working_dir)

# Funzione utile 
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

# Caricamento datasets
ristorante1 <- read.csv("ristorante1.csv")

# Metto i NA a 0
ristorante1$lordototale[is.na(ristorante1$lordototale)] <- 0
ristorante1$scontrini[is.na(ristorante1$scontrini)] <- 0  
ristorante1$Prezzo_medio_per_scontrino[is.na(ristorante1$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data
ristorante1$data <- parse_date(ristorante1$data, "%Y-%m-%d", locale = locale("it"))

# Creo una copia togliendo i dati aggregati mensilmente dei primi 8 mesi del 2018
# (parto dal 3 settembre 2018 che è lunedì)
copy_ristorante1 <- ristorante1[-c(1:245),]

copy_ristorante1$Giorno <- as.factor(copy_ristorante1$Giorno)
copy_ristorante1$Giorno <- factor(copy_ristorante1$Giorno, 
                                  levels=c('Monday','Tuesday','Wednesday',
                                           'Thursday','Friday','Saturday',
                                           'Sunday'))

copy_ristorante1$Month <- as.factor(copy_ristorante1$Month)

copy_ristorante1$Year <- as.factor(copy_ristorante1$Year)

copy_ristorante1$Season <- as.factor(copy_ristorante1$Season)
copy_ristorante1$Season <- factor(copy_ristorante1$Season, 
                                  levels=c('Spring','Summer','Autumn',
                                           'Winter'))

copy_ristorante1$Weekend <- as.factor(copy_ristorante1$Weekend)

copy_ristorante1$Festivo <- as.factor(copy_ristorante1$Festivo)

copy_ristorante1$Pioggia <- as.factor(copy_ristorante1$Pioggia)
copy_ristorante1$ColoreCOVID <- as.factor(copy_ristorante1$ColoreCOVID)

# Data di riferimento COVID 23-02-2020
data_covid <- as.Date("2020-02-23", format = "%Y-%m-%d")
# Ristorante 1 pre-COVID
copy_ristorante1_pre_covid <- copy_ristorante1 %>% filter(copy_ristorante1$data <= data_covid)

write.csv(copy_ristorante1_pre_covid, "pre-covid_R1.csv")

# Dataset covid per previsioni (tutto il resto del 2020)
ref_date <- as.Date("2020-12-31", format = "%Y-%m-%d")

r1_covidperiod <- copy_ristorante1 %>% filter(copy_ristorante1$data > data_covid, copy_ristorante1$data <= ref_date)

write.csv(r1_covidperiod, "periodo-covid_r1.csv")
