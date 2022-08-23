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
working_dir = "~/GitHub/Data-Science-Lab"
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
copy_ristorante1 <- ristorante1[-c(1:243),]

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

# Data di riferimento COVID 06-01-2020 (data del primo caso accertato in Cina)
data_covid <- as.Date("2020-01-07", format = "%Y-%m-%d")
# Ristorante 1 pre-COVID
copy_ristorante1_pre_covid <- copy_ristorante1 %>% filter(copy_ristorante1$data < data_covid)

### RANDOM FOREST

### PERIODO COVID

randomforest_r1_precovid <- copy_ristorante1_pre_covid[, c("data", "scontrini", "lordototale",
                                                           "Prezzo_medio_per_scontrino", "Giorno",
                                                           "Month", "Year", "Season", "Weekend",
                                                           "Festivo", "Precipitazioni.mm.", "Pioggia",
                                                           "ColoreCOVID", "Dose1Cum", "Dose2Cum",
                                                           "DoseUnicaCum", "Booster1Cum", "Booster3DosiCum",
                                                           "Booster2Cum", "BENZINA.LITRO", "GASOLIO_AUTO.LITRO",
                                                           "GPL.LITRO", "GASOLIO_RISCALDAMENTO.LITRO",
                                                           "O.C._FLUIDO_BTZ.LITRO", "O.C._DENSO_BTZ.LITRO",
                                                           "Durum.wheat....Tonne.", "Feed.barley....Tonne.",
                                                           "Maize....Tonne.",
                                                           "Milling.wheat....Tonne.")]

randomforest_r1_precovid[is.na(randomforest_r1_precovid)] <- 0

# divisione in train e test
index_rf <- sample(1:nrow(randomforest_r1_precovid),
                   size = 0.7*nrow(randomforest_r1_precovid))
train_rf <- randomforest_r1_precovid[index_rf,]
test_rf <- randomforest_r1_precovid[-index_rf,] 

# implementazione modelli
MRF <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                      Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                      Dose1Cum + Dose2Cum + DoseUnicaCum + Booster1Cum + Booster3DosiCum +
                      Booster2Cum + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                      GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                      Durum.wheat....Tonne. + Feed.barley....Tonne. + Maize....Tonne. +
                      Milling.wheat....Tonne., data = train_rf)
varImpPlot(MRF)
print(MRF)
# % Var explained: 84.06

### DALL' IMPORTANCE PLOT DELLE VARIABILI POSSIAMO VEDERE CHE TUTTE LE VARIABILI LEGATI AI VACCINI
### NON SONO RILEVANTI, COMINCIO AD ELIMINARE QUELLE

MRF_V2 <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                      Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                      BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                      GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                      Durum.wheat....Tonne. + Feed.barley....Tonne. + Maize....Tonne. +
                      Milling.wheat....Tonne., data = train_rf)
varImpPlot(MRF_V2)
print(MRF_V2)
# % Var explained: 83.94

### CAMBIA DI POCO, QUINDI A QUESTO PUNTO MI LIMITO ALLE 7 (numero casuale) VARIABILI PIU' IMPORTANTI

MRF_V3 <- randomForest(lordototale ~ Giorno + Month + Weekend + Festivo + GASOLIO_AUTO.LITRO 
                       + Durum.wheat....Tonne. + Maize....Tonne., data = train_rf)
varImpPlot(MRF_V3)
print(MRF_V3)
# % Var explained: 84.21

### TOGLIENDO ALTRE VARIABILI, PARTENDO DALLA MENO IMPORTANTE, LA VARIANZA SPIEGATA SCENDE PARECCHIO

# si valutano le performance del modello sul train e test set
predictions_rf <- predict(MRF_V3, newdata = train_rf)
mape(train_rf$lordototale, predictions_rf)
# MAPE 9.03 (DATI DI TRAIN)

predictions_rf <- predict(MRF_V3, newdata = test_rf)
mape(test_rf$lordototale, predictions_rf)
# MAPE 11.30 (DATI DI TEST)

accuracy(predictions_rf, test_rf$lordototale)
# RMSE 2833.62
# MAPE 11.30

# Creato il modello, vado a fare le previsioni su valori nuovi, ossia sul periodo COVID

r1_rf_covid <- copy_ristorante1[, c('lordototale', 'data', 'Giorno', 'Month', 'Weekend',
                                    'Festivo', 'GASOLIO_AUTO.LITRO', 'Durum.wheat....Tonne.', 'Maize....Tonne.')]

# selezione periodo covid (su cui verranno fatte le previsioni)
reference_date_rf <- as.Date("2020-01-06", format = "%Y-%m-%d")
r1_rf_covid <- r1_rf_covid %>%
  filter(r1_rf_covid$data > reference_date_rf)

# Si seleziona la lunghezza del periodo da prevedere
# Prendo in considerazione tutto il 2020
r1_rf_covid <- r1_rf_covid[1:360,]
# C'è una settimana di luglio 2020 dove manca il valore della variabile "Durum Wheat",
# probabilmente perchè in quel periodo c'è la chiusura dell'anno fiscale (nello stesso
# periodo mancano dati anche negli altri anni). Ho deciso di considerare per quella settimana
# il prezzo delle settimane precedenti, che sembra stabile
r1_rf_covid$Durum.wheat....Tonne.[is.na(r1_rf_covid$Durum.wheat....Tonne.)] <- 276.5

# si utilizza il modello appena creato per fare previsioni
previsione_covid_rf <- predict(MRF_V3, r1_rf_covid)
previsione_covid_rf <- as.data.frame(previsione_covid_rf)

# Unisco le due serie storiche

# Serie storica previsioni periodo covid 
interval_covid <- seq(as.Date("2020-01-07"), as.Date("2020-12-31"), by = "day")
interval_covid_df <- data.frame(date = interval_covid, 
                                val=previsione_covid_rf)
interval_covid_df$date <- as.Date(interval_covid_df$date)  

interval_covid_ts <- xts(interval_covid_df$val, interval_covid_df$date)

plot(interval_covid_df$date, interval_covid_df$previsione_covid_rf, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1")

# Serie storica dati reali fino al pre covid (r1_pre_covid_rf$lordototale)

interval_pre_covid <- seq(as.Date("2018-09-01"), as.Date("2020-01-06"), by = "day")
interval_pre_covid_df <- data.frame(date = interval_pre_covid, 
                                    val=randomforest_r1_precovid$lordototale)

interval_pre_covid_df$date<-as.Date(interval_pre_covid_df$date)  
interval_covid_ts_pre <- xts(interval_pre_covid_df$val, interval_pre_covid_df$date)

# Uniformo i nomi e unisco
names(interval_covid_df)[1] <- "data"
names(interval_covid_df)[2] <- "vendite"

names(interval_pre_covid_df)[1] <- "data"
names(interval_pre_covid_df)[2] <- "vendite"

interval_complete <- rbind(interval_covid_df, interval_pre_covid_df)
interval_complete <- interval_complete[order(interval_complete$data), ]
row.names(interval_complete) <- NULL

# Mostro le due serie storiche
par(mfrow=c(2,1))

# Serie storica con previsioni
plot(interval_complete$data, interval_complete$vendite, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 1 previsioni")

# Serie storica originale
rownames(copy_ristorante1) <- NULL # Ho bisogno di resettare l'indice delle righe
ristorante1_complete <- copy_ristorante1[1:853,]  # fino al 31 maggio 2020
plot(ristorante1_complete$data, ristorante1_complete$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 1 dati reali")

# Sovrapposizione serie storiche
par(mfrow=c(1,1))

rf_complete <- cbind(interval_complete, ristorante1_complete$lordototale)
names(rf_complete)[1] <- "data"
names(rf_complete)[2] <- "previsione"
names(rf_complete)[3] <- "datoreale"

plot(rf_complete$data, rf_complete$previsione, type="l", col="blue", xlab="data", ylab="vendite", lty=1)
lines(rf_complete$data, rf_complete$datoreale, col="red",lty=2)

# Stima perdite
rf_complete$perdite <- rf_complete$previsione - rf_complete$datoreale





### ANALISI FUTURO

copy_ristorante1[is.na(copy_ristorante1)] <- 0

# Implementazione modello
MRF_future <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                             Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                             Dose1Cum + Dose2Cum + DoseUnicaCum + Booster1Cum + Booster3DosiCum +
                             Booster2Cum + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                             GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                             Durum.wheat....Tonne. + Feed.barley....Tonne. + Maize....Tonne. +
                             Milling.wheat....Tonne., data = copy_ristorante1)
varImpPlot(MRF_future)
print(MRF_future)
# % Var explained: 87.01

### TOLGO LE ULTIME 10 VARIABILI

MRF_future_V2 <- randomForest(lordototale ~ Giorno + Month + Year + Weekend + Festivo + 
                                ColoreCOVID + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                                GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + 
                                O.C._DENSO_BTZ.LITRO + Durum.wheat....Tonne. + 
                                Feed.barley....Tonne. + Maize....Tonne., data = copy_ristorante1)
varImpPlot(MRF_future_V2)
print(MRF_future_V2)
# % Var explained: 86.6

#-----------PROCEDI DA QUI---------------#

# Setto il periodo su cui fare previsioni, considerando i regressori selezionati
future_interval = seq(as.Date("2022-05-01"), as.Date("2022-09-30"), by="days")
ristorante1_future <- data.frame(future_interval)
colnames(ristorante1_future) <- "data"

# colonne Mese, Anno
ristorante1_future$Month <- month(ristorante1_future$data)
ristorante1_future$Month <- factor(ristorante1_future$Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
ristorante1_future$Year <- year(ristorante1_future$data)
ristorante1_future$Year <- factor(ristorante1_future$Year, levels = c("2018", "2019", "2020", "2021", "2022"))

# colonna Giorno, Weekend
ristorante1_future <- ristorante1_future %>%
  mutate(weekday = wday(data, label = TRUE, abbr = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        locale = Sys.getlocale("LC_TIME"))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c("Saturday", "Sunday")) ~ "weekend"
    , (weekday < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )
ristorante1_future$weekday <- as.factor(ristorante1_future$weekday)
ristorante1_future["tipo_giorno"][ristorante1_future["tipo_giorno"] == "weekend"] <- "True"
ristorante1_future["tipo_giorno"][ristorante1_future["tipo_giorno"] == "weekday"] <- "False"
colnames(ristorante1_future)[which(names(ristorante1_future) == "weekday")] <- "Giorno"
colnames(ristorante1_future)[which(names(ristorante1_future) == "tipo_giorno")] <- "Weekend"
ristorante1_future$Weekend <- as.factor(ristorante1_future$Weekend)
ristorante1_future$Giorno <- factor(ristorante1_future$Giorno, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                                          "Saturday", "Sunday"))

# colonna ColoreCOVID
ristorante1_future$ColoreCOVID <- "bianco"
ristorante1_future$ColoreCOVID <- factor(ristorante1_future$ColoreCOVID, levels = c("", "arancione", "bianco", "giallo", "rosso"))


# colonna lordototale
ristorante1_future$lordototale <- 0

# riordino le colonne
ristorante1_future <- data.frame(ristorante1_future$data,
                                 ristorante1_future$lordototale,
                                 ristorante1_future$Giorno,
                                 ristorante1_future$Month,
                                 ristorante1_future$Year,
                                 ristorante1_future$Weekend,
                                 ristorante1_future$ColoreCOVID)

names(ristorante1_future)<- c("data", 
                              "lordototale",
                              "Giorno",
                              "Month",
                              "Year",
                              "Weekend",
                              "ColoreCOVID")

ristorante1_RF_full <-rbind(copy_ristorante1[,c("data", "lordototale", "Giorno",
                                                "Month", "Year", "Weekend", 
                                                "ColoreCOVID")],
                            ristorante1_future)

# si selezionano le variabili più rilevanti
MRF_future_best <- randomForest(lordototale ~ Giorno + Month + Year + Weekend
                                + ColoreCOVID, data = ristorante1_RF_full[1:1337,]) # faccio così per non avere problemi sui
# livelli dei factor
varImpPlot(MRF_future_best)
print(MRF_future_best)
# % Var explained: 79.73

# si utilizza il modello appena creato per fare previsioni sul futuro
vendite_forecast_rf_new <- predict(MRF_future_best, ristorante1_RF_full[1339:1491,])
vendite_forecast_rf_new <- as.data.frame(vendite_forecast_rf_new)

# si uniscono le tue serie storiche

# serie storica previsioni
future_interval_df <- data.frame(date = future_interval, 
                                 val=vendite_forecast_rf_new)
future_interval_df$date<-as.Date(future_interval_df$date)  
future_interval_ts <- xts(future_interval_df$val, future_interval_df$date)

plot(future_interval_df$date, future_interval_df$vendite_forecast, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino al 1 maggio 2022
reference_date_pre <- as.Date("2022-04-30", format = "%Y-%m-%d")
vendite_pre_aprile <- copy_ristorante1 %>%
  filter(data <= reference_date_pre) %>%
  select(data, lordototale)

interval_pre <- seq(as.Date("2018-09-01"), as.Date("2022-04-30"), by = "day")
interval_pre_df <- data.frame(date = interval_pre, 
                              val=vendite_pre_aprile$lordototale)

interval_pre_df$date<-as.Date(interval_pre_df$date)  
interval_pre_ts <- xts(interval_pre_df$val, interval_pre_df$date)

# si uniscono le due serie storiche
names(future_interval_df)[1] <- "data"
names(future_interval_df)[2] <- "vendite"

names(interval_pre_df)[1] <- "data"
names(interval_pre_df)[2] <- "vendite"

interval_complete_new <- rbind(future_interval_df, interval_pre_df)
interval_complete_new <- interval_complete_new[order(interval_complete_new$data), ]
row.names(interval_complete_new) <- NULL

# serie storica con previsioni
plot(interval_complete_new$data, interval_complete_new$vendite, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1 previsioni")

# verifica performance modello
RMSE.rf <- sqrt(mean((MRF_future_best$predicted - copy_ristorante1$lordototale)^2))  # 1537.093