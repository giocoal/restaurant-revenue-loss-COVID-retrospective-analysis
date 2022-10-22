# SETTING PROGETTO -------------------------------------------------------------

set.seed(100)

# Setting librerie utili
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "magrittr", "ggplot2",
              "forcats", "lubridate", "RQuantLib", "devtools", "patchwork", "KFAS",
              "caret", "tseries", "urca", "TSstudio", "gridExtra", "randomForest",
              "prophet", "xts", "corrplot", "rstan", 'reshape2') 

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
ristorazione_completo <- read.csv("dataset_clean.csv")
ristorante1 <- read.csv("ristorante1.csv")
ristorante2 <- read.csv("ristorante2.csv")
ristorante3 <- read.csv("ristorante3.csv")
ristorante4 <- read.csv("ristorante4.csv")
ristorante5 <- read.csv("ristorante5.csv")
ristorante6 <- read.csv("ristorante6.csv")

# Valuto la presenza di NaN per ciascun ristorante

## Ristorante 1
sum(is.na(ristorante1$scontrini)) # 300 NA
# which(is.na(ristorante1$scontrini))
subset(ristorante1[,c(2,6)], is.na(ristorante1$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 857) e 
# alcune festivit?

## Ristorante 2
sum(is.na(ristorante2$scontrini)) # 322 NA
# which(is.na(ristorante2$scontrini))
subset(ristorante2[,c(2,6)], is.na(ristorante2$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 854) e 
# alcune festivit?. Sembrerebbe esserci un periodo di chiusura fra le righe 1366
# e 1395, ma da 1390 ? presente il dato del lordo totale. C'? da sistemare il 
# valore medio degli scontrini per quei giorni

## Ristorante 3
sum(is.na(ristorante3$scontrini)) # 71 NA
# which(is.na(ristorante3$scontrini))
subset(ristorante3[,c(2,6)], is.na(ristorante3$scontrini))
# I primi 7 valori mancanti sono causati, probabilmente, dall'inizio dell'attivit?
# datata 08-11-2019. Dopodich? abbiamo NaN per COVID (da 133 a 188) e festivit?

## Ristorante 4
sum(is.na(ristorante4$scontrini)) # 341 NA
# which(is.na(ristorante4$scontrini))
subset(ristorante4[,c(2,6)], is.na(ristorante4$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 884, 
# di diversi giorni pi? lungo rispetto agli altri ristoranti) e alcune festivit?

## Ristorante 5
sum(is.na(ristorante5$scontrini)) # 293 NA
# which(is.na(ristorante5$scontrini))
subset(ristorante5[,c(2,6)], is.na(ristorante5$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 854) e 
# alcune festivit?

## Ristorante 6
sum(is.na(ristorante6$scontrini)) # 292 NA
# which(is.na(ristorante6$scontrini))
subset(ristorante6[,c(2,6)], is.na(ristorante6$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 854) e 
# alcune festivit?

### C'E' DA CAPIRE COME GESTIRE I CASI DI NUMERO NULLO DI SCONTRINI MA LORDO 
### DIVERSO DA 0. AGGIUNGERE VARIABILE APERTO/CHIUSO?

### Metto a 0 i Na, per comodit?

ristorante1$lordototale[is.na(ristorante1$lordototale)] <- 0
ristorante1$scontrini[is.na(ristorante1$scontrini)] <- 0  
ristorante1$Prezzo_medio_per_scontrino[is.na(ristorante1$Prezzo_medio_per_scontrino)] <- 0

ristorante2$lordototale[is.na(ristorante2$lordototale)] <- 0
ristorante2$scontrini[is.na(ristorante2$scontrini)] <- 0  
ristorante2$Prezzo_medio_per_scontrino[is.na(ristorante2$Prezzo_medio_per_scontrino)] <- 0

ristorante3$lordototale[is.na(ristorante3$lordototale)] <- 0
ristorante3$scontrini[is.na(ristorante3$scontrini)] <- 0  
ristorante3$Prezzo_medio_per_scontrino[is.na(ristorante3$Prezzo_medio_per_scontrino)] <- 0

ristorante4$lordototale[is.na(ristorante4$lordototale)] <- 0
ristorante4$scontrini[is.na(ristorante4$scontrini)] <- 0  
ristorante4$Prezzo_medio_per_scontrino[is.na(ristorante4$Prezzo_medio_per_scontrino)] <- 0

ristorante5$lordototale[is.na(ristorante5$lordototale)] <- 0
ristorante5$scontrini[is.na(ristorante5$scontrini)] <- 0  
ristorante5$Prezzo_medio_per_scontrino[is.na(ristorante5$Prezzo_medio_per_scontrino)] <- 0

ristorante6$lordototale[is.na(ristorante6$lordototale)] <- 0
ristorante6$scontrini[is.na(ristorante6$scontrini)] <- 0  
ristorante6$Prezzo_medio_per_scontrino[is.na(ristorante6$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data

ristorazione_completo$data <- parse_date(ristorazione_completo$data, "%Y-%m-%d", locale = locale("it"))
ristorante1$data <- parse_date(ristorante1$data, "%Y-%m-%d", locale = locale("it"))
ristorante2$data <- parse_date(ristorante2$data, "%Y-%m-%d", locale = locale("it"))
ristorante3$data <- parse_date(ristorante3$data, "%Y-%m-%d", locale = locale("it"))
ristorante4$data <- parse_date(ristorante4$data, "%Y-%m-%d", locale = locale("it"))
ristorante5$data <- parse_date(ristorante5$data, "%Y-%m-%d", locale = locale("it"))
ristorante6$data <- parse_date(ristorante6$data, "%Y-%m-%d", locale = locale("it"))

### Dato che per tutti i ristoranti ad eccezione del 3 i primi 8 mesi presentano
### solo dati aggregati mensilmente, creo delle copie alla quale tolgo tali dati

copy_ristorante1 <- ristorante1[-c(1:243),]
copy_ristorante2 <- ristorante2[-c(1:243),]
copy_ristorante4 <- ristorante4[-c(1:243),]
copy_ristorante5 <- ristorante5[-c(1:243),]
copy_ristorante6 <- ristorante6[-c(1:243),]

# PRIMA ESPLORAZIONE DATASET ---------------------------------------------------
## Vendite giornaliere, per ciascun ristorante ---------------------------------
par(mfrow=c(3,2))

# Ristorante 1
plot(copy_ristorante1$data, copy_ristorante1$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 1")
abline(h=mean(as.integer(copy_ristorante1$lordototale)))

# Ristorante 2
plot(copy_ristorante2$data, copy_ristorante2$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 2")
abline(h=mean(as.integer(copy_ristorante2$lordototale)))

# Ristorante 3
plot(ristorante3$data, ristorante3$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 3")
abline(h=mean(as.integer(ristorante3$lordototale)))

# Ristorante 4
plot(copy_ristorante4$data, copy_ristorante4$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 4")
abline(h=mean(as.integer(copy_ristorante4$lordototale)))

# Ristorante 5
plot(copy_ristorante5$data, copy_ristorante5$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 5")
abline(h=mean(as.integer(copy_ristorante5$lordototale)))

# Ristorante 6
plot(copy_ristorante6$data, copy_ristorante6$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 6")
abline(h=mean(as.integer(copy_ristorante6$lordototale)))

### Analisi nel dettaglio del singolo ristorante

# si analizza singolarmente ciascun ristorante più nel dettaglio

# Ristorante 1
ggplot(copy_ristorante1, aes(data, lordototale)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(copy_ristorante1$data[yday(copy_ristorante1$data)==1]), 
             size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 1")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Ristorante 2
ggplot(copy_ristorante2, aes(data, lordototale)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(copy_ristorante2$data[yday(copy_ristorante2$data)==1]), 
             size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 2")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Ristorante 3
ggplot(ristorante3, aes(data, lordototale)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante3$data[yday(ristorante3$data)==1]), 
             size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 3")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Ristorante 4
ggplot(copy_ristorante4, aes(data, lordototale)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(copy_ristorante4$data[yday(copy_ristorante4$data)==1]), 
             size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 4")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Ristorante 5
ggplot(copy_ristorante5, aes(data, lordototale)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(copy_ristorante5$data[yday(copy_ristorante5$data)==1]), 
             size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 5")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Ristorante 6
ggplot(copy_ristorante6, aes(data, lordototale)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(copy_ristorante6$data[yday(copy_ristorante6$data)==1]), 
             size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 6")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))


### Analisi trend scontrini

# Ristorante 1
trend_scontrini_rist1 <- data.frame(copy_ristorante1$Prezzo_medio_per_scontrino, copy_ristorante1$scontrini)
names(trend_scontrini_rist1) <- c("Prezzo medio scontrino", "Scontrini registrati")
trend_scontrini_ts_rist1 <- ts(trend_scontrini_rist1, start = decimal_date(as.Date("2018-09-01")), frequency = 365)
autoplot(trend_scontrini_ts_rist1, facets = 1, main = "Ristorante 1: analisi trend scontrini")

# Ristorante 2
trend_scontrini_rist2 <- data.frame(copy_ristorante2$Prezzo_medio_per_scontrino, copy_ristorante2$scontrini)
names(trend_scontrini_rist2) <- c("Prezzo medio scontrino", "Scontrini registrati")
trend_scontrini_ts_rist2 <- ts(trend_scontrini_rist2, start = decimal_date(as.Date("2018-09-01")), frequency = 365)
autoplot(trend_scontrini_ts_rist2, facets = 1, main = "Ristorante 2: analisi trend scontrini")

# Ristorante 3
trend_scontrini_rist3 <- data.frame(ristorante3$Prezzo_medio_per_scontrino, ristorante3$scontrini)
names(trend_scontrini_rist3) <- c("Prezzo medio scontrino", "Scontrini registrati")
trend_scontrini_ts_rist3 <- ts(trend_scontrini_rist3, start = decimal_date(as.Date("2019-11-08")), frequency = 365)
autoplot(trend_scontrini_ts_rist3, facets = 1, main = "Ristorante 3: analisi trend scontrini")

# Ristorante 4
trend_scontrini_rist4 <- data.frame(copy_ristorante4$Prezzo_medio_per_scontrino, copy_ristorante4$scontrini)
names(trend_scontrini_rist4) <- c("Prezzo medio scontrino", "Scontrini registrati")
trend_scontrini_ts_rist4 <- ts(trend_scontrini_rist4, start = decimal_date(as.Date("2018-09-01")), frequency = 365)
autoplot(trend_scontrini_ts_rist4, facets = 1, main = "Ristorante 4: analisi trend scontrini")

# Ristorante 5
trend_scontrini_rist5 <- data.frame(copy_ristorante5$Prezzo_medio_per_scontrino, copy_ristorante5$scontrini)
names(trend_scontrini_rist5) <- c("Prezzo medio scontrino", "Scontrini registrati")
trend_scontrini_ts_rist5 <- ts(trend_scontrini_rist5, start = decimal_date(as.Date("2018-09-01")), frequency = 365)
autoplot(trend_scontrini_ts_rist5, facets = 1, main = "Ristorante 5: analisi trend scontrini")

# Ristorante 6
trend_scontrini_rist6 <- data.frame(copy_ristorante6$Prezzo_medio_per_scontrino, copy_ristorante6$scontrini)
names(trend_scontrini_rist6) <- c("Prezzo medio scontrino", "Scontrini registrati")
trend_scontrini_ts_rist6 <- ts(trend_scontrini_rist6, start = decimal_date(as.Date("2018-09-01")), frequency = 365)
autoplot(trend_scontrini_ts_rist6, facets = 1, main = "Ristorante 6: analisi trend scontrini")


### Creo alcuni boxplot potenzialmente utili considerando il dataset completo

# Elimino i dati prima di settembre 2018 dal dataset completo, essendo quelli
# aggregati mensilmente
copy_ristorazione_completo <- ristorazione_completo[which(ristorazione_completo$data>'2018-08-31'),]

# Rendo gli attributi Giorno, Month, Year, ... dei fattori. In questo modo riesco a
# manipolari i boxplot correttamente
copy_ristorazione_completo$Giorno <- as.factor(copy_ristorazione_completo$Giorno)
copy_ristorazione_completo$Giorno <- factor(copy_ristorazione_completo$Giorno, 
                                            levels=c('Monday','Tuesday','Wednesday',
                                                     'Thursday','Friday','Saturday',
                                                     'Sunday'))

copy_ristorazione_completo$Month <- as.factor(copy_ristorazione_completo$Month)

copy_ristorazione_completo$Year <- as.factor(copy_ristorazione_completo$Year)

copy_ristorazione_completo$Season <- as.factor(copy_ristorazione_completo$Season)
copy_ristorazione_completo$Season <- factor(copy_ristorazione_completo$Season, 
                                            levels=c('Spring','Summer','Autumn',
                                                     'Winter'))

copy_ristorazione_completo$Weekend <- as.factor(copy_ristorazione_completo$Weekend)

copy_ristorazione_completo$Festivo <- as.factor(copy_ristorazione_completo$Festivo)

copy_ristorazione_completo$Pioggia <- as.factor(copy_ristorazione_completo$Pioggia)

# Creo i diversi boxplot (sia vendite che scontrini)

### Giorno della settimana
ggplot(copy_ristorazione_completo, aes(Giorno, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Giorno, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Mese dell'anno
ggplot(copy_ristorazione_completo, aes(Month, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Month, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Anno
ggplot(copy_ristorazione_completo, aes(Year, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Year, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Stagione
ggplot(copy_ristorazione_completo, aes(Season, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Season, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Weekend/settimana (il venerd? ? considerato giorno della settimana)
ggplot(copy_ristorazione_completo, aes(Weekend, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Weekend, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Giorno feriale vs. festivo
ggplot(copy_ristorazione_completo, aes(Festivo, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Festivo, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Pioggia si/no
ggplot(copy_ristorazione_completo, aes(Pioggia, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorazione_completo, aes(Pioggia, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))


# Serie storica per ristorante

weeks <- as.Date(cut(ristorante1$data, "week"))

ristorante3 <- ristorante3[-c(1:3),]
weeks3 <- as.Date(cut(ristorante3$data, "week"))

r1 <- ristorante1[, c('data', 'lordototale')]
names(r1)[names(r1) == 'lordototale'] <- 'R1'
r1 <- aggregate(R1 ~ weeks, data = r1, mean)

r2 <- ristorante2[, c('data', 'lordototale')]
names(r2)[names(r2) == 'lordototale'] <- 'R2'
r2 <- aggregate(R2 ~ weeks, data = r2, mean)

r3 <- ristorante3[, c('data', 'lordototale')]
names(r3)[names(r3) == 'lordototale'] <- 'R3'
r3 <- aggregate(R3 ~ weeks3, data = r3, mean)
names(r3)[names(r3) == 'weeks3'] <- 'weeks'

r4 <- ristorante4[, c('data', 'lordototale')]
names(r4)[names(r4) == 'lordototale'] <- 'R4'
r4 <- aggregate(R4 ~ weeks, data = r4, mean)

r5 <- ristorante5[, c('data', 'lordototale')]
names(r5)[names(r5) == 'lordototale'] <- 'R5'
r5 <- aggregate(R5 ~ weeks, data = r5, mean)

r6 <- ristorante6[, c('data', 'lordototale')]
names(r6)[names(r6) == 'lordototale'] <- 'R6'
r6 <- aggregate(R6 ~ weeks, data = r6, mean)

r1 <- r1[-c(1:35),]
r2 <- r2[-c(1:35),]
r4 <- r4[-c(1:35),]
r5 <- r5[-c(1:35),]
r6 <- r6[-c(1:35),]

ristoranti_ts <- merge(r1, r2, by = 'weeks', all.x = TRUE)
ristoranti_ts <- merge(ristoranti_ts, r3, by = 'weeks', all.x = TRUE)
ristoranti_ts <- merge(ristoranti_ts, r4, by = 'weeks', all.x = TRUE)
ristoranti_ts <- merge(ristoranti_ts, r5, by = 'weeks', all.x = TRUE)
ristoranti_ts <- merge(ristoranti_ts, r6, by = 'weeks', all.x = TRUE)

ristoranti_ts <- melt(ristoranti_ts, id.vars = "weeks")
ristoranti_ts <- ristoranti_ts[ristoranti_ts$weeks != '2022-04-25', ] 

p <- ggplot(ristoranti_ts,                           
       aes(x = weeks,
           y = value,
           col = variable)) + geom_line()
p + labs(x = "data", y='lordototale')

# Serie storica per ristorante (PRE-COVID)

ristoranti_ts_covid <- ristoranti_ts[which(ristoranti_ts$weeks>'2020-02-22'),]

p <- ggplot(ristoranti_ts_covid,                           
       aes(x = weeks,
           y = value,
           col = variable)) + geom_line()
p + labs(x = "data", y='lordototale')
