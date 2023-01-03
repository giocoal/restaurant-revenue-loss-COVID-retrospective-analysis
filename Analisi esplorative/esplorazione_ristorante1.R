# SETTING PROGETTO -------------------------------------------------------------

set.seed(100)

# Setting librerie utili
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "magrittr", "ggplot2",
              "forecast", "lubridate", "RQuantLib", "devtools", "patchwork", "KFAS",
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

# Presenza di NaN

sum(is.na(ristorante1$scontrini)) # 300 NA
# which(is.na(ristorante1$scontrini))
subset(ristorante1[,c(2,6)], is.na(ristorante1$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 857) e 
# alcune festivit?

### Metto a 0 i Na, per comodit?

ristorante1$lordototale[is.na(ristorante1$lordototale)] <- 0
ristorante1$scontrini[is.na(ristorante1$scontrini)] <- 0  
ristorante1$Prezzo_medio_per_scontrino[is.na(ristorante1$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data

ristorante1$data <- parse_date(ristorante1$data, "%Y-%m-%d", locale = locale("it"))

# Creo una copia togliendo i dati aggregati mensilmente dei primi 8 mesi del 2018

copy_ristorante1 <- ristorante1[-c(1:243),]

# Aggiungo una label pre/post pandemia
data_covid <- as.Date("2020-02-22", format = "%Y-%m-%d")

copy_ristorante1$Pandemia <- "Pre" 
copy_ristorante1$Pandemia[copy_ristorante1$data > data_covid] <- "Post" 

### Creo alcuni boxplot potenzialmente utili

# Rendo gli attributi Giorno, Month, Year, ... dei fattori. In questo modo riesco a
# manipolari i boxplot correttamente
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

copy_ristorante1$Pandemia <- as.factor(copy_ristorante1$Pandemia)

# Creo i diversi boxplot (sia vendite che scontrini)
### Giorno della settimana
ggplot(copy_ristorante1, aes(Giorno, lordototale, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Vendite") +
  ggtitle("Box-plot vendite per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"), 
        legend.position = c(0.06, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))

ggplot(copy_ristorante1, aes(Giorno, scontrini, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Mese dell'anno
ggplot(copy_ristorante1, aes(Month, lordototale, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Month, scontrini, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Anno
ggplot(copy_ristorante1, aes(Year, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Year, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Stagione
ggplot(copy_ristorante1, aes(Season, lordototale, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Season, scontrini, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Weekend/settimana (il venerd? ? considerato giorno della settimana)
ggplot(copy_ristorante1, aes(Weekend, lordototale, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ylab("Vendite") + 
  ggtitle("Box-plot vendite weekend vs. resto della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"),
        legend.position = c(0.06, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))

ggplot(copy_ristorante1, aes(Weekend, scontrini, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Giorno feriale vs. festivo
ggplot(copy_ristorante1, aes(Festivo, lordototale, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ylab("Vendite") +
  ggtitle("Box-plot vendite giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"),
        legend.position = c(0.06, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))

ggplot(copy_ristorante1, aes(Festivo, scontrini, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Pioggia si/no
ggplot(copy_ristorante1, aes(Pioggia, lordototale, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ylab("Vendite") +
  ggtitle("Box-plot vendite giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"),
        legend.position = c(0.06, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))

ggplot(copy_ristorante1, aes(Pioggia, scontrini, fill=Pandemia)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Vendite giornaliere 
vendite1_day <- ts(copy_ristorante1$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(vendite1_day) +
    ggtitle("Ristorante 1: vendite giornaliere") +
    xlab("Anno") +
    ylab("Vendite")
)

plot(vendite1_day, xlab = "Anno", ylab = "Vendite", main = "Vendite giornaliere")
grid(col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
abline(v=c(2019, 2020, 2021, 2022), col = "darkgrey", lty=2)

# Vendite settimanali medie
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist1 <- as.Date(cut(ristorante1$data, "week"))
vendite1_sett_avg <- aggregate(lordototale ~ week_rist1, data = ristorante1, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
vendite1_sett_avg <- vendite1_sett_avg[-c(1:35),]
vendite1_sett_avg <- vendite1_sett_avg$lordototale
vendite1_sett_avg <- ts(vendite1_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(vendite1_sett_avg) +
    ggtitle("Ristorante 1: vendite medie settimanali") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili medie
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist1 <- as.Date(cut(ristorante1$data, "month"))

vendite1_mens_avg <- aggregate(lordototale ~ month_rist1, data = ristorante1, mean)
vendite1_mens_avg <- vendite1_mens_avg$lordototale
vendite1_mens_avg <- ts(vendite1_mens_avg, start=2018, frequency=12)

print(
  autoplot(vendite1_mens_avg) +
    ggtitle("Ristorante 1: vendite medie mensili") +
    xlab("Anno") +
    ylab("Vendite")
)

plot(vendite1_sett_avg, xlab = "Anno", ylab = "Vendite", main = "Vendite settimanali e mensili")
grid(col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
lines(vendite1_mens_avg, col = "red")
abline(v=c(2019, 2020, 2021, 2022), col = "darkgrey", lty=2)
legend("topright", c("media settimanale", "media mensile"), lty = 1, col = 1:2)

### Vendite giornaliere/settimanali/mensili periodo pre-COVID
# Prendo come data di riferimeno quella in cui sono iniziate le chiusure in Italia
# il virus
data_covid <- as.Date("2020-02-22", format = "%Y-%m-%d")
# Ristorante 1 pre-COVID
ristorante1_pre_covid <- ristorante1 %>% filter(ristorante1$data < data_covid)
copy_ristorante1_pre_covid <- copy_ristorante1 %>% filter(copy_ristorante1$data < data_covid)

# Vendite giornaliere pre-COVID
pre_covid_1_day <- ts(copy_ristorante1_pre_covid$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(pre_covid_1_day) +
    ggtitle("Ristorante 1: vendite giornaliere pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite settimanali pre-COVID
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist1_pre_covid <- as.Date(cut(ristorante1_pre_covid$data, "week"))
pre_covid_1_sett_avg <- aggregate(lordototale ~ week_rist1_pre_covid, data = ristorante1_pre_covid, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
pre_covid_1_sett_avg <- pre_covid_1_sett_avg[-c(1:35),]
pre_covid_1_sett_avg <- pre_covid_1_sett_avg$lordototale
pre_covid_1_sett_avg <- ts(pre_covid_1_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(pre_covid_1_sett_avg) +
    ggtitle("Ristorante 1: vendite medie settimanali pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili pre-COVID
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist1_pre_covid <- as.Date(cut(ristorante1_pre_covid$data, "month"))

pre_covid_1_mens_avg <- aggregate(lordototale ~ month_rist1_pre_covid, data = ristorante1_pre_covid, mean)
pre_covid_1_mens_avg <- pre_covid_1_mens_avg$lordototale
pre_covid_1_mens_avg <- ts(pre_covid_1_mens_avg, start=2018, frequency=12)

print(
  autoplot(pre_covid_1_mens_avg) +
    ggtitle("Ristorante 1: vendite medie mensili pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)


### Faccio la stessa analisi precedente sul numero di scontrini

# Scontrini giornalieri 
scontrini1_day <- ts(copy_ristorante1$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini1_day) +
    ggtitle("Ristorante 1: scontrini giornalieri") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali medi
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist1 <- as.Date(cut(ristorante1$data, "week"))
scontrini1_sett_avg <- aggregate(scontrini ~ week_rist1, data = ristorante1, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
scontrini1_sett_avg <- scontrini1_sett_avg[-c(1:35),]
scontrini1_sett_avg <- scontrini1_sett_avg$scontrini
scontrini1_sett_avg <- ts(scontrini1_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(scontrini1_sett_avg) +
    ggtitle("Ristorante 1: scontrini medi settimanali") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili medi
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist1 <- as.Date(cut(ristorante1$data, "month"))

scontrini1_mens_avg <- aggregate(scontrini ~ month_rist1, data = ristorante1, mean)
scontrini1_mens_avg <- scontrini1_mens_avg$scontrini
scontrini1_mens_avg <- ts(scontrini1_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini1_mens_avg) +
    ggtitle("Ristorante 1: scontrini medi mensili") +
    xlab("Anno") +
    ylab("Scontrini")
)

### Vendite giornaliere/settimanali/mensili periodo pre-COVID

# Scontrini giornalieri pre-COVID
scontrini_pre_covid_1_day <- ts(copy_ristorante1_pre_covid$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini_pre_covid_1_day) +
    ggtitle("Ristorante 1: scontrini giornalieri pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali pre-COVID
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist1_pre_covid <- as.Date(cut(ristorante1_pre_covid$data, "week"))
scontrini_pre_covid_1_sett_avg <- aggregate(scontrini ~ week_rist1_pre_covid, data = ristorante1_pre_covid, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
scontrini_pre_covid_1_sett_avg <- scontrini_pre_covid_1_sett_avg[-c(1:35),]
scontrini_pre_covid_1_sett_avg <- scontrini_pre_covid_1_sett_avg$scontrini
scontrini_pre_covid_1_sett_avg <- ts(scontrini_pre_covid_1_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(scontrini_pre_covid_1_sett_avg) +
    ggtitle("Ristorante 1: scontrini medi settimanali pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili pre-COVID
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist1_pre_covid <- as.Date(cut(ristorante1_pre_covid$data, "month"))

scontrini_pre_covid_1_mens_avg <- aggregate(scontrini ~ month_rist1_pre_covid, data = ristorante1_pre_covid, mean)
scontrini_pre_covid_1_mens_avg <- scontrini_pre_covid_1_mens_avg$scontrini
scontrini_pre_covid_1_mens_avg <- ts(scontrini_pre_covid_1_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini_pre_covid_1_mens_avg) +
    ggtitle("Ristorante 1: scontrini medi mensili pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)


### Stagionalit? considerando tutti gli anni

print(
  ggseasonplot(vendite1_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("Vendite") +
    ggtitle("Seasonal plot: vendite settimanali")
)

vendite1_sett_avg <- as.numeric(vendite1_sett_avg)
initial <- rep(NA, 36)
vendite1_sett_avg <- c(initial, vendite1_sett_avg)
vendite1_sett_avg <- ts(vendite1_sett_avg, start = decimal_date(as.Date("2018-01-01")), frequency=52)

# Nel grafico precedente c'? un problema sull'anno 2018, che dovrebbe partire dalla
# settimana 36 ma per qualche motivo "interpola" a partire dalla settimana 1. 
# Non ho trovato come risolvere questa cosa

print(
  ggseasonplot(vendite1_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("Vendite") +
    ggtitle("Seasonal plot: vendite mensili")
)

### Seasonal sub series plot
print(
  ggsubseriesplot(vendite1_mens_avg) +
    ylab("Vendite") +
    ggtitle("Seasonal subseries plot - serie storica completa"))


### Stagionalit? considerando il periodo pre-COVID

print(
  ggseasonplot(pre_covid_1_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 1: vendite settimanali pre-COVID")
)

print(
  ggseasonplot(pre_covid_1_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 1: vendite mensili pre-COVID")
)

### Seasonal sub series plot

print(
  ggsubseriesplot(pre_covid_1_mens_avg) +
    ylab("Vendite") +
    ggtitle("Seasonal subseries plot - periodo pre-Covid")
)


### Analisi correlazione tra vendite e scontrini

scon_vend_sett_avg_1 <- ts.intersect(vendite1_sett_avg, scontrini1_sett_avg)
colnames(scon_vend_sett_avg_1) <- c('Vendite medie settimanali', 'Scontrini medi settimanali')

print(
  autoplot(scon_vend_sett_avg_1, facets=TRUE) +
    xlab("") + ylab("") +
    ggtitle("Confronto andamento vendite e scontrini")
)

print(
  qplot(lordototale, scontrini, data=as.data.frame(copy_ristorante1), color=factor(Pandemia)) +
    scale_colour_manual(values = c("red","blue"), name="Pandemia") +
    ylab("Scontrini") + xlab("Vendite")+
    ggtitle("Correlazione vendite e scontrini") +
    theme(legend.position = c(0.94, 0.1),
          legend.background = element_rect(fill = "white", color = "black"))
)

# Ho usato la copia senza dati aggregati mensilmente


### Analisi autocorrelazione considerando tutti gli anni
# Per una serie con trend l'autocorrelazione ? alta a lag vicini e si abbassa
# piano piano. Se c'? stagionalit?, invece, l'autocorrelazione presenta delle
# regolarit? nel suo andamento

print(
  ggAcf(vendite1_day, lag=28) +
    ggtitle("Ristorante 1: Autocorrelation vendite giornaliere")
)

print(
  ggAcf(vendite1_sett_avg, lag=104) +
    ggtitle("Ristorante 1: Autocorrelation vendite medie settimanali")
)

print(
  ggAcf(vendite1_mens_avg, lag=36) +
    ggtitle("Ristorante 1: Autocorrelation vendite medie mensili")
)

### Analisi autocorrelazione pre-COVID

print(
  ggAcf(pre_covid_1_day, lag=28) +
    ggtitle("Ristorante 1: Autocorrelation vendite giornaliere pre-COVID")
)

print(
  ggAcf(pre_covid_1_sett_avg, lag=104) +
    ggtitle("Ristorante 1: Autocorrelation vendite medie settimanali pre-COVID")
)

print(
  ggAcf(pre_covid_1_mens_avg, lag=24) +
    ggtitle("Ristorante 1: Autocorrelation vendite medie mensili pre-COVID")
)


### Decomposizione serie storica
# Decomposizione giornaliera 
multi_vendite1 <- msts(copy_ristorante1$lordototale, ts.frequency = 365, start = decimal_date(as.Date("2018-09-03")), seasonal.periods = c(7,365))
multi_vendite1_dec <- mstl(multi_vendite1, s.window = "periodic")
print(autoplot(multi_vendite1_dec) + ggtitle("Ristorante 1: Decomposizione giornaliera"))

# Decomposizione settimanale
vendite1_sett.fit <- stl(vendite1_sett_avg, s.window="periodic")
trend.vendite1_sett <- vendite1_sett.fit$time.series[,2]
stag.vendite1_sett <- vendite1_sett.fit$time.series[,1]
res.vendite1_sett <- vendite1_sett.fit$time.series[,3]
print(autoplot(vendite1_sett.fit) + ggtitle("Ristorante 1: Decomposizione settimanale"))

# Decomposizione mensile 
vendite1_mens.fit <- stl(vendite1_mens_avg,s.window="periodic")
trend.vendite1_mens <- vendite1_mens.fit$time.series[,2]
stag.vendite1_mens <- vendite1_mens.fit$time.series[,1]
res.vendite1_mens <- vendite1_mens.fit$time.series[,3]
print(autoplot(vendite1_mens.fit) + ggtitle("Ristorante 1: Decomposizione mensile"))

# Alternativa
# components.ts_1 = decompose(vendite1_mens_avg)
# plot(components.ts_1)

### Decomposizione giornaliera serie storica pre-COVID

multi_vendite1_pre_covid <- msts(copy_ristorante1_pre_covid$lordototale, ts.frequency = 365, start = decimal_date(as.Date("2018-09-03")), seasonal.periods = c(7,365))
multi_vendite1_dec_pre_covid <- mstl(multi_vendite1_pre_covid, s.window = "periodic")
print(autoplot(multi_vendite1_dec_pre_covid) + ggtitle("Ristorante 1 pre-COVID: Decomposizione giornaliera"))


# Confronto estati 2019/2020/2021 (pre-durante-post COVID)

r1_estate_2019 <- subset(copy_ristorante1, Year==2019 & Season == 'Summer')
r1_estate_2020 <- subset(copy_ristorante1, Year==2020 & Season == 'Summer')
r1_estate_2021 <- subset(copy_ristorante1, Year==2021 & Season == 'Summer')

r1_totale_estati <- rbind(r1_estate_2019, r1_estate_2020, r1_estate_2021)

# Creo un attributo per creare le label del grafico
r1_totale_estati$Year <- format(r1_totale_estati$data, "%Y")
r1_totale_estati$Month <- format(r1_totale_estati$data, "%b")
r1_totale_estati$Giorno <- format(r1_totale_estati$data, "%d")
r1_totale_estati$MonthDay <- format(r1_totale_estati$data, "%d-%b")

# Per le label ne tengo una ogni 3 giorni
r1_totale_estati$MonthDay2 <- r1_totale_estati$MonthDay
r1_totale_estati$MonthDay2[as.numeric(row.names(r1_totale_estati))%%3!=0] <- ""
labels <- r1_totale_estati$MonthDay2

# Calcolo la media per anno
mean <- r1_totale_estati %>% group_by(Year) %>% summarise(mean_val=mean(lordototale))

p <- ggplot(data=r1_totale_estati, mapping=aes(x=MonthDay, y=lordototale, shape=Year, color=Year)) + geom_point() +
  geom_line(aes(group = 1)) + geom_hline(data = mean, aes(yintercept = mean_val, col=Year), linetype = 'dashed')
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 1: confronto estati")
)


# Analisi andamento scontrino medio

df_scontrino_medio <- copy_ristorante1[, c("data", "Prezzo_medio_per_scontrino")]

# Divido in due parti i miei dati: il pre-covid, che arriva fino al 11-03-2020 compreso (ultimo giorno di 
# apertura prima della quarantena), e il post-covid, che parte dal 07-05-2020 compreso (primo giorno di 
# riapertura)

df_scontrino_medio <- df_scontrino_medio %>%
  mutate(Periodo = case_when(
    (data <= "2020-03-11") ~ "Pre"
    , TRUE ~ "Post"
  )
)

# Elimino le righe del periodo di chisura, che non mi interessano

df_scontrino_medio <- df_scontrino_medio %>% filter(df_scontrino_medio$data <= "2020-03-11" |
                                                      df_scontrino_medio$data >= "2020-05-07")

# Decido di eliminare gli outlier, per una stima più consistente della media

Q1 <- quantile(df_scontrino_medio$Prezzo_medio_per_scontrino, .25)
Q3 <- quantile(df_scontrino_medio$Prezzo_medio_per_scontrino, .75)
IQR <- IQR(df_scontrino_medio$Prezzo_medio_per_scontrino)

df_scontrino_medio_no_out <- subset(df_scontrino_medio, df_scontrino_medio$Prezzo_medio_per_scontrino > (Q1 - 1.5*IQR)
                                    & df_scontrino_medio$Prezzo_medio_per_scontrino < (Q3 + 1.5*IQR))

# Calcolo la media per periodo
mean_scontrino <- df_scontrino_medio_no_out %>% group_by(Periodo) %>% 
  summarise(mean_val=mean(Prezzo_medio_per_scontrino))

var_scontrino <- df_scontrino_medio_no_out %>% group_by(Periodo) %>% 
  summarise(varianza=var(Prezzo_medio_per_scontrino))

p <- ggplot(df_scontrino_medio_no_out, aes(x = data, y = Prezzo_medio_per_scontrino,
           col = Periodo)) + geom_line() + 
  geom_hline(data = mean_scontrino, aes(yintercept = mean_val, col=Periodo), linetype = 'dashed')
  # + stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") aggiunge una sorta di trend
print(
  p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Confronto scontrino medio") + ylab("Prezzo medio per scontrino") + xlab(" ") +
    theme(legend.position = c(0.95, 0.1),
          legend.background = element_rect(fill = "white", color = "black"))
)

# Valuto la differenza fra il valore medio del prezzo per scontrino fra post e pre

mean_scontrino$mean_val[1] - mean_scontrino$mean_val[2]

# Varianza pre/post

var_scontrino
























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
                                                           "Durum_Wheat", "Feed_Barley", "Maize",
                                                           "Milling_Wheat")]

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
                      Durum_Wheat + Feed_Barley + Maize + Milling_Wheat, data = train_rf)
varImpPlot(MRF)
print(MRF)
# % Var explained: 84.06

### DALL' IMPORTANCE PLOT DELLE VARIABILI POSSIAMO VEDERE CHE TUTTE LE VARIABILI LEGATI AI VACCINI
### NON SONO RILEVANTI, COMINCIO AD ELIMINARE QUELLE

MRF_V2 <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                         Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                         BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                         GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                         Durum_Wheat + Feed_Barley + Maize + Milling_Wheat, data = train_rf)
varImpPlot(MRF_V2)
print(MRF_V2)
# % Var explained: 83.94

### CAMBIA DI POCO, QUINDI A QUESTO PUNTO MI LIMITO ALLE 7 (numero casuale) VARIABILI PIU' IMPORTANTI

MRF_V3 <- randomForest(lordototale ~ Giorno + Month + Weekend + Festivo + GASOLIO_AUTO.LITRO 
                       + Durum_Wheat + Maize, data = train_rf)
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
                                    'Festivo', 'GASOLIO_AUTO.LITRO', 'Durum_Wheat', 'Maize')]

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
r1_rf_covid$Durum_Wheat[is.na(r1_rf_covid$Durum_Wheat)] <- 276.5

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
plot(rf_complete$data, rf_complete$perdite, type="l", col="black", xlab="data", ylab="vendite", lty=1)

data_inizio <- as.Date("2020-01-01", format = "%Y-%m-%d")
stima_trend_rf <- rf_complete %>%
  filter(rf_complete$data > data_inizio)

perdite_stimate <- msts(stima_trend_rf$perdite, ts.frequency = 365, start = decimal_date(as.Date("2020-01-01")), seasonal.periods = c(7,365))
perdite_stimate_dec <- mstl(perdite_stimate, s.window = "periodic")
print(autoplot(perdite_stimate_dec) + ggtitle("Ristorante 1: perdite stimate"))








### Prophet ----
# Le vendite giornaliere pre-COVID (ristorante1_pre_covid$lordototale) vengono divise 
# in train e test  per cercare di modellare i dati a disposizione e cercare di 
# valutarne la qualit? del modello ottenuto.
# Il seguente modello viene utilizzato per fare previsioni su valori futuri, in 
# particolar modo per prevedere come le vendite sarebbero andate durante il periodo
# covid, durante il quale per alcune settimane le vendite effettive invece sono 
# state pari a zero

prophet_vendite <- copy_ristorante1_pre_covid %>% 
  select(data, lordototale)
colnames(prophet_vendite) <- c("ds", "y")

# divisione in train e test
index_pr <- 344  # train-70% di prophet_vendite (344), test-30% di prophet_vendite (148)
train_pr <- prophet_vendite[1:344,]
test_pr <- prophet_vendite[345:493,] 

# si crea il modello
MPROPHET <- prophet(train_pr)

# vengono fatte le previsioni
future_prophet <- make_future_dataframe(MPROPHET, periods=281) # fino al 17 maggio 2020
vendite_forecast_prophet <- predict(MPROPHET, future_prophet)
tail(vendite_forecast_prophet[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# yhat containing the vendite_forecast_prophet. It has additional columns for uncertainty 
# intervals and seasonal components

plot(MPROPHET, vendite_forecast_prophet)
prophet_plot_components(MPROPHET, vendite_forecast_prophet)
# prophet_vendite.cv <- cross_validation(MPROPHET, initial=180, period=60, horizon=120, units='days')
# plot_cross_validation_metric(prophet_vendite.cv, metric='mape')
dyplot.prophet(MPROPHET, vendite_forecast_prophet)

# performance sul test set
accuracy(test_pr$y, vendite_forecast_prophet[345:493, "yhat"])
# RMSE 1353.581
# MAPE 14.9611

### ABBIAMO POCHI DATI PRIMA DELLA PANDEMIA, QUINDI NON SO SE LE CONCLUSIONI DEI 
### VARI MODELLI POSSANO ESSERE CONSISTENTI




### TABTS ----
# Le vendite giornaliere pre-COVID (ristorante1_pre_covid$lordototale) vengono divise 
# in train e test  per cercare di modellare i dati a disposizione e cercare di 
# valutarne la qualit? del modello ottenuto.
# Il seguente modello viene utilizzato per fare previsioni su valori futuri, in 
# particolar modo per prevedere come le vendite sarebbero andate durante il periodo
# covid, durante il quale per alcune settimane le vendite effettive invece sono 
# state pari a zero

vendite1_day_pre_split_tbats <- ts_split(pre_covid_1_day)
train_tbats <- vendite1_day_pre_split_tbats$train
test_tbats <- vendite1_day_pre_split_tbats$test

tbats_data <- msts(train_tbats, seasonal.periods=c(7,365.25))
MTABTS <- tbats(tbats_data)

#  considerando test set
vendite_forecast_tbats <- forecast(MTABTS, h=length(test_tbats))
autoplot(vendite_forecast_tbats, pre_covid_1_day.colour = 'black')

MTABTS %>%
  forecast(h=length(test_tbats)) %>%  
  autoplot() + autolayer(test_tbats)

# performance
accuracy(vendite_forecast_tbats$mean, test_tbats)
# RMSE 1431.843
# MAPE 14.83464

# previsioni periodo covid
MTABTS %>%
  forecast(h=320) %>%  # fino a met? maggio circa
  autoplot() + autolayer(train_tbats)

# cofronto con valori reali
autoplot(ts(vendite1_day[1:1233], start=2017,frequency=365))




# PREVISIONE ANDAMENTO VENDITE POST APRILE 2022 --------------------------------
# Le previsioni sono state fatte per il periodo che va dal 1 maggio 2022 al 
# 30 settembre 2022. Si può tranquillamente estendere
# In questo caso non vi è una suddivisione in train e test dei dati a disposizione


### HoltWinters ----
# Il modello viene addestrato su tutti i dati (settimanali) a disposizione (vendite1_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022. 

# Metodo lisciamento esponenziale
MHW <- HoltWinters(vendite1_sett_avg)  # il modello permette di catturare trend e stagionalità
plot(MHW)

# parametri
MHW$alpha  # 0.6926003 
MHW$beta  # 0
MHW$gamma  # 1

# analisi residui
acf(residuals(MHW), lag = 52)

# previsione, fino alla settimana 26 settembre - 2 ottobre 2022 compresa
vendite_forecast_hw <- forecast(MHW, h=23)
autoplot(vendite_forecast_hw)

# in alternativa
vendite_forecast_hw <- predict(MHW, 23, prediction.interval=TRUE)
plot(MHW, vendite_forecast_hw)

# RMSE
# sqrt(mean((vendite1_sett_avg[53:223]- MHW$fitted[,"xhat"])^2)) non so bene come gestire il calcolo nel nostro caso



### SARIMAX ----
# Il modello viene addestrato su tutti i dati (settimanali) a disposizione (vendite1_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022.

# setting regressori

# dati "COVID" su base settimanale (somma, si contano i giorni della settimana in cui c'è il covid)
# aggiungo la colonna COVID SI (1)/NO (0)
copy_ristorante1$COVID <- 0
copy_ristorante1[copy_ristorante1$data > "2020-03-09",]$COVID <- 1
copy_ristorante1$COVID  <- as.factor(copy_ristorante1$COVID)

copy_ristorante1$COVID <- as.numeric(as.character(copy_ristorante1$COVID))
week_rist1_sarimax <- week_rist1[-c(1: 243)] # prendo solo le settimane contenute in copy_ristorante1
week_covid_sum <- aggregate(COVID ~ week_rist1_sarimax, copy_ristorante1, sum)  
week_covid_sum <- week_covid_sum$COVID
copy_ristorante1$COVID <- as.factor(copy_ristorante1$COVID)

# dati "chiuso" su base settimanale (somma, si contano i giorni della settimana in cui il ristorante è chiuso, quindi senza vendite)
# aggiungo la colonna CHIUSO SI (1)/NO (0)
copy_ristorante1$CHIUSO <- 0
copy_ristorante1[copy_ristorante1$lordototale == 0,]$CHIUSO <- 1
copy_ristorante1$CHIUSO  <- as.factor(copy_ristorante1$CHIUSO)

copy_ristorante1$CHIUSO <- as.numeric(as.character(copy_ristorante1$CHIUSO))
week_chiuso_sum <- aggregate(CHIUSO ~ week_rist1_sarimax, copy_ristorante1, sum)  
week_chiuso_sum <- week_chiuso_sum$CHIUSO
copy_ristorante1$CHIUSO <- as.factor(copy_ristorante1$CHIUSO)

# dati "zona rossa" su base settimanale (somma, si contano i giorni della settimana in cui c'è zona rossa)
# aggiungo la colonna ROSSA SI (1)/NO (0)
copy_ristorante1$ROSSA <- 0
copy_ristorante1[copy_ristorante1$ColoreCOVID == "rosso",]$ROSSA <- 1
copy_ristorante1$ROSSA  <- as.factor(copy_ristorante1$ROSSA)

copy_ristorante1$ROSSA <- as.numeric(as.character(copy_ristorante1$ROSSA))
week_rossa_sum <- aggregate(ROSSA ~ week_rist1_sarimax, copy_ristorante1, sum)  
week_rossa_sum <- week_rossa_sum$ROSSA
copy_ristorante1$ROSSA <- as.factor(copy_ristorante1$ROSSA)

regressori_week <- data.frame(week_covid_sum, week_chiuso_sum, week_rossa_sum)

# trasformazione colonne precedenti in valori binari
regressori_week <- regressori_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))  # se almeno un giorno durante la settimana ha registrato il covid allora tale settimana viene etichettata come settimana covid
#regressori_week$week_covid_bin <- as.factor(regressori_week$week_covid_bin)

regressori_week <- regressori_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>3, 1, 0))  # se un ristorante durante la settimana rimane chiuso per più di 3 giorni allora tale settimana viene etichettata come settimana chiusa
#regressori_week$week_chiuso_bin <- as.factor(regressori_week$week_chiuso_bin)

regressori_week <- regressori_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>3, 1, 0))  # se un ristorante durante la settimana è in zona rossa per più di 3 giorni allora tale settimana viene etichettata come settimana rossa
#regressori_week$week_rossa_bin <- as.factor(regressori_week$week_rossa_bin)

# verifica collinearità variabili
numeric.var <- sapply(regressori_week, is.numeric)
corr.matrix <- cor(regressori_week[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# regressori: "week_covid_bin", "week_rossa_bin"

MSARIMAX <- auto.arima(vendite1_sett_avg, seasonal = TRUE, 
                 xreg = data.matrix(regressori_week[c(-1), c("week_covid_bin", "week_rossa_bin")])) 
                                          # tolto la prima settimana di regressori_week per uniformare dimensionalità
summary(MSARIMAX) 
# AIC 3548.42
# BIC 3568.87

# considerando il training set
# RMSE 667.4276
# MAPE Inf

checkresiduals(MSARIMAX)
tsdisplay(residuals(MSARIMAX), lag.max=52, main='Seasonal Model Residuals')

valori <- MSARIMAX$coef["week_covid_bin"]/sqrt(diag(MSARIMAX$var.coef))
pvalue = 2*pt(valori["week_covid_bin"] ,220)
pvalue

valori <- MSARIMAX$coef["week_rossa_bin"]/sqrt(diag(MSARIMAX$var.coef))
pvalue = 2*pt(valori["week_rossa_bin"] ,220)
pvalue

# verifica adattamento modello
autoplot(MSARIMAX$fitted) + autolayer(vendite1_sett_avg)

# Si procede ora utilizzando il modello ottenuto per fare previsioni su dati nuovi,
# in particolare si cerca di prevedere le vendite dopo aprile 2022, date per cui 
# non si hanno a disposizone informazioni relative a vendite. In particolare si cerca 
# di prevedere per il periodo che va dal 1 maggio 2022 al 30 settembre 2022, date per cui si 
# possono ricavare i valori dei regressori ma non si possono avere i valori di 
# vendite, valori che dunque vengono previsti utilizzando il modello precedente
# e i regressori ottenuti per le nuove date

# NB. Il 30 settembre è un venerdì, quindi non viene presa in considerazione la 
# settimana intera ma ciò non influenza il valore dei regressori (la settimana 
# è comunque etichettata covid, non ci sono possibili chiusure e non c'è zona rossa)

# Per procedere bisogna prima avere i valori dei regressori per le date per cui
# verranno eseguite le previsioni

# creazione df per previsione (dal 1 maggio 2022 al 30 settembre 2022) 
date_previsione <- seq(as.Date("2022-05-01"), as.Date("2022-09-30"), by="days")
regressori_forecast_day <- data.frame(date_previsione)
regressori_forecast_day$COVID <- 1
regressori_forecast_day$CHIUSO <- 0 # non ci sono date in cui sicuramente i ristoranti sono stati chiusi
regressori_forecast_day$ROSSA <- 0 # non ci sono più state date in zona rossa

# divisione in settimane
week_new_rist1 <- as.Date(cut(regressori_forecast_day$date_previsione, "week"))

week_rossa_new <- aggregate(ROSSA ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana
week_chiuso_new <- aggregate(CHIUSO ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana
week_covid_new <- aggregate(COVID ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana

regressori_forecast_week <- data.frame(week_covid_new$COVID, week_chiuso_new$CHIUSO, week_rossa_new$ROSSA)
colnames(regressori_forecast_week) <- c("week_covid_sum", "week_chiuso_sum", "week_rossa_sum")

# trasformazione colonne precedenti in valori binari
regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))
#regressori_forecast_week$week_covid_bin <- as.factor(regressori_forecast_week$week_covid_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>3, 1, 0))
#regressori_forecast_week$week_rossa_bin <- as.factor(regressori_forecast_week$week_rossa_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>3, 1, 0))
#regressori_forecast_week$week_chiuso_bin <- as.factor(regressori_forecast_week$week_chiuso_bin)


# previsione vendite settimanali su dati nuovi
forecast_2022 <- MSARIMAX %>%
  forecast(h=23,  xreg =data.matrix(regressori_forecast_week[, c("week_covid_bin", "week_rossa_bin")])) 

autoplot(forecast_2022)



### RANDOM FOREST

copy_ristorante1[is.na(copy_ristorante1)] <- 0

# Implementazione modello
MRF_future <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                             Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                             Dose1Cum + Dose2Cum + DoseUnicaCum + Booster1Cum + Booster3DosiCum +
                             Booster2Cum + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                             GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                             Durum_Wheat + Feed_Barley + Maize + Milling_Wheat, data = copy_ristorante1)
varImpPlot(MRF_future)
print(MRF_future)
# % Var explained: 87.01

### TOLGO LE ULTIME 10 VARIABILI

MRF_future_V2 <- randomForest(lordototale ~ Giorno + Month + Year + Weekend + Festivo + 
                                ColoreCOVID + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                                GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + 
                                O.C._DENSO_BTZ.LITRO + Durum_Wheat + Maize, 
                              data = copy_ristorante1)
varImpPlot(MRF_future_V2)
print(MRF_future_V2)
# % Var explained: 86.6

# Decido di non eliminare ulteriori variabili, poichè l'importanza delle stesse è piuttosto alta

# Setto il periodo su cui fare previsioni, considerando i regressori selezionati
# Avendo mantenuto anche variabili legate a carburanti e cereali posso spingermi 
# solo fino a una data per cui sono noti i relativi valori
future_interval = seq(as.Date("2022-05-01"), as.Date("2022-08-31"), by="days")
ristorante1_future <- data.frame(future_interval)
colnames(ristorante1_future) <- "data"

# colonne Mese, Anno
ristorante1_future$Month <- month(ristorante1_future$data)
ristorante1_future$Month <- factor(ristorante1_future$Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
ristorante1_future$Year <- year(ristorante1_future$data)
ristorante1_future$Year <- factor(ristorante1_future$Year, levels = c("2018", "2019", "2020", "2021", "2022"))

# colonna Giorno, Weekend
Sys.setlocale("LC_ALL","English") # Per avere i nomi in inglese
ristorante1_future <- ristorante1_future %>%
  mutate(weekday = wday(data, label = TRUE, abbr = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        locale = Sys.getlocale("LC_TIME"))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c("Saturday", "Sunday")) ~ "weekend"
    , TRUE ~ "weekday"
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

# colonna Festivo
ristorante1_future <- ristorante1_future %>%
  mutate(Festivo = case_when(
    (Giorno %in% c("Sunday")) ~ "True"
    , TRUE ~ "False"
  )
  )
ristorante1_future$Festivo[ristorante1_future$data == "2022-06-02"] <- "True"
ristorante1_future$Festivo[ristorante1_future$data == "2022-07-04"] <- "True" # Patrono Piacenza
ristorante1_future$Festivo[ristorante1_future$data == "2022-08-15"] <- "True"
ristorante1_future$Festivo <- as.factor(ristorante1_future$Festivo)

# colonna ColoreCOVID
ristorante1_future$ColoreCOVID <- "bianco"
ristorante1_future$ColoreCOVID <- factor(ristorante1_future$ColoreCOVID, levels = c("", "arancione", "bianco", "giallo", "rosso"))

# colonne carburanti
carburanti <- read.csv("Dati/Dati per RANDOM FOREST/carburanti_agosto_22.csv")
# tengo soltanto le colonne necessarie
carburanti <- carburanti[, c("DATA_RILEVAZIONE", "BENZINA.LITRO", "GASOLIO_AUTO.LITRO", "GPL.LITRO", 
                             "GASOLIO_RISCALDAMENTO.LITRO", "O.C._FLUIDO_BTZ.LITRO",
                             "O.C._DENSO_BTZ.LITRO")]
carburanti$DATA_RILEVAZIONE <- parse_date(carburanti$DATA_RILEVAZIONE, "%Y-%m-%d", locale = locale("it"))

ristorante1_future <- merge(ristorante1_future, carburanti, by.x = "data", by.y = "DATA_RILEVAZIONE",
                            all.x = TRUE)

# colonne cereali
cereali <- read.csv("Dati/Dati per RANDOM FOREST/cereali_rf.csv")
# tengo soltanto le colonne necessarie
cereali <- cereali[, c("Reference.period", "Durum_Wheat", "Maize")]
# come fatto sopra, quando ho dei na per Durum_Wheat metto il dato della settimana precedente
cereali$Durum_Wheat[is.na(cereali$Durum_Wheat)] <- 542.5
cereali$Reference.period <- parse_date(cereali$Reference.period, "%Y-%m-%d", locale = locale("it"))

ristorante1_future <- merge(ristorante1_future, cereali, by.x = "data", by.y = "Reference.period",
                            all.x = TRUE)

# colonna lordototale
ristorante1_future$lordototale <- 0

# riordino le colonne
ristorante1_future <- data.frame(ristorante1_future$data,
                                 ristorante1_future$lordototale,
                                 ristorante1_future$Giorno,
                                 ristorante1_future$Month,
                                 ristorante1_future$Year,
                                 ristorante1_future$Weekend,
                                 ristorante1_future$Festivo,
                                 ristorante1_future$ColoreCOVID,
                                 ristorante1_future$BENZINA.LITRO,
                                 ristorante1_future$GASOLIO_AUTO.LITRO,
                                 ristorante1_future$GPL.LITRO,
                                 ristorante1_future$GASOLIO_RISCALDAMENTO.LITRO,
                                 ristorante1_future$O.C._FLUIDO_BTZ.LITRO,
                                 ristorante1_future$O.C._DENSO_BTZ.LITRO,
                                 ristorante1_future$Durum_Wheat,
                                 ristorante1_future$Maize
)

names(ristorante1_future)<- c("data", 
                              "lordototale",
                              "Giorno",
                              "Month",
                              "Year",
                              "Weekend",
                              "Festivo",
                              "ColoreCOVID",
                              "BENZINA.LITRO", 
                              "GASOLIO_AUTO.LITRO", 
                              "GPL.LITRO", 
                              "GASOLIO_RISCALDAMENTO.LITRO", 
                              "O.C._FLUIDO_BTZ.LITRO",
                              "O.C._DENSO_BTZ.LITRO",
                              "Durum_Wheat",
                              "Maize"
)

ristorante1_RF_full <-rbind(copy_ristorante1[,c("data", "lordototale", "Giorno",
                                                "Month", "Year", "Weekend", "Festivo",
                                                "ColoreCOVID", "BENZINA.LITRO", 
                                                "GASOLIO_AUTO.LITRO", "GPL.LITRO", 
                                                "GASOLIO_RISCALDAMENTO.LITRO", 
                                                "O.C._FLUIDO_BTZ.LITRO",
                                                "O.C._DENSO_BTZ.LITRO",
                                                "Durum_Wheat", "Maize")], ristorante1_future)

# si utilizza il modello precedente per fare previsioni sul futuro
vendite_forecast_rf <- predict(MRF_future_V2, ristorante1_RF_full[1339:1461,])
vendite_forecast_rf <- as.data.frame(vendite_forecast_rf)

# si uniscono le tue serie storiche

# serie storica previsioni
future_interval_df <- data.frame(date = future_interval, 
                                 val=vendite_forecast_rf)
future_interval_df$date<-as.Date(future_interval_df$date)  
future_interval_ts <- xts(future_interval_df$val, future_interval_df$date)

plot(future_interval_df$date, future_interval_df$vendite_forecast, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino al 1 maggio 2022
reference_date_attuale <- as.Date("2022-04-30", format = "%Y-%m-%d")
vendite_reali <- ristorante1_RF_full %>%
  filter(data <= reference_date_attuale) %>%
  select(data, lordototale)

interval_reale <- seq(as.Date("2018-09-01"), as.Date("2022-04-30"), by = "day")
interval_pre_df <- data.frame(date = interval_reale, 
                              val=vendite_reali$lordototale)
interval_pre_df$date<-as.Date(interval_pre_df$date)  
interval_pre_ts <- xts(interval_pre_df$val, interval_pre_df$date)

# si uniscono le due serie storiche
names(future_interval_df)[1] <- "data"
names(future_interval_df)[2] <- "vendite"

names(interval_pre_df)[1] <- "data"
names(interval_pre_df)[2] <- "vendite"

forecast_completo <- rbind(future_interval_df, interval_pre_df)
forecast_completo <- forecast_completo[order(forecast_completo$data), ]
row.names(forecast_completo) <- NULL

# serie storica con previsioni
plot(forecast_completo$data, forecast_completo$vendite, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1 previsioni")

# verifica performance modello
RMSE.rf <- sqrt(mean((MRF_future_V2$predicted - copy_ristorante1$lordototale)^2))






### Prophet con regressori ----
# Il modello viene addestrato su tutti i dati (settimanali) a disposizione (vendite1_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022.

covid <- function(ds) {
  dates <- as.Date(ds)
  reference_date_pre <- as.Date("2020-03-09", format = "%Y-%m-%d")
  as.numeric(dates > reference_date_pre)
}

# periodo pre maggio 2022
ristorante1_pre_maggio_prophet <- data.frame(copy_ristorante1$data, 
                                             copy_ristorante1$ROSSA)
names(ristorante1_pre_maggio_prophet) <- c("data", "rossa")

# periodo post maggio 2022
ristorante1_post_maggio_prophet <- data.frame(future_interval)
ristorante1_post_maggio_prophet$rossa <- 0
names(ristorante1_post_maggio_prophet) <- c("data", "rossa")


# unione periodo pre aprile e periodo post aprile
vendite_prophet_new <- rbind(ristorante1_pre_maggio_prophet, 
                             ristorante1_post_maggio_prophet)

# si considerano le vendite fino ad aprile 2022
prophet_vendite_totali <- copy_ristorante1 %>% 
  select(data, lordototale)
colnames(prophet_vendite_totali) <- c("ds", "y")
prophet_vendite_totali$covid <- covid(prophet_vendite_totali$ds)
prophet_vendite_totali$rossa <- ristorante1_pre_maggio_prophet$rossa

# si crea il modello
MPROPHET_NEW <- prophet(daily.seasonality=FALSE)
MPROPHET_NEW <- add_country_holidays(MPROPHET_NEW, country_name = 'IT')
MPROPHET_NEW <- add_regressor(MPROPHET_NEW, 'covid')
MPROPHET_NEW <- add_regressor(MPROPHET_NEW, 'rossa')
MPROPHET_NEW <- fit.prophet(MPROPHET_NEW, prophet_vendite_totali)

# vengono fatte le previsioni
future_prophet_regr <- make_future_dataframe(MPROPHET_NEW, periods=153)

future_prophet_regr$covid <- covid(future_prophet_regr$ds)
future_prophet_regr$rossa <- vendite_prophet_new$rossa

vendite_forecast_prophet_regr <- predict(MPROPHET_NEW, future_prophet_regr)

plot(MPROPHET_NEW, vendite_forecast_prophet_regr)
prophet_plot_components(MPROPHET_NEW, vendite_forecast_prophet_regr)

dyplot.prophet(MPROPHET_NEW, vendite_forecast_prophet_regr)

sqrt(mean((prophet_vendite_totali$y- vendite_forecast_prophet_regr[1:1563,"yhat"])^2))
# RMSE 1344.155




### TBATS ----
# Il modello viene addestrato su tutti i dati (giornalieri) a disposizione (vendite1_day) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022.

tbats_data_new <- msts(vendite1_day, seasonal.periods=c(7,365.25))
MTBATS_NEW <- tbats(tbats_data_new)

# previsioni post aprile 2022
vendite_forecast_tbats_new <- forecast(MTBATS_NEW, h=122)
autoplot(vendite_forecast_tbats_new, tbats_data = 'black') # non riesco a capire perchè questo andamento

# RMSE
sqrt(mean((vendite1_day- MTBATS_NEW$fitted.values)^2))  # 1144.543
