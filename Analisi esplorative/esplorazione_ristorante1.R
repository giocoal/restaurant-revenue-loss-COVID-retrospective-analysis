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