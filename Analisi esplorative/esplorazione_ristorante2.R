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
ristorante2 <- read.csv("ristorante2.csv")

# Presenza di NaN

sum(is.na(ristorante2$scontrini)) # 322 NA
# which(is.na(ristorante2$scontrini))
subset(ristorante2[,c(2,6)], is.na(ristorante2$scontrini))
# Fino alla riga 243 la presenza di NaN ? causata dal fatto che i dati sono
# aggregati mensilmente. Dopodich? c'? tutto il periodo COVID (da 802 a 854) e 
# alcune festivit?. Sembrerebbe esserci un periodo di chiusura fra le righe 1366
# e 1395, ma da 1390 ? presente il dato del lordo totale. C'? da sistemare il 
# valore medio degli scontrini per quei giorni

### Metto a 0 i Na, per comodit?

ristorante2$lordototale[is.na(ristorante2$lordototale)] <- 0
ristorante2$scontrini[is.na(ristorante2$scontrini)] <- 0  
ristorante2$Prezzo_medio_per_scontrino[is.na(ristorante2$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data

ristorante2$data <- parse_date(ristorante2$data, "%Y-%m-%d", locale = locale("it"))

# Creo una copia togliendo i dati aggregati mensilmente dei primi 8 mesi del 2018

copy_ristorante2 <- ristorante2[-c(1:243),]

### Creo alcuni boxplot potenzialmente utili

# Rendo gli attributi Giorno, Month, Year, ... dei fattori. In questo modo riesco a
# manipolari i boxplot correttamente
copy_ristorante2$Giorno <- as.factor(copy_ristorante2$Giorno)
copy_ristorante2$Giorno <- factor(copy_ristorante2$Giorno, 
                                            levels=c('Monday','Tuesday','Wednesday',
                                                     'Thursday','Friday','Saturday',
                                                     'Sunday'))

copy_ristorante2$Month <- as.factor(copy_ristorante2$Month)

copy_ristorante2$Year <- as.factor(copy_ristorante2$Year)

copy_ristorante2$Season <- as.factor(copy_ristorante2$Season)
copy_ristorante2$Season <- factor(copy_ristorante2$Season, 
                                            levels=c('Spring','Summer','Autumn',
                                                     'Winter'))

copy_ristorante2$Weekend <- as.factor(copy_ristorante2$Weekend)

copy_ristorante2$Festivo <- as.factor(copy_ristorante2$Festivo)

copy_ristorante2$Pioggia <- as.factor(copy_ristorante2$Pioggia)

# Creo i diversi boxplot (sia vendite che scontrini)

### Giorno della settimana
ggplot(copy_ristorante2, aes(Giorno, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Giorno, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Mese dell'anno
ggplot(copy_ristorante2, aes(Month, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Month, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Anno
ggplot(copy_ristorante2, aes(Year, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Year, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Stagione
ggplot(copy_ristorante2, aes(Season, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Season, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Weekend/settimana (il venerd? ? considerato giorno della settimana)
ggplot(copy_ristorante2, aes(Weekend, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Weekend, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Giorno feriale vs. festivo
ggplot(copy_ristorante2, aes(Festivo, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Festivo, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Pioggia si/no
ggplot(copy_ristorante2, aes(Pioggia, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante2, aes(Pioggia, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Vendite giornaliere 
vendite2_day <- ts(copy_ristorante2$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(vendite2_day) +
    ggtitle("Ristorante 2: vendite giornaliere") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite settimanali medie
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist2 <- as.Date(cut(ristorante2$data, "week"))
vendite2_sett_avg <- aggregate(lordototale ~ week_rist2, data = ristorante2, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
vendite2_sett_avg <- vendite2_sett_avg[-c(1:35),]
vendite2_sett_avg <- vendite2_sett_avg$lordototale
vendite2_sett_avg <- ts(vendite2_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(vendite2_sett_avg) +
    ggtitle("Ristorante 2: vendite medie settimanali") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili medie
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist2 <- as.Date(cut(ristorante2$data, "month"))

vendite2_mens_avg <- aggregate(lordototale ~ month_rist2, data = ristorante2, mean)
vendite2_mens_avg <- vendite2_mens_avg$lordototale
vendite2_mens_avg <- ts(vendite2_mens_avg, start=2018, frequency=12)

print(
  autoplot(vendite2_mens_avg) +
    ggtitle("Ristorante 2: vendite medie mensili") +
    xlab("Anno") +
    ylab("Vendite")
)

### Vendite giornaliere/settimanali/mensili periodo pre-COVID
# Prendo come data di riferimeno quella in cui le autorit? cinesi hanno identificato
# il virus
data_covid <- as.Date("2020-01-07", format = "%Y-%m-%d")
# Ristorante 2 pre-COVID
ristorante2_pre_covid <- ristorante2 %>% filter(ristorante2$data < data_covid)
copy_ristorante2_pre_covid <- copy_ristorante2 %>% filter(copy_ristorante2$data < data_covid)

# Vendite giornaliere pre-COVID
pre_covid_2_day <- ts(copy_ristorante2_pre_covid$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(pre_covid_2_day) +
    ggtitle("Ristorante 2: vendite giornaliere pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite settimanali pre-COVID
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist2_pre_covid <- as.Date(cut(ristorante2_pre_covid$data, "week"))
pre_covid_2_sett_avg <- aggregate(lordototale ~ week_rist2_pre_covid, data = ristorante2_pre_covid, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
pre_covid_2_sett_avg <- pre_covid_2_sett_avg[-c(1:35),]
pre_covid_2_sett_avg <- pre_covid_2_sett_avg$lordototale
pre_covid_2_sett_avg <- ts(pre_covid_2_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(pre_covid_2_sett_avg) +
    ggtitle("Ristorante 2: vendite medie settimanali pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili pre-COVID
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist2_pre_covid <- as.Date(cut(ristorante2_pre_covid$data, "month"))

pre_covid_2_mens_avg <- aggregate(lordototale ~ month_rist2_pre_covid, data = ristorante2_pre_covid, mean)
pre_covid_2_mens_avg <- pre_covid_2_mens_avg$lordototale
pre_covid_2_mens_avg <- ts(pre_covid_2_mens_avg, start=2018, frequency=12)

print(
  autoplot(pre_covid_2_mens_avg) +
    ggtitle("Ristorante 2: vendite medie mensili pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)


### Faccio la stessa analisi precedente sul numero di scontrini

# Scontrini giornalieri 
scontrini2_day <- ts(copy_ristorante2$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini2_day) +
    ggtitle("Ristorante 2: scontrini giornalieri") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali medi
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist2 <- as.Date(cut(ristorante2$data, "week"))
scontrini2_sett_avg <- aggregate(scontrini ~ week_rist2, data = ristorante2, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
scontrini2_sett_avg <- scontrini2_sett_avg[-c(1:35),]
scontrini2_sett_avg <- scontrini2_sett_avg$scontrini
scontrini2_sett_avg <- ts(scontrini2_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(scontrini2_sett_avg) +
    ggtitle("Ristorante 2: scontrini medi settimanali") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili medi
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist2 <- as.Date(cut(ristorante2$data, "month"))

scontrini2_mens_avg <- aggregate(scontrini ~ month_rist2, data = ristorante2, mean)
scontrini2_mens_avg <- scontrini2_mens_avg$scontrini
scontrini2_mens_avg <- ts(scontrini2_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini2_mens_avg) +
    ggtitle("Ristorante 2: scontrini medi mensili") +
    xlab("Anno") +
    ylab("Scontrini")
)

### Scontrini giornalieri/settimanali/mensili periodo pre-COVID

# Scontrini giornalieri pre-COVID
scontrini_pre_covid_2_day <- ts(copy_ristorante2_pre_covid$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini_pre_covid_2_day) +
    ggtitle("Ristorante 2: scontrini giornalieri pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali pre-COVID
# Per comodit? utilizzo il dataset completo perch? il 01-01-2018 ? un luned?.
# Poi toglier? i dati delle prime 35 settimane perch? sono aggregati mensilmente
week_rist2_pre_covid <- as.Date(cut(ristorante2_pre_covid$data, "week"))
scontrini_pre_covid_2_sett_avg <- aggregate(scontrini ~ week_rist2_pre_covid, data = ristorante2_pre_covid, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
scontrini_pre_covid_2_sett_avg <- scontrini_pre_covid_2_sett_avg[-c(1:35),]
scontrini_pre_covid_2_sett_avg <- scontrini_pre_covid_2_sett_avg$scontrini
scontrini_pre_covid_2_sett_avg <- ts(scontrini_pre_covid_2_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(scontrini_pre_covid_2_sett_avg) +
    ggtitle("Ristorante 2: scontrini medi settimanali pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili pre-COVID
# Uso direttamente il dataset completo, considerando anche i dati gi? aggregati
# mensilmente
month_rist2_pre_covid <- as.Date(cut(ristorante2_pre_covid$data, "month"))

scontrini_pre_covid_2_mens_avg <- aggregate(scontrini ~ month_rist2_pre_covid, data = ristorante2_pre_covid, mean)
scontrini_pre_covid_2_mens_avg <- scontrini_pre_covid_2_mens_avg$scontrini
scontrini_pre_covid_2_mens_avg <- ts(scontrini_pre_covid_2_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini_pre_covid_2_mens_avg) +
    ggtitle("Ristorante 2: scontrini medi mensili pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)


### Stagionalit? considerando tutti gli anni

print(
  ggseasonplot(vendite2_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 2: vendite settimanali")
)

# Nel grafico precedente c'? un problema sull'anno 2018, che dovrebbe partire dalla
# settimana 36 ma per qualche motivo "interpola" a partire dalla settimana 1. 
# Non ho trovato come risolvere questa cosa

print(
  ggseasonplot(vendite2_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 2: vendite mensili")
)

### Seasonal sub series plot
print(
  ggsubseriesplot(vendite2_mens_avg) +
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 2: vendite medie mensili"))


### Stagionalit? considerando il periodo pre-COVID

print(
  ggseasonplot(pre_covid_2_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 2: vendite settimanali pre-COVID")
)

print(
  ggseasonplot(pre_covid_2_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 2: vendite mensili pre-COVID")
)

### Seasonal sub series plot

print(
  ggsubseriesplot(pre_covid_2_mens_avg) +
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 2: vendite medie mensili pre-COVID")
)


### Analisi correlazione tra vendite e scontrini

scon_vend_sett_avg_2 <- ts.intersect(vendite2_sett_avg, scontrini2_sett_avg)

print(
  autoplot(scon_vend_sett_avg_2, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 2")
)

print(
  qplot(lordototale, scontrini, data=as.data.frame(copy_ristorante2)) +
    ylab("Scontrini") + xlab("Vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 2")
)

# Ho usato la copia senza dati aggregati mensilmente


### Analisi autocorrelazione considerando tutti gli anni
# Per una serie con trend l'autocorrelazione ? alta a lag vicini e si abbassa
# piano piano. Se c'? stagionalit?, invece, l'autocorrelazione presenta delle
# regolarit? nel suo andamento

print(
  ggAcf(vendite2_day, lag=28) +
    ggtitle("Ristorante 2: Autocorrelation vendite giornaliere")
)

print(
  ggAcf(vendite2_sett_avg, lag=104) +
    ggtitle("Ristorante 2: Autocorrelation vendite medie settimanali")
)

print(
  ggAcf(vendite2_mens_avg, lag=36) +
    ggtitle("Ristorante 2: Autocorrelation vendite medie mensili")
)

### Analisi autocorrelazione pre-COVID

print(
  ggAcf(pre_covid_2_day, lag=28) +
    ggtitle("Ristorante 2: Autocorrelation vendite giornaliere pre-COVID")
)

print(
  ggAcf(pre_covid_2_sett_avg, lag=104) +
    ggtitle("Ristorante 2: Autocorrelation vendite medie settimanali pre-COVID")
)

print(
  ggAcf(pre_covid_2_mens_avg, lag=24) +
    ggtitle("Ristorante 2: Autocorrelation vendite medie mensili pre-COVID")
)


### Decomposizione serie storica
# Decomposizione giornaliera 
multi_vendite2 <- msts(copy_ristorante2$lordototale, ts.frequency = 365, start = decimal_date(as.Date("2018-09-03")), seasonal.periods = c(7,365))
multi_vendite2_dec <- mstl(multi_vendite2, s.window = "periodic")
print(autoplot(multi_vendite2_dec) + ggtitle("Ristorante 2: Decomposizione giornaliera"))

# Decomposizione settimanale
vendite2_sett.fit <- stl(vendite2_sett_avg, s.window="periodic")
trend.vendite2_sett <- vendite2_sett.fit$time.series[,2]
stag.vendite2_sett <- vendite2_sett.fit$time.series[,1]
res.vendite2_sett <- vendite2_sett.fit$time.series[,3]
print(autoplot(vendite2_sett.fit) + ggtitle("Ristorante 2: Decomposizione settimanale"))

# Decomposizione mensile 
vendite2_mens.fit <- stl(vendite2_mens_avg,s.window="periodic")
trend.vendite2_mens <- vendite2_mens.fit$time.series[,2]
stag.vendite2_mens <- vendite2_mens.fit$time.series[,1]
res.vendite2_mens <- vendite2_mens.fit$time.series[,3]
print(autoplot(vendite2_mens.fit) + ggtitle("Ristorante 2: Decomposizione mensile"))

# Alternativa
# components.ts_2 = decompose(vendite2_mens_avg)
# plot(components.ts_2)

### Decomposizione serie storica pre-COVID
# Non so quanto senso possa avere farla, dal momento che i nostri dati coprono
# poco pi? di un anno prima del COVID


# Confronto estati 2019/2020/2021 (pre-durante-post COVID)

r2_estate_2019 <- subset(copy_ristorante2, Year==2019 & Season == 'Summer')
r2_estate_2020 <- subset(copy_ristorante2, Year==2020 & Season == 'Summer')
r2_estate_2021 <- subset(copy_ristorante2, Year==2021 & Season == 'Summer')

r2_totale_estati <- rbind(r2_estate_2019, r2_estate_2020, r2_estate_2021)

# Creo un attributo per creare le label del grafico
r2_totale_estati$Year <- format(r2_totale_estati$data, "%Y")
r2_totale_estati$Month <- format(r2_totale_estati$data, "%b")
r2_totale_estati$Giorno <- format(r2_totale_estati$data, "%d")
r2_totale_estati$MonthDay <- format(r2_totale_estati$data, "%d-%b")

# Per le label ne tengo una ogni 3 giorni
r2_totale_estati$MonthDay2 <- r2_totale_estati$MonthDay
r2_totale_estati$MonthDay2[as.numeric(row.names(r2_totale_estati))%%3!=0] <- ""
labels <- r2_totale_estati$MonthDay2

# Calcolo la media per anno
mean <- r2_totale_estati %>% group_by(Year) %>% summarise(mean_val=mean(lordototale))

p <- ggplot(data=r2_totale_estati, mapping=aes(x=MonthDay, y=lordototale, shape=Year, color=Year)) + geom_point() +
  geom_line(aes(group = 1)) + geom_hline(data = mean, aes(yintercept = mean_val, col=Year), linetype = 'dashed')
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 2: confronto estati")
)


# Analisi andamento scontrino medio

df_scontrino_medio <- copy_ristorante2[, c("data", "Prezzo_medio_per_scontrino")]

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

p <- ggplot(df_scontrino_medio_no_out, aes(x = data, y = Prezzo_medio_per_scontrino,
                                           col = Periodo)) + geom_line() + 
  geom_hline(data = mean_scontrino, aes(yintercept = mean_val, col=Periodo), linetype = 'dashed')
# + stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") aggiunge una sorta di trend
print(
  p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 2: confronto scontrino medio pre/post COVID")
)