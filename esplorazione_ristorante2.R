# SETTING PROGETTO -------------------------------------------------------------

set.seed(100)

# Setting librerie utili
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "magrittr", "ggplot2",
              "forcats", "lubridate", "RQuantLib", "devtools", "patchwork", "KFAS",
              "caret", "tseries", "urca", "TSstudio", "gridExtra", "randomForest",
              "prophet", "xts", "corrplot", "rstan") 

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

### Vendite giornaliere/settimanali/mensili periodo pre-COVID

# Vendite giornaliere pre-COVID
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

# Decido di eliminare gli outlier, per una stima pi√π consistente della media

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



### RANDOM FOREST

### PERIODO COVID

randomforest_r2_precovid <- copy_ristorante2_pre_covid[, c("data", "scontrini", "lordototale",
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

randomforest_r2_precovid[is.na(randomforest_r2_precovid)] <- 0

# divisione in train e test
index_rf <- sample(1:nrow(randomforest_r2_precovid),
                   size = 0.7*nrow(randomforest_r2_precovid))
train_rf <- randomforest_r2_precovid[index_rf,]
test_rf <- randomforest_r2_precovid[-index_rf,] 

# implementazione modelli
MRF <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                      Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                      Dose1Cum + Dose2Cum + DoseUnicaCum + Booster1Cum + Booster3DosiCum +
                      Booster2Cum + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                      GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                      Durum_Wheat + Feed_Barley + Maize + Milling_Wheat, data = train_rf)
varImpPlot(MRF)
print(MRF)
# % Var explained: 79.34

### DALL' IMPORTANCE PLOT DELLE VARIABILI POSSIAMO VEDERE CHE TUTTE LE VARIABILI LEGATI AI VACCINI
### NON SONO RILEVANTI, COMINCIO AD ELIMINARE QUELLE

MRF_V2 <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                         Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                         BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                         GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                         Durum_Wheat + Feed_Barley + Maize + Milling_Wheat, data = train_rf)
varImpPlot(MRF_V2)
print(MRF_V2)
# % Var explained: 79.02

### CAMBIA DI POCO, QUINDI A QUESTO PUNTO MI LIMITO ALLE 7 (numero casuale) VARIABILI PIU' IMPORTANTI

MRF_V3 <- randomForest(lordototale ~ Giorno + Month + Weekend + Festivo + GASOLIO_RISCALDAMENTO.LITRO 
                       + GPL.LITRO + Durum_Wheat, data = train_rf)
varImpPlot(MRF_V3)
print(MRF_V3)
# % Var explained: 78.9

# si valutano le performance del modello sul train e test set
predictions_rf <- predict(MRF_V3, newdata = train_rf)
mape(train_rf$lordototale, predictions_rf)
# MAPE 5.72 (DATI DI TRAIN)

predictions_rf <- predict(MRF_V3, newdata = test_rf)
mape(test_rf$lordototale, predictions_rf)
# MAPE 8.53 (DATI DI TEST)

accuracy(predictions_rf, test_rf$lordototale)
# RMSE 3592.747
# MAPE 8.53

# Creato il modello, vado a fare le previsioni su valori nuovi, ossia sul periodo COVID

r2_rf_covid <- copy_ristorante2[, c('lordototale', 'data', 'Giorno', 'Month', 'Weekend',
                                    'Festivo', 'GASOLIO_RISCALDAMENTO.LITRO', 'GPL.LITRO', 'Durum_Wheat')]

# selezione periodo covid (su cui verranno fatte le previsioni)
reference_date_rf <- as.Date("2020-01-06", format = "%Y-%m-%d")
r2_rf_covid <- r2_rf_covid %>%
  filter(r2_rf_covid$data > reference_date_rf)

# Si seleziona la lunghezza del periodo da prevedere
# Prendo in considerazione tutto il 2020
r2_rf_covid <- r2_rf_covid[1:360,]
# C'Ë una settimana di luglio 2020 dove manca il valore della variabile "Durum Wheat",
# probabilmente perchË in quel periodo c'Ë la chiusura dell'anno fiscale (nello stesso
# periodo mancano dati anche negli altri anni). Ho deciso di considerare per quella settimana
# il prezzo delle settimane precedenti, che sembra stabile
r2_rf_covid$Durum_Wheat[is.na(r2_rf_covid$Durum_Wheat)] <- 276.5

# si utilizza il modello appena creato per fare previsioni
previsione_covid_rf <- predict(MRF_V3, r2_rf_covid)
previsione_covid_rf <- as.data.frame(previsione_covid_rf)

# Unisco le due serie storiche

# Serie storica previsioni periodo covid 
interval_covid <- seq(as.Date("2020-01-07"), as.Date("2020-12-31"), by = "day")
interval_covid_df <- data.frame(date = interval_covid, 
                                val=previsione_covid_rf)
interval_covid_df$date <- as.Date(interval_covid_df$date)  

interval_covid_ts <- xts(interval_covid_df$val, interval_covid_df$date)

plot(interval_covid_df$date, interval_covid_df$previsione_covid_rf, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 2")

# Serie storica dati reali fino al pre covid

interval_pre_covid <- seq(as.Date("2018-09-01"), as.Date("2020-01-06"), by = "day")
interval_pre_covid_df <- data.frame(date = interval_pre_covid, 
                                    val=randomforest_r2_precovid$lordototale)

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
     type="l", main = "Ristorante 2 previsioni")

# Serie storica originale
rownames(copy_ristorante2) <- NULL # Ho bisogno di resettare l'indice delle righe
ristorante2_complete <- copy_ristorante2[1:853,]  # fino al 31 maggio 2020
plot(ristorante2_complete$data, ristorante2_complete$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 2 dati reali")

# Sovrapposizione serie storiche
par(mfrow=c(1,1))

rf_complete <- cbind(interval_complete, ristorante2_complete$lordototale)
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
print(autoplot(perdite_stimate_dec) + ggtitle("Ristorante 2: perdite stimate"))



### Prophet ----
# Le vendite giornaliere pre-COVID (ristorante2_pre_covid$lordototale) vengono divise 
# in train e test  per cercare di modellare i dati a disposizione e cercare di 
# valutarne la qualit? del modello ottenuto.
# Il seguente modello viene utilizzato per fare previsioni su valori futuri, in 
# particolar modo per prevedere come le vendite sarebbero andate durante il periodo
# covid, durante il quale per alcune settimane le vendite effettive invece sono 
# state pari a zero

prophet_vendite <- copy_ristorante2_pre_covid %>% 
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
# Le vendite giornaliere pre-COVID (ristorante2_pre_covid$lordototale) vengono divise 
# in train e test  per cercare di modellare i dati a disposizione e cercare di 
# valutarne la qualit? del modello ottenuto.
# Il seguente modello viene utilizzato per fare previsioni su valori futuri, in 
# particolar modo per prevedere come le vendite sarebbero andate durante il periodo
# covid, durante il quale per alcune settimane le vendite effettive invece sono 
# state pari a zero

vendite2_day_pre_split_tbats <- ts_split(pre_covid_2_day)
train_tbats <- vendite2_day_pre_split_tbats$train
test_tbats <- vendite2_day_pre_split_tbats$test

tbats_data <- msts(train_tbats, seasonal.periods=c(7,365.25))
MTABTS <- tbats(tbats_data)

#  considerando test set
vendite_forecast_tbats <- forecast(MTABTS, h=length(test_tbats))
autoplot(vendite_forecast_tbats, pre_covid_2_day.colour = 'black')

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
autoplot(ts(vendite2_day[1:1233], start=2017,frequency=365))





# PREVISIONE ANDAMENTO VENDITE POST APRILE 2022 --------------------------------
# Le previsioni sono state fatte per il periodo che va dal 1 maggio 2022 al 
# 30 settembre 2022. Si pu? tranquillamente estendere
# In questo caso non vi ? una suddivisione in train e test dei dati a disposizione


### HoltWinters ----
# Il modello viene addestrato su tutti i dati (settimanali) a disposizione (vendite2_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022. 

# Metodo lisciamento esponenziale
MHW <- HoltWinters(vendite2_sett_avg)  # il modello permette di catturare trend e stagionalit?
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
# sqrt(mean((vendite2_sett_avg[53:223]- MHW$fitted[,"xhat"])^2)) non so bene come gestire il calcolo nel nostro caso



### SARIMAX ----
# Il modello viene addestrato su tutti i dati (settimanali) a disposizione (vendite2_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022.

# setting regressori

# dati "COVID" su base settimanale (somma, si contano i giorni della settimana in cui c'? il covid)
# aggiungo la colonna COVID SI (1)/NO (0)
copy_ristorante2$COVID <- 0
copy_ristorante2[copy_ristorante2$data > "2020-03-09",]$COVID <- 1
copy_ristorante2$COVID  <- as.factor(copy_ristorante2$COVID)

copy_ristorante2$COVID <- as.numeric(as.character(copy_ristorante2$COVID))
week_rist2_sarimax <- week_rist2[-c(1: 243)] # prendo solo le settimane contenute in copy_ristorante2
week_covid_sum <- aggregate(COVID ~ week_rist2_sarimax, copy_ristorante2, sum)  
week_covid_sum <- week_covid_sum$COVID
copy_ristorante2$COVID <- as.factor(copy_ristorante2$COVID)

# dati "chiuso" su base settimanale (somma, si contano i giorni della settimana in cui il ristorante ? chiuso, quindi senza vendite)
# aggiungo la colonna CHIUSO SI (1)/NO (0)
copy_ristorante2$CHIUSO <- 0
copy_ristorante2[copy_ristorante2$lordototale == 0,]$CHIUSO <- 1
copy_ristorante2$CHIUSO  <- as.factor(copy_ristorante2$CHIUSO)

copy_ristorante2$CHIUSO <- as.numeric(as.character(copy_ristorante2$CHIUSO))
week_chiuso_sum <- aggregate(CHIUSO ~ week_rist2_sarimax, copy_ristorante2, sum)  
week_chiuso_sum <- week_chiuso_sum$CHIUSO
copy_ristorante2$CHIUSO <- as.factor(copy_ristorante2$CHIUSO)

# dati "zona rossa" su base settimanale (somma, si contano i giorni della settimana in cui c'? zona rossa)
# aggiungo la colonna ROSSA SI (1)/NO (0)
copy_ristorante2$ROSSA <- 0
copy_ristorante2[copy_ristorante2$ColoreCOVID == "rosso",]$ROSSA <- 1
copy_ristorante2$ROSSA  <- as.factor(copy_ristorante2$ROSSA)

copy_ristorante2$ROSSA <- as.numeric(as.character(copy_ristorante2$ROSSA))
week_rossa_sum <- aggregate(ROSSA ~ week_rist2_sarimax, copy_ristorante2, sum)  
week_rossa_sum <- week_rossa_sum$ROSSA
copy_ristorante2$ROSSA <- as.factor(copy_ristorante2$ROSSA)

regressori_week <- data.frame(week_covid_sum, week_chiuso_sum, week_rossa_sum)

# trasformazione colonne precedenti in valori binari
regressori_week <- regressori_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))  # se almeno un giorno durante la settimana ha registrato il covid allora tale settimana viene etichettata come settimana covid
#regressori_week$week_covid_bin <- as.factor(regressori_week$week_covid_bin)

regressori_week <- regressori_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>3, 1, 0))  # se un ristorante durante la settimana rimane chiuso per pi? di 3 giorni allora tale settimana viene etichettata come settimana chiusa
#regressori_week$week_chiuso_bin <- as.factor(regressori_week$week_chiuso_bin)

regressori_week <- regressori_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>3, 1, 0))  # se un ristorante durante la settimana ? in zona rossa per pi? di 3 giorni allora tale settimana viene etichettata come settimana rossa
#regressori_week$week_rossa_bin <- as.factor(regressori_week$week_rossa_bin)

# verifica collinearit? variabili
numeric.var <- sapply(regressori_week, is.numeric)
corr.matrix <- cor(regressori_week[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# regressori: "week_covid_bin", "week_rossa_bin"

MSARIMAX <- auto.arima(vendite2_sett_avg, seasonal = TRUE, 
                       xreg = data.matrix(regressori_week[c(-1), c("week_covid_bin", "week_rossa_bin")])) 
# tolto la prima settimana di regressori_week per uniformare dimensionalit?
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
autoplot(MSARIMAX$fitted) + autolayer(vendite2_sett_avg)

# Si procede ora utilizzando il modello ottenuto per fare previsioni su dati nuovi,
# in particolare si cerca di prevedere le vendite dopo aprile 2022, date per cui 
# non si hanno a disposizone informazioni relative a vendite. In particolare si cerca 
# di prevedere per il periodo che va dal 1 maggio 2022 al 30 settembre 2022, date per cui si 
# possono ricavare i valori dei regressori ma non si possono avere i valori di 
# vendite, valori che dunque vengono previsti utilizzando il modello precedente
# e i regressori ottenuti per le nuove date

# NB. Il 30 settembre ? un venerd?, quindi non viene presa in considerazione la 
# settimana intera ma ci? non influenza il valore dei regressori (la settimana 
# ? comunque etichettata covid, non ci sono possibili chiusure e non c'? zona rossa)

# Per procedere bisogna prima avere i valori dei regressori per le date per cui
# verranno eseguite le previsioni

# creazione df per previsione (dal 1 maggio 2022 al 30 settembre 2022) 
date_previsione <- seq(as.Date("2022-05-01"), as.Date("2022-09-30"), by="days")
regressori_forecast_day <- data.frame(date_previsione)
regressori_forecast_day$COVID <- 1
regressori_forecast_day$CHIUSO <- 0 # non ci sono date in cui sicuramente i ristoranti sono stati chiusi
regressori_forecast_day$ROSSA <- 0 # non ci sono pi? state date in zona rossa

# divisione in settimane
week_new_rist2 <- as.Date(cut(regressori_forecast_day$date_previsione, "week"))

week_rossa_new <- aggregate(ROSSA ~ week_new_rist2, regressori_forecast_day, sum)  # per settimana
week_chiuso_new <- aggregate(CHIUSO ~ week_new_rist2, regressori_forecast_day, sum)  # per settimana
week_covid_new <- aggregate(COVID ~ week_new_rist2, regressori_forecast_day, sum)  # per settimana

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



### RANDOM FOREST FUTURO

copy_ristorante2[is.na(copy_ristorante2)] <- 0

# Implementazione modello
MRF_future <- randomForest(lordototale ~ Giorno + Month + Year + Season + Weekend +
                             Festivo + Precipitazioni.mm. + Pioggia + ColoreCOVID +
                             Dose1Cum + Dose2Cum + DoseUnicaCum + Booster1Cum + Booster3DosiCum +
                             Booster2Cum + BENZINA.LITRO + GASOLIO_AUTO.LITRO + GPL.LITRO +
                             GASOLIO_RISCALDAMENTO.LITRO + O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO +
                             Durum_Wheat + Feed_Barley + Maize + Milling_Wheat, data = copy_ristorante2)
varImpPlot(MRF_future)
print(MRF_future)
# % Var explained: 87.88

### TENGO SOLO LE PRIME 11 VARIABILI, QUELLE CON L'IMPORTANZA MAGGIORE

MRF_future_V2 <- randomForest(lordototale ~ Giorno + Month + Year + Weekend + BENZINA.LITRO + 
                                GASOLIO_AUTO.LITRO + GPL.LITRO + GASOLIO_RISCALDAMENTO.LITRO + 
                                O.C._FLUIDO_BTZ.LITRO + O.C._DENSO_BTZ.LITRO + Durum_Wheat, 
                              data = copy_ristorante2)
varImpPlot(MRF_future_V2)
print(MRF_future_V2)
# % Var explained: 85.04

# Decido di non eliminare ulteriori variabili, poichË l'importanza delle stesse Ë piuttosto alta

# Setto il periodo su cui fare previsioni, considerando i regressori selezionati
# Avendo mantenuto anche variabili legate a carburanti e cereali posso spingermi 
# solo fino a una data per cui sono noti i relativi valori
future_interval = seq(as.Date("2022-05-01"), as.Date("2022-08-31"), by="days")
ristorante2_future <- data.frame(future_interval)
colnames(ristorante2_future) <- "data"

# colonne Mese, Anno
ristorante2_future$Month <- month(ristorante2_future$data)
ristorante2_future$Month <- factor(ristorante2_future$Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
ristorante2_future$Year <- year(ristorante2_future$data)
ristorante2_future$Year <- factor(ristorante2_future$Year, levels = c("2018", "2019", "2020", "2021", "2022"))

# colonna Giorno, Weekend
Sys.setlocale("LC_ALL","English") # Per avere i nomi in inglese
ristorante2_future <- ristorante2_future %>%
  mutate(weekday = wday(data, label = TRUE, abbr = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        locale = Sys.getlocale("LC_TIME"))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c("Saturday", "Sunday")) ~ "weekend"
    , TRUE ~ "weekday"
  )
  )
ristorante2_future$weekday <- as.factor(ristorante2_future$weekday)
ristorante2_future["tipo_giorno"][ristorante2_future["tipo_giorno"] == "weekend"] <- "True"
ristorante2_future["tipo_giorno"][ristorante2_future["tipo_giorno"] == "weekday"] <- "False"
colnames(ristorante2_future)[which(names(ristorante2_future) == "weekday")] <- "Giorno"
colnames(ristorante2_future)[which(names(ristorante2_future) == "tipo_giorno")] <- "Weekend"
ristorante2_future$Weekend <- as.factor(ristorante2_future$Weekend)
ristorante2_future$Giorno <- factor(ristorante2_future$Giorno, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                                          "Saturday", "Sunday"))

# colonne carburanti
carburanti <- read.csv("Dati/Dati per RANDOM FOREST/carburanti_agosto_22.csv")
# tengo soltanto le colonne necessarie
carburanti <- carburanti[, c("DATA_RILEVAZIONE", "BENZINA.LITRO", "GASOLIO_AUTO.LITRO", "GPL.LITRO", 
                             "GASOLIO_RISCALDAMENTO.LITRO", "O.C._FLUIDO_BTZ.LITRO",
                             "O.C._DENSO_BTZ.LITRO")]
carburanti$DATA_RILEVAZIONE <- parse_date(carburanti$DATA_RILEVAZIONE, "%Y-%m-%d", locale = locale("it"))

ristorante2_future <- merge(ristorante2_future, carburanti, by.x = "data", by.y = "DATA_RILEVAZIONE",
                            all.x = TRUE)

# colonne cereali
cereali <- read.csv("Dati/Dati per RANDOM FOREST/cereali_rf.csv")
# tengo soltanto le colonne necessarie
cereali <- cereali[, c("Reference.period", "Durum_Wheat")]
# come fatto sopra, quando ho dei na per Durum_Wheat metto il dato della settimana precedente
cereali$Durum_Wheat[is.na(cereali$Durum_Wheat)] <- 542.5
cereali$Reference.period <- parse_date(cereali$Reference.period, "%Y-%m-%d", locale = locale("it"))

ristorante2_future <- merge(ristorante2_future, cereali, by.x = "data", by.y = "Reference.period",
                            all.x = TRUE)

# colonna lordototale
ristorante2_future$lordototale <- 0

# riordino le colonne
ristorante2_future <- data.frame(ristorante2_future$data,
                                 ristorante2_future$lordototale,
                                 ristorante2_future$Giorno,
                                 ristorante2_future$Month,
                                 ristorante2_future$Year,
                                 ristorante2_future$Weekend,
                                 ristorante2_future$BENZINA.LITRO,
                                 ristorante2_future$GASOLIO_AUTO.LITRO,
                                 ristorante2_future$GPL.LITRO,
                                 ristorante2_future$GASOLIO_RISCALDAMENTO.LITRO,
                                 ristorante2_future$O.C._FLUIDO_BTZ.LITRO,
                                 ristorante2_future$O.C._DENSO_BTZ.LITRO,
                                 ristorante2_future$Durum_Wheat
)

names(ristorante2_future)<- c("data", 
                              "lordototale",
                              "Giorno",
                              "Month",
                              "Year",
                              "Weekend",
                              "BENZINA.LITRO", 
                              "GASOLIO_AUTO.LITRO", 
                              "GPL.LITRO", 
                              "GASOLIO_RISCALDAMENTO.LITRO", 
                              "O.C._FLUIDO_BTZ.LITRO",
                              "O.C._DENSO_BTZ.LITRO",
                              "Durum_Wheat"
)

ristorante1_RF_full <-rbind(copy_ristorante2[,c("data", "lordototale", "Giorno",
                                                "Month", "Year", "Weekend", "BENZINA.LITRO", 
                                                "GASOLIO_AUTO.LITRO", "GPL.LITRO", 
                                                "GASOLIO_RISCALDAMENTO.LITRO", 
                                                "O.C._FLUIDO_BTZ.LITRO",
                                                "O.C._DENSO_BTZ.LITRO",
                                                "Durum_Wheat")], ristorante2_future)

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
     ylab = "vendite", type="l", main = "Ristorante 2")

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
     ylab = "vendite", type="l", main = "Ristorante 2 previsioni")

# verifica performance modello
RMSE.rf <- sqrt(mean((MRF_future_V2$predicted - copy_ristorante2$lordototale)^2))




### Prophet con regressori ----
# Il modello viene addestrato su tutti i dati (settimanali) a disposizione (vendite2_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022.

covid <- function(ds) {
  dates <- as.Date(ds)
  reference_date_pre <- as.Date("2020-03-09", format = "%Y-%m-%d")
  as.numeric(dates > reference_date_pre)
}

# periodo pre maggio 2022
ristorante2_pre_maggio_prophet <- data.frame(copy_ristorante2$data, 
                                             copy_ristorante2$ROSSA)
names(ristorante2_pre_maggio_prophet) <- c("data", "rossa")

# periodo post maggio 2022
ristorante2_post_maggio_prophet <- data.frame(future_interval)
ristorante2_post_maggio_prophet$rossa <- 0
names(ristorante2_post_maggio_prophet) <- c("data", "rossa")


# unione periodo pre aprile e periodo post aprile
vendite_prophet_new <- rbind(ristorante2_pre_maggio_prophet, 
                             ristorante2_post_maggio_prophet)

# si considerano le vendite fino ad aprile 2022
prophet_vendite_totali <- copy_ristorante2 %>% 
  select(data, lordototale)
colnames(prophet_vendite_totali) <- c("ds", "y")
prophet_vendite_totali$covid <- covid(prophet_vendite_totali$ds)
prophet_vendite_totali$rossa <- ristorante2_pre_maggio_prophet$rossa

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
# Il modello viene addestrato su tutti i dati (giornalieri) a disposizione (vendite2_day) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2022.

tbats_data_new <- msts(vendite2_day, seasonal.periods=c(7,365.25))
MTBATS_NEW <- tbats(tbats_data_new)

# previsioni post aprile 2022
vendite_forecast_tbats_new <- forecast(MTBATS_NEW, h=122)
autoplot(vendite_forecast_tbats_new, tbats_data = 'black') # non riesco a capire perch? questo andamento

# RMSE
sqrt(mean((vendite2_day- MTBATS_NEW$fitted.values)^2))  # 1144.543