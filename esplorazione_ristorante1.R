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

# Creo i diversi boxplot (sia vendite che scontrini)

### Giorno della settimana
ggplot(copy_ristorante1, aes(Giorno, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Giorno, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Mese dell'anno
ggplot(copy_ristorante1, aes(Month, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Month, scontrini)) + geom_boxplot() +
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
ggplot(copy_ristorante1, aes(Season, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Season, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Weekend/settimana (il venerdì è considerato giorno della settimana)
ggplot(copy_ristorante1, aes(Weekend, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Weekend, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Giorno feriale vs. festivo
ggplot(copy_ristorante1, aes(Festivo, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Festivo, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Pioggia si/no
ggplot(copy_ristorante1, aes(Pioggia, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante1, aes(Pioggia, scontrini)) + geom_boxplot() +
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

# Vendite settimanali medie
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
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
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
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

### Vendite giornaliere/settimanali/mensili periodo pre-COVID
# Prendo come data di riferimeno quella in cui le autorità cinesi hanno identificato
# il virus
data_covid <- as.Date("2020-01-07", format = "%Y-%m-%d")
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
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
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
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
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
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
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
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
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

# Vendite giornaliere pre-COVID
scontrini_pre_covid_1_day <- ts(copy_ristorante1_pre_covid$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini_pre_covid_1_day) +
    ggtitle("Ristorante 1: scontrini giornalieri pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali pre-COVID
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
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
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
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


### Stagionalità considerando tutti gli anni

print(
  ggseasonplot(vendite1_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 1: vendite settimanali")
)

# Nel grafico precedente c'è un problema sull'anno 2018, che dovrebbe partire dalla
# settimana 36 ma per qualche motivo "interpola" a partire dalla settimana 1. 
# Non ho trovato come risolvere questa cosa

print(
  ggseasonplot(vendite1_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 1: vendite mensili")
)

### Seasonal sub series plot
print(
  ggsubseriesplot(vendite1_mens_avg) +
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 1: vendite medie mensili"))


### Stagionalità considerando il periodo pre-COVID

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
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 1: vendite medie mensili pre-COVID")
)


### Analisi correlazione tra vendite e scontrini

scon_vend_sett_avg_1 <- ts.intersect(vendite1_sett_avg, scontrini1_sett_avg)

print(
  autoplot(scon_vend_sett_avg_1, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 1")
)

print(
  qplot(lordototale, scontrini, data=as.data.frame(copy_ristorante1)) +
    ylab("Scontrini") + xlab("Vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 1")
)

# Ho usato la copia senza dati aggregati mensilmente


### Analisi autocorrelazione considerando tutti gli anni
# Per una serie con trend l'autocorrelazione è alta a lag vicini e si abbassa
# piano piano. Se c'è stagionalità, invece, l'autocorrelazione presenta delle
# regolarità nel suo andamento

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

### Decomposizione serie storica pre-COVID
# Non so quanto senso possa avere farla, dal momento che i nostri dati coprono
# poco più di un anno prima del COVID


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





### Random forest ----
# Le vendite giornaliere pre-COVID (ristorante1_pre_covid$lordototale) vengono divise 
# in train e test per cercare di modellare i dati a disposizione e cercare di 
# valutarne la qualità del modello ottenuto.
# Il seguente modello viene utilizzato per fare previsioni su valori futuri, in 
# particolar modo per prevedere come le vendite sarebbero andate durante il periodo
# COVID, durante il quale per alcune settimane le vendite effettive invece sono 
# state pari a zero

# Uso solo alcune variabili:

r1_pre_covid_rf <- copy_ristorante1_pre_covid[, c('lordototale', 'data', 'Giorno', 
                                                  'Month', 'Season', 'Weekend',
                                                  'Festivo', 'Pioggia')]

# divisione in train e test
index_rf <- sample(1:nrow(r1_pre_covid_rf),
                   size = 0.7*nrow(r1_pre_covid_rf))
train_rf <- r1_pre_covid_rf[index_rf,]
test_rf <- r1_pre_covid_rf[-index_rf,] 

# implementazione modelli
MRF <- randomForest(lordototale ~ Giorno + Month + Season + Weekend + Festivo +
                      Pioggia, data = train_rf)
varImpPlot(MRF)
print(MRF)
# % Var explained: 83.65

### SELEZIONARE SOLO VARIABILI PIU' RILEVANTI?

# si valutano le performance del modello sul train e test set
predictions_rf <- predict(MRF, newdata = train_rf)
mape(train_rf$lordototale, predictions_rf)
# MAPE 13.63

predictions_rf <- predict(MRF, newdata = test_rf)
mape(test_rf$lordototale, predictions_rf)
# MAPE 16.63

accuracy(predictions_rf, test_rf$lordototale)
# RMSE 4037.16
# MAPE 16.63

# Previsioni su valori nuovi (sul periodo covid dove nei dati reali si hanno 0)

copy_ristorante1_rf <- copy_ristorante1[, c('lordototale', 'data', 'Giorno', 'Month', 
                                        'Season', 'Weekend','Festivo', 'Pioggia')]

# selezione periodo covid (su cui verranno fatte le previsioni)
reference_date_rf <- as.Date("2020-01-06", format = "%Y-%m-%d")
ristorante1_periodo_covid <- copy_ristorante1_rf %>%
  filter(copy_ristorante1_rf$data >= reference_date_rf)

# si seleziona la lunghezza del periodo da prevedere
# viene selezionato un periodo che include i giorni in cui si registrano 0 
# vendite/scontrini
ristorante1_periodo_covid <- ristorante1_periodo_covid[1:133,]

# si utilizza il modello appena creato per fare previsioni
vendite_forecast_rf <- predict(MRF, ristorante1_periodo_covid)
vendite_forecast_rf <- as.data.frame(vendite_forecast_rf)

# si uniscono le due serie storiche

# serie storica previsioni durante periodo covid 
interval_covid <- seq(as.Date("2020-01-06"), as.Date("2020-05-17"), by = "day")
interval_covid_df <- data.frame(date = interval_covid, 
                                val=vendite_forecast_rf)
interval_covid_df$date <- as.Date(interval_covid_df$date)  
interval_covid_ts <- xts(interval_covid_df$val, interval_covid_df$date)

plot(interval_covid_df$date, interval_covid_df$vendite_forecast_rf, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino a prima covid (r1_pre_covid_rf$lordototale)
r1_pre_covid_rf <- head(r1_pre_covid_rf, -1) # Andava tolta l'ultima riga

interval_pre_covid <- seq(as.Date("2018-09-01"), as.Date("2020-01-05"), by = "day")
interval_pre_covid_df <- data.frame(date = interval_pre_covid, 
                                    val=r1_pre_covid_rf$lordototale)

interval_pre_covid_df$date<-as.Date(interval_pre_covid_df$date)  
interval_covid_ts_pre <- xts(interval_pre_covid_df$val, interval_pre_covid_df$date)

# si uniscono le due serie storiche
names(interval_covid_df)[1] <- "data"
names(interval_covid_df)[2] <- "vendite"

names(interval_pre_covid_df)[1] <- "data"
names(interval_pre_covid_df)[2] <- "vendite"

interval_complete <- rbind(interval_covid_df, interval_pre_covid_df)
interval_complete <- interval_complete[order(interval_complete$data), ]
row.names(interval_complete) <- NULL

par(mfrow=c(2,1))

# serie storica con previsioni
plot(interval_complete$data, interval_complete$vendite, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 1 previsioni")

# serie storica originale
rownames(copy_ristorante1) <- NULL # Ho bisogno di resettare l'indice delle righe
ristorante1_complete <- copy_ristorante1[1:625,]  # fino al 17 maggio 2020
ristorante1_complete$lordototale[is.na(ristorante1_complete$vendite1)] <- 0
plot(ristorante1_complete$data, ristorante1_complete$lordototale, xlab = "data", ylab = "vendite", 
     type="l", main = "Ristorante 1 dati reali")

# sovrappongo i due grafici
par(mfrow=c(1,1))

rf_complete <- cbind(interval_complete, ristorante1_complete$lordototale)
plot(rf_complete$data, rf_complete$vendite, type="l", col="blue", xlab="data", ylab="vendite", lty=1)
lines(rf_complete$data, rf_complete$`ristorante1_complete$lordototale`, col="red",lty=2)



### Prophet ----
# Le vendite giornaliere pre-COVID (ristorante1_pre_covid$lordototale) vengono divise 
# in train e test  per cercare di modellare i dati a disposizione e cercare di 
# valutarne la qualità del modello ottenuto.
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
# valutarne la qualità del modello ottenuto.
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
  forecast(h=320) %>%  # fino a metà maggio circa
  autoplot() + autolayer(train_tbats)

# cofronto con valori reali
autoplot(ts(vendite1_day[1:1233], start=2017,frequency=365))