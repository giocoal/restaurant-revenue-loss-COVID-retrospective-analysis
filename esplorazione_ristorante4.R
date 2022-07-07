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
ristorante4 <- read.csv("ristorante4.csv")

# Presenza di NaN

## Ristorante 4
sum(is.na(ristorante4$scontrini)) # 341 NA
# which(is.na(ristorante4$scontrini))
subset(ristorante4[,c(2,6)], is.na(ristorante4$scontrini))
# Fino alla riga 243 la presenza di NaN è causata dal fatto che i dati sono
# aggregati mensilmente. Dopodichè c'è tutto il periodo COVID (da 802 a 884, 
# di diversi giorni più lungo rispetto agli altri ristoranti) e alcune festività

### Metto a 0 i Na, per comodità

ristorante4$lordototale[is.na(ristorante4$lordototale)] <- 0
ristorante4$scontrini[is.na(ristorante4$scontrini)] <- 0  
ristorante4$Prezzo_medio_per_scontrino[is.na(ristorante4$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data

ristorante4$data <- parse_date(ristorante4$data, "%Y-%m-%d", locale = locale("it"))

# Creo una copia togliendo i dati aggregati mensilmente dei primi 8 mesi del 2018

copy_ristorante4 <- ristorante4[-c(1:243),]

### Creo alcuni boxplot potenzialmente utili

# Rendo gli attributi Giorno, Month, Year, ... dei fattori. In questo modo riesco a
# manipolari i boxplot correttamente
copy_ristorante4$Giorno <- as.factor(copy_ristorante4$Giorno)
copy_ristorante4$Giorno <- factor(copy_ristorante4$Giorno, 
                                            levels=c('Monday','Tuesday','Wednesday',
                                                     'Thursday','Friday','Saturday',
                                                     'Sunday'))

copy_ristorante4$Month <- as.factor(copy_ristorante4$Month)

copy_ristorante4$Year <- as.factor(copy_ristorante4$Year)

copy_ristorante4$Season <- as.factor(copy_ristorante4$Season)
copy_ristorante4$Season <- factor(copy_ristorante4$Season, 
                                            levels=c('Spring','Summer','Autumn',
                                                     'Winter'))

copy_ristorante4$Weekend <- as.factor(copy_ristorante4$Weekend)

copy_ristorante4$Festivo <- as.factor(copy_ristorante4$Festivo)

copy_ristorante4$Pioggia <- as.factor(copy_ristorante4$Pioggia)

# Creo i diversi boxplot (sia vendite che scontrini)

### Giorno della settimana
ggplot(copy_ristorante4, aes(Giorno, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Giorno, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Mese dell'anno
ggplot(copy_ristorante4, aes(Month, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Month, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per mese dell'anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Anno
ggplot(copy_ristorante4, aes(Year, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Year, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per anno")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Stagione
ggplot(copy_ristorante4, aes(Season, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Season, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini per stagione")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Weekend/settimana (il venerdì è considerato giorno della settimana)
ggplot(copy_ristorante4, aes(Weekend, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Weekend, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini weekend vs. giorno della settimana")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Giorno feriale vs. festivo
ggplot(copy_ristorante4, aes(Festivo, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Festivo, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorno festivo vs. feriale")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

### Pioggia si/no
ggplot(copy_ristorante4, aes(Pioggia, lordototale)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot vendite giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

ggplot(copy_ristorante4, aes(Pioggia, scontrini)) + geom_boxplot() +
  theme_bw() +
  ggtitle("Box-plot scontrini giorni di pioggia")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# Vendite giornaliere 
vendite4_day <- ts(copy_ristorante4$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(vendite4_day) +
    ggtitle("Ristorante 4: vendite giornaliere") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite settimanali medie
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
week_rist4 <- as.Date(cut(ristorante4$data, "week"))
vendite4_sett_avg <- aggregate(lordototale ~ week_rist4, data = ristorante4, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
vendite4_sett_avg <- vendite4_sett_avg[-c(1:35),]
vendite4_sett_avg <- vendite4_sett_avg$lordototale
vendite4_sett_avg <- ts(vendite4_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(vendite4_sett_avg) +
    ggtitle("Ristorante 4: vendite medie settimanali") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili medie
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
# mensilmente
month_rist4 <- as.Date(cut(ristorante4$data, "month"))

vendite4_mens_avg <- aggregate(lordototale ~ month_rist4, data = ristorante4, mean)
vendite4_mens_avg <- vendite4_mens_avg$lordototale
vendite4_mens_avg <- ts(vendite4_mens_avg, start=2018, frequency=12)

print(
  autoplot(vendite4_mens_avg) +
    ggtitle("Ristorante 4: vendite medie mensili") +
    xlab("Anno") +
    ylab("Vendite")
)

### Vendite giornaliere/settimanali/mensili periodo pre-COVID
# Prendo come data di riferimeno quella in cui le autorità cinesi hanno identificato
# il virus
data_covid <- as.Date("2020-01-07", format = "%Y-%m-%d")
# Ristorante 4 pre-COVID
ristorante4_pre_covid <- ristorante4 %>% filter(ristorante4$data < data_covid)
copy_ristorante4_pre_covid <- copy_ristorante4 %>% filter(copy_ristorante4$data < data_covid)

# Vendite giornaliere pre-COVID
pre_covid_4_day <- ts(copy_ristorante4_pre_covid$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(pre_covid_4_day) +
    ggtitle("Ristorante 4: vendite giornaliere pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite settimanali pre-COVID
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
week_rist4_pre_covid <- as.Date(cut(ristorante4_pre_covid$data, "week"))
pre_covid_4_sett_avg <- aggregate(lordototale ~ week_rist4_pre_covid, data = ristorante4_pre_covid, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
pre_covid_4_sett_avg <- pre_covid_4_sett_avg[-c(1:35),]
pre_covid_4_sett_avg <- pre_covid_4_sett_avg$lordototale
pre_covid_4_sett_avg <- ts(pre_covid_4_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(pre_covid_4_sett_avg) +
    ggtitle("Ristorante 4: vendite medie settimanali pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili pre-COVID
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
# mensilmente
month_rist4_pre_covid <- as.Date(cut(ristorante4_pre_covid$data, "month"))

pre_covid_4_mens_avg <- aggregate(lordototale ~ month_rist4_pre_covid, data = ristorante4_pre_covid, mean)
pre_covid_4_mens_avg <- pre_covid_4_mens_avg$lordototale
pre_covid_4_mens_avg <- ts(pre_covid_4_mens_avg, start=2018, frequency=12)

print(
  autoplot(pre_covid_4_mens_avg) +
    ggtitle("Ristorante 4: vendite medie mensili pre-COVID") +
    xlab("Anno") +
    ylab("Vendite")
)


### Faccio la stessa analisi precedente sul numero di scontrini

# Scontrini giornalieri 
scontrini4_day <- ts(copy_ristorante4$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini4_day) +
    ggtitle("Ristorante 4: scontrini giornalieri") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali medi
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
week_rist4 <- as.Date(cut(ristorante4$data, "week"))
scontrini4_sett_avg <- aggregate(scontrini ~ week_rist4, data = ristorante4, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
scontrini4_sett_avg <- scontrini4_sett_avg[-c(1:35),]
scontrini4_sett_avg <- scontrini4_sett_avg$scontrini
scontrini4_sett_avg <- ts(scontrini4_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(scontrini4_sett_avg) +
    ggtitle("Ristorante 4: scontrini medi settimanali") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili medi
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
# mensilmente
month_rist4 <- as.Date(cut(ristorante4$data, "month"))

scontrini4_mens_avg <- aggregate(scontrini ~ month_rist4, data = ristorante4, mean)
scontrini4_mens_avg <- scontrini4_mens_avg$scontrini
scontrini4_mens_avg <- ts(scontrini4_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini4_mens_avg) +
    ggtitle("Ristorante 4: scontrini medi mensili") +
    xlab("Anno") +
    ylab("Scontrini")
)

### Vendite giornaliere/settimanali/mensili periodo pre-COVID

# Vendite giornaliere pre-COVID
scontrini_pre_covid_4_day <- ts(copy_ristorante4_pre_covid$scontrini, start = decimal_date(as.Date("2018-09-01")), frequency=365)

print(
  autoplot(scontrini_pre_covid_4_day) +
    ggtitle("Ristorante 4: scontrini giornalieri pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali pre-COVID
# Per comodità utilizzo il dataset completo perchè il 01-01-2018 è un lunedì.
# Poi toglierò i dati delle prime 35 settimane perchè sono aggregati mensilmente
week_rist4_pre_covid <- as.Date(cut(ristorante4_pre_covid$data, "week"))
scontrini_pre_covid_4_sett_avg <- aggregate(scontrini ~ week_rist4_pre_covid, data = ristorante4_pre_covid, mean)
# Tolgo le settimane nei periodi in cui ho dati mensili (la prima settimana
# considerata parte dal 03-09-2018)
scontrini_pre_covid_4_sett_avg <- scontrini_pre_covid_4_sett_avg[-c(1:35),]
scontrini_pre_covid_4_sett_avg <- scontrini_pre_covid_4_sett_avg$scontrini
scontrini_pre_covid_4_sett_avg <- ts(scontrini_pre_covid_4_sett_avg, start = decimal_date(as.Date("2018-09-03")), frequency=52)

print(
  autoplot(scontrini_pre_covid_4_sett_avg) +
    ggtitle("Ristorante 4: scontrini medi settimanali pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili pre-COVID
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
# mensilmente
month_rist4_pre_covid <- as.Date(cut(ristorante4_pre_covid$data, "month"))

scontrini_pre_covid_4_mens_avg <- aggregate(scontrini ~ month_rist4_pre_covid, data = ristorante4_pre_covid, mean)
scontrini_pre_covid_4_mens_avg <- scontrini_pre_covid_4_mens_avg$scontrini
scontrini_pre_covid_4_mens_avg <- ts(scontrini_pre_covid_4_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini_pre_covid_4_mens_avg) +
    ggtitle("Ristorante 4: scontrini medi mensili pre-COVID") +
    xlab("Anno") +
    ylab("Scontrini")
)


### Stagionalità considerando tutti gli anni

print(
  ggseasonplot(vendite4_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite settimanali")
)

# Nel grafico precedente c'è un problema sull'anno 2018, che dovrebbe partire dalla
# settimana 36 ma per qualche motivo "interpola" a partire dalla settimana 1. 
# Non ho trovato come risolvere questa cosa

print(
  ggseasonplot(vendite4_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite mensili")
)

### Seasonal sub series plot
print(
  ggsubseriesplot(vendite4_mens_avg) +
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 4: vendite medie mensili"))


### Stagionalità considerando il periodo pre-COVID

print(
  ggseasonplot(pre_covid_4_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite settimanali pre-COVID")
)

print(
  ggseasonplot(pre_covid_4_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite mensili pre-COVID")
)

### Seasonal sub series plot

print(
  ggsubseriesplot(pre_covid_4_mens_avg) +
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 4: vendite medie mensili pre-COVID")
)


### Analisi correlazione tra vendite e scontrini

scon_vend_sett_avg_4 <- ts.intersect(vendite4_sett_avg, scontrini4_sett_avg)

print(
  autoplot(scon_vend_sett_avg_4, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 4")
)

print(
  qplot(lordototale, scontrini, data=as.data.frame(copy_ristorante4)) +
    ylab("Scontrini") + xlab("Vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 4")
)

# Ho usato la copia senza dati aggregati mensilmente


### Analisi autocorrelazione considerando tutti gli anni
# Per una serie con trend l'autocorrelazione è alta a lag vicini e si abbassa
# piano piano. Se c'è stagionalità, invece, l'autocorrelazione presenta delle
# regolarità nel suo andamento

print(
  ggAcf(vendite4_day, lag=28) +
    ggtitle("Ristorante 4: Autocorrelation vendite giornaliere")
)

print(
  ggAcf(vendite4_sett_avg, lag=104) +
    ggtitle("Ristorante 4: Autocorrelation vendite medie settimanali")
)

print(
  ggAcf(vendite4_mens_avg, lag=36) +
    ggtitle("Ristorante 4: Autocorrelation vendite medie mensili")
)

### Analisi autocorrelazione pre-COVID

print(
  ggAcf(pre_covid_4_day, lag=28) +
    ggtitle("Ristorante 4: Autocorrelation vendite giornaliere pre-COVID")
)

print(
  ggAcf(pre_covid_4_sett_avg, lag=104) +
    ggtitle("Ristorante 4: Autocorrelation vendite medie settimanali pre-COVID")
)

print(
  ggAcf(pre_covid_4_mens_avg, lag=24) +
    ggtitle("Ristorante 4: Autocorrelation vendite medie mensili pre-COVID")
)


### Decomposizione serie storica
# Decomposizione giornaliera 
multi_vendite4 <- msts(copy_ristorante4$lordototale, ts.frequency = 365, start = decimal_date(as.Date("2018-09-03")), seasonal.periods = c(7,365))
multi_vendite4_dec <- mstl(multi_vendite4, s.window = "periodic")
print(autoplot(multi_vendite4_dec) + ggtitle("Ristorante 4: Decomposizione giornaliera"))

# Decomposizione settimanale
vendite4_sett.fit <- stl(vendite4_sett_avg, s.window="periodic")
trend.vendite4_sett <- vendite4_sett.fit$time.series[,2]
stag.vendite4_sett <- vendite4_sett.fit$time.series[,1]
res.vendite4_sett <- vendite4_sett.fit$time.series[,3]
print(autoplot(vendite4_sett.fit) + ggtitle("Ristorante 4: Decomposizione settimanale"))

# Decomposizione mensile 
vendite4_mens.fit <- stl(vendite4_mens_avg,s.window="periodic")
trend.vendite4_mens <- vendite4_mens.fit$time.series[,2]
stag.vendite4_mens <- vendite4_mens.fit$time.series[,1]
res.vendite4_mens <- vendite4_mens.fit$time.series[,3]
print(autoplot(vendite4_mens.fit) + ggtitle("Ristorante 4: Decomposizione mensile"))

# Alternativa
# components.ts_4 = decompose(vendite4_mens_avg)
# plot(components.ts_4)

### Decomposizione serie storica pre-COVID
# Non so quanto senso possa avere farla, dal momento che i nostri dati coprono
# poco più di un anno prima del COVID


# Confronto estati 2019/2020/2021 (pre-durante-post COVID)

r4_estate_2019 <- subset(copy_ristorante4, Year==2019 & Season == 'Summer')
r4_estate_2020 <- subset(copy_ristorante4, Year==2020 & Season == 'Summer')
r4_estate_2021 <- subset(copy_ristorante4, Year==2021 & Season == 'Summer')

r4_totale_estati <- rbind(r4_estate_2019, r4_estate_2020, r4_estate_2021)

# Creo un attributo per creare le label del grafico
r4_totale_estati$Year <- format(r4_totale_estati$data, "%Y")
r4_totale_estati$Month <- format(r4_totale_estati$data, "%b")
r4_totale_estati$Giorno <- format(r4_totale_estati$data, "%d")
r4_totale_estati$MonthDay <- format(r4_totale_estati$data, "%d-%b")

# Per le label ne tengo una ogni 3 giorni
r4_totale_estati$MonthDay2 <- r4_totale_estati$MonthDay
r4_totale_estati$MonthDay2[as.numeric(row.names(r4_totale_estati))%%3!=0] <- ""
labels <- r4_totale_estati$MonthDay2

# Calcolo la media per anno
mean <- r4_totale_estati %>% group_by(Year) %>% summarise(mean_val=mean(lordototale))

p <- ggplot(data=r4_totale_estati, mapping=aes(x=MonthDay, y=lordototale, shape=Year, color=Year)) + geom_point() +
  geom_line(aes(group = 1)) + geom_hline(data = mean, aes(yintercept = mean_val, col=Year), linetype = 'dashed')
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 4: confronto estati")
)