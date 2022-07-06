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
ristorante3 <- read.csv("ristorante3.csv")

# Presenza di NaN

sum(is.na(ristorante3$scontrini)) # 71 NA
# which(is.na(ristorante3$scontrini))
subset(ristorante3[,c(2,6)], is.na(ristorante3$scontrini))
# I primi 7 valori mancanti sono causati, probabilmente, dall'inizio dell'attività
# datata 08-11-2019. Dopodichè abbiamo NaN per COVID (da 133 a 188) e festività

### Metto a 0 i Na, per comodità

ristorante3$lordototale[is.na(ristorante3$lordototale)] <- 0
ristorante3$scontrini[is.na(ristorante3$scontrini)] <- 0  
ristorante3$Prezzo_medio_per_scontrino[is.na(ristorante3$Prezzo_medio_per_scontrino)] <- 0

# Definisco il formato della data

ristorante3$data <- parse_date(ristorante3$data, "%Y-%m-%d", locale = locale("it"))

# Vendite giornaliere 
vendite3_day <- ts(ristorante3$lordototale, start = decimal_date(as.Date("2019-11-04")), frequency=365)

print(
  autoplot(vendite3_day) +
    ggtitle("Ristorante 3: vendite giornaliere") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite settimanali medie
# Per comodità parto dal 2019-11-04 essendo un lunedì (e i 3 giorni precedenti
# il ristorante era probabilmente chiuso)
ristorante3 <- ristorante3[-c(1:3),]
week_rist3 <- as.Date(cut(ristorante3$data, "week"))
vendite3_sett_avg <- aggregate(lordototale ~ week_rist3, data = ristorante3, mean)

vendite3_sett_avg <- vendite3_sett_avg$lordototale
vendite3_sett_avg <- ts(vendite3_sett_avg, start = decimal_date(as.Date("2019-11-04")), frequency=52)

print(
  autoplot(vendite3_sett_avg) +
    ggtitle("Ristorante 3: vendite medie settimanali") +
    xlab("Anno") +
    ylab("Vendite")
)

# Vendite mensili medie
month_rist3 <- as.Date(cut(ristorante3$data, "month"))

vendite3_mens_avg <- aggregate(lordototale ~ month_rist3, data = ristorante3, mean)
vendite3_mens_avg <- vendite3_mens_avg$lordototale
vendite3_mens_avg <- ts(vendite3_mens_avg, start = decimal_date(as.Date("2019-11-04")), frequency=12)

print(
  autoplot(vendite3_mens_avg) +
    ggtitle("Ristorante 3: vendite medie mensili") +
    xlab("Anno") +
    ylab("Vendite")
)


### NON HO FATTO L'ANALISI PRE-COVID PER QUESTO RISTORANTE DAL MOMENTO CHE CI 
### RIDURREMMO A MENO DI UN CENTINAIO DI DATI


### Faccio la stessa analisi precedente sul numero di scontrini

# Scontrini giornalieri 
scontrini3_day <- ts(ristorante3$scontrini, start = decimal_date(as.Date("2019-11-04")), frequency=365)

print(
  autoplot(scontrini3_day) +
    ggtitle("Ristorante 3: scontrini giornalieri") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini settimanali medi
# Per comodità parto dal 2019-11-04 essendo un lunedì (e i 3 giorni precedenti
# il ristorante era probabilmente chiuso)
scontrini3_sett_avg <- aggregate(scontrini ~ week_rist3, data = ristorante3, mean)

scontrini3_sett_avg <- scontrini3_sett_avg$scontrini
scontrini3_sett_avg <- ts(scontrini3_sett_avg, start = decimal_date(as.Date("2019-11-04")), frequency=52)

print(
  autoplot(scontrini3_sett_avg) +
    ggtitle("Ristorante 3: scontrini medi settimanali") +
    xlab("Anno") +
    ylab("Scontrini")
)

# Scontrini mensili medi
# Uso direttamente il dataset completo, considerando anche i dati già aggregati
# mensilmente
month_rist3 <- as.Date(cut(ristorante3$data, "month"))

scontrini3_mens_avg <- aggregate(scontrini ~ month_rist3, data = ristorante3, mean)
scontrini3_mens_avg <- scontrini3_mens_avg$scontrini
scontrini3_mens_avg <- ts(scontrini3_mens_avg, start=2018, frequency=12)

print(
  autoplot(scontrini3_mens_avg) +
    ggtitle("Ristorante 3: scontrini medi mensili") +
    xlab("Anno") +
    ylab("Scontrini")
)

### Stagionalità considerando tutti gli anni

print(
  ggseasonplot(vendite3_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 3: vendite settimanali")
)

# Nel grafico precedente c'è un problema sull'anno 2019, che dovrebbe partire dalla
# settimana 44 ma per qualche motivo "interpola" a partire dalla settimana 1. 
# Non ho trovato come risolvere questa cosa

print(
  ggseasonplot(vendite3_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 3: vendite mensili")
)

### Seasonal sub series plot
print(
  ggsubseriesplot(vendite3_mens_avg) +
    ylab("euro") +
    ggtitle("Seasonal subseries plot Ristorante 3: vendite medie mensili"))


### Analisi correlazione tra vendite e scontrini

scon_vend_sett_avg_3 <- ts.intersect(vendite3_sett_avg, scontrini3_sett_avg)

print(
  autoplot(scon_vend_sett_avg_3, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 3")
)

print(
  qplot(lordototale, scontrini, data=as.data.frame(ristorante3)) +
    ylab("Scontrini") + xlab("Vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 3")
)

# Ho usato la copia senza dati aggregati mensilmente


### Analisi autocorrelazione considerando tutti gli anni
# Per una serie con trend l'autocorrelazione è alta a lag vicini e si abbassa
# piano piano. Se c'è stagionalità, invece, l'autocorrelazione presenta delle
# regolarità nel suo andamento

print(
  ggAcf(vendite3_day, lag=28) +
    ggtitle("Ristorante 3: Autocorrelation vendite giornaliere")
)

print(
  ggAcf(vendite3_sett_avg, lag=104) +
    ggtitle("Ristorante 3: Autocorrelation vendite medie settimanali")
)

print(
  ggAcf(vendite3_mens_avg, lag=36) +
    ggtitle("Ristorante 3: Autocorrelation vendite medie mensili")
)

### Decomposizione serie storica
# Decomposizione giornaliera 
multi_vendite3 <- msts(ristorante3$lordototale, ts.frequency = 365, start = decimal_date(as.Date("2019-11-04")), seasonal.periods = c(7,365))
multi_vendite3_dec <- mstl(multi_vendite3, s.window = "periodic")
print(autoplot(multi_vendite3_dec) + ggtitle("Ristorante 3: Decomposizione giornaliera"))

# Decomposizione settimanale
vendite3_sett.fit <- stl(vendite3_sett_avg, s.window="periodic")
trend.vendite3_sett <- vendite3_sett.fit$time.series[,2]
stag.vendite3_sett <- vendite3_sett.fit$time.series[,1]
res.vendite3_sett <- vendite3_sett.fit$time.series[,3]
print(autoplot(vendite3_sett.fit) + ggtitle("Ristorante 3: Decomposizione settimanale"))

# Decomposizione mensile 
vendite3_mens.fit <- stl(vendite3_mens_avg,s.window="periodic")
trend.vendite3_mens <- vendite3_mens.fit$time.series[,2]
stag.vendite3_mens <- vendite3_mens.fit$time.series[,1]
res.vendite3_mens <- vendite3_mens.fit$time.series[,3]
print(autoplot(vendite3_mens.fit) + ggtitle("Ristorante 3: Decomposizione mensile"))

# Alternativa
# components.ts_3 = decompose(vendite3_mens_avg)
# plot(components.ts_3)


# Confronto estati 2020/2021 (durante-post COVID, ristorante 3 ha dati da novembre 2019)

r3_estate_2020 <- subset(ristorante3, Year==2020 & Season == 'Summer')
r3_estate_2021 <- subset(ristorante3, Year==2021 & Season == 'Summer')

r3_totale_estati <- rbind(r3_estate_2020, r3_estate_2021)

# Creo un attributo per creare le label del grafico
r3_totale_estati$Year <- format(r3_totale_estati$data, "%Y")
r3_totale_estati$Month <- format(r3_totale_estati$data, "%b")
r3_totale_estati$Giorno <- format(r3_totale_estati$data, "%d")
r3_totale_estati$MonthDay <- format(r3_totale_estati$data, "%d-%b")

# Per le label ne tengo una ogni 3 giorni
r3_totale_estati$MonthDay2 <- r3_totale_estati$MonthDay
r3_totale_estati$MonthDay2[as.numeric(row.names(r3_totale_estati))%%3!=0] <- ""
labels <- r3_totale_estati$MonthDay2

# Calcolo la media per anno
mean <- r3_totale_estati %>% group_by(Year) %>% summarise(mean_val=mean(lordototale))

p <- ggplot(data=r3_totale_estati, mapping=aes(x=MonthDay, y=lordototale, shape=Year, color=Year)) + geom_point() +
  geom_line(aes(group = 1)) + geom_hline(data = mean, aes(yintercept = mean_val, col=Year), linetype = 'dashed')
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 3: confronto estati")
)