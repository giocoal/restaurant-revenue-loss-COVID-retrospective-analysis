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
ristorante1$lordototale[is.na(ristorante1$lordototale)] <- 0

precovid1 <- read.csv("pre-covid_r1.csv")

# Rimozione dati aggregati mensilmente
copy_ristorante1<- ristorante1[-c(1:245),]
copy_ristorante1 <- copy_ristorante1[-c(1334:1336),]

# Estrazione serie storiche vendite totali e pre-covid
vendite <- ts(copy_ristorante1$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)
vendite_precovid <- ts(precovid1$lordototale, start = decimal_date(as.Date("2018-09-01")), frequency=365)

# Forma state space vendite totali
varv <- var(vendite_precovid)

pars <- log(c(logVarEta  = varv/50,
              logVarZeta = 10,
              logVarEps  = varv/10))

mod1 <- SSModel(
  vendite_precovid ~ SSMtrend(degree = 2, Q = list(NA, NA))+
            SSMseasonal(period = 7, sea.type = c("trigonometric")),
  H = NA
)

fit1 <- fitSSM(mod1, pars)

kfs1 <- KFS(fit1$model,
            filtering = c("state", "signal"),
            smoothing = c("state", "disturbance", "signal"))

plot(vendite_precovid)
lines(kfs1$alphahat[, 1], col = "red", lwd=2)

pre1 <- predict(fit1$model, n.ahead = 100)
lines(pre1, col="green")
