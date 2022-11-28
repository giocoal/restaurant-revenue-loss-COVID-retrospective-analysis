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

# PROVA FORMA STATE SPACE VENDITE PRE-COVID

y <- vendite_precovid
y[501:539] <- NA

plot(vendite_precovid)

# Modello con trend e stagionalità a dummy stocastiche

mod1 <- SSModel(y ~ SSMtrend(2, list(NA,NA)) +
                    SSMseasonal(7, NA, "dummy"),
                H = NA)

varv <- var(vendite_precovid, na.rm = TRUE)

pars <- log(c(
  logVarEta = varv/100, # Varianza del trend
  logVarZeta = 0.00001, # Varianza dello slope
  logVarOmega = varv/100, # Varianza della stagionalità
  logVarEps = varv/10 # Varianza del White Noise
))

fit1 <- fitSSM(mod1, pars)
fit1$optim.out

kfs1 <- KFS(fit1$model, filtering = c("state", "signal"),
            smoothing = c("state", "signal", "disturbance"))

smooth_level <- kfs1$alphahat[, "level"]
smooth_level_stand_err <- sqrt(kfs1$V[1, 1, ]) 
plot(smooth_level_stand_err) 

plot(vendite_precovid)
lines(smooth_level, col = "red", lwd = 3)
lines(smooth_level+2*smooth_level_stand_err, col = "red", lty = 2, lwd = 2)
lines(smooth_level-2*smooth_level_stand_err, col = "red", lty = 2, lwd = 3)

plot(kfs1$alphahat[, "sea_dummy1"])

# Stima 1-step ahead

plot(vendite_precovid[500:539], type = "l")
lines(kfs1$m[500:539], col = "red")

pre1 <- predict(kfs1$model, n.ahead = 50)

plot(vendite_precovid, xlim = c(2018.666, 2020.277))
lines(pre1, col = "red", lwd = 2)

# --------------------------------------------------------------------------------------------------------------------

mod2 <- SSModel(y ~ SSMtrend(2, list(NA,NA)) +
                  SSMseasonal(7, NA, "trigonometric"),
                H = NA)

updt2 <- function(pars, model) { 
  m <- nrow(model$Q)            
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[3:m, 3:m, 1]) <- exp(pars[3])
  model$H[1, 1, 1] <- exp(pars[4])
  model
}

fit2 <- fitSSM(mod2, fit1$optim.out$par, updt2)
fit2$optim.out

kfs2 <- KFS(fit2$model, filtering = c("state", "signal"),
            smoothing = c("state", "signal", "disturbance"))

plot(vendite_precovid)
lines(kfs1$alphahat[, "level"], col = "red", lwd = 2)
lines(kfs2$alphahat[, "level"], col = "green", lwd = 2)

pre2 <- predict(kfs2$model, n.ahead = 50)

plot(vendite_precovid, xlim = c(2018.666, 2020.277))
lines(pre1, col = "blue", lwd = 2)
lines(pre2, col = "red", lwd = 2)

mae1 <- mean(abs(vendite_precovid[500:539]-kfs1$m[500:539]))
mae2 <- mean(abs(vendite_precovid[500:539]-kfs2$m[500:539]))

mse1 <- mean((vendite_precovid[500:539]-kfs1$m[500:539])^2)
mse2 <- mean((vendite_precovid[500:539]-kfs2$m[500:539])^2)
