```{r}
print(ts[is.na(ts$lordototale)==TRUE,])
# Presenti ancora alcui valori nulli. In particolare, si nota come tali valori mancanti siano in prossimità di alcunne feste nazionali, dove probabilmente il punto vendita era chiuso
```

```{r}
require(imputeTS)
ggplot_na_distribution(xts1) # Distribuzione dei valori nulli
statsNA(xts1)
```

```{r}
require(imputeTS)
# Imputazione dei valori nulli. Utilizzo della libreria "impute TS" con metodo "StructTS" che fa uso della massima verosimiglianza
xts1_noNA <- na_kalman(xts1, model = "StructTS", smooth = TRUE) # Utilizzo del filtro di kalman
ggplot_na_imputations(xts1, xts1_noNA)
```
```{r}
# Salvo l'oggetto xts1_noNA
write.zoo(xts1_noNA, file="ristorante1_xts.csv", sep=",")
```
