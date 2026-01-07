# este es un documento de prueba

#Librerías
library(tsibble)
library(dplyr)
library(lubridate)
library(fpp3)
library(tidyverse)
library(leaflet)
library(zoo)
library(fable)
library(imputeTS)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(feasts)
library(knitr)
library(leaflet)
library(readxl)
#install.packages("GGally")
library(GGally)

df_holidays <- "holidays_events.csv"
df_oil <- "oil.csv"
df_stores <- "stores.csv"
df_test <- "test.csv"
df_train <- "train.csv"
df_transactions <- "transactions.csv"

df_holidays <- read.csv(df_holidays)
df_oil <- read.csv(df_oil)
df_stores <- read.csv(df_stores)
df_test <- read.csv(df_test)
df_train <- read.csv(df_train)
df_transactions <- read.csv(df_transactions)

df_holidays <- df_holidays |> 
  as_tibble() |>
  select(date, locale, locale_name, description) |> 
  mutate(date = as.Date(date))

df_transactions <- df_transactions |> 
  filter(store_nbr == 1) |> 
  as_tibble() |>
  select(-store_nbr) |> 
  mutate(date = as.Date(date)) |> 
  as_tsibble(index = date) |> 
  fill_gaps(transactions = 0)

df <- df_train |> 
  filter(store_nbr == 1) |> 
  as_tibble() |> 
  select(date, family, sales, onpromotion) |> 
  mutate(date = as.Date(date)) |> 
  as_tsibble(key = family, index = date) |> 
  fill_gaps(sales = 0, onpromotion = 0)

df <- df |>
  select(-onpromotion)

# las promociones de los productos no son relevantes para predecir el numero de
# transacciones dado que se obtuvo las correlaciones con las transacciones y
# ninguna de ella resulto ser mayor a 0.6

df <- df |>
  filter(!date %in% c(as.Date("2013-01-01"), setdiff(df$date, df_transactions$date)))

df_oil <- df_oil |> 
  mutate(date = as.Date(date)) |> 
  as_tsibble(index = date) |> 
  fill_gaps() |> 
  fill(dcoilwtico, .direction = "downup") |> 
  as_tibble()

df_oil <- df_oil |>
  filter(!date %in% c(as.Date("2013-01-01"), setdiff(df_oil$date, df_transactions$date))) |> 
  filter(date < as.Date("2017-08-16"))

df |>
  autoplot(sales) + 
  labs(title = "Ventas de productos de la tienda 1") +
  facet_wrap(vars(family), scales = "fixed", ncol = 5) +
  theme(legend.position = "none")

df |>
  filter(family == "FROZEN FOODS", year(date) >= 2016) |> 
  autoplot(sales) + 
  labs(title = "Ventas de productos de la tienda 1")

# correlación entre las variables

# vamos a considerar aquellas variables que sean significativas al 95% de confiabilidad,
# cuya correlacion con la variable TRANSACTIONS sea mayor o igual a 0.6 y la correlacion
# con las demas variables que se agrupen en un mismo grupo de familias  no sea mayor a 0.4

# Cabe notar que al observar los gráficos de las ventas de los productos, hay algunos de
# los productos con los que la tienda obtiene altas ganancias diariamente, por ende, al
# ser un supermercado se puede inferir que se requiere vender una gran cantidad de unidades
# de productos de esas familias para poder obtener tal cantidad de ganancias. Es por tal 
# motivo que se va a considerar una excepcion con estos productos por ser potencialmente 
# significativos para predecir el numero de transacciones de la tienda

tb_total <- df |> 
  pivot_wider(
    names_from = family,
    values_from = sales
  ) |> 
  left_join(
    df_transactions |> 
      select(date, TRANSACTIONS = transactions),
    by = "date"
  )

tb_total <- tb_total |> 
  left_join(
    df_oil |> 
      select(date, DCOILWTICO = dcoilwtico),
    by = "date"
  )

# correlacion entre TRANSACTIONS y HOLIDAYS

df_transactions <- df_transactions |> 
  mutate(
    TOTAL = ifelse(date %in% df_holidays$date, 1, 0)
  )

grupo0 <- df_transactions$transactions[df_transactions$TOTAL == 0]
grupo1 <- df_transactions$transactions[df_transactions$TOTAL == 1]

mean0 <- mean(grupo0)
mean1 <- mean(grupo1)

sd0 <- sd(grupo0)
sd1 <- sd(grupo1)

# se usa la prueba t estandar sin asumir varianzas iguales pues sd0 != sd1

test1 <- t.test(grupo0, grupo1, var.equal = FALSE)

# como el p-valor es < 0.05, se concluye que existe una diferencia significativa
# entre las dos medias. Ahora, analizemos cuando se tienen feriados nacionales

df_holidays_nacional <- df_holidays |> 
  filter(locale == "National")

df_transactions <- df_transactions |> 
  mutate(
    NACIONAL = ifelse(date %in% df_holidays_nacional$date, 1, 0)
  )

grupo0 <- df_transactions$transactions[df_transactions$NACIONAL == 0]
grupo1 <- df_transactions$transactions[df_transactions$NACIONAL == 1]

mean0 <- mean(grupo0)
mean1 <- mean(grupo1)

sd0 <- sd(grupo0)
sd1 <- sd(grupo1)

# se usa la prueba t estandar sin asumir varianzas iguales pues sd0 != sd1

test2 <- t.test(grupo0, grupo1, var.equal = FALSE)

# como el p-valor es < 0.05, se concluye que existe una diferencia significativa
# entre las dos medias. Así, consideraremos los feriados nacionales en el modelo.
# Ahora, analizemos cuando se tienen feriados regionales y locales

df_holidays_regional <- df_holidays |> 
  filter(locale == "Regional")

df_transactions <- df_transactions |> 
  mutate(
    REGIONAL = ifelse(date %in% df_holidays_regional$date, 1, 0)
  )

grupo0 <- df_transactions$transactions[df_transactions$REGIONAL == 0]
grupo1 <- df_transactions$transactions[df_transactions$REGIONAL == 1]

mean0 <- mean(grupo0)
mean1 <- mean(grupo1)

sd0 <- sd(grupo0)
sd1 <- sd(grupo1)
