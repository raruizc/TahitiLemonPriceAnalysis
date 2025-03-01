rm(list = ls(all=T))

# Time Series ####

# List of required packages
packages <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
              "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
              "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
              "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
              "quantmod","dgof","seasonal", "astsa")

# Install and load packages
if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  installer <- packages[!packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}

## Loading Database - Months ####
historical_bucaramanga <- read_excel("Data/Bucaramanga_Centroabastos_2012_2025_Months.xlsx")
str(historical_bucaramanga)

## Transforming as Date ####
historical_bucaramanga <- historical_bucaramanga %>% 
  mutate(FECHA = as.Date(paste0(FECHA, "-01"), format = "%Y-%m-%d"))
head(historical_bucaramanga)
tail(historical_bucaramanga)


## Review NAs - Data ####

# Complete Sequence of Date
complete_dates <- seq(as.Date("2012-06-01"), as.Date("2025-01-01"), by = "month")

# Create a data frame with dates
complete_data <- data.frame(FECHA = complete_dates)

# Merge with database
complete_data <- merge(complete_data, historical_bucaramanga, by = "FECHA", all.x = TRUE)

plot(complete_data)
str(complete_data)

## Transforming data in COP to USD ####
library(quantmod)

### 1. Download COP/USD exchange rate using quantmod ####
getSymbols("USDCOP=X", src = "yahoo", from = "2012-06-01", to = "2025-01-01")
typeof(`USDCOP=X`)

### 2. Extract exchange rates ####
exchange_rate <- data.frame(FECHA = index(`USDCOP=X`), EXCHANGE_RATE = Cl(`USDCOP=X`))
head(exchange_rate)
colnames(exchange_rate)[2] <- "EXCHANGE_RATE"

### 3. Merge datasets by FECHA column
complete_data <- complete_data %>%
  left_join(exchange_rate, by = "FECHA")

# Check the result 
head(complete_data) # We see there are NAs

### 4. Interpolation with splines for exchange rate #### 
# This is useful when data has a non-linear trend.
complete_data <- complete_data %>%
  mutate(EXCHANGE_RATE = spline(FECHA, EXCHANGE_RATE, xout = FECHA)$y)

# Check the result
head(complete_data)

### 5. Convert values to USD ####

complete_data <- complete_data %>%
  mutate(
    AVERAGE_USD = PROMEDIO / EXCHANGE_RATE,
    MINIMUM_USD = MINIMO / EXCHANGE_RATE,
    MAXIMUM_USD = MAXIMO / EXCHANGE_RATE
  )

# Check the result
head(complete_data)
colnames(complete_data)

### 6. Interpolation with splines for missing data #### 
# This is useful when data has a non-linear trend.
complete_data <- complete_data %>%
  mutate(AVERAGE_USD = spline(FECHA, AVERAGE_USD, xout = FECHA)$y,
         MINIMUM_USD = spline(FECHA, MINIMUM_USD, xout = FECHA)$y,
         MAXIMUM_USD = spline(FECHA, MAXIMUM_USD, xout =FECHA)$y)


## Transforming the dataset into a ts object ####
prices_ts <- ts(data = complete_data[, 8],
                start = c(2012, 6),
                end = c(2025, 1),
                frequency = 12)
length(prices_ts)
length(complete_data$FECHA)
plot(prices_ts)

## Plotting the time series with maximum and minimum points ####
# Find the maximum and minimum points of average prices

historical_bucaramanga <- complete_data

maxima <-  historical_bucaramanga %>%
  group_by(year = format(FECHA, "%Y")) %>%
  slice(which.max(AVERAGE_USD))

minima <- historical_bucaramanga %>%
  group_by(year = format(FECHA, "%Y")) %>%
  slice(which.min(AVERAGE_USD))

series_plot <- historical_bucaramanga %>% ggplot() +
  geom_line(aes(x = FECHA, y = AVERAGE_USD, color = "Price"), 
            size = 0.75) +  # Time series line
  geom_line(aes(x = FECHA, y = MINIMUM_USD, color = "Minimum Price"), 
            size = 0.75, linetype = "dashed") +  # Minimum time series line
  geom_line(aes(x = FECHA, y = MAXIMUM_USD, color = "Maximum Price"), 
            size = 0.75, linetype = "dashed") +  # Maximum time series line
  geom_point(data = maxima, aes(x = FECHA, y = AVERAGE_USD, color = "Maximum"), 
             size = 1.5, shape = 21, fill = "#636363") +  # Maximum points
  geom_point(data = minima, aes(x = FECHA, y = AVERAGE_USD, color = "Minimum"), 
             size = 1.5, shape = 21, fill = "#ece7f2") +  # Minimum points
  scale_color_manual(
    name = "Legend:",  # Legend name
    values = c("Price" = "black", 
               "Maximum" = "#636363", # Maximum points color
               "Minimum" = "#ece7f2", # Minimum points color
               "Minimum Price" = "#969696",       # Minimum price line color
               "Maximum Price" = "#969696"),  # Maximum price line color
    breaks = c()  # Legend item order
  ) +
  xlab("DATE") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.4),
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid = element_line(color = "grey90"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "bottom"
  )

print(series_plot)
ggsave("Figures/01_Series_Bucaramanga.png", plot = series_plot, width = 12, 
       height = 8, dpi = 300)

## Plotting with minimum and maximum area ####

series_plot_area <- historical_bucaramanga %>% ggplot() +
  geom_line(aes(x = FECHA, y = AVERAGE_USD, color = "Average Price"),
            size = 0.8) +  # Average line
  geom_ribbon(aes(x = FECHA, ymin = MINIMUM_USD, ymax = MAXIMUM_USD, 
                  fill = "Price Range"), alpha = 0.3) +  # Fill between minimum and maximum
  scale_color_manual(
    name = "Lines:",
    values = c("Average Price" = "black")
  ) +
  scale_fill_manual(
    name = "Areas:",
    values = c("Price Range" = "grey50")
  ) +
  xlab("") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.4),
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid = element_line(color = "grey90"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "bottom"
  )

print(series_plot_area)
ggsave("Figures/01_Series_Area_Bucaramanga.png", plot = series_plot_area, width = 12, 
       height = 8, dpi = 300)

## Decomposing the price using the additive model ####
decpib <- decompose(x = prices_ts,
                    type = "additive")


# Checking the length of the time series compared to the original #
length(decpib$x)
length(decpib$trend)
length(decpib$seasonal)
length(decpib$random)
length(historical_bucaramanga$FECHA)

### Transforming the decpib object into a data frame ####
decpib_df <- data.frame(time = historical_bucaramanga$FECHA,
                        series = unlist(decpib$x),
                        trend = unlist(decpib$trend),
                        seasonality = unlist(decpib$seasonal),
                        deseasonalized = prices_ts - decpib$seasonal,
                        error = unlist(decpib$random)) %>%
  rename(time = 1,
         series = 2,
         trend = 3,
         seasonality = 4,
         deseasonalized = 5,
         error = 6)

#### Plotting the decomposition together ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = time, y = series, color = "Price"), size = 1.2) +
  geom_line(aes(x = time, y = trend, color = "Trend"), size = 1) +
  geom_line(aes(x = time, y = seasonality, color = "Seasonal"), size = 1.2) +
  geom_line(aes(x = time, y = error, color = "Resid"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legend:",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#440154FF", "#3CBB75FF", "#39568CFF", "#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

# Observing each component - see excel
decpib$trend
decpib$seasonal
decpib$random

#### Plotting the decomposition individually ####

##### a) Series #####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = time, y = series, color = "Price")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#39568CFF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_series

##### b) Seasonality ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = time, y = seasonality, color = "Seasonal")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seasonal",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#3CBB75FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_seasonality

##### c) Trend ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = time, y = trend, color = "Trend")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Trend",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#DCE319FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_trend

##### d) Error #####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = time, y = error, color = "Resid")) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Resid",
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c("#440154FF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none") -> decomp_error

##### Plotting ####
# Zoom for a better view
grid.arrange(decomp_series,
             decomp_seasonality,
             decomp_trend,
             decomp_error,
             ncol = 2)

grid.arrange(decomp_series,
             decomp_trend,
             ncol = 2)

###### Efficient Plotting - Aesthetics ####
library(patchwork)
library(scales)

# Theme configuration
theme_set(theme_minimal(base_size = 12) +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 12, hjust = 0.5),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.position = "none"
            ))

# Function to create individual graphs
create_decomp_plot <- function(data, y_var, title, y_axis, color) {
  ggplot(data, aes(x = time, y = !!sym(y_var))) +
    geom_line(color = color, size = 1.2) +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = title,
      x = NULL,
      y = y_axis
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.4),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}

# Creating individual graphs
decomp_series <- create_decomp_plot(decpib_df, "series", "Series", 
                                    "Price (USD)",  "#39568CFF")
decomp_seasonality <- create_decomp_plot(decpib_df, "seasonality", 
                                         "Seasonality", NULL, "#3CBB75FF")
decomp_trend <- create_decomp_plot(decpib_df, "trend", "Trend", 
                                   "Price (USD)", "#DCE319FF")
decomp_error <- create_decomp_plot(decpib_df, "error", "Residuals",
                                   NULL, "#440154FF")

# Combining graphics with patchwork package
combined_plot <- (decomp_series + decomp_seasonality) / 
  (decomp_trend + decomp_error) +
  plot_annotation(
    #title = "Decomposition of Price Time Series",
    #subtitle = "Analysis of Trend, Seasonality, and Residuals",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )

# Displaying the combined graph
print(combined_plot)

# Saving the graphic in high resolution
ggsave("Figures/02_Decomposition_plot_Bucaramanga.png", plot = combined_plot, width = 12, 
       height = 8, dpi = 300)


#### Plotting the deseasonalized series ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = time, y = series, color = "Series"), size = 1.2) +
  geom_line(aes(x = time, y = deseasonalized, color = "Deseasonalized"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legend:",
       x = NULL,
       y = NULL) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")


## Decomposing the GDP using the multiplicative model ####
decpib <- decompose(prices_ts, type = "multiplicative")
plot(decpib)


# Observing each component - see excel
decpib$trend
decpib$seasonal
decpib$random

# Stationarity Test ####
####
## Analyzing autoregressive series
####
## Performing Stationarity Tests
## We need the URCA package - Unit Root and Cointegration Test
####
#### Dickey-Fuller Test ###
# Ho: The series is Not Stationary
# H1: The series is Stationary

## Division of the time series ####
price_train <- window(prices_ts, start=c(2012,6), end=c(2023,12))
price_train
price_test <- window(prices_ts, start=c(2024,1), end=c(2025,1))
length(price_test)
length(price_train)

testprice <- ur.df(price_train)
testprice
summary(testprice)

### Test indicating it is not seasonal ####
adf.test(prices_ts, alternative = "stationary") 

# With one difference 
#### It should be stationary
diffprice_series <- diff(prices_ts)
diffprice_series
ggtsdisplay(diffprice_series)
adf.test(diffprice_series)
testdiffprice_series <-  ur.df(diffprice_series)
summary(testdiffprice_series)

### ARIMA Selection ####
arimaprice <- auto.arima(price_train, trace = T)

# Check the fitted model
summary(arimaprice)

#### 1. Ljung-Box Test ####
# According to the p-value, we accept H0: residuals are not correlated

checkresiduals(arimaprice)

#### 2. Normality of residuals ####

ks.test(arimaprice$residuals, "pnorm", mean(arimaprice$residuals),
        sd(arimaprice$residuals))

# Confirmed the absence of serial autocorrelation and normality of residuals
# We can check the stationarity of variance
#### 3. Check for ARCH effects ####

ArchTest(arimaprice$residuals)

### Forecast for the lime price series ####

forecastprice <- forecast::forecast(arimaprice, h=length(price_test))

autoplot(forecastprice) +
  theme_bw()

accuracy(forecastprice, price_test)

#### Forecast Plot ####
ggplotly(
  autoplot(price_train)+
    autolayer(price_test,series="Real Values")+
    autolayer(forecastprice$mean, serie="Forecast")+
    labs(x = "Time",
         y = "Price (USD)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

#### Forecast Plot Aesthetics ####
forecast_plot <- ggplotly(
  autoplot(price_train, color = "#1f77b4") +  # Cor da série de treinamento
    autolayer(price_test, series = "Real Values") +  # Cor dos valores reais
    autolayer(forecastprice$mean, series = "Forecast") +  # Cor da previsão
    labs(
      #title = "Price Forecast vs Real Values - Bucaramanga",  # Título do gráfico
      #subtitle = "Comparison between forecasted and actual prices",  # Subtítulo
      x = "Time",  # Rótulo do eixo X
      y = "Price (USD)",  # Rótulo do eixo Y
      color = "Series"  # Título da legenda
    ) +
    scale_color_manual(
      values = c("Price Train" = "#1f77b4", "Real Values" = "#ff7f0e",
                 "Forecast" = "#2ca02c")  # Cores personalizadas
    ) +
    scale_y_continuous(labels = scales::comma) +  # Formatação do eixo Y
    theme_bw() +  # Tema básico
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Estilo do título
      plot.subtitle = element_text(size = 12, hjust = 0.5),  # Estilo do subtítulo
      axis.title = element_text(size = 12, face = "bold"),  # Estilo dos rótulos dos eixos
      axis.text = element_text(size = 10),  # Estilo do texto dos eixos
      legend.title = element_text(size = 12, face = "bold"),  # Estilo do título da legenda
      legend.text = element_text(size = 10),  # Estilo do texto da legenda
      legend.position = "bottom"  # Posição da legenda
    )
)

print(forecast_plot)

# Save as HTML
library(htmlwidgets)
saveWidget(forecast_plot, file = "Figures/04_Forecast_Bucaramanga.html")

## Improving the model in COP ####

### Manual ARIMA Adjustment (ARIMA obtained with Autoarima) ####
# Best model: ARIMA(2,0,1)(0,1,1)[12]
manual_arima_model <- Arima(price_train, order = c(2, 0, 1), seasonal = c(0, 1, 1))
summary(manual_arima_model)

#### Check AIC and BIC
AIC(manual_arima_model)
BIC(manual_arima_model)

# Residual autocorrelation plots
residuals <- residuals(manual_arima_model)
Acf(residuals, main = "ACF of Residuals")
Pacf(residuals, main = "PACF of Residuals")
checkresiduals(manual_arima_model)

# Plot of residuals over time
plot(residuals, main = "Residuals Over Time", ylab = "Residuals")
abline(h = 0, col = "red")

#### Forecasts and evaluation ####
manual_forecasts <- forecast(manual_arima_model, h = length(price_test))

autoplot(manual_forecasts) +
  theme_bw()

accuracy(manual_forecasts, price_test)

##### Forecast Plot ####
ggplotly(
  autoplot(price_train)+
    autolayer(price_test,series="Real Values")+
    autolayer(manual_forecasts$mean, serie="Forecast")+
    labs(x = "Time",
         y = "Price (USD)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

##### Forecast Plot Aesthetics ####
ggplotly(
  autoplot(price_train, color = "#1f77b4") +  # Cor da série de treinamento
    autolayer(price_test, series = "Real Values") +  # Cor dos valores reais
    autolayer(manual_forecasts$mean, series = "Forecast") +  # Cor da previsão
    labs(
      title = "Price Forecast vs Real Values - Bucaramanga",  # Título do gráfico
      subtitle = "Comparison between forecasted and actual prices",  # Subtítulo
      x = "Time",  # Rótulo do eixo X
      y = "Price (USD)",  # Rótulo do eixo Y
      color = "Series"  # Título da legenda
    ) +
    scale_color_manual(
      values = c("Price Train" = "#1f77b4", "Real Values" = "#ff7f0e",
                 "Forecast" = "#2ca02c")  # Cores personalizadas
    ) +
    scale_y_continuous(labels = scales::comma) +  # Formatação do eixo Y
    theme_bw() +  # Tema básico
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Estilo do título
      plot.subtitle = element_text(size = 12, hjust = 0.5),  # Estilo do subtítulo
      axis.title = element_text(size = 12, face = "bold"),  # Estilo dos rótulos dos eixos
      axis.text = element_text(size = 10),  # Estilo do texto dos eixos
      legend.title = element_text(size = 12, face = "bold"),  # Estilo do título da legenda
      legend.text = element_text(size = 10),  # Estilo do texto da legenda
      legend.position = "bottom"  # Posição da legenda
    )
)

### SARIMA Adjustment ####
sarima_model <- Arima(price_train, order = c(2, 1, 1), seasonal = c(2, 1, 1))
summary(sarima_model)


# Check AIC and BIC
AIC(sarima_model)
BIC(sarima_model)

# Residual autocorrelation plots
residuals <- residuals(sarima_model)
Acf(residuals, main = "ACF of Residuals")
Pacf(residuals, main = "PACF of Residuals")
checkresiduals(sarima_model)

# Plot of residuals over time
plot(residuals, main = "Residuals Over Time", ylab = "Residuals")
abline(h = 0, col = "red")

#### Forecasts and evaluation ####
sarima_forecasts <- forecast(sarima_model, h = length(price_test))

autoplot(sarima_forecasts) +
  theme_bw()

accuracy(sarima_forecasts, price_test)

##### Forecast Plot ####
ggplotly(
  autoplot(price_train)+
    autolayer(price_test,series="Real Values")+
    autolayer(sarima_forecasts$mean, serie="Forecast")+
    labs(x = "Time",
         y = "Price (USD)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)


##### Forecast Plot Aesthetics ####
ggplotly(
  autoplot(price_train, color = "#1f77b4") +  # Cor da série de treinamento
    autolayer(price_test, series = "Real Values") +  # Cor dos valores reais
    autolayer(sarima_forecasts$mean, series = "Forecast") +  # Cor da previsão
    labs(
      title = "Price Forecast vs Real Values - Bucaramanga",  # Título do gráfico
      subtitle = "Comparison between forecasted and actual prices",  # Subtítulo
      x = "Time",  # Rótulo do eixo X
      y = "Price (USD)",  # Rótulo do eixo Y
      color = "Series"  # Título da legenda
    ) +
    scale_color_manual(
      values = c("Price Train" = "#1f77b4", "Real Values" = "#ff7f0e",
                 "Forecast" = "#2ca02c")  # Cores personalizadas
    ) +
    scale_y_continuous(labels = scales::comma) +  # Formatação do eixo Y
    theme_bw() +  # Tema básico
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Estilo do título
      plot.subtitle = element_text(size = 12, hjust = 0.5),  # Estilo do subtítulo
      axis.title = element_text(size = 12, face = "bold"),  # Estilo dos rótulos dos eixos
      axis.text = element_text(size = 10),  # Estilo do texto dos eixos
      legend.title = element_text(size = 12, face = "bold"),  # Estilo do título da legenda
      legend.text = element_text(size = 10),  # Estilo do texto da legenda
      legend.position = "bottom"  # Posição da legenda
    )
)

### ARIMA-GARCH Model Adjustment ####
library(rugarch)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(2, 1)),  # ARIMA(2,1,1)
  distribution.model = "norm"
)

# Model fitting
garch_model <- ugarchfit(spec, data = price_train)
garch_model

# Forecasts
garch_forecasts <- ugarchforecast(garch_model, n.ahead = length(price_test))

# Plot of conditional mean forecasts
plot(garch_forecasts, type = "l", col = "blue",
     main = "Conditional Mean Forecasts", xlab = "Time", ylab = "Price")

garch_forecasts

### Model Comparison ####
results <- data.frame(
  Model = c("ARIMA", "SARIMA", "ARIMA-GARCH"),
  AIC = c(AIC(manual_arima_model), AIC(sarima_model), infocriteria(garch_model)["AIC"]),
  RMSE = c(accuracy(manual_forecasts, price_test)["Test set", "RMSE"],
           accuracy(sarima_forecasts, price_test)["Test set", "RMSE"],
           sqrt(mean((price_test - garch_forecasts@forecast$seriesFor)^2)))
)

print(results)
