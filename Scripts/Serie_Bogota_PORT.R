rm(list = ls(all=T))

# Serie de Tiempo ####

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal", "astsa")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

## Carregar base de dados ####
historico_bogota <- read_excel("Data/Bogota_Corabastos_2012_2025_Months.xlsx")
str(historico_bogota)
## Transformar a tipo data ####
historico_bogota <- historico_bogota %>% 
  mutate(FECHA = as.Date(paste0(FECHA, "-01"), format = "%Y-%m-%d"))
head(historico_bogota)
tail(historico_bogota)

##Verificar se possui sequencias incompletas nas datas ####

# Criar uma sequência completa de datas
datas_completas <- seq(as.Date("2012-06-01"), as.Date("2025-01-01"), by = "month")

# Criar um data frame com as datas completas
dados_completos <- data.frame(FECHA = datas_completas)

# Juntar com os dados originais
dados_completos <- merge(dados_completos, historico_bogota, by = "FECHA", all.x = TRUE)

plot(dados_completos)
str(dados_completos)

## Transformas os dados de COP a USD ####
library(quantmod)

### 1.  Baixar taxa de câmbio COP/USD com o pacote quantmod ####
getSymbols("USDCOP=X", src = "yahoo", from = "2012-06-01", to = "2025-01-01")
typeof(`USDCOP=X`)

### 2. Extrair as taxas de câmbio ####
taxa_cambio <- data.frame(FECHA = index(`USDCOP=X`), TAXA_CAMBIO = Cl(`USDCOP=X`))
head(taxa_cambio)
colnames(taxa_cambio)[2] <- "TAXA_CAMBIO"

### 3. Juntar as bases de dados pela coluna FECHA
dados_completos <- dados_completos %>%
  left_join(taxa_cambio, by = "FECHA")

# Verificar o resultado 
head(dados_completos) # Verificamos que tem NA

### 4. Interpolação com splines de taxa de cambio #### 
#Isso é útil quando os dados têm uma tendência não linear.
dados_completos <- dados_completos %>%
  mutate(TAXA_CAMBIO = spline(FECHA, TAXA_CAMBIO, xout = FECHA)$y)

# Verificar o resultado
head(dados_completos)

### 5. Converter os Valores para USD ####

dados_completos <- dados_completos %>%
  mutate(
    PROMEDIO_USD = PROMEDIO / TAXA_CAMBIO,
    MINIMO_USD = MINIMO / TAXA_CAMBIO,
    MAXIMO_USD = MAXIMO / TAXA_CAMBIO
  )

# Verificar o resultado
head(dados_completos)
colnames(dados_completos)

### 6. Interpolação com splines dados faltantes #### 
#Isso é útil quando os dados têm uma tendência não linear.
dados_completos <- dados_completos %>%
  mutate(PROMEDIO_USD = spline(FECHA, PROMEDIO_USD, xout = FECHA)$y,
         MINIMO_USD = spline(FECHA, MINIMO_USD, xout = FECHA)$y,
         MAXIMO_USD = spline(FECHA, MAXIMO_USD, xout =FECHA)$y)


## Transformando a base de dados em um objeto de classe ts ####
precios_ts <- ts(data = dados_completos[, 8],
             start = c(2012, 6),
             end = c(2025, 1),
             frequency = 12)

length(precios_ts)
length(dados_completos$FECHA)
plot(precios_ts)

## Fazendo Plotagem da série temporal com pontos máximos e mínimos ####
# Encontrar os pontos máximos e mínimos dos precios medios

historico_bogota <- dados_completos

maximos <-  historico_bogota %>%
  group_by(year = format(FECHA, "%Y")) %>%
  slice(which.max(PROMEDIO_USD))

minimos <- historico_bogota %>%
  group_by(year = format(FECHA, "%Y")) %>%
  slice(which.min(PROMEDIO_USD))

historico_bogota %>% ggplot() +
  geom_line(aes(x = FECHA, y = PROMEDIO_USD, color = "Precio"), 
            size = 0.75) +  # Linha da série temporal
  geom_line(aes(x = FECHA, y = MINIMO_USD, color = "Precio Mínimo"), 
            size = 0.75, linetype = "dashed") +  # Linha da série temporal minimos
  geom_line(aes(x = FECHA, y = MAXIMO_USD, color = "Precio Máximo"), 
            size = 0.75, linetype = "dashed") +  # Linha da série temporal maximos
  geom_point(data = maximos, aes(x = FECHA, y = PROMEDIO_USD, color = "Máximo"), 
             size = 1.5, shape = 21, fill = "#636363") +  # Pontos máximos
  geom_point(data = minimos, aes(x = FECHA, y = PROMEDIO_USD, color = "Mínimo"), 
             size = 1.5, shape = 21, fill = "#ece7f2") +  # Pontos mínimos
  scale_color_manual(
    name = "Legenda:",  # Nome da legenda
    values = c("Precio" = "black", 
               "Máximo" = "#636363", # Cor pontos máximos
               "Mínimo" = "#ece7f2", # Cor pontos mínimos
               "Precio Mínimo" = "#969696",       # Cor da linha dos preco mínimo
               "Precio Máximo" = "#969696"),  # Cor linha máximo
    breaks = c()  # Ordem dos itens na legenda
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


## Fazendo Plotagem com área de mínimo e máximo ####

historico_bogota %>% ggplot() +
  geom_line(aes(x = FECHA, y = PROMEDIO_USD, color = "Precio Promedio"),
            size = 0.8) +  # Linha da média
  geom_ribbon(aes(x = FECHA, ymin = MINIMO_USD, ymax = MAXIMO_USD, 
                  fill = "Amplitude de Preços"), alpha = 0.3) +  # Preenchimento entre mínimo e máximo
  scale_color_manual(
    name = "Linhas:",
    values = c("Precio Promedio" = "black")
  ) +
  scale_fill_manual(
    name = "Áreas:",
    values = c("Amplitude de Preços" = "grey50")
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

## Decompondo o preco pelo modelo aditivo ####
decpib <- decompose(x = precios_ts,
                    type = "additive")


#Verificando o tamanho da serie temporal com repeito a original #
length(decpib$x)
length(decpib$trend)
length(decpib$seasonal)
length(decpib$random)
length(historico_bogota$FECHA)

### Transformando o objeto decpib num data frame ####
decpib_df <- data.frame(tempo = historico_bogota$FECHA,
                        serie = unlist(decpib$x),
                        tendencia = unlist(decpib$trend),
                        sazonalidade = unlist(decpib$seasonal),
                        dessazonalizada = precios_ts - decpib$seasonal,
                        erro = unlist(decpib$random)) %>%
  rename(tempo = 1,
         serie = 2,
         tendencia = 3,
         sazonalidade = 4,
         dessazonalizada = 5,
         erro = 6)

#### Plotando a decomposição de forma conjunta ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Price"), size = 1.2) +
  geom_line(aes(x = tempo, y = tendencia, color = "Trend"), size = 1) +
  geom_line(aes(x = tempo, y = sazonalidade, color = "Seasonal"), size = 1.2) +
  geom_line(aes(x = tempo, y = erro, color = "Resid"), size = 1) +
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

# observando cada componente - vide excel
decpib$trend
decpib$seasonal
decpib$random

#### Plotando a decomposição individualmente ####

##### a) Série #####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Price")) +
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
        legend.position = "none") -> decomp_serie

##### b) Sazonalidade ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = sazonalidade, color = "Seasonal")) +
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
        legend.position = "none") -> decomp_sazonalidade

##### c) Tendência ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = tendencia, color = "Trend")) +
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
        legend.position = "none") -> decomp_tendencia

##### d) Erro #####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = erro, color = "Resid")) +
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
        legend.position = "none") -> decomp_erro

##### Plotagem ####
#Dê zoom para uma visualização mais agradável
grid.arrange(decomp_serie,
             decomp_sazonalidade,
             decomp_tendencia,
             decomp_erro,
             ncol = 2)

grid.arrange(decomp_serie,
             decomp_tendencia,
             ncol = 2)

#### Plotando a série dessazonalizada ####
decpib_df %>%
  ggplot() +
  geom_line(aes(x = tempo, y = serie, color = "Série"), size = 1.2) +
  geom_line(aes(x = tempo, y = dessazonalizada, color = "Desestacionalizada"), size = 1) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Legenda:",
       x = NULL,
       y = NULL) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")


## Decompondo o PIB pelo modelo multiplicativo ####
decpib <- decompose(precios_ts, type = "multiplicative")
plot(decpib)


# observando cada componente - vide excel
decpib$trend
decpib$seasonal
decpib$random

# Test Estacionaridade ####
####
## Analisando as séries autoregressivas
####
## Realizar os testes de Estacionariedade
## Precisamos do pacote URCA - Unit Root and Cointegration Test
####
#### Teste de Dickey-Fuller ###
# Ho: A série Não é Estacionária
# H1: A série é Estacionária


## División de la serie temporal ####
precio_entreno <- window(precios_ts, start=c(2012,6), end=c(2023,12))
precio_entreno
precio_teste <- window(precios_ts, start=c(2024,1), end=c(2025,1))
length(precio_teste)
length(precio_entreno)

testeprecio <- ur.df(precio_entreno)
testeprecio
summary(testeprecio)

###Test que indica no es estacional ####
adf.test(precios_ts, alternative = "stationary") 

#Con una diferencia 
#### Debe ser estacionaria
seriedifprecio <- diff(precios_ts)
seriedifprecio
ggtsdisplay(seriedifprecio)
adf.test(seriedifprecio)
testeseriepreciodif <-  ur.df(seriedifprecio)
summary(testeseriepreciodif)

### Selecao Arima ####
arimaprecio <- auto.arima(precio_entreno, trace = T)

# Verificar o modelo ajustado
summary(arimaprecio)

#### 1. Teste de Ljung-Box ####
#segundo o p-value se aceitamos H0 os resíduos não são correlacionados

checkresiduals(arimaprecio)

#### 2. Normalidade dos resíduos ####

ks.test(arimaprecio$residuals, "pnorm", mean(arimaprecio$residuals),
        sd(arimaprecio$residuals))

# confirmada a não existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variãncia
#### 3. Verificar se existe efeitos ARCH ####

ArchTest(arimaprecio$residuals)

### Previsao para a série dos precos do limao ####

prevpreciobog <- forecast::forecast(arimaprecio, h=length(precio_teste))

autoplot(prevpreciobog) +
  theme_bw()

accuracy(prevpreciobog, precio_teste)

#### Grafico da predicao ####
ggplotly(
  autoplot(precio_entreno)+
    autolayer(precio_teste,serie="Valores Reales")+
    autolayer(prevpreciobog$mean, serie="Forecast")+
    labs(x = "Tiempo",
         y = "Precio (USD)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)


## Melhorando o modelo em COP ####

### Ajuste manual do ARIMA (Autoarima) ####
modelo_arima_manual <- Arima(precio_entreno, order = c(1, 0, 1), seasonal = c(0, 1, 1))
summary(modelo_arima_manual)

#### Verifique o AIC e BIC
AIC(modelo_arima_manual)
BIC(modelo_arima_manual)

# Gráficos de autocorrelação dos resíduos
residuos <- residuals(modelo_arima_manual)
Acf(residuos, main = "ACF dos Resíduos")
Pacf(residuos, main = "PACF dos Resíduos")
checkresiduals(modelo_arima_manual)

# Plot dos resíduos ao longo do tempo
plot(residuos, main = "Resíduos ao Longo do Tempo", ylab = "Resíduos")
abline(h = 0, col = "red")

#### Previsões e avaliação ####
previsoes_manual <- forecast(modelo_arima_manual, h = length(precio_teste))

autoplot(previsoes_manual) +
  theme_bw()

accuracy(previsoes_manual, precio_teste)

#### Gráfico de Predicao ####
ggplotly(
  autoplot(precio_entreno)+
    autolayer(precio_teste,serie="Valores Reales")+
    autolayer(previsoes_manual$mean, serie="Forecast")+
    labs(x = "Tiempo",
         y = "Precio (USD)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

### Ajuste do SARIMA ####
modelo_sarima <- Arima(precio_entreno, order = c(2, 1, 1), seasonal = c(2, 1, 1))
summary(modelo_sarima)

# Verifique o AIC e BIC
AIC(modelo_sarima)
BIC(modelo_sarima)

# Gráficos de autocorrelação dos resíduos
residuos <- residuals(modelo_sarima)
Acf(residuos, main = "ACF dos Resíduos")
Pacf(residuos, main = "PACF dos Resíduos")
checkresiduals(modelo_sarima)

# Plot dos resíduos ao longo do tempo
plot(residuos, main = "Resíduos ao Longo do Tempo", ylab = "Resíduos")
abline(h = 0, col = "red")

#### Previsões e avaliação ####
previsoes_sarima <- forecast(modelo_sarima, h = length(precio_teste))

autoplot(previsoes_sarima) +
  theme_bw()

accuracy(previsoes_sarima, precio_teste)

##### Gráfico de predicao ####
ggplotly(
  autoplot(precio_entreno)+
    autolayer(precio_teste,serie="Real Values")+
    autolayer(previsoes_sarima$mean, serie="Forecast")+
    labs(x = "Tiempo",
         y = "Precio (USD)")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)


### Ajuste do modelo ARIMA-GARCH ####
library(rugarch)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(2, 1)),  # ARIMA(2,1,1)
  distribution.model = "norm"
)

# Ajuste do modelo
modelo_garch <- ugarchfit(spec, data = precio_entreno)
modelo_garch

# Previsões
previsoes_garch <- ugarchforecast(modelo_garch, n.ahead = length(precio_teste))

# Plot das previsões da média condicional
plot(previsoes_garch, type = "l", col = "blue",
     main = "Previsões da Média Condicional", xlab = "Tempo", ylab = "Preço")

previsoes_garch

### Comparação de modelos ####
resultados <- data.frame(
  Modelo = c("ARIMA", "SARIMA", "ARIMA-GARCH"),
  AIC = c(AIC(modelo_arima_manual), AIC(modelo_sarima), infocriteria(modelo_garch)["AIC"]),
  RMSE = c(accuracy(previsoes_manual, precio_teste)["Test set", "RMSE"],
           accuracy(previsoes_sarima, precio_teste)["Test set", "RMSE"],
           sqrt(mean((precio_teste - previsoes_garch@forecast$seriesFor)^2)))
)

print(resultados)

