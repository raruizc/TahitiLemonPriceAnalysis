# Limpar o ambiente de trabalho
rm(list = ls(all = TRUE))

# Carregar pacotes necessários
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl,      # Para ler arquivos Excel
  dplyr,       # Para manipulação de dados
  tsibble,     # Para trabalhar com séries temporais
  fable,       # Para modelos ARIMA
  fabletools,  # Ferramentas adicionais para fable
  ggplot2,      # Para visualização de dados
  tidyverse,
  plotly#,
  #lubridate #Manipulacao de datas
)

# Carregar os dados do arquivo Excel
historico_bogota <- read_excel("Data/Bogota_2013_2025_Daily.xls")

# Visualizar a estrutura dos dados
str(historico_bogota)

# Transformar a coluna FECHA para o tipo Date
historico_bogota <- historico_bogota %>%
  mutate(FECHA = as.Date(paste0(FECHA, "-01"), format = "%Y-%m-%d"))

# Verificar as primeiras e últimas linhas dos dados
head(historico_bogota)
tail(historico_bogota)

historico_bogota_1 <- historico_bogota %>%
  mutate(PROMEDIO = spline(FECHA, PROMEDIO, xout = FECHA)$y,
         MINIMO = spline(FECHA, MINIMO, xout = FECHA)$y,
         MAXIMO = spline(FECHA, MAXIMO, xout =FECHA)$y
  )

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
historico_bogota_1 <- historico_bogota_1 %>%
  left_join(taxa_cambio, by = "FECHA")

# Verificar o resultado 
head(historico_bogota_1) # Verificamos que tem NA

### 4. Interpolação com splines de taxa de cambio #### 
#Isso é útil quando os dados têm uma tendência não linear.
historico_bogota_1 <- historico_bogota_1 %>%
  mutate(TAXA_CAMBIO = spline(FECHA, TAXA_CAMBIO, xout = FECHA)$y)

# Verificar o resultado
head(historico_bogota_1)

### 5. Converter os Valores para USD ####

historico_bogota_1 <- historico_bogota_1 %>%
  mutate(
    PROMEDIO_USD = PROMEDIO / TAXA_CAMBIO,
    MINIMO_USD = MINIMO / TAXA_CAMBIO,
    MAXIMO_USD = MAXIMO / TAXA_CAMBIO
  )

# Verificar o resultado
head(historico_bogota_1)
colnames(historico_bogota_1)


# Criar um tsibble (objeto de série temporal) com frequência mensal
dados_tsibble <- historico_bogota_1 %>%
  as_tsibble(index = FECHA)

# Verificar se há lacunas nos dados
has_gaps(dados_tsibble)  # deve retornar FALSE

# # Preencher lacunas temporais com NA (garantir que todos os meses estejam presentes)
# dados_tsibble <- dados_tsibble %>%
#   fill_gaps()

# Filtrar os dados para o período de treino (2012-06 até 2023-12)
treino_tsibble <- dados_tsibble %>%
  filter(FECHA >= as.Date("2013-01-01") & FECHA <= as.Date("2023-12-31"))

# Filtrar os dados para o período de teste (2024-01 até 2025-01)
teste_tsibble <- dados_tsibble %>%
  filter(FECHA >= as.Date("2024-01-01") & FECHA <= as.Date("2025-01-01"))

# Ajustar o modelo ARIMA usando o pacote fable
modelo_arima_fable <- treino_tsibble %>%
  model(arima = ARIMA(PROMEDIO))

# Verificar o resumo do modelo usando report()
report(modelo_arima_fable)

# Fazer previsões para o período de teste
previsoes_arima <- modelo_arima_fable %>%
  forecast(h = nrow(teste_tsibble))
previsoes_arima$.mean
# Visualizar as previsões
autoplot(previsoes_arima) +
  autolayer(teste_tsibble, PROMEDIO, color = "red") +
  labs(title = "Previsão ARIMA para o Preço Médio do Limão Tahití (COP)",
       x = "Data",
       y = "Preço Médio (COP)") +
  theme_bw()

# Converter os dados de treino, teste e previsões para data frames
preco_treino_df <- treino_tsibble %>%
  as_tibble() %>%
  select(FECHA, PROMEDIO) %>%
  rename(Valor = PROMEDIO) %>%
  mutate(Tipo = "Treino")

preco_teste_df <- teste_tsibble %>%
  as_tibble() %>%
  select(FECHA, PROMEDIO) %>%
  rename(Valor = PROMEDIO) %>%
  mutate(Tipo = "Valores Reales")

previsoes_df <- previsoes_arima %>%
  as_tibble() %>%
  select(FECHA, .mean) %>%
  rename(Valor = .mean) %>%
  mutate(Tipo = "Forecast")

# Combinar os data frames
dados_plot <- bind_rows(preco_treino_df, preco_teste_df, previsoes_df)

# Criar o gráfico com plotly
grafico <- plot_ly(dados_plot, x = ~FECHA, y = ~Valor, color = ~Tipo, colors = viridis::viridis(3)) %>%
  add_lines() %>%
  layout(
    title = "Previsão ARIMA para o Preço Médio do Limão Tahití (COP)",
    xaxis = list(title = "Tiempo"),
    yaxis = list(title = "Precio (COP)", tickformat = ",.0f"),
    legend = list(title = list(text = "Legenda"))
  )


# Exibir o gráfico
grafico


# Avaliar a precisão do modelo
accuracy(previsoes_arima, teste_tsibble)

# Verificar os resíduos do modelo
checkresiduals(modelo_arima_fable)

# Testar a normalidade dos resíduos
residuos <- residuals(modelo_arima_fable)
ks.test(residuos$.resid, "pnorm", mean(residuos$.resid), sd(residuos$.resid))