##################################
######        SELIC         #######
##################################

# install.packages(c("Quandl", "dygraphs", "magrittr", "PerformanceAnalytics", "quantmod"))

source("/cloud/project/install_and_load_packages.R") 
install.packages("dplyr")
library("dplyr")
library("ggpubr")
library(quantmod)
library(PerformanceAnalytics)
library(forecast)
library("fBasics")

# Carregar pacotes necessários
suppressMessages(require(Quandl))
suppressMessages(require(dygraphs))
suppressMessages(require(magrittr))
suppressMessages(require(PerformanceAnalytics))
suppressMessages(require(quantmod))

# Definir sua api key
Quandl.api_key('3gMh2BSgyDSpD4qxrsaT')
#Quandl.api_key('NjGc22-41R7zD_K7Pt7z')

# Coletar o dado da SELIC. Observe que adicionamos BCB/ ao código da série temporal. Sempre usaremos BCB/ 
# para coletar dados do BACEN por meio do Quandl. Ele tem o significado de determinar de qual banco de 
# dados o Quandl deve buscar pela série que o número definido. Como padrão o Quandl coletará os dados na
# periodicidade divulgada pelo BACEN.
selic_copom <- Quandl('BCB/432')

# Coletar a mesma informação para um período específico
selic_copom <- Quandl('BCB/432', start_date = "1999-05-03", end_date = "2022-12-07")

# Coletar definindo apenas a data inicial 
selic_copom <- Quandl('BCB/432', start_date = "1999-05-03")

# Coletar definindo a periodicidade de interesse
# Opções: daily, weekly, monthly, quarterly, annual
selic_copom <- Quandl("BCB/432", collapse = "quarterly", start_date = "1999-05-03")

# Coletar fazendo alterações nos dados. Transformações nos dados permitidas pelo Quandl:
# - diff: z[t] = y[t] – y[t-1] (diferença)
# - rdiff: z[t] = (y[t] – y[t-1]) / y[t-1] (diferença %)
# - rdiff_from: z[t] = (y[latest] – y[t]) / y[t] (incremento % em relação à última observação)
# - cumul:  z[t] = y[0] + y[1] + … + y[t] (soma acumulativa)
# - normalize: z[t] = y[t] ÷ y[0] * 100 (série iniciar em 100)
selic_copom <- Quandl("BCB/432", transform = "diff", start_date = "1999-05-03")

# Coletar definido o tipo de dado que queremos no R
# - ts: série temporal
# - zoo: objeto zoo 
# - xts: no formato xts
# Detalhes sobre a diferença entre os tipos no link abaixo
# https://stackoverflow.com/questions/33714660/what-is-the-difference-the-zoo-object-and-ts-object-in-r
selic_copom <- Quandl("BCB/432", start_date = "1999-05-03", type = "ts")
selic_copom <- Quandl("BCB/432", start_date = "1999-05-03", type = "xts")

# Alterar o nome da coluna do objeto para 
colnames(selic_copom) <- "Selic"

# Visualizar os dados usando o pacote dygraphs. Mais detalhes em
# https://rstudio.github.io/dygraphs/
dygraphs::dygraph(selic_copom, main = "Sistema Especial de Liquidação e de Custódia - Copom (SELIC)") %>% dyRangeSelector()

# Gráfico da série temporal
par(mfrow=c(1,1))
plot(selic_copom, xlab = "", ylab = "", main = "Variação percentual da Selic - mensal")


#####
##   2: Se necessário, transformar os dados para estabilizar a variância (logaritmo dos dados, variação ou retorno, por exemplo)
#####
# Calcular a SELIC usando o logaritmo [lnPt-ln(Pt-1)]
# Gráfico da série temporal dos retornos.

logselic <- log(selic_copom[2:316]) - log(selic_copom[1:316])

plot.ts(logselic, xlab = "", ylab = "", main = "Variação SELIC")

par(mfrow=c(3,1)) 
plot.ts(selic_copom, xlab = "", ylab = "", main = "SELIC")
plot.ts(log(selic_copom), xlab = "", ylab = "", main = "Log-Preço da taxa Selic")
plot.ts(logselic, xlab = "", ylab = "", main = "Variação Selic")

## 3ºPasso: Gráfico da Série

plot(selic_copom, main = "Gráfico da série original")  
dygraph(selic_copom)
#plot(decompose(selic_copom))

## Devido à grande variabilidade dos dados, vamos trabalhar com o logaritmo dos dados

logselic <- log(selic_copom)
plot.ts(logselic, main = "Gráfico do Log da Série original")

####################################################################################################################################

## 4ºPasso: FAC da série transformada

  logselic %>% ggtsdisplay(main = "Fac do Log-Dados")

####################################################################################################################################

## 5ºPasso: Teste de Dickey - Fuller

adf_logselic <- fUnitRoots::adfTest(logselic, lags = 40, type = c("nc"))
print(adf_logselic)

####################################################################################################################################

## 6ºPasso: Realizar a Diferenciação da parte NÃO SAZONAL e analisar a FAC dos dados resultantes

dados_log_diff <- timeSeries::diff(logselic, lag = 1, differences = 1)

####################################################################################################################################

## 7ºPasso: Analisar a FAC dos dados resultantes

logselic %>% ggtsdisplay(main = "Fac do Log-Dados-Dif")

####################################################################################################################################

## 8ºPasso: Realizando a diferenciação da parte sazonal dos dados e analisando a FAC correspondente

dados_log_diff_saz <- timeSeries::diff(dados_log_diff, lag = 40, differences = 1, lag.max = 48)
dados_log_diff_saz %>% ggtsdisplay(main = "Fac do Log-Dados-Dif-Saz")

####################################################################################################################################

## 9ºPasso: Realizando o teste de Dickey-Fuller da série diferenciada

adf_dados_log_diff_saz <- fUnitRoots::adfTest(dados_log_diff_saz, lags = 12, type = c("nc"))
print(adf_dados_log_diff_saz)

####################################################################################################################################

## 10ºPasso: Analisar FAC/FACP do modelo estacionário

dados_log_diff_saz %>% ggtsdisplay(main = "Fac do Log-Dados-Dif-Saz")

####################################################################################################################################

## 11ºPasso: Estimar todas as combinações possíveis de modelo SARIMA(p,d,q)(P,D,Q)_S

pars_dados <- expand.grid (ar = 0, diff = 1, ma = 0, ars = 0:1, diffs = 1:1, mas = 0:1)

modelos <- list()

for (i in 1:nrow(pars_dados)) {
  modelos[[i]] <- arima(x = selic_copom, order = unlist(pars_dados[i, 1:3]), 
                        seasonal = list(order = unlist(pars_dados[i,4:6]), period = 40), method = "ML")
}

log_verossimilhanca <- list()

for (i in 1:length(modelos)){
  log_verossimilhanca[[i]] <- modelos[[i]]$loglik
}

aic_sarima <- list()

for (i in 1:length(modelos)) {
  aic_sarima[[i]] <- stats::AIC(modelos[[i]])
}

bic_sarima <- list()

for (i in 1:length(modelos)) {
  bic_sarima[[i]] <- stats::BIC(modelos[[i]])
}

quant_parametros <- list()

for (i in 1:length(modelos)) {
  quant_parametros[[i]] <- length(modelos[[i]]$coef)+1 
}

especificacao <- paste0("SARIMA",pars_dados$ar, pars_dados$diff, pars_dados$ma, pars_dados$ars,
                        pars_dados$diffs, pars_dados$mas)

resultado <- data.frame(especificacao,
                        ln_verossimilhanca = unlist(log_verossimilhanca),
                        quant_parametros = unlist(quant_parametros),                         
                        aic = unlist(aic_sarima), 
                        bic = unlist(bic_sarima))

print(resultado)

best1 <- which.min(resultado$aic)
print(best1)

best2 <- which.min(resultado$bic)
print(best2)

####################################################################################################################################  

## 12ºpasso: Escolher o melhor modelo baseado no AIC/BIC

coeftest(modelos[[best1]])
coeftest(modelos[[best2]])

####################################################################################################################################

## 13ºPasso: Validação do modelo

acf_arima101_est <- stats::acf(modelos[[6]]$residuals, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_arima101_est, main = "", ylab = "", xlab = "Defasagem")
title("Função de Autocorrelação (FAC) dos Resíduos do ARIMA(1,0,1)", adj = 0.5, line = 1)
Box.test(modelos[[best2]]$residuals,type="Ljung",lag = 1)

# Teste de heterocedasticidade condicional
#  - H0: quadrado dos resíduos são não autocorrelacionados
#  - H1: quadrado dos resíduos são autocorrelacionados
acf_arima101_square <- acf(modelos[[6]]$residuals^2, na.action = na.pass, plot = FALSE, lag.max = 15)
plot(acf_arima101_square, main = "", ylab = "", xlab = "Defasagem")
title("FAC do quadrado dos resíduos do ARIMA(1,0,1)", adj = 0.5, line = 1)
Box.test(modelos[[best2]]$residuals^2,type="Ljung",lag = 1)
archTest(modelos[[best2]]$residuals)

# Teste de Normalidade dos resíduos. As hipóteses para os dois testes são:
#  - H0: resíduos normalmente distribuídos
#  - H1: resíduos não são normalmente distribuídos

normalTest(modelos[[6]]$residuals, method = "jb")
jarque.bera.test(na.remove(modelos[[best2]]$residuals))

####################################################################################################################################

## 14ºPasso: Previsões utilizando o modelo

modelo_prev <- arima(selic_copom, order = c(0,1,0), seasonal = list(order=c(0,1,1), period = 40))
prev <- predict(modelo_prev, n.ahead = 40)
print(prev)

## Gráfico da previsão

forecast::forecast(object = modelo_prev, h = 40, level = 0.95)
plot(forecast::forecast(object = modelo_prev, h = 40, level = 0.95))

## Gráfico Real x Previsto

par(mfrow=c(1,1))
fitted_modelo <- stats::fitted(modelo_prev)
plot(fitted_modelo, type = "l", lty = 1, col = 2)

