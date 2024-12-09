#-----------------------------------------------------------------------
# Controle de Processos Industriais
# Metodologia de Superfície de Resposta 
#-----------------------------------------------------------------------

# artigo: 
# browseURL("https://doi.org/10.1016/j.csite.2020.100816")

# Factor 1 -> A: Solar radiation (W/m^2)
# Factor 2 -> B: Ambient Temperature (ºC)
# Factor 3 -> C: Water Depth (m)
# Factor 4 -> D: thickness of the insulation (m)
# Response 1 -> y: Daily productivity

#-----------------------------------------------------------------------
# Pacotes necessários
library(tidyverse)
library(deSolve)
library(rsm)
library(ggplot2)

#-----------------------------------------------------------------------
# Recriando os Dados Originais

dados <- tibble::tribble(
  ~run, ~A, ~B, ~C, ~D, ~y,
  1, 600, 35, 0.05, 0.04, 0.38761,
  2, 600, 25, 0.03, 0.04, 0.45108,
  3, 600, 35, 0.03, 0.04, 0.62997,
  4, 300, 45, 0.01, 0.07, 0.84856,
  5, 600, 35, 0.03, 0.04, 0.62997,
  6, 600, 35, 0.03, 0.04, 0.62997,
  7, 600, 35, 0.03, 0.07, 0.63244,
  8, 900, 45, 0.01, 0.07, 2.53573,
  9, 900, 45, 0.05, 0.01, 0.76763,
  10, 900, 25, 0.01, 0.07, 2.49854,
  11, 300, 35, 0.03, 0.04, 0.32334,
  12, 900, 45, 0.05, 0.07, 0.77491,
  13, 300, 45, 0.01, 0.01, 0.83017,
  14, 900, 25, 0.05, 0.07, 0.4045,
  15, 300, 25, 0.01, 0.01, 0.50184,
  16, 600, 35, 0.01, 0.04, 1.63863,
  17, 600, 45, 0.03, 0.04, 0.82953,
  18, 300, 25, 0.05, 0.07, 0.15661,
  19, 900, 25, 0.05, 0.01, 0.40005,
  20, 900, 45, 0.01, 0.01, 2.48021,
  21, 300, 45, 0.05, 0.07, 0.33008,
  22, 600, 35, 0.03, 0.01, 0.62413,
  23, 600, 35, 0.03, 0.04, 0.62997,
  24, 300, 25, 0.01, 0.07, 0.51539,
  25, 300, 25, 0.05, 0.01, 0.15523,
  26, 900, 25, 0.01, 0.01, 2.42855,
  27, 300, 45, 0.05, 0.01, 0.32757,
  28, 600, 35, 0.03, 0.04, 0.62997,
  29, 900, 35, 0.03, 0.04, 1.02184,
  30, 600, 35, 0.03, 0.04, 0.62997) 


xtabs(~A + ~B + ~C + ~D, data = dados)

# Plackett-Burman


ggplot(dados, aes(y = y, x = C, color = factor(B))) +
  facet_wrap(~A) + geom_point()

dados %>%
  count(A, B, C)

dados_coded <- rsm::coded.data(dados,
                               A ~ (freq - 5) / 2,
                               B ~ (amp - 60) / 20,
                               C ~ (spacing - 35) / 10)

count <- dados %>%
  count(cargo, braking, speed)
count

# Aplicando a codificação aos dados
dados_codificados <- dados %>%
  mutate(
    A = ifelse(cargo > mean(dados$cargo), 1, -1), 
    B = ifelse(braking > mean(dados$braking), 1, -1), 
    C = ifelse(speed > mean(dados$speed), 1, -1) 
  ) %>%
  select(-cargo, -braking, -speed) %>%
  separate(id, into = c("dia1", "dia2", "dia3"), sep = "/", convert = TRUE) %>% 
  pivot_longer(
    cols = starts_with("dia"), 
    names_to = "intervalo", 
    values_to = "dia"
  ) %>%
  arrange(run, intervalo) %>%
  select(-intervalo) 

#-----------------------------------------------------------------------
# Simulação com deSolve
modelo_ode <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dDeltaQ_Q0 <- a * A + b * B + c * C - k * DeltaQ_Q0
    list(c(dDeltaQ_Q0))
  })
}

parameters <- c(a = 0.2, b = 0.1, c = 0.05, k = 0.01)
state <- c(DeltaQ_Q0 = 0.5)
times <- seq(0, 10, by = 0.1)

# Resolver para cada combinação
simulations <- dados_codificados %>%
  mutate(simulation = pmap(list(A = A, B = B, C = C), ~ {
    parms <- c(parameters, A = ..1, B = ..2, C = ..3)
    as.data.frame(ode(y = state, times = times, func = modelo_ode, parms = parms))
  }))

# Expandir os resultados
resultados <- simulations %>%
  select(run, dia, simulation) %>%
  unnest(simulation) %>%
  rename(tempo = time, DeltaQ_Q0 = DeltaQ_Q0)

#-----------------------------------------------------------------------
# Ajuste de Modelo de Superfície de Resposta
dados_codificados <- dados_codificados %>%
  mutate(DeltaQ_Q0 = map_dbl(simulations$simulation, ~ .[["DeltaQ_Q0"]][1]))

modelo0 <- rsm::rsm(DeltaQ_Q0 ~ FO(A, B, C), data = dados_codificados)
summary(modelo0)

#-----------------------------------------------------------------------
# Gráficos de Resultados

# Preparação para Inner e Outer Wheels
dados_codificados <- dados_codificados %>%
  mutate(
    `1L` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `2L` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `3L` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `4L` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `1R` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `2R` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `3R` = DeltaQ_Q0 + runif(n(), -0.05, 0.05),
    `4R` = DeltaQ_Q0 + runif(n(), -0.05, 0.05)
  )


#-----------------------------------------------------------------------
# Gráficos

library(plotly)

# Preparando os dados
dados_long <- dados %>%
  select(-DeltaQ_Q0) %>%
  pivot_longer(
    cols = ends_with("L") | ends_with("R"),
    names_to = "Wheel",
    values_to = "DeltaQ_Q0"
  ) %>%
  mutate(
    Type = ifelse(grepl("L$", Wheel), "Inner Wheels", "Outer Wheels"),
    Group = case_when(
      dia <= 15 ~ "Group 1",
      dia > 15 & dia <= 30 ~ "Group 2",
      TRUE ~ "Group 3"
    )
  )

# Configuração de cores
cores <- c(
  "1L" = "#E63946", "2L" = "#457B9D", "3L" = "#F4A261", "4L" = "#2A9D8F",
  "1R" = "#264653", "2R" = "#E76F51", "3R" = "#D62828", "4R" = "#003049"
)

# Criando o gráfico
ggplot(dados_long, aes(x = factor(dia), y = DeltaQ_Q0, fill = Wheel)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cores) +
  facet_wrap(vars(Group, Type), scales = "free", nrow = 3, labeller = label_value) + # Remove os títulos extras como "Group 1"
  labs(
    title = "Delta Q/Q0",
    x = "Simulation Day",
    y = "Delta Q/Q0",
    fill = "Wheel Position"
  ) +
  # theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centraliza o título
    legend.position = "bottom", # Posiciona a legenda abaixo
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.background = element_blank(), # Remove o fundo do rótulo
    strip.placement = "outside", # Coloca os rótulos fora da área do gráfico
    strip.text = element_text(size = 12, face = "bold") # Estiliza os rótulos
  )

#-----------------------------------------------------------------------

# Superfícies para pares de variáveis fixando as demais em 0.
par(mfrow = c(2, 2))
contour(modelo0, ~ A + B + C,
        at = list(A = 0, B = 0, C = 0),
        col = rainbow(40))
graphics::layout(1)

par(mfrow = c(2, 2))
contour(m1_rsm, ~ A + B + C,
        at = list(A = 0, B = 0, C = 0),
        image = TRUE)
graphics::layout(1)

par(mfrow = c(2, 2))
# range(tb_pred$fit)
persp(m1_rsm, ~ A + B + C,
      zlim = c(20, 50),
      at = list(A = 0, B = 0, C = 0),
      col = viridis::viridis(40),
      contours = "bottom",
      theta = 45,
      phi = 30)
graphics::layout(1)
