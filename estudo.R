#-----------------------------------------------------------------------
# Controle de Processos Industriais
# Metodologia de Superfície de Resposta 
#-----------------------------------------------------------------------

# artigo: 
# browseURL("https://www.tandfonline.com/doi/epdf/10.1080/00423114.2020.1815810?needAccess=true")

# run     <- Simulation ID
# Cargo   <- Lateral deviation cargo [m]
# Braking <- Braking moment [kN.m]
# Speed   <- Vehicle speed [km / h]

#-----------------------------------------------------------------------
# Pacotes necessários
library(lattice)
library(latticeExtra)
library(plotly)
library(shiny)
library(tidyverse)
library(deSolve)
library(ggplot2)
library(rsm)
library(cowplot)

#-----------------------------------------------------------------------
# Recriando os Dados Originais
dados <- tibble::tribble(
  ~run, ~id, ~cargo, ~braking, ~speed,
  1, '1/16/31', -0.048, 1.622, 94,
  2, '2/17/32', 0.048, 1.622, 94,
  3, '3/18/33', -0.048, 6.378, 94,
  4, '4/19/34', 0.048, 6.378, 94,
  5, '5/20/35', -0.048, 1.622, 106,
  6, '6/21/36', 0.048, 1.622, 106,
  7, '7/22/37', -0.048, 6.378, 106,
  8, '8/23/38', 0.048, 6.378, 106,
  9, '9/24/39', -0.080, 4.000, 100,
  10, '10/25/40', 0.080, 4.000, 100,
  11, '11/26/41', 0.000, 0.000, 100,
  12, '12/27/42', 0.000, 8.000, 100,
  13, '13/28/43', 0.000, 4.000, 90,
  14, '14/29/44', 0.000, 0.000, 110,
  15, '15/30/45', 0.000, 8.000, 100
)

xtabs(~cargo + ~braking + ~speed, data = dados)

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
  select(-cargo, -braking, -speed) 



# Separar os IDs em dias e expandir os dados
dados_codificados <- dados_codificados %>%
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

# Definindo o modelo dinâmico
modelo_ode <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dDeltaQ_Q0 <- a * A + b * B + c * C - k * DeltaQ_Q0
    list(c(dDeltaQ_Q0))
  })
}

# Parâmetros do modelo
parameters <- c(a = 0.2, b = 0.1, c = 0.05, k = 0.01)

# Estado inicial
state <- c(DeltaQ_Q0 = 0.5)

# Tempo de simulação
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

# Ajuste de Modelo de Superfície de Resposta
dados_codificados <- dados_codificados %>%
  mutate(DeltaQ_Q0 = map_dbl(simulations$simulation, ~ .[["DeltaQ_Q0"]][1]))

#-----------------------------------------------------------------------
modelo0 <- rsm::rsm(DeltaQ_Q0 ~ FO(A, B, C), data = dados_codificados)
coef(modelo0)

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
