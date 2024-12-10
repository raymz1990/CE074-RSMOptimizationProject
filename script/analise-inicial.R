#-----------------------------------------------------------------------
# Controle de Processos Industriais
# Metodologia de Superfície de Resposta 
#-----------------------------------------------------------------------

# artigo: 
browseURL("https://doi.org/10.1016/j.enconman.2024.118915")

# Factors:
# Factor 1 -> A: Solar multiple
# Factor 2 -> B: Row spacing (m)
# Factor 3 -> C: No. of solar collector assemblies per loop
# Factor 4 -> D: Thermal energy storage size (h)

# Responses:
# Response 1 -> y1: Energy (kWh)
# Response 2 -> y2: Capacity factor (%)
# Response 3 -> y3: LCOE (¢/kWh)
# Response 4 -> y4: Water consumption (m3)
# Response 5 -> y5: Area (Acres)

#-----------------------------------------------------------------------
# Pacotes
library(tidyverse)
library(deSolve)
library(rsm)
library(ggplot2)
library(scales)
library(plotly)

#-----------------------------------------------------------------------
# Recriando os Dados Originais

dados <- tibble::tribble(
  ~run, ~solar, ~spacing, ~assemblies, ~storage, ~y1, ~y2, ~y3, ~y4, ~y5,
  1, 1, 10, 4, 5, 80901840, 18.5, 28.02, 170883, 183,
  2, 5, 10, 4, 5, 213502480, 48.7, 24.81, 457032, 907,
  3, 1, 30, 4, 5, 86828856, 19.8, 27.01, 182988, 548,
  4, 5, 30, 4, 5, 212109744, 48.4, 25.24, 470202, 2721,
  5, 1, 10, 10, 5, 72318936, 16.5, 29.98, 163059, 185,
  6, 5, 10, 10, 5, 216170432, 49.4, 24.71, 457562, 908,
  7, 1, 30, 10, 5, 79653408, 18.2, 28.37, 177969, 554,
  8, 5, 30, 10, 5, 228318656, 52.1, 24.14, 481062, 2724,
  9, 1, 10, 4, 15, 77971872, 17.8, 36.76, 170378, 183,
  10, 5, 10, 4, 15, 333468288, 76.1, 20.4, 656915, 907,
  11, 1, 30, 4, 15, 83925288, 19.2, 35.14, 182493, 548,
  12, 5, 30, 4, 15, 319167552, 72.9, 21.22, 644373, 2721,
  13, 1, 10, 10, 15, 69414704, 15.8, 40.81, 162994, 185,
  14, 5, 10, 10, 15, 339091392, 77.4, 20.14, 681640, 908,
  15, 1, 30, 10, 15, 76591784, 17.5, 37.3, 177942, 554,
  16, 5, 30, 10, 15, 344158976, 78.6, 20.19, 691931, 2724,
  17, 1, 20, 7, 10, 87385928, 20, 30.46, 187418, 366,
  18, 5, 20, 7, 10, 303626368, 69.3, 20.78, 610245, 1817,
  19, 3, 10, 7, 10, 244371072, 55.8, 20.25, 478649, 546,
  20, 3, 30, 7, 10, 259963408, 59.4, 19.65, 507318, 1637,
  21, 3, 20, 4, 10, 256355984, 58.5, 19.78, 497265, 1091,
  22, 3, 20, 10, 10, 248066240, 56.6, 20.14, 495255, 1098,
  23, 3, 20, 7, 5, 214178496, 48.9, 20.84, 423034, 1091,
  24, 3, 20, 7, 15, 276967136, 63.2, 20, 535398, 1091,
  25, 3, 20, 7, 10, 261283136, 59.7, 19.53, 507792, 1091,
  26, 3, 20, 7, 10, 261283136, 59.7, 19.53, 507792, 1091,
  27, 3, 20, 7, 10, 261283136, 59.7, 19.53, 507792, 1091,
  28, 3, 20, 7, 10, 261283136, 59.7, 19.53, 507792, 1091,
  29, 3, 20, 7, 10, 261283136, 59.7, 19.53, 507792, 1091,
  30, 3, 20, 7, 10, 261283136, 59.7, 19.53, 507792, 1091)

xtabs(~~solar + ~spacing + ~assemblies + ~storage, data = dados)

#-----------------------------------------------------------------------
# analise das variaveis individualmente para cada resposta

## Transformação e Preparação dos Dados para Gráficos
plot_dados <- dados %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "y",
    values_to = "valor_y"
  ) %>%
  pivot_longer(
    cols = c(solar, spacing, assemblies, storage),
    names_to = "variavel",
    values_to = "valor_variavel"
  ) %>%
  mutate(
    variavel = case_when(
      variavel == "solar" ~ "A: Geração Solar (W/m²)",
      variavel == "spacing" ~ "B: Distância entre Coletores (m)",
      variavel == "assemblies" ~ "C: Quantidade de Coletores",
      variavel == "storage" ~ "D: Capacidade de Armazenamento (h)",
      TRUE ~ variavel
    ),
    valor_variavel = factor(valor_variavel)
  ) %>%
  group_by(variavel, valor_variavel, y) %>%
  summarise(media_y = mean(valor_y, na.rm = TRUE), .groups = "drop")

## Função para Gerar Gráficos
gerar_grafico <- function(data, resposta, cor, eixo_y) {
  ggplot(data %>% filter(y == resposta),
         aes(y = media_y, x = valor_variavel)) +
    geom_point(size = 3, color = cor) +
    geom_line(aes(group = 1), color = cor) +
    facet_wrap(~variavel, scales = "free_x", ncol = 4) +
    labs(
      title = "Efeitos Principais para as Respostas",
      subtitle = "Médias das Variáveis",
      x = "",
      y = eixo_y
    ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
}

## Gráficos para Cada Resposta
grafico_y1 <- gerar_grafico(plot_dados, "y1", "#FF2400", "Energia (kWh)")
grafico_y2 <- gerar_grafico(plot_dados, "y2", "#4169E1", "Fator Capacidade (%)")
grafico_y3 <- gerar_grafico(plot_dados, "y3", "#32CD32", "LCOE (¢/kWh)")
grafico_y4 <- gerar_grafico(plot_dados, "y4", "#FFD700", "Consumo de Água (m³)")
grafico_y5 <- gerar_grafico(plot_dados, "y5", "#8A2BE2", "Área (Acres)")

## Visualização dos Gráficos
print(grafico_y1)
print(grafico_y2)
print(grafico_y3)
print(grafico_y4)
print(grafico_y5)


#-----------------------------------------------------------------------
# Codificação dos Fatores
dados_coded <- rsm::coded.data(
  dados,
  A ~ (solar - 3) / 2,
  B ~ (spacing - 20) / 10,
  C ~ (assemblies - 7) / 3,
  D ~ (storage - 10) / 5
)

class(dados_coded)
getS3method("print", "coded.data")

print(dados_coded, decode = TRUE)
print(dados_coded, decode = FALSE)


# y3
m1 <- rsm::rsm(y3 ~ SO(A, B, C, D), data = dados_coded)
coef(m1)

summary(m1)

anova1 <- aov(m1)
summary(anova1)

# grafico de pareto dos efeitos padronizados
t_critico <- qt(0.025, df.residual(m1), lower.tail = FALSE)

MSE <- deviance(m1)/df.residual(m1)
SE_coef <- sqrt(MSE/30)
t0 <- m1$coefficients/SE_coef

t_0 <- data.frame(names(coef(m1)), abs(t0))
colnames(t_0) <- c("termo", "t0")

# grafico
pPar <- ggbarplot(data = t_0[-1,],
                  x = "termo", y = "t0",
                  col = "green4",
                  fill = "lightgreen",
                  rotate = TRUE,
                  sort.val = "asc") +
  theme_classic() +
  geom_hline(yintercept = t_critico, col = "darkred") + 
  labs(
    title = "Gráfico de Pareto dos Efeitos",
    subtitle = "Resposta: LCOE (¢/kWh)",
    x = "Termo",
    y = "Efeito"
  )

pPar

# Superfícies para pares de variáveis fixando as demais em 0.
par(mfrow = c(2, 2))
contour(m1, ~ A + B + C + D,
        at = list(A = 0, B = 0, C = 0, D = 0),
        col = rainbow(20))
graphics::layout(1)

par(mfrow = c(2, 2))
contour(m1, ~ A + B + C + D,
        at = list(A = 0, B = 0, C = 0, D = 0),
        image = TRUE)
graphics::layout(1)

par(mfrow = c(2, 2))
# range(dados_pred$fit)
persp(m1, ~ A + B + C + D,
      zlim = c(20, 50),
      at = list(A = 0, B = 0, C = 0, D = 0),
      col = viridis::viridis(40),
      contours = "bottom",
      theta = 45,
      phi = 30)
graphics::layout(1)

#-----------------------------------------------------------------------
# Predição.

# Grid fino com valores das variáveis na escala codificada.
dados_pred <-
  with(dados,
       expand.grid(A = seq(-1, 1, length.out = 31),
                   B = seq(-1, 1, length.out = 31),
                   C = seq(-1, 1, length.out = 3),
                   D = seq(-1, 1, length.out = 3),
                   KEEP.OUT.ATTRS = FALSE)
  )

# Pede o valor predito e o erro-padrão para o valor predito.
dados_pred <-
  predict(m1, newdata = dados_pred, se.fit = TRUE)[c("fit", "se.fit")] |>
  as.data.frame() |>
  bind_cols(dados_pred)
str(dados_pred)

# Gráficos dos valores preditos.
lattice::levelplot(fit ~ A + B | C,
                   data = dados_pred,
                   contour = TRUE,
                   labels = TRUE,
                   aspect = 1)
lattice::wireframe(fit ~ A + B | C,
                   data = dados_pred,
                   screen = list(z = 45, x = -60),
                   shade = TRUE)


# Gráficos interativos.

library(plotly)

# Gráfico de contornos.
dados_pred |>
  filter(C == unique(C)[2]) |>
  plot_ly(data = _,
          x = ~A, y = ~B, z = ~fit,
          type = "contour")

# Use `mesh3d` para dados no formato longo.
dados_pred |>
  filter(C == unique(C)[2]) |>
  plot_ly(data = _,
          x = ~A, y = ~B, 
          z = ~fit,
          intensity = ~fit,
          type = "mesh3d",
          colors = topo.colors(n = 4),
          contour = list(color = "red", show = TRUE, width = 16))

