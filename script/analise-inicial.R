#-----------------------------------------------------------------------
# Controle de Processos Industriais
# Metodologia de Superfície de Resposta 
#-----------------------------------------------------------------------
rm(list=ls())

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
    )
  ) %>%
  group_by(variavel, valor_variavel, y) %>%
  summarise(media_y = mean(valor_y, na.rm = TRUE), .groups = "drop")

## Função para Gerar Gráficos
gerar_grafico <- function(data, resposta, cor, eixo_y) {
  ggplot(data %>% filter(y == resposta),
         aes(y = media_y, x = valor_variavel)) +
    geom_point(size = 3, color = cor) +
    # geom_line(aes(group = 1), color = cor) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = cor) +
    facet_wrap(~variavel, scales = "free_x", ncol = 2) +
    labs(
      title = "Efeitos Principais para as Respostas",
      subtitle = paste("Médias das Variáveis para", eixo_y),
      x = "",
      y = eixo_y
    ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme_classic()
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


#-----------------------------------------------------------------------
# Ajuste do Modelo para y1 (Energy)
m_y1 <- rsm::rsm(y1 ~ SO(A, B, C, D), data = dados_coded)
summary(m_y1)

# ANOVA do modelo y1
anova_y1 <- anova(m_y1)
print(anova_y1)

#-----------------------------------------------------------------------
# Ajuste do Modelo para y3 (LCOE)
m_y3 <- rsm::rsm(y3 ~ SO(A, B, C, D), data = dados_coded)
summary(m_y3)

# ANOVA do modelo y3
anova_y3 <- anova(m_y3)
print(anova_y3)

#-----------------------------------------------------------------------
# Gráfico de Pareto para y1 e y3
gerar_pareto <- function(model, response) {
  t_critico <- qt(0.025, df.residual(model), lower.tail = FALSE)
  MSE <- deviance(model) / df.residual(model)
  SE_coef <- sqrt(MSE / nrow(model$model))
  t0 <- coef(model) / SE_coef
  t0 <- data.frame(Termos = names(t0)[-1], Efeito = abs(t0[-1]))
  
  ggplot(t0, aes(x = reorder(Termos, Efeito), y = Efeito)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") +
    geom_hline(yintercept = t_critico, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = paste("Gráfico de Pareto dos Efeitos"),
      subtitle = paste("Resposta:", response),
      x = "Termos",
      y = "Efeitos Padronizados"
    ) +
    theme_classic()
}

# Gerar gráficos de Pareto
pareto_y1 <- gerar_pareto(m_y1, "Energy (kWh)")
pareto_y3 <- gerar_pareto(m_y3, "LCOE (¢/kWh)")

print(pareto_y1)
print(pareto_y3)

#-----------------------------------------------------------------------
# Superfícies de Resposta
gerar_superficie <- function(model, var_x, var_y, fixed_vars, response) {
  par(mfrow = c(1, 1))
  contour(
    model, 
    ~ A + B + C + D,
    at = fixed_vars,
    xlab = var_x,
    ylab = var_y,
    main = paste("Superfície de Resposta para", response),
    col = terrain.colors(10)
  )
}

# Contornos para y1
gerar_superficie(m_y1, "A", "B", list(C = 0, D = 0), "Energy (kWh)")

# Contornos para y3
gerar_superficie(m_y3, "A", "B", list(C = 0, D = 0), "LCOE (¢/kWh)")

# Superfície 3D
persp(m_y1, ~ A + B + C + D, 
      zlim = c(min(dados$y1), max(dados$y1)),
      theta = 30, phi = 20, 
      col = viridis::viridis(40), 
      contours = "bottom")

#-----------------------------------------------------------------------
# Predições
# Grid para predição
grid_pred <- expand.grid(
  A = seq(-1, 1, length.out = 31),
  B = seq(-1, 1, length.out = 31),
  C = seq(-1, 1, length.out = 3),
  D = seq(-1, 1, length.out = 3)
)

# Predição e erro padrão
pred_y1 <- predict(m_y1, newdata = grid_pred, se.fit = TRUE)
pred_y3 <- predict(m_y3, newdata = grid_pred, se.fit = TRUE)

# Adicionando resultados ao grid
grid_pred <- grid_pred %>%
  mutate(
    fit_y1 = pred_y1$fit,
    fit_y3 = pred_y3$fit,
    se_y1 = pred_y1$se.fit,
    se_y3 = pred_y3$se.fit
  )

#-----------------------------------------------------------------------
# Função para gerar predições e gráficos
gerar_grafico_predicao <- function(model, response_name, response_label, color) {
  # Gerar predições e erro padrão
  dados_pred <- as.data.frame(dados_coded) %>%
    mutate(
      pred = predict(model, newdata = dados_coded),
      se_pred = predict(model, newdata = dados_coded, se.fit = TRUE)$se.fit
    )
  
  # Preparar os dados para o gráfico
  plot_dados <- dados_pred %>%
    pivot_longer(
      cols = c(A, B, C, D),
      names_to = "variavel",
      values_to = "valor_variavel"
    ) %>%
    mutate(
      variavel = case_when(
        variavel == "A" ~ "A: Geração Solar (W/m²)",
        variavel == "B" ~ "B: Distância entre Coletores (m)",
        variavel == "C" ~ "C: Quantidade de Coletores",
        variavel == "D" ~ "D: Capacidade de Armazenamento (h)",
        TRUE ~ variavel
      )
    ) %>%
    group_by(variavel, valor_variavel) %>%
    summarise(media_pred = mean(pred, na.rm = TRUE), .groups = "drop")
  
  # Gerar o gráfico
  ggplot(plot_dados, aes(y = media_pred, x = as.numeric(valor_variavel))) +
    geom_point(size = 3, color = color) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = color) +
    facet_wrap(~variavel, scales = "free_x", ncol = 2) +
    labs(
      title = "Efeitos Principais para as Respostas",
      subtitle = paste("Médias das Variáveis para", response_label),
      x = "",
      y = response_label
    ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_continuous(labels = scales::number_format()) +
    theme_classic()
}

#-----------------------------------------------------------------------
# Gráficos para y1 (Energy) e y3 (LCOE)
grafico_y1 <- gerar_grafico_predicao(
  model = m_y1,
  response_name = "y1",
  response_label = "Energia (kWh)",
  color = "#FF2400"
)

grafico_y3 <- gerar_grafico_predicao(
  model = m_y3,
  response_name = "y3",
  response_label = "LCOE (¢/kWh)",
  color = "#32CD32"
)

#-----------------------------------------------------------------------
# Exibir os gráficos
print(grafico_y1)
print(grafico_y3)


#-----------------------------------------------------------------------

# graficos plotly

#-----------------------------------------------------------------------
# Ajuste do Modelo para y1 (Energy) com Curvatura
m_y1_curvature <- rsm::rsm(y1 ~ SO(A, C, D), data = dados_coded)

#-----------------------------------------------------------------------
# Grid para Predição - A e C
grid_pred_AC <- expand.grid(
  A = seq(-1, 1, length.out = 50),
  C = seq(-1, 1, length.out = 50),
  B = 0,  # Fixando B em 0
  D = 0   # Fixando D em 0 para análise de A e C
)

# Adicionando Predições para A e C
grid_pred_AC <- grid_pred_AC %>%
  mutate(
    fit = predict(m_y1_curvature, newdata = grid_pred_AC)
  )

# Convertendo para Forma de Array - A e C
array_pred_AC <- list(
  A = unique(grid_pred_AC$A),
  C = unique(grid_pred_AC$C),
  fit = matrix(grid_pred_AC$fit, nrow = length(unique(grid_pred_AC$A)))
)

#-----------------------------------------------------------------------
# Grid para Predição - A e D
grid_pred_AD <- expand.grid(
  A = seq(-1, 1, length.out = 50),
  D = seq(-1, 1, length.out = 50),
  B = 0,  # Fixando B em 0
  C = 0   # Fixando C em 0 para análise de A e D
)

# Adicionando Predições para A e D
grid_pred_AD <- grid_pred_AD %>%
  mutate(
    fit = predict(m_y1_curvature, newdata = grid_pred_AD)
  )

# Convertendo para Forma de Array - A e D
array_pred_AD <- list(
  A = unique(grid_pred_AD$A),
  D = unique(grid_pred_AD$D),
  fit = matrix(grid_pred_AD$fit, nrow = length(unique(grid_pred_AD$A)))
)

#-----------------------------------------------------------------------
# Gráfico 3D Interativo com Plotly

library(plotly)

# Gráfico Interativo para A e C
plot_AC <- plot_ly(
  x = array_pred_AC$A,
  y = array_pred_AC$C,
  z = array_pred_AC$fit
  ) %>%
  add_surface(
    colors = viridis::turbo(50),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>%
  layout(
    title = "Interação entre A (Geração Solar) e C (Quantidade de Coletores)",
    scene = list(
      xaxis = list(title = "A: Geração Solar (W/m²)"),
      yaxis = list(title = "C: Quantidade de Coletores"),
      zaxis = list(title = "Energia (kWh)")
    ),
    showlegend = FALSE
  )
plot_AC


# Gráfico Interativo para A e D
plot_AD <- plot_ly(
  x = array_pred_AD$A,
  y = array_pred_AD$D,
  z = array_pred_AD$fit
) %>%
  add_surface(
    colors = viridis::turbo(50),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>%
  layout(
    title = "Interação entre A (Geração Solar) e D (Capacidade de Armazenamento)",
    scene = list(
      xaxis = list(title = "A: Geração Solar (W/m²)"),
      yaxis = list(title = "D: Capacidade de Armazenamento (h)"),
      zaxis = list(title = "Energia (kWh)")
    )
  )

#-----------------------------------------------------------------------
# Exibindo os Gráficos Interativos
plot_AC
plot_AD

#-----------------------------------------------------------------------
# Ajuste do Modelo para y3 (LCOE)
m_y3 <- rsm::rsm(y3 ~ SO(A, B, C, D), data = dados_coded)

#-----------------------------------------------------------------------
# Grids para Predição

# Grid para A e B
grid_pred_AB <- expand.grid(
  A = seq(-1, 1, length.out = 50),
  B = seq(-1, 1, length.out = 50),
  C = 0,  # Fixando C em 0
  D = 0   # Fixando D em 0 para análise de A e B
) %>%
  mutate(fit = predict(m_y3, newdata = .))

# Grid para A e C
grid_pred_AC <- expand.grid(
  A = seq(-1, 1, length.out = 50),
  C = seq(-1, 1, length.out = 50),
  B = 0,  # Fixando B em 0
  D = 0   # Fixando D em 0 para análise de A e C
) %>%
  mutate(fit = predict(m_y3, newdata = .))

# Grid para A e D
grid_pred_AD <- expand.grid(
  A = seq(-1, 1, length.out = 50),
  D = seq(-1, 1, length.out = 50),
  B = 0,  # Fixando B em 0
  C = 0   # Fixando C em 0 para análise de A e D
) %>%
  mutate(fit = predict(m_y3, newdata = .))

#-----------------------------------------------------------------------
# Convertendo Grids para Formato de Array

# A e B
array_pred_AB <- list(
  A = unique(grid_pred_AB$A),
  B = unique(grid_pred_AB$B),
  fit = matrix(grid_pred_AB$fit, nrow = length(unique(grid_pred_AB$A)))
)

# A e C
array_pred_AC <- list(
  A = unique(grid_pred_AC$A),
  C = unique(grid_pred_AC$C),
  fit = matrix(grid_pred_AC$fit, nrow = length(unique(grid_pred_AC$A)))
)

# A e D
array_pred_AD <- list(
  A = unique(grid_pred_AD$A),
  D = unique(grid_pred_AD$D),
  fit = matrix(grid_pred_AD$fit, nrow = length(unique(grid_pred_AD$A)))
)

#-----------------------------------------------------------------------
# Gráficos 3D Interativos com Plotly

library(plotly)

# Gráfico Interativo para A e B
plot_AB <- plot_ly(
  x = array_pred_AB$A,
  y = array_pred_AB$B,
  z = array_pred_AB$fit
) %>%
  add_surface(
    colors = viridis::inferno(50),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>%
  layout(
    title = "Interação entre A (Geração Solar) e B (Distância entre Coletores)",
    scene = list(
      xaxis = list(title = "A: Geração Solar (W/m²)"),
      yaxis = list(title = "B: Distância entre Coletores (m)"),
      zaxis = list(title = "LCOE (¢/kWh)")
    )
  )

# Gráfico Interativo para A e C
plot_AC <- plot_ly(
  x = array_pred_AC$A,
  y = array_pred_AC$C,
  z = array_pred_AC$fit
) %>%
  add_surface(
    colors = viridis::inferno(50),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>%
  layout(
    title = "Interação entre A (Geração Solar) e C (Quantidade de Coletores)",
    scene = list(
      xaxis = list(title = "A: Geração Solar (W/m²)"),
      yaxis = list(title = "C: Quantidade de Coletores"),
      zaxis = list(title = "LCOE (¢/kWh)")
    )
  )

# Gráfico Interativo para A e D
plot_AD <- plot_ly(
  x = array_pred_AD$A,
  y = array_pred_AD$D,
  z = array_pred_AD$fit
) %>%
  add_surface(
    colors = viridis::inferno(50),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>%
  layout(
    title = "Interação entre A (Geração Solar) e D (Capacidade de Armazenamento)",
    scene = list(
      xaxis = list(title = "A: Geração Solar (W/m²)"),
      yaxis = list(title = "D: Capacidade de Armazenamento (h)"),
      zaxis = list(title = "LCOE (¢/kWh)")
    )
  )

#-----------------------------------------------------------------------
# Exibindo os Gráficos

plot_AB
plot_AC
plot_AD

      
