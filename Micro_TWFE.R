
# A empresa lançou um programa piloto no "Delivery" (frete grátis fixo) em 5 cidades de médio porte,
# mantendo outras 5 cidades similares como controle. O piloto rodou por 6 meses. 
# Qual é o impacto deste programa? O programa apresenta canibalização? 



# --------------------------------------------------------------------------------------------
# 1. IMPORTAR PACOTES E ORGANIZAR DADOS
# --------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(broom)
library(fixest)
library(stringr)

dados <- read.csv("C:/Users/leona/Downloads/IFOOD/Micro/dataset_micro_case_delivery.csv")

# 1.1) Criar dummy para definir se mês analisado é pré ou pós-tratamento (>= 7)
dados <- dados %>% mutate(post = if_else(month >= 7, 1L, 0L))

# 1.2) Converter tipos das variáveis para categórico e inteiro
dados <- dados %>%  mutate(user_id = as.factor(user_id), city_id = as.factor(city_id),
                           is_treated_city = as.integer(is_treated_city))

# 1.3) Ordenar painel
dados <- dados %>% arrange(user_id, month)

# 1.4) Preparar dados para plot
dados_mes <- dados %>% group_by(month, is_treated_city) %>%
  summarise(
    mean_orders = mean(orders_count),
    mean_spend  = mean(monthly_spend),
    .groups = "drop")




# --------------------------------------------------------------------------------------------
# 2. ANÁLISE EXPLORATÓRIA
# --------------------------------------------------------------------------------------------

# 2.1) Calcular estatísticas descritivas
media_resumida <- dados %>% group_by(is_treated_city, post) %>%
  summarise(
  mean_orders=mean(orders_count),
  mean_spend=mean(monthly_spend),
  mean_ticket=mean(monthly_spend) / mean(orders_count),
  .groups = "drop") %>%
  mutate(across(c(mean_spend, mean_ticket), \(x) format(round(x, 2), nsmall = 2)))

print(media_resumida)

# 2.2) Gráfico dos pedidos médios
ggplot(dados_mes, aes(x = month, y = mean_orders, color = factor(is_treated_city))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +  # Adiciona pontos nos meses
  geom_vline(xintercept = 6, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "#2C3E50", "1" = "#E74C3C"),  # Cores mais distintas
    labels = c("0" = "Controle", "1" = "Tratadas")
  ) +
  scale_x_continuous(breaks = 1:12) +  # Marca todos os meses
  labs(
    title = "Evolução dos Pedidos ao Longo do Tempo",
    subtitle = "Comparação entre cidades tratadas e controle",
    x = "Mês",
    y = "Pedidos médios por usuário",
    color = "Grupo"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# 2.3) Gráfico dos gastos médios
ggplot(dados_mes, aes(x = month, y = mean_spend, color = factor(is_treated_city))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +  
  geom_vline(xintercept = 6, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_color_manual(
    values = c("0" = "#2C3E50", "1" = "#E74C3C"), 
    labels = c("0" = "Controle", "1" = "Tratadas")
  ) +
  scale_x_continuous(breaks = 1:12) +  
  labs(
    title = "Evolução dos Gastos ao Longo do Tempo",
    subtitle = "Comparação entre cidades tratadas e controle",
    x = "Mês",
    y = "Gastos médios por usuário",
    color = "Grupo"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Aqui já há evidências brutas (não controladas) de efeito.
# Gastos e Pedidos aumentam para TRATADAS, mas não para CONTROLES.
# Não sugere canibalização: pedidos e gastos aumentam juntos.




# --------------------------------------------------------------------------------------------
# 3. MODELOS DIFF-IN-DIFF
# -------------------------------------------------------------------------------
# 3.1) Modelos com efeitos fixos de indivíduo e de tempo com clusterização de erros por cidade

  # 3.1.1) Modelos para pedidos e gastos
  mod_pedidos <- feols(orders_count ~ is_treated_city*post  | user_id + month, data = dados, cluster = ~ city_id)
  mod_gastos  <- feols(monthly_spend ~ is_treated_city*post | user_id + month, data = dados, cluster = ~ city_id)

  # 3.1.2) Modelo adicional para ticket médio
  dados_ticket <- dados %>% filter(orders_count > 0) %>%
  mutate(ticket_medio = monthly_spend / orders_count)

  mod_ticket <- feols(ticket_medio ~ is_treated_city*post | user_id + month, data = dados_ticket, cluster = ~ city_id)

etable(mod_pedidos, mod_gastos, mod_ticket, se='cluster', cluster='city_id')

# Usuários em cidades TRATADAS passaram a:
  # Fazer  +0.49 pedidos por mês.
  # Gastar +R$12,42 por mês.
  # Reduzir ticket médio em -R$0,73 por pedido.


# 3.2) Calcular ATT em %

# Para pedidos e gastos: TODAS as observações
media_pre <- dados %>% 
  filter(month < 7, is_treated_city == 1) %>%
  summarise(
    pre_pedidos = mean(orders_count),
    pre_gastos  = mean(monthly_spend)
  )

# Para ticket: apenas observações COM pedidos
media_pre_ticket <- dados %>% 
  filter(month < 7, is_treated_city == 1, orders_count > 0) %>%
  mutate(ticket_medio = monthly_spend / orders_count) %>%
  summarise(pre_ticket = mean(ticket_medio))

att_pedidos <- coef(mod_pedidos)["is_treated_city:post"]
att_gastos  <- coef(mod_gastos)["is_treated_city:post"]
att_ticket  <- coef(mod_ticket)["is_treated_city:post"]

att_pedidos_perc <- att_pedidos / media_pre$pre_pedidos * 100
att_gastos_perc  <- att_gastos  / media_pre$pre_gastos * 100
att_ticket_perc  <- att_ticket  / media_pre_ticket$pre_ticket * 100

# ATT é positivo e estatisticamente significativo.
# Pedidos e Gastos crescem em proporção semelhante: 10.02% e 10.06%.
# Já o Ticket cai, mas somente -2.26%, o que sugere não haver canibalização relevante.

# 3.2) Estudo de eventos
es_pedidos <- feols(orders_count ~ i(month, is_treated_city, ref = 6) | user_id + month,
                    data = dados, cluster = ~ city_id)

es_gastos <- feols(monthly_spend ~ i(month, is_treated_city, ref = 6) | user_id + month,
                    data = dados, cluster = ~ city_id)

es_ticket <- feols(ticket_medio ~ i(month, is_treated_city, ref = 6) | user_id + month,
                   data = dados_ticket, cluster = ~ city_id)

# 3.3) Organizar dataframes dos estudos
t0 <- 7
ref_month <- 6

att_pedidos_df <- tidy(es_pedidos, conf.int = TRUE) %>% filter(str_detect(term, "^month::")) %>%
  mutate(m = as.integer(str_extract(term, "\\d+")), rel = m - t0, period = if_else(rel < 0, "Pré", "Pós")) %>%
  select(rel, estimate, conf.low, conf.high, period) %>%
  bind_rows(tibble(rel = ref_month - t0, estimate = 0, conf.low = 0, conf.high = 0, period = "Pré")) %>%
  distinct(rel, .keep_all = TRUE) %>%
  arrange(rel)

att_gastos_df <- tidy(es_gastos, conf.int = TRUE) %>% filter(str_detect(term, "^month::")) %>%
  mutate(m = as.integer(str_extract(term, "\\d+")), rel = m - t0, period = if_else(rel < 0, "Pré", "Pós")) %>%
  select(rel, estimate, conf.low, conf.high, period) %>%
  bind_rows(tibble(rel = ref_month - t0, estimate = 0, conf.low = 0, conf.high = 0, period = "Pré")) %>%
  distinct(rel, .keep_all = TRUE) %>%
  arrange(rel)

att_ticket_df <- tidy(es_ticket, conf.int = TRUE) %>% 
  filter(str_detect(term, "^month::")) %>%
  mutate(m = as.integer(str_extract(term, "\\d+")), 
         rel = m - t0, 
         period = if_else(rel < 0, "Pré", "Pós")) %>%
  select(rel, estimate, conf.low, conf.high, period) %>%
  bind_rows(tibble(rel = ref_month - t0, estimate = 0, conf.low = 0, 
                   conf.high = 0, period = "Pré")) %>%
  distinct(rel, .keep_all = TRUE) %>%
  arrange(rel)

# 3.4) Plotar estudos de evento
ggplot(att_pedidos_df, aes(x = rel, y = estimate, color = period)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.7) +
  geom_point(size = 3, shape = 19) +
  scale_color_manual(
    values = c("Pré" = "#3498DB", "Pós" = "#E67E22"),
    labels = c("Pré" = "Pré-tratamento", "Pós" = "Pós-tratamento")
  ) +
  scale_x_continuous(
    breaks = seq(min(att_pedidos_df$rel), max(att_pedidos_df$rel), by = 1),
    minor_breaks = NULL
  ) +
  labs(
    title = "Estudo de Evento: Efeito Dinâmico sobre Pedidos",
    subtitle = "Estimativas do ATT por período relativo ao tratamento",
    x = "Meses relativos ao início do tratamento",
    y = "Efeito sobre pedidos (ATT)",
    color = "Período"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

ggplot(att_gastos_df, aes(x = rel, y = estimate, color = period)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.7) +
  geom_point(size = 3, shape = 19) +
  scale_color_manual(
    values = c("Pré" = "#3498DB", "Pós" = "#E67E22"),
    labels = c("Pré" = "Pré-tratamento", "Pós" = "Pós-tratamento")
  ) +
  scale_x_continuous(
    breaks = seq(min(att_pedidos_df$rel), max(att_pedidos_df$rel), by = 1),
    minor_breaks = NULL
  ) +
  labs(
    title = "Estudo de Evento: Efeito Dinâmico sobre Gastos",
    subtitle = "Estimativas do ATT por período relativo ao tratamento",
    x = "Meses relativos ao início do tratamento",
    y = "Efeito sobre gastos (ATT)",
    color = "Período"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

ggplot(att_ticket_df, aes(x = rel, y = estimate, color = period)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.7) +
  geom_point(size = 3, shape = 19) +
  scale_color_manual(
    values = c("Pré" = "#3498DB", "Pós" = "#E67E22"),
    labels = c("Pré" = "Pré-tratamento", "Pós" = "Pós-tratamento")
  ) +
  scale_x_continuous(
    breaks = seq(min(att_pedidos_df$rel), max(att_pedidos_df$rel), by = 1),
    minor_breaks = NULL
  ) +
  labs(
    title = "Estudo de Evento: Efeito Dinâmico sobre Ticket",
    subtitle = "Estimativas do ATT por período relativo ao tratamento",
    x = "Meses relativos ao início do tratamento",
    y = "Efeito sobre ticket (ATT)",
    color = "Período"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3)
  )

# Gráficos sugerem que efeitos de tratamento são persistentes no tempo e estatisticamente significativos.
# Intervalos de confiança são relativamente estreitos.
# Hipótese de tendências paralelas parece válida: não há comportamento tendencial no pré-tratamento.
# Efeito negativo sobre o ticket não é muito significativo e se dissolve no tempo!




# --------------------------------------------------------------------------------------------
# 4. ANÁLISE POR COORTES (QUARTIS)
# --------------------------------------------------------------------------------------------
# 4.1) Calcular médias de Pedidos de cada usuário ao longo dos primeiros seis meses
coortes <- dados %>% filter(month < 7) %>% group_by(user_id) %>%
  summarise(pre_pedidos = mean(orders_count), .groups = "drop")

# 4.2) Calcular quartis e criar 3 grupos: Q1 (Casuais), Q2+Q3 (Moderados), Q4 (Assíduos)
quartis <- quantile(coortes$pre_pedidos, probs = c(0.25, 0.5, 0.75))
q1 <- quartis[1]
q2 <- quartis[2]
q3 <- quartis[3]

cat("\n=== QUARTIS DE PEDIDOS PRÉ-TRATAMENTO ===\n")
cat(sprintf("Q1 (25%%): %.2f pedidos/mês\n", q1))
cat(sprintf("Q2 (50%%): %.2f pedidos/mês\n", q2))
cat(sprintf("Q3 (75%%): %.2f pedidos/mês\n", q3))

# Criar variável de coorte com 3 grupos
coortes <- coortes %>%
  mutate(cohort = case_when(
    pre_pedidos < q2 ~ "Casual",      # Q1: abaixo da mediana
    pre_pedidos < q3 ~ "Moderado",    # Q2+Q3: entre mediana e Q3
    TRUE ~ "Assiduo"                  # Q4: acima de Q3
  ))

# Contar usuários por coorte
contagem_coortes <- coortes %>% count(cohort)
cat("\n=== DISTRIBUIÇÃO DE USUÁRIOS POR COORTE ===\n")
print(contagem_coortes)

# Juntar coortes aos dados principais
dados <- dados %>% left_join(coortes, by = "user_id")

# Garantir ordenação das coortes para plots
dados$cohort <- factor(dados$cohort, levels = c("Casual", "Moderado", "Assiduo"))
coortes$cohort <- factor(coortes$cohort, levels = c("Casual", "Moderado", "Assiduo"))

# 4.3) Plot da distribuição por coorte
ggplot(coortes, aes(x = pre_pedidos, fill = cohort)) +
  geom_histogram(bins = 46, color = "white", linewidth = 0.3, alpha = 0.8) +
  scale_fill_manual(
    values = c("Casual" = "#3498DB", "Moderado" = "#F39C12", "Assiduo" = "#E74C3C"),
    labels = c("Casual" = "Casuais (Q1)", 
               "Moderado" = "Moderados (Q2+Q3)", 
               "Assiduo" = "Assíduos (Q4)")
  ) +
  labs(
    title = "Distribuição de Usuários por Pedidos Pré-Tratamento",
    subtitle = "Três grupos: Casuais (Q1), Moderados (Q2+Q3) e Assíduos (Q4)",
    x = "Média de pedidos por mês (pré-tratamento)",
    y = "Número de usuários",
    fill = "Grupos"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# 4.4) Estatísticas descritivas por coorte
stats_coortes <- dados %>% 
  filter(month < 7) %>%
  group_by(cohort) %>%
  summarise(
    n_usuarios = n_distinct(user_id),
    pedidos_medio = mean(orders_count),
    gastos_medio = mean(monthly_spend),
    .groups = "drop"
  )

cat("\n=== ESTATÍSTICAS PRÉ-TRATAMENTO POR COORTE ===\n")
print(stats_coortes)

# 4.5) Modelos DiD com interação por coorte (APENAS PEDIDOS E GASTOS)

# Modelo para PEDIDOS com interação tripla
mod_pedidos_coorte <- feols(orders_count ~ is_treated_city*post*cohort | user_id + month,
                            data = dados, cluster = ~ city_id)

# Modelo para GASTOS com interação tripla
mod_gastos_coorte <- feols(monthly_spend ~ is_treated_city*post*cohort | user_id + month,
                           data = dados, cluster = ~ city_id)

# 4.6) Mostrar resultados
cat("\n=== RESULTADOS DOS MODELOS COM INTERAÇÃO POR COORTE ===\n")
etable(mod_pedidos_coorte, mod_gastos_coorte, se='cluster', cluster='city_id')

# 4.7) Extrair e calcular ATTs por coorte

# Extrair coeficientes
coefs_pedidos <- coef(mod_pedidos_coorte)
coefs_gastos <- coef(mod_gastos_coorte)

# ATT para grupo base (Casuais)
att_pedidos_casual <- coefs_pedidos["is_treated_city:post"]
att_gastos_casual <- coefs_gastos["is_treated_city:post"]

# ATT para Moderados = base + interação Moderado
att_pedidos_moderado <- coefs_pedidos["is_treated_city:post"] + 
                        coefs_pedidos["is_treated_city:post:cohortModerado"]
att_gastos_moderado <- coefs_gastos["is_treated_city:post"] + 
                       coefs_gastos["is_treated_city:post:cohortModerado"]

# ATT para Assíduos = base + interação Assíduo
att_pedidos_assiduo <- coefs_pedidos["is_treated_city:post"] + 
                       coefs_pedidos["is_treated_city:post:cohortAssiduo"]
att_gastos_assiduo <- coefs_gastos["is_treated_city:post"] + 
                      coefs_gastos["is_treated_city:post:cohortAssiduo"]

# Calcular médias pré por coorte para porcentagens
medias_pre_coortes <- dados %>% 
  filter(month < 7, is_treated_city == 1) %>%
  group_by(cohort) %>%
  summarise(
    pre_pedidos = mean(orders_count),
    pre_gastos = mean(monthly_spend),
    .groups = "drop"
  )

# Criar tabela de resultados
resultados_coortes <- tibble(
  Coorte = c("Casuais (Q1)", "Moderados (Q2+Q3)", "Assíduos (Q4)"),
  ATT_Pedidos = c(att_pedidos_casual, att_pedidos_moderado, att_pedidos_assiduo),
  ATT_Gastos = c(att_gastos_casual, att_gastos_moderado, att_gastos_assiduo),
  Pre_Pedidos = medias_pre_coortes$pre_pedidos,
  Pre_Gastos = medias_pre_coortes$pre_gastos
) %>%
  mutate(
    ATT_Pedidos_Perc = 100 * ATT_Pedidos / Pre_Pedidos,
    ATT_Gastos_Perc = 100 * ATT_Gastos / Pre_Gastos
  )

cat("\n=== ATT POR COORTE ===\n")
cat("\nPEDIDOS:\n")
cat(sprintf("  Casuais:   %+.2f pedidos/mês (%+.1f%%)\n", 
            resultados_coortes$ATT_Pedidos[1], 
            resultados_coortes$ATT_Pedidos_Perc[1]))

cat(sprintf("  Moderados: %+.2f pedidos/mês (%+.1f%%)\n", 
            resultados_coortes$ATT_Pedidos[2], 
            resultados_coortes$ATT_Pedidos_Perc[2]))
cat(sprintf("  Assíduos:  %+.2f pedidos/mês (%+.1f%%)\n", 
            resultados_coortes$ATT_Pedidos[3], 
            resultados_coortes$ATT_Pedidos_Perc[3]))

cat("\nGASTOS:\n")
cat(sprintf("  Casuais:   +R$ %.2f/mês (%+.1f%%)\n", 
            resultados_coortes$ATT_Gastos[1], 
            resultados_coortes$ATT_Gastos_Perc[1]))
cat(sprintf("  Moderados: +R$ %.2f/mês (%+.1f%%)\n", 
            resultados_coortes$ATT_Gastos[2], 
            resultados_coortes$ATT_Gastos_Perc[2]))
cat(sprintf("  Assíduos:  +R$ %.2f/mês (%+.1f%%)\n", 
            resultados_coortes$ATT_Gastos[3], 
            resultados_coortes$ATT_Gastos_Perc[3]))

# 4.8) Visualizar heterogeneidade
resultados_coortes$Coorte <- factor(
  resultados_coortes$Coorte,
  levels = c("Casuais (Q1)", "Moderados (Q2+Q3)", "Assíduos (Q4)")
)


# Gráfico de barras para ATT de Pedidos
ggplot(resultados_coortes, aes(x = Coorte, y = ATT_Pedidos, fill = Coorte)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("+%.2f\n(%+.1f%%)", ATT_Pedidos, ATT_Pedidos_Perc)), 
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Casuais (Q1)" = "#3498DB", 
                                "Moderados (Q2+Q3)" = "#F39C12", 
                                "Assíduos (Q4)" = "#E74C3C")) +
  labs(
    title = "Heterogeneidade do Efeito em Pedidos por Grupo",
    subtitle = "ATT do programa de frete grátis sobre número de pedidos",
    x = NULL,
    y = "ATT (pedidos adicionais por mês)",
    fill = "Coorte"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  ylim(0, max(resultados_coortes$ATT_Pedidos) * 1.15)

# Gráfico de barras para ATT de Gastos
ggplot(resultados_coortes, aes(x = Coorte, y = ATT_Gastos, fill = Coorte)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("+R$ %.2f\n(%+.1f%%)", ATT_Gastos, ATT_Gastos_Perc)), 
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Casuais (Q1)" = "#3498DB", 
                                "Moderados (Q2+Q3)" = "#F39C12", 
                                "Assíduos (Q4)" = "#E74C3C")) +
  labs(
    title = "Heterogeneidade do Efeito em Gastos por Grupo",
    subtitle = "ATT do programa de frete grátis sobre gastos mensais",
    x = NULL,
    y = "ATT (gastos adicionais por mês em R$)",
    fill = "Coorte"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  ylim(0, max(resultados_coortes$ATT_Gastos) * 1.15)


# 4.9) Event Studies por coorte

# Event study para cada coorte - PEDIDOS
es_pedidos_casual_coorte <- feols(orders_count ~ i(month, is_treated_city, ref = 6) | user_id + month,
                                  data = dados %>% filter(cohort == "Casual"), cluster = ~ city_id)

es_pedidos_moderado_coorte <- feols(orders_count ~ i(month, is_treated_city, ref = 6) | user_id + month,
                                    data = dados %>% filter(cohort == "Moderado"), cluster = ~ city_id)

es_pedidos_assiduo_coorte <- feols(orders_count ~ i(month, is_treated_city, ref = 6) | user_id + month,
                                   data = dados %>% filter(cohort == "Assiduo"), cluster = ~ city_id)

# Event study para cada coorte - GASTOS
es_gastos_casual_coorte <- feols(monthly_spend ~ i(month, is_treated_city, ref = 6) | user_id + month,
                                 data = dados %>% filter(cohort == "Casual"), cluster = ~ city_id)

es_gastos_moderado_coorte <- feols(monthly_spend ~ i(month, is_treated_city, ref = 6) | user_id + month,
                                   data = dados %>% filter(cohort == "Moderado"), cluster = ~ city_id)

es_gastos_assiduo_coorte <- feols(monthly_spend ~ i(month, is_treated_city, ref = 6) | user_id + month,
                                  data = dados %>% filter(cohort == "Assiduo"), cluster = ~ city_id)

# Processar resultados - PEDIDOS
t0 <- 7
ref_month <- 6

processar_es <- function(modelo, cohort_name) {
  tidy(modelo, conf.int = TRUE) %>% 
    filter(str_detect(term, "^month::")) %>%
    mutate(
      m = as.integer(str_extract(term, "\\d+")), 
      rel = m - t0,
      period = if_else(rel < 0, "Pré", "Pós"),
      cohort = cohort_name
    ) %>%
    select(rel, estimate, conf.low, conf.high, period, cohort) %>%
    bind_rows(tibble(rel = ref_month - t0, estimate = 0, conf.low = 0, 
                     conf.high = 0, period = "Pré", cohort = cohort_name)) %>%
    distinct(rel, .keep_all = TRUE) %>%
    arrange(rel)
}

es_pedidos_todas_coortes <- bind_rows(
  processar_es(es_pedidos_casual_coorte, "Casual"),
  processar_es(es_pedidos_moderado_coorte, "Moderado"),
  processar_es(es_pedidos_assiduo_coorte, "Assiduo")
) %>%
  mutate(
    cohort = factor(cohort, levels = c("Casual", "Moderado", "Assiduo")),
    rel_ajustado = rel + 1
  )

es_gastos_todas_coortes <- bind_rows(
  processar_es(es_gastos_casual_coorte, "Casual"),
  processar_es(es_gastos_moderado_coorte, "Moderado"),
  processar_es(es_gastos_assiduo_coorte, "Assiduo")
) %>%
  mutate(
    cohort = factor(cohort, levels = c("Casual", "Moderado", "Assiduo")),
    rel_ajustado = rel + 1
  )

# Gráfico - EVENT STUDY PEDIDOS POR COORTE
ggplot(es_pedidos_todas_coortes, aes(x = rel_ajustado, y = estimate, color = cohort, group = cohort)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cohort), 
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5, shape = 19) +
  scale_color_manual(
    values = c("Casual" = "#3498DB", "Moderado" = "#F39C12", "Assiduo" = "#E74C3C"),
    labels = c("Casual" = "Casuais (Q1)", "Moderado" = "Moderados (Q2+Q3)", "Assiduo" = "Assíduos (Q4)")
  ) +
  scale_fill_manual(
    values = c("Casual" = "#3498DB", "Moderado" = "#F39C12", "Assiduo" = "#E74C3C"),
    labels = c("Casual" = "Casuais (Q1)", "Moderado" = "Moderados (Q2+Q3)", "Assiduo" = "Assíduos (Q4)")
  ) +
  labs(
    title = "Efeito Dinâmico sobre Pedidos por Grupo",
    subtitle = "Comparação da resposta entre usuários Casuais, Moderados e Assíduos",
    x = "Meses relativos ao início do tratamento",
    y = "Efeito sobre pedidos (ATT)",
    color = "Grupo",
    fill = "Grupo"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

# Gráfico - EVENT STUDY GASTOS POR COORTE
ggplot(es_gastos_todas_coortes, aes(x = rel_ajustado, y = estimate, color = cohort, group = cohort)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cohort), 
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5, shape = 19) +
  scale_color_manual(
    values = c("Casual" = "#3498DB", "Moderado" = "#F39C12", "Assiduo" = "#E74C3C"),
    labels = c("Casual" = "Casuais (Q1)", "Moderado" = "Moderados (Q2+Q3)", "Assiduo" = "Assíduos (Q4)")
  ) +
  scale_fill_manual(
    values = c("Casual" = "#3498DB", "Moderado" = "#F39C12", "Assiduo" = "#E74C3C"),
    labels = c("Casual" = "Casuais (Q1)", "Moderado" = "Moderados (Q2+Q3)", "Assiduo" = "Assíduos (Q4)")
  ) +
  labs(
    title = "Efeito Dinâmico sobre Gastos por Grupo",
    subtitle = "Comparação da resposta entre usuários Casuais, Moderados e Assíduos",
    x = "Meses relativos ao início do tratamento",
    y = "Efeito sobre gastos em R$ (ATT)",
    color = "Grupo",
    fill = "Grupo"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(face = "italic", color = "gray40", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )





# --------------------------------------------------------------------------------------------
# 5. CÁLCULO DO ATT PONDERADO
# --------------------------------------------------------------------------------------------
# pesos explícitos: tratados no pós por intensidade
pesos_intensidade <- dados %>%
  filter(is_treated_city == 1, post == 1) %>%
  mutate(cohort = recode(cohort,
                         "Casual" = "Casuais (Q1)",
                         "Moderado" = "Moderados (Q2+Q3)",
                         "Assiduo" = "Assíduos (Q4)")) %>%
  count(cohort, name = "n_tratados_pos") %>%
  mutate(w = n_tratados_pos / sum(n_tratados_pos))

# ATTs por intensidade (exemplo: vindos do TWFE com interação)
atts_gastos <- tibble(
  cohort = c("Casuais (Q1)", "Moderados (Q2+Q3)", "Assíduos (Q4)"),
  att = c(att_gastos_casual, att_gastos_moderado, att_gastos_assiduo)
)

atts_pedidos <- tibble(
  cohort = c("Casuais (Q1)", "Moderados (Q2+Q3)", "Assíduos (Q4)"),
  att = c(att_pedidos_casual, att_pedidos_moderado, att_pedidos_assiduo)
)

# ATT agregado com pesos explícitos
ATT_pedidos_ponderado <- atts_pedidos %>%
  inner_join(pesos_intensidade, by = "cohort") %>%
  summarise(ATT = sum(w * att)) %>%
  pull(ATT)

ATT_gastos_ponderado <- atts_gastos %>%
  inner_join(pesos_intensidade, by = "cohort") %>%
  summarise(ATT = sum(w * att)) %>%
  pull(ATT)

