# Analisis de la base de datos Global Superstore
# =============================================================================
# Global Superstore: descuentos, costos de envío y rentabilidad
# Proyecto final de Business Analytics (BA-2630)
#
# Correr desde la carpeta del repositorio: source("Global-Superstore.R")
# Lee data/Global_Superstore.xls y escribe tablas/ (CSV y PNG) y figuras/ (PNG).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(lubridate)
  library(broom); library(sandwich); library(lmtest); library(car); library(tseries)
  library(ggthemes); library(patchwork); library(scales); library(gridExtra); library(grid)
})
set.seed(2026)
dir.create("tablas", showWarnings = FALSE); dir.create("figuras", showWarnings = FALSE)
# ---- Utilidades --------------------------------------------------------------
OI <- c(azul = "#0072B2", naranja = "#E69F00", verde = "#009E73", rojo = "#D55E00",
        cielo = "#56B4E9", gris = "#6B7280")
theme_set(theme_tufte(base_size = 11, base_family = "sans") +
            theme(plot.title = element_text(face = "bold", size = 12),
                  axis.line = element_line(colour = "grey70", linewidth = 0.3),
                  panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3)))
usd  <- function(x, d = 0) paste0("$", formatC(x, format = "f", digits = d, big.mark = ","))
num  <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pct  <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
estrellas <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

guardar_tabla <- function(df, nombre, ancho_col = NULL) {
  write_csv(df, file.path("tablas", paste0(nombre, ".csv")))
  tt <- ttheme_minimal(base_size = 9,
          core = list(fg_params = list(hjust = 1, x = 0.95), bg_params = list(fill = c("white", "grey96"), col = "grey55", lwd = 0.6)),
          colhead = list(fg_params = list(fontface = "bold", hjust = 1, x = 0.95), bg_params = list(fill = "grey85", col = "grey55", lwd = 0.6)),
          rowhead = list(fg_params = list(hjust = 0, x = 0.05)))
  g <- tableGrob(as.data.frame(df), rows = NULL, theme = tt)
  g$widths <- g$widths * 1.05
  w <- convertWidth(sum(g$widths), "in", valueOnly = TRUE) + 0.2
  h <- convertHeight(sum(g$heights), "in", valueOnly = TRUE) + 0.2
  png(file.path("tablas", paste0(nombre, ".png")), width = w, height = h, units = "in", res = 200)
  grid.newpage(); grid.draw(g); dev.off()
}
guardar_fig <- function(p, nombre, w = 7.5, h = 4.6)
  ggsave(file.path("figuras", paste0(nombre, ".png")), p, width = w, height = h, dpi = 200, bg = "white")

# =============================================================================
# PUNTO 1. Datos y preparación
# =============================================================================
bruto <- read_excel("data/Global_Superstore.xls", sheet = "Orders")
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
dias_sem <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

d <- bruto %>%
  transmute(id_pedido = `Order ID`, fecha = as.Date(`Order Date`), id_producto = `Product ID`,
            mercado = Market, pais = Country, categoria = Category, subcategoria = `Sub-Category`,
            ventas = Sales, cantidad = Quantity, descuento = Discount,
            utilidad = Profit, envio = `Shipping Cost`) %>%
  mutate(mercado = relevel(factor(mercado), "US"),                 # base: Estados Unidos
         categoria = relevel(factor(categoria), "Office Supplies"),# base: Office Supplies
         margen = utilidad / ventas, util_neta = utilidad - envio)

calidad <- tibble(
  control = c("Filas (hoja Orders)", "Valores faltantes (sin contar código postal)",
              "Ventas, cantidad o costo de envío menores o iguales a 0", "Descuentos fuera de [0, 1]",
              "Fecha mínima", "Fecha máxima", "Días del calendario", "Días con pedidos", "Días sin pedidos (todos domingos)",
              "Pedidos distintos"),
  resultado = c(num(nrow(bruto)), as.character(sum(is.na(select(bruto, -`Postal Code`)))),
                as.character(sum(d$ventas <= 0 | d$cantidad <= 0 | d$envio <= 0)),
                as.character(sum(d$descuento < 0 | d$descuento > 1)),
                as.character(min(d$fecha)), as.character(max(d$fecha)),
                num(as.numeric(max(d$fecha) - min(d$fecha)) + 1), num(n_distinct(d$fecha)),
                num(as.numeric(max(d$fecha) - min(d$fecha)) + 1 - n_distinct(d$fecha)), num(n_distinct(d$id_pedido))))
guardar_tabla(calidad, "tabla_00_calidad")

ped <- d %>% group_by(id_pedido, fecha) %>% summarise(ventas = sum(ventas), .groups = "drop")
ped_grandes <- ped %>% filter(ventas >= 10000) %>% arrange(desc(ventas))
write_csv(ped_grandes, "tablas/tabla_A3_pedidos_extraordinarios.csv")


# Eventos: (a) día con un pedido de $10,000 o más; (b) Black Friday y Cyber Monday
bf <- function(y) { nov <- as.Date(paste0(y, "-11-01")); jue <- nov + ((4 - wday(nov, week_start = 1)) %% 7) + 21; jue + 1 }
fechas_bf <- c(bf(2011:2014), bf(2011:2014) + 3)


# Una fila por día (se excluyen los días sin pedidos: ln(0) no existe)
dia <- d %>% group_by(fecha) %>%
  summarise(descuento = weighted.mean(descuento, ventas),
            part_tec = sum(ventas[categoria == "Technology"]) / sum(ventas),
            part_mue = sum(ventas[categoria == "Furniture"]) / sum(ventas),
            ordenes = n_distinct(id_pedido), lineas = n(), envio = sum(envio), utilidad = sum(utilidad),
            ventas = sum(ventas), .groups = "drop") %>%
  mutate(envio_medio = envio / lineas,
         mes = factor(month(fecha), 1:12, meses), anio = factor(year(fecha)),
         dia_sem = factor(dias_sem[wday(fecha, week_start = 1)], dias_sem),
         pedido_grande = as.integer(fecha %in% ped_grandes$fecha), black_friday = as.integer(fecha %in% fechas_bf),
         ln_ventas = log(ventas), ln_ordenes = log(ordenes), ln_envio = log(envio_medio))


# Panel día x mercado (para comparar mercados con Estados Unidos como base)
dia_m <- d %>% group_by(fecha, mercado) %>%
  summarise(descuento = weighted.mean(descuento, ventas),
            part_tec = sum(ventas[categoria == "Technology"]) / sum(ventas),
            part_mue = sum(ventas[categoria == "Furniture"]) / sum(ventas),
            ordenes = n_distinct(id_pedido), lineas = n(), envio = sum(envio), ventas = sum(ventas), .groups = "drop") %>%
  mutate(envio_medio = envio / lineas, mes = factor(month(fecha), 1:12, meses), anio = factor(year(fecha)),
         dia_sem = factor(dias_sem[wday(fecha, week_start = 1)], dias_sem),
         pedido_grande = as.integer(fecha %in% ped_grandes$fecha), black_friday = as.integer(fecha %in% fechas_bf),
         ln_ventas = log(ventas), ln_ordenes = log(ordenes), ln_envio = log(envio_medio))


# =============================================================================
# PUNTO 2. Tabla descriptiva (N, media, desviación estándar, mínimo, máximo)
# =============================================================================
desc_fila <- function(x, nombre, dd = 2)
  tibble(Variable = nombre, N = num(sum(!is.na(x))), Media = num(mean(x), dd), `Desv. est.` = num(sd(x), dd),
         Mínimo = num(min(x), dd), Máximo = num(max(x), dd))
t_desc <- bind_rows(
  desc_fila(dia$ventas,       "Ventas diarias (USD)"),
  desc_fila(dia$ordenes,      "Órdenes por día (número)", 1),
  desc_fila(dia$descuento,    "Descuento medio del día (0 a 1)", 3),
  desc_fila(dia$envio_medio,  "Costo de envío medio por línea (USD)"),
  desc_fila(dia$part_tec,     "Participación de Technology en ventas del día", 3),
  desc_fila(dia$part_mue,     "Participación de Furniture en ventas del día", 3),
  desc_fila(dia$utilidad,     "Utilidad diaria (USD)"),
  desc_fila(d$ventas,         "Ventas por línea de pedido (USD)"),
  desc_fila(d$descuento,      "Descuento por línea (0 a 1)", 3))
guardar_tabla(t_desc, "tabla_01_descriptiva")
pct_dias_menor_20k <- mean(dia$ventas < 20000)

# =============================================================================
# PUNTO 3. Gráficas: tiempo (semanal y diaria), calendario, mercados y categorías
# =============================================================================
sem <- dia %>% mutate(semana = floor_date(fecha, "week", week_start = 1)) %>%
  group_by(semana) %>% summarise(ventas = sum(ventas), ordenes = sum(ordenes), .groups = "drop")
f1a <- ggplot(sem, aes(semana, ventas / 1000)) + geom_line(colour = OI["azul"], linewidth = 0.6) +
  labs(x = NULL, y = "Miles de USD", title = "Ventas por semana, 2011-2014")
f1b <- ggplot(sem, aes(semana, ordenes)) + geom_line(colour = OI["verde"], linewidth = 0.6) +
  labs(x = NULL, y = "Órdenes", title = "Órdenes por semana, 2011-2014")
guardar_fig(f1a / f1b, "fig_01_serie_semanal", h = 6)

top_dias <- dia %>% filter(pedido_grande == 1)
f2 <- ggplot(dia, aes(fecha, ventas / 1000)) + geom_line(colour = "grey55", linewidth = 0.3) +
  geom_point(data = top_dias, colour = OI["rojo"], size = 2) +
  geom_text(data = top_dias, aes(label = format(fecha, "%d-%b-%Y")), colour = OI["rojo"], size = 2.8, vjust = -0.7, check_overlap = TRUE) +
  labs(x = NULL, y = "Miles de USD", title = "Ventas diarias: los puntos rojos son días con un pedido de $10,000 o más")
guardar_fig(f2, "fig_02_serie_diaria", h = 3.8)

cal_mes <- dia %>% group_by(mes) %>% summarise(m = mean(ventas), .groups = "drop")
cal_dia <- dia %>% group_by(dia_sem) %>% summarise(m = mean(ventas), o = mean(ordenes), .groups = "drop")
f3a <- ggplot(cal_mes, aes(mes, m / 1000)) + geom_col(fill = OI["azul"], width = 0.7) +
  labs(x = NULL, y = "Ventas diarias medias (miles de USD)", title = "Por mes")
f3b <- ggplot(cal_dia, aes(dia_sem, m / 1000)) + geom_col(fill = OI["naranja"], width = 0.7) +
  labs(x = NULL, y = NULL, title = "Por día de la semana") + theme(axis.text.x = element_text(angle = 35, hjust = 1))
guardar_fig(f3a + f3b + plot_layout(widths = c(1.6, 1)), "fig_03_calendario", h = 3.8)

dm <- d %>% group_by(mercado, categoria) %>% summarise(ventas = sum(ventas), utilidad = sum(utilidad), .groups = "drop")
orden_m <- dm %>% group_by(mercado) %>% summarise(v = sum(ventas)) %>% arrange(v) %>% pull(mercado)
f4 <- ggplot(dm, aes(factor(mercado, levels = orden_m), ventas / 1e6, fill = categoria)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.4) + coord_flip() +
  scale_fill_manual(values = unname(OI[c("azul", "naranja", "verde")]), name = NULL) +
  labs(x = NULL, y = "Ventas acumuladas 2011-2014 (millones de USD)", title = "Dónde y qué categoría vende más") +
  theme(legend.position = "top")
guardar_fig(f4, "fig_04_mercado_categoria", h = 4)

t_mercado <- d %>% group_by(Mercado = mercado) %>%
  summarise(`Ventas (USD)` = sum(ventas), `% de ventas` = sum(ventas) / sum(d$ventas),
            `Utilidad (USD)` = sum(utilidad), Margen = sum(utilidad) / sum(ventas),
            `Descuento medio` = mean(descuento), .groups = "drop") %>% arrange(desc(`Ventas (USD)`))
guardar_tabla(t_mercado %>% mutate(`Ventas (USD)` = num(`Ventas (USD)`), `% de ventas` = pct(`% de ventas`),
                                   `Utilidad (USD)` = num(`Utilidad (USD)`), Margen = pct(Margen),
                                   `Descuento medio` = pct(`Descuento medio`)), "tabla_02_mercados")

t_categoria <- d %>% group_by(Categoría = categoria) %>%
  summarise(`Ventas (USD)` = sum(ventas), `% de ventas` = sum(ventas) / sum(d$ventas),
            `Utilidad (USD)` = sum(utilidad), Margen = sum(utilidad) / sum(ventas),
            `Descuento medio` = mean(descuento), `% de líneas con pérdida` = mean(utilidad < 0), .groups = "drop") %>%
  arrange(desc(`Ventas (USD)`))
guardar_tabla(t_categoria %>% mutate(`Ventas (USD)` = num(`Ventas (USD)`), `% de ventas` = pct(`% de ventas`),
                                     `Utilidad (USD)` = num(`Utilidad (USD)`), Margen = pct(Margen),
                                     `Descuento medio` = pct(`Descuento medio`),
                                     `% de líneas con pérdida` = pct(`% de líneas con pérdida`)), "tabla_03_categorias")

# Distribución de las ventas diarias en dólares y en logaritmo
skew <- function(x) mean(((x - mean(x)) / sd(x))^3)
f5a <- ggplot(dia, aes(ventas)) + geom_histogram(bins = 50, fill = OI["azul"], colour = "white", linewidth = 0.1) +
  scale_x_continuous(labels = label_comma()) + labs(x = "Ventas diarias (USD)", y = "Días", title = "Ventas diarias en dólares")
f5b <- ggplot(dia, aes(ln_ventas)) + geom_histogram(bins = 50, fill = OI["verde"], colour = "white", linewidth = 0.1) +
  labs(x = "ln(ventas diarias)", y = "Días", title = "Ventas diarias en logaritmo")
guardar_fig(f5a + f5b, "fig_05_distribucion_ventas", h = 3.8)
asim <- c(niveles = skew(dia$ventas), logs = skew(dia$ln_ventas))

# =============================================================================
# PUNTO 4. Categorías base (con qué se compara cada beta)
# =============================================================================
# Bases: enero, lunes, 2011, Office Supplies; US en el panel por mercado.
base_tab <- tibble(Variable = c("Mes del pedido", "Día de la semana", "Año", "Categoría (participación en ventas)", "Mercado (panel del anexo)"),
                   `Categoría base` = c("Enero", "Lunes", "2011", "Office Supplies", "US"),
                   `Betas estimados` = c("11", "6", "3", "2", "6"))
guardar_tabla(base_tab, "tabla_04_categorias_base")


# =============================================================================
# PUNTO 5. Las cuatro estimaciones   y = c + descuento + controles + error
# =============================================================================
# Variable dependiente en logaritmo natural: Sales (ventas diarias) y Orders (órdenes diarias)
ctrl <- "ln_envio + part_tec + part_mue + mes + dia_sem + anio + pedido_grande + black_friday"
m1 <- lm(ln_ventas  ~ descuento, dia)
m2 <- lm(as.formula(paste("ln_ventas  ~ descuento +", ctrl)), dia)
m3 <- lm(ln_ordenes ~ descuento, dia)
m4 <- lm(as.formula(paste("ln_ordenes ~ descuento +", ctrl)), dia)
modelos <- list(`(1) Sales` = m1, `(2) Sales + controles` = m2, `(3) Orders` = m3, `(4) Orders + controles` = m4)

# Anexo: panel día x mercado (compara cada mercado con Estados Unidos)
ctrl_p <- sub("ln_envio", "ln_envio + mercado", ctrl)
mp_v <- lm(as.formula(paste("ln_ventas  ~ descuento +", ctrl_p)), dia_m)
mp_o <- lm(as.formula(paste("ln_ordenes ~ descuento +", ctrl_p)), dia_m)

etiqueta <- function(t) {
  t <- sub("^mercado", "Mercado: ", t); t <- sub("^mes", "Mes: ", t); t <- sub("^dia_sem", "Día: ", t); t <- sub("^anio", "Año: ", t)
  t <- dplyr::recode(t, `(Intercept)` = "Constante", descuento = "Descuento (0 a 1)", ln_envio = "ln(envío medio por línea)",
         part_mue = "Furniture (participación; base Office Supplies)", part_tec = "Technology (participación; base Office Supplies)",
         pedido_grande = "Día con pedido de $10,000 o más", black_friday = "Black Friday o Cyber Monday")
  t
}
coefs <- imap_dfr(modelos, function(m, nm) tidy(m) %>% transmute(modelo = nm, termino = etiqueta(term), coef = estimate, ee = std.error, p = p.value))
coefs_p <- bind_rows(tidy(mp_v) %>% mutate(modelo = "(A1) Sales por mercado"), tidy(mp_o) %>% mutate(modelo = "(A2) Orders por mercado")) %>%
  transmute(modelo, termino = etiqueta(term), coef = estimate, ee = std.error, p = p.value)
resumen_mod <- imap_dfr(modelos, ~ tibble(modelo = .y, r2 = summary(.x)$r.squared, r2_aj = summary(.x)$adj.r.squared,
                                          n = nobs(.x), sigma = summary(.x)$sigma, k = length(coef(.x)) - 1))
resumen_p <- tibble(modelo = c("(A1) Sales por mercado", "(A2) Orders por mercado"), r2 = c(summary(mp_v)$r.squared, summary(mp_o)$r.squared),
                    r2_aj = c(summary(mp_v)$adj.r.squared, summary(mp_o)$adj.r.squared), n = c(nobs(mp_v), nobs(mp_o)))

# Efecto de los picos: R² antes y después de incorporar las variables de eventos
sin_ev <- function(y) lm(as.formula(paste(y, "~ descuento + ln_envio + part_tec + part_mue + mes + dia_sem + anio")), dia)
t_eventos <- tibble(Estimación = c("(2) Sales", "(4) Orders"),
                    `R² sin variables de eventos` = c(summary(sin_ev("ln_ventas"))$r.squared, summary(sin_ev("ln_ordenes"))$r.squared),
                    `R² con variables de eventos` = c(summary(m2)$r.squared, summary(m4)$r.squared))
# =============================================================================
# PUNTO 6. Tabla de resultados (R² y asteriscos de significancia)
# =============================================================================
armar_tabla <- function(coefs, terminos, nombres, resumen, controles_txt) {
  ancho <- coefs %>% filter(termino %in% terminos) %>%
    mutate(celda = paste0(formatC(coef, format = "f", digits = 3), estrellas(p)), ee_txt = paste0("(", formatC(ee, format = "f", digits = 3), ")"))
  filas <- map_dfr(terminos, function(tt) {
    a <- ancho %>% filter(termino == tt)
    g <- function(col) map_chr(nombres, ~ { z <- a %>% filter(modelo == .x); if (nrow(z)) z[[col]] else "" })
    bind_rows(tibble(Variable = tt, !!!set_names(as.list(g("celda")), nombres)), tibble(Variable = "", !!!set_names(as.list(g("ee_txt")), nombres)))
  })
  pie <- bind_rows(
    tibble(Variable = "Observaciones", !!!set_names(as.list(num(resumen$n)), nombres)),
    tibble(Variable = "R²", !!!set_names(as.list(formatC(resumen$r2, format = "f", digits = 3)), nombres)),
    tibble(Variable = "R² ajustado", !!!set_names(as.list(formatC(resumen$r2_aj, format = "f", digits = 3)), nombres)),
    tibble(Variable = "Controles", !!!set_names(as.list(controles_txt), nombres)))
  bind_rows(filas, pie)
}
term_prin <- c("Constante", "Descuento (0 a 1)", "ln(envío medio por línea)", "Furniture (participación; base Office Supplies)",
               "Technology (participación; base Office Supplies)", "Día: Sábado", "Día: Domingo",
               "Día con pedido de $10,000 o más", "Black Friday o Cyber Monday")
t_res <- armar_tabla(coefs, term_prin, names(modelos), resumen_mod, c("No", "Sí", "No", "Sí"))
guardar_tabla(t_res, "tabla_05_resultados")
orden_terminos <- unique(coefs$termino)
write_csv(armar_tabla(coefs, orden_terminos, names(modelos), resumen_mod, c("No", "Sí", "No", "Sí")), "tablas/tabla_A1_resultados_completos.csv")
term_panel <- c("Constante", "Descuento (0 a 1)", "ln(envío medio por línea)", "Furniture (participación; base Office Supplies)",
                "Technology (participación; base Office Supplies)", "Día con pedido de $10,000 o más", "Black Friday o Cyber Monday",
                paste0("Mercado: ", c("APAC", "EMEA", "EU", "LATAM", "Africa", "Canada")))
write_csv(armar_tabla(coefs_p, term_panel, c("(A1) Sales por mercado", "(A2) Orders por mercado"), resumen_p, c("Sí", "Sí")),
          "tablas/tabla_A4_panel_mercados.csv")
t_mercados_beta <- coefs_p %>% filter(grepl("^Mercado", termino)) %>%
  transmute(Modelo = modelo, Mercado = sub("Mercado: ", "", termino), Coeficiente = coef, `Efecto %` = 100 * (exp(coef) - 1), p = p)
write_csv(t_mercados_beta, "tablas/tabla_A5_betas_mercado.csv")

efecto <- function(b, delta) 100 * (exp(b * delta) - 1)
b_desc <- sapply(modelos, function(m) unname(coef(m)["descuento"]))
efectos <- tibble(modelo = names(modelos), beta_desc = b_desc, efecto_10pp = efecto(b_desc, 0.10),
                  beta_envio = c(NA, coef(m2)["ln_envio"], NA, coef(m4)["ln_envio"]))

# =============================================================================
# PUNTO 7. Análisis robustos
# =============================================================================
diagnostico <- function(m, titulo) {
  g <- tibble(real = model.response(model.frame(m)), pred = fitted(m), res = resid(m)) %>% mutate(e2 = res^2)
  p1 <- ggplot(g, aes(pred, real)) + geom_point(alpha = 0.35, size = 0.8, colour = OI["azul"]) +
    geom_abline(colour = OI["rojo"], linewidth = 0.6) + labs(x = "Predicho (ln)", y = "Real (ln)", title = "Predichos vs reales")
  p2 <- ggplot(g, aes(pred, e2)) + geom_point(alpha = 0.35, size = 0.8, colour = OI["naranja"]) +
    geom_smooth(method = "loess", se = FALSE, colour = OI["rojo"], linewidth = 0.7, formula = y ~ x) +
    labs(x = "Predicho (ln)", y = "Error al cuadrado", title = "Errores al cuadrado")
  p3 <- ggplot(g, aes(res)) + geom_histogram(aes(y = after_stat(density)), bins = 45, fill = OI["verde"], colour = "white", linewidth = 0.1) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = sd(g$res)), colour = OI["rojo"], linewidth = 0.7) +
    labs(x = "Residuo", y = "Densidad", title = "Histograma de residuos")
  p4 <- ggplot(g, aes(sample = res)) + stat_qq(size = 0.8, alpha = 0.4, colour = OI["azul"]) + stat_qq_line(colour = OI["rojo"]) +
    labs(x = "Cuantiles teóricos", y = "Cuantiles de los residuos", title = "Gráfico cuantil-cuantil")
  (p1 + p2) / (p3 + p4) + plot_annotation(title = titulo)
}
guardar_fig(diagnostico(m2, "Estimación 2: Sales con controles"), "fig_06_robustez_sales", w = 8, h = 7)
guardar_fig(diagnostico(m4, "Estimación 4: Orders con controles"), "fig_07_robustez_orders", w = 8, h = 7)



 
