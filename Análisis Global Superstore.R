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





 
