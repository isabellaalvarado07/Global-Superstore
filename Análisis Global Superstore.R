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



 
