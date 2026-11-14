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

 
