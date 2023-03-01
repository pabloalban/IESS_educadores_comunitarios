message(paste(rep("-", 100), collapse = ""))

# Carga de los cesantes con su número de aportaciones y meses consecutivos cesante -----------------
message("\tCargando cesantes mensuales")
load(paste0(parametros$RData_seg, "IESS_CES_DES_cesantes_mensuales.RData"))

# Número de afiliados con 24 aportaciones y 3 meses cesantes, y no jubilados -----------------------
cesantes <- cesantes %>%
  lazy_dt() %>%
  filter(anio %in% c("2016", "2017", "2018")) %>%
  mutate(
    fecdefper = if_else(is.na(fecdefper),
      as.Date("31/12/2099", "%d/%m/%Y"),
      fecdefper
    ),
    fecha_derecho = if_else(is.na(fecha_derecho),
      as.Date("31/12/2099", "%d/%m/%Y"),
      fecha_derecho
    ),
    sexo = if_else(is.na(sexo), "M", sexo),
    edad = if_else(edad < 18, round(mean(cesantes$edad)), edad)
  ) %>%
  filter(fecha < fecdefper) %>%
  filter(fecha + 30 < fecha_derecho) %>%
  group_by(edad, sexo, anio) %>%
  mutate(cesantes = n()) %>%
  distinct(edad, sexo, anio, .keep_all = TRUE) %>%
  select(edad, sexo, anio, cesantes) %>%
  as.data.frame()

cortes <- c(6, seq(20, 80, 5), 119)
etiquetas <- c(paste0("(", c(18, seq(20, 80, 5)), "-", c(seq(20, 80, 5), 118), "]"))

cesantes["rango"] <- cut(cesantes$edad,
  breaks = cortes,
  labels = etiquetas,
  # include.lowest = TRUE,
  right = TRUE
)

# Guardando en un Rdata ----------------------------------------------------------------------------
message("\tGuardando número de cesantes con al menos 24 aportaciones y 2 meses consecutivos cesantes")

save(cesantes,
  file = paste0(parametros$RData_seg, "IESS_CES_DES_cesantes_con_requisitos_edad_sexo.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
