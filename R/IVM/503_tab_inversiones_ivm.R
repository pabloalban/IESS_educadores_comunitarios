message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <- paste0( parametros$RData_seg, 'IESS_IVM_inversiones.RData' )
load( file = file_inversiones )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla de la evolución del portafolio -------------------------------------------------------------
aux <- recurs_adm_biess %>% select(-rendimiento_ponderado_real, -inflacion)
aux$ano <- as.character(aux$ano)
aux$rendimiento_neto <- aux$rendimiento_neto*100
aux$rendimiento_ponderado <- aux$rendimiento_ponderado*100
aux$rendimiento_neto_real <- aux$rendimiento_neto_real*100

aux_xtab <- xtable( aux, digits = c(0,0,rep(2,6),0) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))


#Tabla Resumen situacion actual de las inversiones--------------------------------------------------
aux <- inver_corte
aux$rendimiento_promedio <- aux$rendimiento_promedio*100
aux$rendimiento_promedio_real <- aux$rendimiento_promedio_real*100 
aux$instrumento <-c("Titularizaciones",
                    'Cr\\\'{e}ditos',
                    'Obligaciones',
                    'Bonos del Estado',
                    'Renta Variable',
                    'Fideicomisos',
                    'Total Inversiones')

aux <- as.data.table( aux )

aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2))
#aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_corte', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))



# Tabla Rendimientos con ingresos y gastos -----------------------------------------------------
aux <- rendimientos_netos %>% select(-fondo_administrativos)
aux$rendimiento_bruto <- aux$rendimiento_bruto*100
aux$rendimiento_neto <- aux$rendimiento_neto*100
aux$corte_a <- format(aux$corte_a, "%b/%Y")
aux$corte_a <- as.character(aux$corte_a)
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rend_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

# Tabla del detalle de los ingresos que producieron las inversiones --------------------------------
aux <- ingresos %>% select(-x2012, -x2013)
aux$ingresos <- c('Acciones',
                  'Derechos fiduciarios',
                  'De deuda renta fija sector privado',
                  'De deuda renta fija sector p\\\'{u}blico',
                  'En venta de inversiones',
                  'Ingresos intereses interfondos',
                  'Inversiones privativas pr\\\'{e}stamos hipotecarios',
                  'Inversiones privativas pr\\\'{e}stamos quirografarios',
                  'Intereses y comisiones ejercicios anteriores',
                  'Valuaci\\\'{o}n de inversiones',
                  'Total'
)

aux_xtab <- xtable( aux, digits = c(0,rep(2,8)) )
#aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingre_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

# Tabla del detalle de los gastos que producieron las inversiones ----------------------------------
aux <- gastos_opera %>% select(-x2012, -x2013)
aux$gastos <- c('Comisi\\\'{o}n bolsa de valores',
'En valuaci\\\'{o}n de inversiones',
'En venta de inversiones',
'Reverso de intereses',
'Provisi\\\'{o}n para cuentas por cobrar',
'Prov. cr\\\'{e}ditos quirografarios',
'Provisi\\\'{o}n para valuaci\\\'{o}n inversiones de capital',
'Servicios custodia de valores',
'Gastos interfondos',
'Provisi\\\'{o}n de pr\\\'{e}stamos hipotecarios',
'Provisiones antic\\\'{i}clicas y gen\\\'{e}ricas',
'Otros gastos',
'Total'
)
aux_xtab <- xtable( aux, digits = c(0,rep(2,8)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       compress = FALSE,
       fileEncoding="UTF-8",
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

# Tabla evolución Inversiones en Créditos --------------------------------------
aux <- creditos
aux$ano <- as.integer(aux$ano)
aux$rendimiento <- aux$rendimiento * 100
aux$rendimiento_ponderado_real <-  aux$rendimiento_ponderado_real * 100
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_creditos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))


# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Bonos del Estado') %>%
  na.omit() %>%
  mutate(rendimiento_ponderado=rendimiento_ponderado*100,
         rendimiento_ponderado_real=rendimiento_ponderado_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))


# Tabla detalle Inversiones en Bonos del Estado Ecuatoriano al corte--------------------------------
aux <- detalle_bonos
#aux <- na.omit(aux)
#aux <- select(aux,-Detalle)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0,0 ))
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))


#Tabla Recepción de Bonos del Estado por el 40% del pago de las pensiones---------------------------
aux <- detalle_bonos_40 %>%
  mutate(tasa =  tasa * 100)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,0,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_repbonos40_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

#Tabla Evolución Inversiones en Obligaciones--------------------------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Obligaciones') %>%
  na.omit() %>%
  mutate(rendimiento_ponderado=rendimiento_ponderado*100,
         rendimiento_ponderado_real=rendimiento_ponderado_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_obligaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

#Tabla detalle de las inversiones en obligaciones---------------------------------------------------
aux <- detalle_obligaciones
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_oblig_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

#Tabla Evolución Inversiones en Titularizaciones----------------------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Titularizaciones') %>%
  na.omit() %>%
  mutate(rendimiento_ponderado=rendimiento_ponderado*100,
         rendimiento_ponderado_real=rendimiento_ponderado_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titularizaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

#Tabla detalle de las inversiones en Titularizaciones-----------------------------------------------
aux <- detalle_titularizaciones
aux$emisor <- c('GUAYAQUIL COUNTRY CLUB S.A.',
                'INMOBILIARIA VOLANN'
)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2 ))
#aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titul_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

#Tabla Evolución Inversiones en Renta Variable----------------------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Renta') %>%
  mutate(rendimiento_ponderado=rendimiento_ponderado*100,
         rendimiento_ponderado_real=rendimiento_ponderado_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento,-plazo)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rv_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))


#Tabla detalle de las inversiones en Renta Variable-----------------------------------------------
# aux <- destalle_renta_variable
# aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2 ))
# 
# print( aux_xtab, 
#        file = paste0( parametros$resultado_tablas, 'iess_renta_variable_detalle_inv', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE, include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity,
#        add.to.row = list(pos = list(nrow(aux_xtab)),
#                          command = c(paste("\\bottomrule \n"))))


#Tabla Evolución Inversiones en Fidecomisos----------------------------------------------------
aux <- inv_instrumento %>%
  filter(instrumento=='Fideicomisos') %>%
  mutate(rendimiento_ponderado=rendimiento_ponderado*100,
         rendimiento_ponderado_real=rendimiento_ponderado_real*100,
         ano=as.character(ano)) %>%
  select(-inflacion,-instrumento,-plazo)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_fidecomisos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)),
                         command = c(paste("\\bottomrule \n"))))

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
gc()

