message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tabla información' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_ECO_tablas_estadisticas.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla educadores por estado-----------------------------------------------------------------------
message( '\tTabla educadores por estado' )

aux <- tab_estados %>%
  mutate( H = as.integer( H ),
          M = as.integer( M ),
          total = as.integer( total ) )

aux_xtab <- xtable( aux, digits = c(0,0,0,2,0,2,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_estados_edu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )

# Tabla educadores por Provincias-------------------------------------------------------------------
message( '\tTabla educadores por provincias' )

aux <- tab_provincia

aux_xtab <- xtable( aux, digits = c(0,0,0,2,0,2,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_provincias_edu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


# Tabla rango por Edad------------------------------------------------------------------------------
message( '\tTabla educadores por rangos de edad' )

aux <- tab_rango_edad

aux_xtab <- xtable( aux, digits = c(0,0,0,2,0,2,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_rango_edad_edu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


# Tabla educadores por Provincias-------------------------------------------------------------------
message( '\tTabla educadores por provincias' )

aux <- tab_provincia

aux_xtab <- xtable( aux, digits = c(0,0,0,2,0,2,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_provincias_edu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


# Tabla masa salarial de educadores comunitarios----------------------------------------------------
message( '\tTabla masa salarial por año' )

aux <- tab_masa

aux_xtab <- xtable( aux, digits = c(0,0,rep(2,7) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_tab_masa', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


# Tabla imposiciones en educadores comunitarios hombre----------------------------------------------
message( '\tTabla imposiciones en educadores comunitarios hombre' )

aux <- tab_imp_edu_h

aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

aux_xtab <- xtable( aux, digits = c(0, rep(0, 6) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_imp_edu_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


# Tabla imposiciones en educadores comunitarios mujer-----------------------------------------------
message( '\tTabla imposiciones en educadores comunitarios mujer' )

aux <- tab_imp_edu_m

aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

aux_xtab <- xtable( aux, digits = c(0, rep(0, 6) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_imp_edu_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )



# Tabla imposiciones en educadores comunitarios hombre en el SGO------------------------------------
message( '\tTabla imposiciones en el SGO de los educadores comunitarios hombre' )

aux <- tab_imp_sgo_h

aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

aux_xtab <- xtable( aux, digits = c(0, rep(0, 6) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_imp_sgo_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


# Tabla imposiciones en educadores comunitarios mujer en el SGO------------------------------------
message( '\tTabla imposiciones en el SGO de los educadores comunitarios mujer' )

aux <- tab_imp_sgo_m

aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

aux_xtab <- xtable( aux, digits = c(0, rep(0, 6) ) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'tab_imp_sgo_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after =  nrow(aux) - 1,
       sanitize.text.function = identity )



#Liberar memoria RAM--------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()