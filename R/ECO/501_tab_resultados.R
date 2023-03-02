message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tabla información' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_ECO_intereses.RData' ) )

load( file = paste0( parametros$RData_seg, 'IESS_ECO_tablas_estadisticas.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Tabla educadores por estado-----------------------------------------------------------------------
message( '\tTabla resultados de intereses' )

aux <- intereses %>%
  mutate( ta = ta * 100,
          anio = as.integer( anio ) ) %>%
  dplyr::select( -ta1,
                 -ta_lead,
                 -i ) %>%
  rbind( (.), c("Total",
               NA,
               as.character(colSums(.[,3:6],  na.rm =TRUE ) ),
               NA,
               as.character(colSums(.[,8],  na.rm =TRUE ) ) ) ) %>%
  distinct( anio, .keep_all = TRUE)

aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

aux_xtab <- xtable( aux, digits = c(0,0,0,2,2,2,2,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_intereses', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )


#Tabla estado del resumen ejecutivo-----------------------------------------------------------------

message( '\tTabla estados del resumen ejecutivo' )

aux <- tab_estados %>%
  dplyr::select( estado,
                 total,
                 total_dist )

aux_xtab <- xtable( aux, digits = c(0,0,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_resumen_estado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux),
                        nrow(aux)-1),
       sanitize.text.function = identity )

# Tabla de resultados del resumen ejecutivo---------------------------------------------------------

message( '\tTabla resultados del resumen ejecutivo' )

aux <- resumen

aux_xtab <- xtable( aux, digits = c(0,0,2) )

aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab,
       file = paste0( parametros$resultado_tablas, 'iess_resumen_resultados', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( 1,
                        3,
                        nrow(aux)-1,
                        nrow(aux)),
       sanitize.text.function = identity )

#Liberar memoria RAM--------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()