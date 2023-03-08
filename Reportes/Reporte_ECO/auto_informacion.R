message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# Auto información----------------------------------------------------------------------------------

# Carga de datos -----------------------------------------------------------------------------------

load( file = paste0( parametros$RData_seg, 'IESS_ECO_intereses.RData' ) )

load( file = paste0( parametros$RData_seg, 'IESS_ECO_tablas_estadisticas.RData' ) )

#Estadísticas demográficas--------------------------------------------------------------------------

REP$est_afiliado <- format( filter(tab_estados,estado == 'Afiliados Activos')$total, 
                             digits = 0, nsmall = 0, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

REP$est_ces_sgo <- format( filter(tab_estados,estado == 'Cesantes con cotizar al SGO')$total, 
                            digits = 0, nsmall = 0, big.mark = '.', 
                            decimal.mark = ',', format = 'f' )

REP$est_ces_edu <- format( filter(tab_estados,estado == 'Cesantes sin cotizar al SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_fal_sgo <- format( filter(tab_estados,estado == 'Fallecidos con cotizaciones al SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_fal_viu <- format( filter(tab_estados,estado == 'Fallecidos Jubilados de viudez sin cotizar al SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_fal_jub <- format( filter(tab_estados,estado == 'Fallecidos Jubilados del SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_fal_edu <- format( filter(tab_estados,estado == 'Fallecidos sin cotizar al SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_jub_edu <- format( filter(tab_estados,estado == 'Jubilados de viudez sin cotizar al SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_jub_sgo <- format( filter(tab_estados,estado == 'Jubilados del SGO')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$est_total <- format( filter(tab_estados,estado == 'Total')$total, 
                           digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

#Resultados monetarios------------------------------------------------------------------------------

REP$res_masa <- format( filter(resumen, concepto == 'Masa Salarial 1941 a 2008')$valores, 
                         digits = 2, nsmall = 2, big.mark = '.', 
                         decimal.mark = ',', format = 'f' )

REP$res_apor_per <- format( filter(resumen, concepto == 'Aportes Personales a IVM')$valores, 
                        digits = 2, nsmall = 2, big.mark = '.', 
                        decimal.mark = ',', format = 'f' )

REP$res_apor_pat <- format( filter(resumen, concepto == 'Aportes Patronales a IVM')$valores, 
                            digits = 2, nsmall = 2, big.mark = '.', 
                            decimal.mark = ',', format = 'f' )

REP$res_apor_ivm <- format( filter(resumen, concepto == 'Total aportes a IVM')$valores, 
                            digits = 2, nsmall = 2, big.mark = '.', 
                            decimal.mark = ',', format = 'f' )

REP$res_int <- format( filter(resumen, concepto == 'Lucro Cesante')$valores, 
                            digits = 2, nsmall = 2, big.mark = '.', 
                            decimal.mark = ',', format = 'f' )
