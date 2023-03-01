message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
#load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )
load( paste0( parametros$RData, 'IESS_sumarizacion_proyeccion_poblacion.RData' ) )
load( paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_CES_proyeccion_poblacion.RData' ) )


message( '\tGenerando tablas de proyección de la población' )

# Generando tabla: iess_tab_pob_proy_cot ------------------------------------------------------------
pob_proy<-as.data.table(pob_proy)
aux_f <- pob_proy[ sexo == 'F', list( t = t + parametros$anio_ini,
                                      l2_cot_f = l2_cot,
                                      l2_ces_f = l2_ces,
                                      l2_cotc_f = l2_cotc, 
                                      l2_cesc_f = l2_cesc ) ]

aux_f <- aux_f[ , list( l2_cot_f = sum( l2_cot_f, na.rm = TRUE ),
                        l2_ces_f = sum( l2_ces_f, na.rm = TRUE ) , 
                        l2_cotc_f = sum( l2_cotc_f, na.rm = TRUE ),
                        l2_cesc_f = sum( l2_cesc_f, na.rm = TRUE ) ) , 
                   by = list( t ) ]

aux_m <- pob_proy[ sexo == 'M', list( t = t + parametros$anio_ini,
                                      l2_cot_m = l2_cot,
                                      l2_ces_m = l2_ces,
                                      l2_cotc_m = l2_cotc, 
                                      l2_cesc_m = l2_cesc ) ]

aux_m <- aux_m[ , list( l2_cot_m = sum( l2_cot_m, na.rm = TRUE ),
                        l2_ces_m = sum( l2_ces_m, na.rm = TRUE ) , 
                        l2_cotc_m = sum( l2_cotc_m, na.rm = TRUE ),
                        l2_cesc_m = sum( l2_cesc_m, na.rm = TRUE ) ) , 
                by = list( t ) ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )

aux[, t := as.character( t ) ]
aux[ , l2_cot := l2_cot_f + l2_cot_m ]
aux[ , l2_ces := l2_ces_f + l2_ces_m ]
aux[ , l2_cotc := l2_cotc_f + l2_cotc_m ]
aux[ , l2_cesc := l2_cesc_f + l2_cesc_m ]
aux <- aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_cot.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )


# Generando proyeccion de beneficiarios mujeres ----------------------------------------------------
pob_proy<-as.data.table(pob_proy)
aux_f <- pob_proy[ sexo == 'F', list( t = t + parametros$anio_ini,
                                      l9,
                                      l10,
                                      l11,
                                      l12,
                                      l13,
                                      l14,
                                      l15,
                                      l16,
                                      l17,
                                      l18,
                                      lt = l9 + l10 + l11 + l12 + l13 + l14 + l15 + l16 + l17 + l18) ]

aux_f <- aux_f[ , list( l9 = sum( l9, na.rm = TRUE),
                        l10 = sum( l10, na.rm = TRUE),
                        l11 = sum( l11, na.rm = TRUE),
                        l12 = sum( l12, na.rm = TRUE),
                        l13 = sum( l13, na.rm = TRUE),
                        l14 = sum( l14, na.rm = TRUE),
                        l15 = sum( l15, na.rm = TRUE),
                        l16 = sum( l16, na.rm = TRUE),
                        l17 = sum( l17, na.rm = TRUE),
                        l18 = sum( l18, na.rm = TRUE),
                        lt = sum( lt, na.rm = TRUE) ), 
                by = list( t ) ]
aux_f <- aux_f[ t > 2018 ]
aux_f[, t := as.character( t ) ]
xtb_aux <- xtable( aux_f, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_ben_f.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )


# Generando proyeccion de beneficiarios hombres ----------------------------------------------------
pob_proy<-as.data.table(pob_proy)
aux_m <- pob_proy[ sexo == 'M', list( t = t + parametros$anio_ini,
                                      l9,
                                      l10,
                                      l11,
                                      l12,
                                      l13,
                                      l14,
                                      l15,
                                      l16,
                                      l17,
                                      l18,
                                      lt = l9 + l10 + l11 + l12 + l13 + l14 + l15 + l16 + l17 + l18) ]

aux_m <- aux_m[ , list( l9 = sum( l9, na.rm = TRUE),
                        l10 = sum( l10, na.rm = TRUE),
                        l11 = sum( l11, na.rm = TRUE),
                        l12 = sum( l12, na.rm = TRUE),
                        l13 = sum( l13, na.rm = TRUE),
                        l14 = sum( l14, na.rm = TRUE),
                        l15 = sum( l15, na.rm = TRUE),
                        l16 = sum( l16, na.rm = TRUE),
                        l17 = sum( l17, na.rm = TRUE),
                        l18 = sum( l18, na.rm = TRUE),
                        lt = sum( lt, na.rm = TRUE) ), 
                by = list( t ) ]
aux_m <- aux_m[ t > 2018 ]
aux_m[, t := as.character( t ) ]
xtb_aux <- xtable( aux_m, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_ben_m.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

