message( paste( rep( '-', 100 ), collapse = '' ) )

# 9 = del retiro de la cesantía del afiliado cesante;
# 10= del retiro de la cesantía del jubilado;
# 11 = débito automático por ejecución de las garantías constituidas en créditos 
# quirografarios en el BIESS;
# 12 = parte variable del Seguro de Desempleo;
# 13 = del retiro de la cesantía del afiliado sin relación de dependencia y del afiliado del
# régimen Especial del Seguro Voluntario
# 14 = derechohabientes en caso de fallecimiento del afiliado; 
# 15 = cruce de fondos de cesantía con obligaciones patronales;
# 16 = del retiro de la cesantía del afiliado de la industria azucarera;
# 17 = del retiro de la cesantía por licencia de maternidad o paternidad; y
# 18 = reliquidación de fondos de Cesantía por aportes extemporáneos.


# Cargando información -----------------------------------------------------------------------------
message("\tCargando datos")
load( paste0( parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_ces_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_jub_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_deb_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_des_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_f_i.RData" ) )
load( paste0( parametros$RData, "IESS_proyeccion_poblacion.RData" ) )

message("\tProyentando de población de beneficiarios")
# cat( names( pob_proy ), sep = "', '" )
setnames( pob_proy, c( 't', 'sexo', 'x', 'l1', 'l2', 'l3', 'l4', 'l5', 
                       'l1_1', 'l1_2', 'l1_5', 'l2_2', 'l2_3', 'l2_4', 'l2_5', 'l3_3', 'l3_5', 
                       'l4_4', 'l4_5', 'tau', 'l2_cot', 'l2_ces', 'l6', 'l7', 'l8' ) )

# Calculando los cotizantes y beneficiarios del seguro de cesantía --------------------------------
tasa_siniestralidad <- full_join( p_ces, p_deb, by = c( 'x', 'sexo' ) ) %>%
  full_join(., p_des, by = c( 'x', 'sexo' ) ) %>%
  full_join(., p_jub, by = c( 'x', 'sexo' ) ) %>%
  select( x, sexo, p9 := p_ces_int, p11 := p_deb_int, p12 := p_des_int, p10 := p_jub_int )

den_cot <- as.data.table( densidad_cotizacion_int )
den_cot <- den_cot[ , list( sexo = genero, x = edad, den_cot_int ) ]

pob_proy <- merge.data.table( pob_proy, den_cot, by = c( 'sexo', 'x' ) )
pob_proy <- merge.data.table( pob_proy, tasa_siniestralidad, by = c( 'sexo', 'x' ) )

pob_proy[ , `:=`( l2_cotc = l2_cot * den_cot_int,
                  l2_cesc = l2_ces * den_cot_int  ) ]

pob_proy[ , `:=`( l9 = l2_cesc * p9,
                  l10 = ( l2_3 + l2_4 ) * p10,
                  l11 = l2_cesc * p11,
                  l12 = l2_cesc * p12 ) ]

# Proporciones para beneficios menos significativos
factor13 <- 5337 / ( 138165 +  93084 + 4319 )
factor14 <- 4167 / ( 138165 +  93084 + 4319 )
factor15 <- 403 / ( 138165 +  93084 + 4319 )
factor16 <- 221 / ( 138165 +  93084 + 4319 )
factor17 <- 14 / ( 138165 +  93084 + 4319 )
factor18 <- 300 / ( 138165 +  93084 + 4319 )

pob_proy[ , `:=`( l13 = ( l9 + l11 + l12 ) * factor13,
                  l14 = ( l9 + l11 + l12 ) * factor14,
                  l15 = ( l9 + l11 + l12 ) * factor15,
                  l16 = ( l9 + l11 + l12 ) * factor16,
                  l17 = ( l9 + l11 + l12 ) * factor17,
                  l18 = ( l9 + l11 + l12 ) * factor18 ) ]

pob_proy_anual <- pob_proy[ , list(  l1 = sum( l1 ),
                                     l2 = sum( l2 ),
                                     l3 = sum( l3 ),
                                     l4 = sum( l4 ),
                                     l5 = sum( l5 ),
                                     l6 = sum( l6 ),
                                     l7 = sum( l7 ),
                                     l7_men = sum( ( x < 18 ) * l7 ),
                                     l8 = sum( l8 ),
                                     l2_cot = sum( l2_cot ),
                                     l2_cotc = sum( l2_cotc ),
                                     l2_ces = sum( l2_ces ),
                                     l2_cesc = sum( l2_cesc ),
                                     l1_1 = sum( l1_1 ),
                                     l1_2 = sum( l1_2 ),
                                     l1_5 = sum( l1_5 ),
                                     l2_2 = sum( l2_2 ),
                                     l2_3 = sum( l2_3 ),
                                     l2_4 = sum( l2_4 ), 
                                     l2_5 = sum( l2_5 ),
                                     l3_3 = sum( l3_3 ),
                                     l3_5 = sum( l3_5 ),
                                     l4_4 = sum( l4_4 ),
                                     l4_5 = sum( l4_5 ),
                                     l9 = sum( l9, na.rm = TRUE ),
                                     l10 = sum( l10, na.rm = TRUE ),
                                     l11 = sum( l11, na.rm = TRUE ),
                                     l12 = sum( l12, na.rm = TRUE ),
                                     l13 = sum( l13, na.rm = TRUE ),
                                     l14 = sum( l14, na.rm = TRUE ),
                                     l15 = sum( l15, na.rm = TRUE ),
                                     l16 = sum( l16, na.rm = TRUE ),
                                     l17 = sum( l17, na.rm = TRUE ),
                                     l18 = sum( l18, na.rm = TRUE ) ),
                            by = list( t ) ]

# Guardando la proyección de la población cotizante y beneficiaría de desempleo --------------------
message("\tGuardando proyección de población")
save( lf, lm, pob_proy, pob_proy_anual,
      file = paste0( parametros$RData_seg, "IESS_CES_proyeccion_poblacion.RData" ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( "parametros" ) ) ] )
gc()
