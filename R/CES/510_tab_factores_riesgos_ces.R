message( paste( rep('-', 100 ), collapse = '' ) )

# Cargando información -----------------------------------------------------------------------------
message("\tCargando datos")
load( paste0( parametros$RData_seg, 'IESS_CES_f_i.RData' ) )
load( paste0( parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_ces_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_jub_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_deb_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_des_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_f_i.RData" ) )


# Calculando los cotizantes y beneficiarios del seguro de cesantía --------------------------------
message( '\tGenerando tablas de las tasas siniestralidad' )
tasa_siniestralidad <- full_join( p_ces, p_deb, by = c( 'x', 'sexo' ) ) %>%
  full_join(., p_des, by = c( 'x', 'sexo' ) ) %>%
  full_join(., p_jub, by = c( 'x', 'sexo' ) ) %>%
  full_join(., densidad_cotizacion_int, by = c( 'x'='edad', 'sexo'='genero' ) ) %>%
  select( x, sexo, phi := den_cot_int,  p9 := p_ces_int, p10 := p_jub_int, p11 := p_deb_int, p12 := p_des_int )

# Generando tabla: iess_tab_pob_proy_cot ------------------------------------------------------------
tasa_siniestralidad <- as.data.table( tasa_siniestralidad )

aux_f <- tasa_siniestralidad[ sexo == 'F', list( x,
                                                 phi,
                                                 p9,
                                                 p10,
                                                 p11,
                                                 p12 ) ]

aux_m <- tasa_siniestralidad[ sexo == 'M', list( x,
                                                 phi,
                                                 p9,
                                                 p10,
                                                 p11,
                                                 p12 ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 5, 10 ) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_tasa_siniestralidad.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )



message( '\tGenerando tablas de las f_i' )
f_i_int <- as.data.table(f_i_int)
f_i <- f_i_int[ , f_i := exp( log_f_i_int ) ]

f_i <- as.data.table(dcast(f_i, sexo + x ~ i, value.var = "f_i"))
colnames(f_i) <- c('sexo','x',paste0(rep('f_',10),c(10:18,9)))
f_i[ is.na(f_i), ] <- 0
f_i <- f_i[ , list( sexo, x, f_9, f_10, f_11, f_12, f_13, f_14 )]

aux_f <- f_i[ sexo == 'F', list( x, 
                                 f_9, 
                                 f_10, 
                                 f_11,
                                 f_12,
                                 f_13,
                                 f_14
                                 ) ]

aux_m <- f_i[ sexo == 'M', list( x, 
                                 f_9, 
                                 f_10, 
                                 f_11,
                                 f_12,
                                 f_13,
                                 f_14
                                  ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 4, 12 ) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_f_i.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

