message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación de tablas biometricas estáticas del SSC' )

source( 'R/502_tab_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_tasas_mortalidad_estimadas.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_SSC_tablas_mortalidad_todos_estados.RData' ) ) 

# Tablas biometrícas brutas  para jefes activos ----------------------------------------------------
message( '\tTablas biometrícas brutas para jefes activos' )
aux <- copy( prob_mue_act )

aux_f <- aux[ sexo == 'F' & edad >=15 & edad <= 105, list( x = edad, Nx_f = N_exp, Dx_f = N_mue, qx_f = qx_est ) ]
aux_m <- aux[ sexo == 'M' & edad >=15 & edad <= 105, list( x = edad, x_m=edad,
                                                           Nx_m = N_exp, Dx_m = N_mue, qx_m = qx_est ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 4, 0, 0, 0, 4 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_bruta_est_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas con variables biométricas para jefes activos ------------------------------------------------------------
message( '\tTablas biometrícas para jefes activos' )

aux <- copy( afi_mor)

aux_f <- aux[ sexo == 'F' & x >=15 & x <= 105 & t==2020, list( x = x, qx_f = qx, px_f = px, lx_f = lx,
                                                               dx_f = dx, ex_f = ex ) ]
aux_m <- aux[ sexo == 'M' & x >=15 & x <= 105 & t==2020, list( x = x, x_m=x, qx_m = qx, px_m = px, lx_m = lx,
                                                               dx_m = dx, ex_m = ex ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 0, 0, 2, 0, 6, 6, 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_est_variables_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas brutas  para jefes inactivos --------------------------------------------------
message( '\tTablas biometrícas brutas para jefes inactivos' )
aux <- copy( prob_mue_inac )

aux_f <- aux[ sexo == 'F' & edad >=15 & edad <= 105, list( x = edad, Nx_f = N_exp, Dx_f = N_mue, qx_f = qx_est ) ]
aux_m <- aux[ sexo == 'M' & edad >=15 & edad <= 105, list( x = edad, x_m=edad,
                                                           Nx_m = N_exp, Dx_m = N_mue, qx_m = qx_est ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 4, 0, 0, 0, 4 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_bruta_est_inac_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas con variables biométricas para jefes inactivos ------------------------------------------------------------
message( '\tTablas biometrícas para jefes inactivos' )

aux <- copy( afi_mor_inac )

aux_f <- aux[ sexo == 'F' & x >=15 & x <= 105 & t==2020, list( x = x, qx_f = qx, px_f = px, lx_f = lx,
                                                               dx_f = dx, ex_f = ex ) ]
aux_m <- aux[ sexo == 'M' & x >=15 & x <= 105 & t==2020, list( x = x, x_m=x, qx_m = qx, px_m = px, lx_m = lx,
                                                               dx_m = dx, ex_m = ex ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 0, 0, 2, 0, 6, 6, 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_est_variables_inac_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas brutas  para pensionistas de vejez---------------------------------------------
message( '\tTablas biometrícas brutas para pensionistas de vejez' )
aux <- copy( prob_mue_ben )

aux_f <- aux[ tipo=='VEJEZ' & sexo == 'F' & edad >=65 & edad <= 105, list( x = edad, Nx_f = N_exp, Dx_f = N_mue, qx_f = qx_est ) ]
aux_m <- aux[ tipo=='VEJEZ' & sexo == 'M' & edad >=65 & edad <= 105, list( x = edad, x_m=edad,
                                                           Nx_m = N_exp, Dx_m = N_mue, qx_m = qx_est ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 4, 0, 0, 0, 4 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_bruta_est_vejez_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas con variables biométricas para pensionistas de vejez --------------------------
message( '\tTablas biometrícas para pensionistas de vejez' )

aux <- copy( pen_vej_mor )

aux_f <- aux[ sexo == 'F' & x >=65 & x <= 105 & t==2020, list( x = x, qx_f = qx, px_f = px, lx_f = lx,
                                                               dx_f = dx, ex_f = ex ) ]
aux_m <- aux[ sexo == 'M' & x >=65 & x <= 105 & t==2020, list( x = x, x_m=x, qx_m = qx, px_m = px, lx_m = lx,
                                                               dx_m = dx, ex_m = ex ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 0, 0, 2, 0, 6, 6, 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_est_variables_vejez_ssc.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas brutas  para pensionistas de vejez y jefes inactivos---------------------------
message( '\tTablas biometrícas brutas para pensionistas de vejez y jefes inactivos' )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )
aux <- copy( iess_mort )

aux_f <- aux[ sexo == 'F' & edad >=15 & edad <= 105, list( x = edad, Nx_f = Nvi_exp, Dx_f = Nvi_mue, qx_f = qvix_est ) ]
aux_m <- aux[ sexo == 'M' & edad >=15 & edad <= 105, list( x = edad, x_m=edad,
                                                                           Nx_m = Nvi_exp, Dx_m = Nvi_mue, qx_m = qvix_est ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 4, 0, 0, 0, 4 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_bruta_est_vejez_inactivos_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas con variables biométricas para pensionistas de vejez y jefes inactivos --------
message( '\tTablas biometrícas para pensionistas de vejez y jefes inactivos' )

aux <- copy( pen_vej_inac_mor )

aux_f <- aux[ sexo == 'F' & x >=15 & x <= 105 & t==2020, list( x = x, qx_f = qx, px_f = px, lx_f = lx,
                                                               dx_f = dx, ex_f = ex ) ]
aux_m <- aux[ sexo == 'M' & x >=15 & x <= 105 & t==2020, list( x = x, x_m=x, qx_m = qx, px_m = px, lx_m = lx,
                                                               dx_m = dx, ex_m = ex ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 0, 0, 2, 0, 6, 6, 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_est_variables_vejez_inact_ssc.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas brutas  para pensionistas de invalidez-----------------------------------------
message( '\tTablas biometrícas brutas para pensionistas de invalidez' )
aux <- copy( prob_mue_ben )

#aux_f <- aux[ tipo=='INVALIDEZ' & sexo == 'F' & edad >=20 & edad <= 105, list( x = edad, Nx_f = N_exp, Dx_f = N_mue, qx_f = qx_est ) ]
aux_m <- aux[ tipo=='INVALIDEZ' & sexo == 'M' & edad >=20 & edad <= 105, list( x = edad,
                                                                           Nx_m = N_exp, Dx_m = N_mue, qx_m = qx_est ) ]

#aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux_m, digits = c( 0, 0, 0, 0, 4) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_bruta_est_invalidez_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas con variables biométricas para pensionistas de invalidez --------
message( '\tTablas biometrícas para pensionistas de invalidez' )

aux <- copy( pen_inv_mor )

aux_f <- aux[ sexo == 'F' & x >=20 & x <= 105 & t==2020, list( x = x, qx_f = qx, px_f = px, lx_f = lx,
                                                               dx_f = dx, ex_f = ex ) ]
aux_m <- aux[ sexo == 'M' & x >=20 & x <= 105 & t==2020, list( x = x, x_m=x, qx_m = qx, px_m = px, lx_m = lx,
                                                               dx_m = dx, ex_m = ex ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 0, 0, 2, 0, 6, 6, 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_est_variables_invalidez_ssc.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

# Tablas biometrícas con variables biométricas para no afiliados del SSC ---------------------------
message( '\tTablas biometrícas para dependientes del SSC' )

aux <- copy( noafi_mor )

aux_f <- aux[ sexo == 'F' & x >=0 & x <= 105 & t==2020, list( x = x, qx_f = qx, px_f = px, lx_f = lx,
                                                               dx_f = dx, ex_f = ex ) ]
aux_m <- aux[ sexo == 'M' & x >=0 & x <= 105 & t==2020, list( x = x, x_m=x, qx_m = qx, px_m = px, lx_m = lx,
                                                               dx_m = dx, ex_m = ex ) ]

aux <- merge( aux_f, aux_m, by = c( 'x' ) )

xtb_aux <- xtable( aux, digits = c( 0, 0, 6, 6, 0, 0, 2, 0, 6, 6, 0, 0, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_biom_est_variables_dependientes_ssc.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL, sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()