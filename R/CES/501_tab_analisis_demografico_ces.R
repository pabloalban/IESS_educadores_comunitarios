message( paste( rep('-', 100 ), collapse = '' ) )

message( '\t Tablas del Análisis demográfico de la población cubierta' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_historicos.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_masa_salarial_historico.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_DES_CES_afi_tiempo_aportacion.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_demografia.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_cuentas_individuales.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_afi_tiempo_aportacion.RData' ) ) 
#Evolución de cotizantes a desempleo y cesantía-----------------------------------------------------
aux<-as.data.table(evo_anual_cotizantes_ces_des)
aux[ , tasa_crecimiento:=100*tasa_crecimiento]
aux$aniper<-as.character(aux$aniper)
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 2) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cotizantes_ces_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Evolución de la masa salarial de los cotizantes a desempleo y cesantía-----------------------------------------------------
aux<-as.data.table(evo_masa_sal_ces_des)
aux[ , tasa_crecimiento:=100*tasa_crecimiento]
aux$anio<-as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial_ces_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Tabla salario promedio por aportaciones-----------------------------------------------------------
aux <- copy( afi_tiempo_aportacion )

aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Tabla salarios y afiliadps a desempleo por edad y aportaciones---------------------------------------
message( '\tTabla salarios y afiliados de desempleo por aportaciones y edad' )
aux <- tabla
aux_xtable <- xtable( aux, digits = c( rep(0,14) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

message( '\t Tablas del Análisis demográfico de la población beneficiaria' )

# Evolución de la cesantía general y adicional------------------------------------------------------
aux <- evo_hist_ces
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,6) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# 1. 1. Débitos automáticos-------------------------------------------------------------------------
aux <- evo_hist_debitos 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_debitos_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


# 1. 2. Parte variable de desempleo-----------------------------------------------------------------
aux <- evo_hist_pv_des 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,10) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_pv_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# 1. 3. Cesantía por jubilación---------------------------------------------------------------------
aux <- evo_hist_jub 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_jub_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# 1. 4. Cesantía por fallecimiento del afiliado-----------------------------------------------------
aux <- evo_hist_fall 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, rep(2,3) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_fall_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# 1. 5. Cesantía Normal-----------------------------------------------------------------------------
aux <- evo_hist_ces_normal 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_normal_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# 1. 6. Cesantía a afiliados voluntarios------------------------------------------------------------
aux <- evo_hist_vol 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_vol_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# 1. 7. Cesantía por licencia de maternidad---------------------------------------------------------
aux <- evo_hist_lic_mat
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_lic_mat_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )
# 1. 8. Cesantía por cruze de obligaciones----------------------------------------------------------
aux <- evo_hist_obli 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_obli_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )
# 1. 9. Cesantía a Zafreros-------------------------------------------------------------------------
aux <- evo_hist_zaf 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_zaf_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )
# 1. 10. Reliquidaciones----------------------------------------------------------------------------
aux <- evo_hist_rel 
aux$anio <- as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2,4) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'evo_hist_rel_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Montos por rango de sexo en 2018------------------------------------------------------
message( '\tTabla de Montos por rango de edad y sexo, en 2018' )

# 2. 1. Débitos automáticos-------------------------------------------------------------------------
aux <- dist_monto_debitos
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_debitos_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))
# 2. 2. Parte variable de desempleo-----------------------------------------------------------------
aux <- dist_monto_pv_des
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_pv_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 3. Cesantía Normal-----------------------------------------------------------------------------
aux <- dist_monto_ces_normal
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_normal_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 4. Cesantía por jubilación---------------------------------------------------------------------
aux <- dist_monto_jb
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_jb_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 5. Cesantía por fallecimiento del afiliado-----------------------------------------------------
aux <- dist_monto_fa
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_fa_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 6. Cesantía a afiliados voluntarios------------------------------------------------------------
aux <- dist_monto_vol
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_vol_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 7. Cesantía por licencia de maternidad---------------------------------------------------------
aux <- dist_monto_lic_mat
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_lic_mat_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 8. Cesantía por cruze de obligaciones----------------------------------------------------------
aux <- dist_monto_obli
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_obli_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 9. Cesantía a Zafreros-------------------------------------------------------------------------
aux <- dist_monto_zaf
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_zaf_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. 10. Reliquidaciones----------------------------------------------------------------------------
aux <- dist_monto_rel
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'dist_monto_rel_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 3. Distribución de pagos por rango y sexo------------------------------------------------------------
aux <- Saldo_dist_monto
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'edad_sexo_cuentas_ind_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()