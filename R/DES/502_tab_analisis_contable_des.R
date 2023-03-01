message( '\tLectura de los balances financieros de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_balances <- paste0( parametros$RData_seg, 'IESS_DES_balances_financieros.RData' )
load( file = file_balances )

message( '\tGenerando tablas de los balances financieros de inversiones' )
#1. Tabla de activos de desempleo---------------------------------------------------------------------
aux <- activos
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_activo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#1. 1.  Tabla de cuentas por cobrar de desempleo----------------------------------------------------------
aux <- cuentas_cobrar
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_cobrar', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#1. 2. Tabla de la evolución del portafolio DES -----------------------------------------------------------------
aux <- rec_admi_BIESS
aux$tasa_crecimiento<-aux$tasa_crecimiento*100
aux$ano<-as.character(aux$ano)

aux_xtab <- xtable( aux, digits = c(0,0,rep(2,3)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rec_admi_BIESS', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#1. 3. Tabla de Fondos disponibles---------------------------------------------------------------------
aux <- fondos_disponibles
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_fondos_disponibles', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)



#2. Tabla de pasivo de desempleo----------------------------------------------------------
aux <- pasivo
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#2. 1.  Tabla de cuentas por pagar de desempleo-----------------------------------------------------
aux <- cuentas_pagar
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_pagar', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#2. 2.  Tabla de pasivos no corrientes de desempleo-------------------------------------------------
aux <- pasivos_no_corrientes
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_no_corrientes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#3. Tabla de patrimonio de desempleo----------------------------------------------------------
aux <- patrimonio
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#3. 1.  Tabla de fondos capitalizados de desempleo--------------------------------------------------
aux <- fondo_cap
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_fondo_cap', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#3. 2.  Tabla de resultados del ejercicio actual de desempleo---------------------------------------
aux <- resultados
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_resultados_ejercicio_actual', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#4. Tabla de ingresos de desempleo----------------------------------------------------------
aux <- ingresos
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

# Tabla de ingresos según DNFTSD de desempleo----------------------------------------------------------
# aux <- ingresos_DNFTSD
# aux$tasa<-aux$tasa*100
# aux$ano<-as.character(aux$ano)
# aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))
# 
# print( aux_xtab, 
#        file = paste0( parametros$resultado_tablas, 'iess_ingresos_DNFTSD', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE, include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity)

#5.  Tabla de gastos de desempleo----------------------------------------------------------
aux <- gastos
aux$incremento_porcentual<-aux$incremento_porcentual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2 ,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
