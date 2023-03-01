message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de los balances financieros de cesantía' )

# Carga de datos -----------------------------------------------------------------------------------
file_balances <- paste0( parametros$RData_seg, 'IESS_CES_balances_financieros.RData' )
load( file = file_balances )

message( '\tGenerando tablas de los balances financieros de cesantía' )
#1. Tabla de activos de cesantía--------------------------------------------------------------------
aux <- activos
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_activo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#1. 1.  Tabla de activos componentes----------------------------------------------------------------
aux <- a11
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_activo_comp', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#1. 2.  Tabla de activos análisis horizontal--------------------------------------------------------
aux <- a12
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_activo_hor', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#1. 3.  Tabla de activos análisis vertical----------------------------------------------------------
aux <- a13
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_activo_ver', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1)
       )

#2. Tabla de pasivos de cesantía--------------------------------------------------------------------
aux <- pasivo
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#2. 1.  Tabla de pasivo componentes-----------------------------------------------------------------
aux <- p21
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_comp', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#2. 2.  Tabla de pasivos análisis horizontal--------------------------------------------------------
aux <- p22
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_hor', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#2. 3.  Tabla de pasivos análisis vertical----------------------------------------------------------
aux <- p23
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pasivo_ver', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity )

#3. Tabla de patrimonio de cesantía-----------------------------------------------------------------
aux <- patrimonio
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#3. 1.  Tabla de patrimonio componentes-------------------------------------------------------------
aux <- p31
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_comp', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#3. 2.  Tabla de patrimonio análisis horizontal-----------------------------------------------------
aux <- p32
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_hor', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#3. 3.  Tabla de patrimonio análisis vertical-------------------------------------------------------
aux <- p33
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_ver', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity 
       )


#4. Tabla de gastos de cesantía---------------------------------------------------------------------
aux <- gastos
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#4. 1.  Tabla de gastos componentes-----------------------------------------------------------------
aux <- g41
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_comp', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#4. 2.  Tabla de gastos análisis horizontal---------------------------------------------------------
aux <- g42
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_hor', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#4. 3.  Tabla de gastos análisis vertical-----------------------------------------------------------
aux <- g43
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_ver', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#5. Tabla de ingresos de cesantía-------------------------------------------------------------------
aux <- ingresos
aux$incremento_porcentual_anual<-aux$incremento_porcentual_anual*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#5. 1.  Tabla de ingresos componentes---------------------------------------------------------------
aux <- i51
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_comp', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#5. 2.  Tabla de ingresos análisis horizontal-------------------------------------------------------
aux <- i52
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_hor', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#5. 3.  Tabla de ingresos análisis vertical---------------------------------------------------------
aux <- i53
aux[2:NCOL(aux)]<-aux[2:NCOL(aux)] * 100
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_ver', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux), nrow(aux)-1),
       sanitize.text.function = identity
       )

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
