message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación Tablas del Capítulo de Analisis Demográfico' )

source( 'R/502_tab_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) ) 
load( file = paste0( parametros$RData, 'IVM/', 'IESS_IVM_analisis_demografico.RData' ) ) 

# Afiliados activos SGO ----------------------------------------------------------------------------
message( '\tTabla de Afiliados activos SGO' )

aux <- copy( pob_afi_ini )
aux[ , Tasa:=100*Tasa]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_activos_sgo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Masa salarial del SGO ----------------------------------------------------------------------------
message( '\tTabla de Masa Salarial del SGO' )
aux <- copy( masa_salarial_ini)
aux[ , anio := as.character( anio ) ]
aux[ , Tasa := Tasa * 100 ]
aux_nombres <- c('Año',
                 'Masa_Anual_Mas','Masa_Anual_Fem', 'Masa_Anual',
                 'Masa_Mensual_Mas', 'Masa_Mensual_Fem', 'Masa_Mensual',
                 'Crecimiento_anual', 'Tasa')

aux_xtable <- xtable( aux, digits = c(0, 0, rep(2,8)))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de Tiempo de aportación ambos sexos del SGO-------------------------------------------------
message( '\tTabla de Tiempo de aportación ambos sexos del SGO' )

aux <- copy( afi_tiempo_aportacion )
ed <- aux[ , 1 ]
aux[ is.na(aux)] <-""
aux <- as.data.frame(aux)
aux[c(35), c(1:13) ] <- paste("\\textbf{", aux[c(35), c(1:13) ], "}")
aux[c(36), c(1:13) ] <- paste("\\textbf{", aux[c(36), c(1:13) ], "}")
aux[c(1:dim(aux)[1]), 13 ] <- paste("\\textbf{", aux[c(1:dim(aux)[1]), 13 ], "}")

aux_xtable <- xtable( aux)
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Tabla tiempo de aportación de afiliados hombres---------------------------------------------------
message( '\tTiempo de aportación de afiliados hombres al SGO' )
aux <-copy(afi_tiempo_aportacion_h)
aux[ is.na(aux)] <-""
aux <- as.data.frame(aux)
aux[c(35), c(1:13) ] <- paste("\\textbf{", aux[c(35), c(1:13) ], "}")
aux[c(36), c(1:13) ] <- paste("\\textbf{", aux[c(36), c(1:13) ], "}")
aux[c(1:dim(aux)[1]), 13 ] <- paste("\\textbf{", aux[c(1:dim(aux)[1]), 13 ], "}")

aux_xtable <- xtable( aux)
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Tabla tiempo de aportación de afiliados ----------------------------------------------------------
message( '\tTiempo de aportación de afiliados mujeres' )
aux <- copy(afi_tiempo_aportacion_m)
aux[ is.na(aux)] <-""
aux <- as.data.frame(aux)
aux[c(35), c(1:13) ] <- paste("\\textbf{", aux[c(35), c(1:13) ], "}")
aux[c(36), c(1:13) ] <- paste("\\textbf{", aux[c(36), c(1:13) ], "}")
aux[c(1:dim(aux)[1]), 13 ] <- paste("\\textbf{", aux[c(1:dim(aux)[1]), 13 ], "}")
aux_xtable <- xtable( aux)

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )


# Jefes de Familia del SSC--------------------------------------------------------------------------
message( '\tTabla de Jefes de Familia del SSC' )
aux <- copy( afi_hist_ssc )
aux[ , Anio := as.character( Anio ) ]
aux[ , Porcentaje_incremento:=Porcentaje_incremento*100]

aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 2 ))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_hist_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Masa salarial del SSSC----------------------------------------------------------------------------
message( '\tTabla de Masa Salarial del SSC' )
aux <- copy( masa_ssc )
aux[ , anio := as.character( anio ) ]
aux[ , por:=por*100]

aux_xtable <- xtable( aux, digits = c(0, 0, 2 ,2, 2, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Jubilacion por vejez------------------------------------------------------------------------------
message( '\tTabla de jubilados de vejez del SSC' )
aux <- copy(jub_vejez_ssc)
aux[ , Anio := as.character( Anio ) ]
aux[ , Tasa_crecimiento_pob:=Tasa_crecimiento_pob * 100]
aux[ , Tasa_crecimiento_ben:=Tasa_crecimiento_ben * 100]
aux[ ,  por_cre:= por_cre * 100]
aux_t <- aux[tipo=='Total', -1]

aux_xtable <- xtable( aux_t, digits = c(0, 0, 0, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jub_vejez_total_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux_t <- aux[tipo=='Hombres', -1]
aux_xtable <- xtable( aux_t, digits = c(0, 0, 0, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jub_vejez_male_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux_t <- aux[tipo=='Mujeres', -1]
aux_xtable <- xtable( aux_t, digits = c(0, 0, 0, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jub_vejez_female_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Jubilada por invalidez del SSC -------------------------------------------------------------------
message( '\tTabla de jubilados de invalidez del SSC' )
aux <- copy( jub_invalidez_ssc)
aux[ , Anio := as.character( Anio ) ]
aux[ , Tasa_crecimiento_pob:=Tasa_crecimiento_pob * 100]
aux[ , Tasa_crecimiento_ben:=Tasa_crecimiento_ben * 100]
aux[ , por_cre:=por_cre * 100]

aux_t <- aux[tipo=='Total', -1]
aux_xtable <- xtable( aux_t, digits = c(0, 0, 0, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jub_invalidez_total_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux_t <- aux[tipo=='Hombres', -1]
aux_xtable <- xtable( aux_t, digits = c(0, 0, 0, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jub_invalidez_male_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux_t <- aux[tipo=='Mujeres', -1]
aux_xtable <- xtable( aux_t, digits = c(0, 0, 0, 2, 2, 2, 2, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jub_invalidez_female_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Dependientes del SSC------------------------------------------------------------------------------
message( '\tTabla de dependientes del SSC' )
aux <- copy(dependientes_ssc)
aux <- aux[ tipo=='Total']
aux[ , Anio := as.character( Anio ) ]
aux[ , Porcentaje_de_crecimiento := Porcentaje_de_crecimiento * 100 ]
aux[ , tipo:=NULL]

aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_dependientes_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Dependientes del SSC por parentezco --------------------------------------------------------------
message( '\tTabla de dependientes del SSC por parentezco' )
aux <- copy(parentesco_ssc)
aux[ , Porcentaje:= Porcentaje * 100]

aux_xtable <- xtable( aux, digits = c(0, 0, 0, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_parentesco_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 17,
       sanitize.text.function = identity )

#Dependientes del SSC por zona ---------------------------------------------------------------------
message( '\tTabla de dependientes del SSC por zona' )
aux <- copy(zona_ssc)
aux[ , Porcentaje:=Porcentaje*100]
aux_xtable <- xtable( aux, digits = c(0, 0, 0, 2))
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_zona_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 7,
       sanitize.text.function = identity )

#Dependientes del SSC por provincia-----------------------------------------------------------------
message( '\tTabla de dependientes del SSC por provincia' )
aux <- copy( prov_ssc )
aux[ , por:=Porcentaje*100]
aux[ , Porcentaje:=NULL]
aux_jef <- copy( prov_jef_ssc )

prov <- cbind( aux, aux_jef[ , c(2,3)])
prov[ , Porcentaje:=Porcentaje*100]
aux_xtable <- xtable( prov, digits = c(0, 0, 0, 2, 0, 2))

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_prov_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 24,
       sanitize.text.function = identity )

#Edad promedio de pensionistas del SSC   -----------------------------------------------------------
message( '\tEdad promedio de pensionistas del SSC ' )
aux <- copy( edad_prom_pen )
aux_xtable <- xtable( aux, digits = c(0, 0, 2, 2, 2))

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_edad_prom_pen_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 2,
       sanitize.text.function = identity )

# Tiempo de aportación de jefes de familia total ---------------------------------------------------
message( '\tTiempo de aportación de jefes de familia total' )
aux <- copy( jef_tiempo_aportacion )
ed <- aux[ , 1 ]
aux <- var_num( aux, 2)

a <- format( round(aux[seq(1,30, 2), 2:dim(aux)[2] ], 0), 
                                              decimal.mark = ',', big.mark = '.' )
b <- format( round(aux[seq(2,30, 2), 2:dim(aux)[2] ], 2) , 
                                              decimal.mark = ',', big.mark = '.' ) 
b <- NA_gsub( b, 1)
b <- esp_gsub(b, 1)

for (i in 1:dim(b)[1]){
        for ( j in 1:dim(b)[2]) {
               if( b[ i , j ] == ""){
                   b[ i , j ]    
               } else{
                b[ i , j ] <- paste0('USD ', b[ i , j ] )
               }
        }
}

c<- data.frame( matrix( 0, 30, 12 ) )
m <- 0
for( i in 1:30){ 
        k <- i 
        c[ k + m,  ] <-  a[ i, ] 
        c[ k + m + 1, ] <-  b[ i, ]
        m <- i
}
c <- c[1:30,]
c <- cbind( ed, c)
c <- NA_gsub( c, 1)
c[ is.na(c)] <-""
c <- as.data.frame(c)
c[c(29), c(1:13) ] <- paste("\\textbf{", c[c(29), c(1:13) ], "}")
c[c(30), c(1:13) ] <- paste("\\textbf{", c[c(30), c(1:13) ], "}")
c[c(1:dim(c)[1]), 13 ] <- paste("\\textbf{", c[c(1:dim(c)[1]), 13 ], "}")

aux_xtable <- xtable( c )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jefes_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Tiempo de aportación de jefes de familia sexo masculino ------------------------------------------
message( '\tTiempo de aportación de jefes de familia sexo masculino ' )
aux <- copy( jef_tiempo_aportacion_male )
aux <- var_num( aux, 2)

a <- format( round(aux[seq(1,30, 2), 2:dim(aux)[2] ], 0), 
             decimal.mark = ',', big.mark = '.' )
b <- format( round(aux[seq(2,30, 2), 2:dim(aux)[2] ], 2) , 
             decimal.mark = ',', big.mark = '.' ) 
b <- NA_gsub( b, 1)
b <- esp_gsub(b, 1)

for (i in 1:dim(b)[1]){
        for ( j in 1:dim(b)[2]) {
                if( b[ i , j ] == ""){
                        b[ i , j ]    
                } else{
                        b[ i , j ] <- paste0('USD ', b[ i , j ] )
                }
        }
}

c <- data.frame( matrix( 0, 30, 12 ) )
m <- 0
for( i in 1:30){ 
        k <- i 
        c[ k + m,  ] <-  a[ i, ] 
        c[ k + m + 1, ] <-  b[ i, ]
        m <- i
}
c <- c[1:30,]
c <- cbind( ed, c)
c <- NA_gsub( c, 1)
c[ is.na(c)] <-""
c <- as.data.frame(c)
c[c(29), c(1:13) ] <- paste("\\textbf{", c[c(29), c(1:13) ], "}")
c[c(30), c(1:13) ] <- paste("\\textbf{", c[c(30), c(1:13) ], "}")
c[c(1:dim(c)[1]), 13 ] <- paste("\\textbf{", c[c(1:dim(c)[1]), 13 ], "}")

aux_xtable <- xtable( c )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jefes_tiempo_aportacion_male', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Tiempo de aportación de jefes de familia sexo femenino -------------------------------------------
message( '\tTiempo de aportación de jefes de familia sexo femenino' )
aux <- copy( jef_tiempo_aportacion_female )
aux <- var_num( aux, 2)

a <- format( round(aux[seq(1,30, 2), 2:dim(aux)[2] ], 0), 
             decimal.mark = ',', big.mark = '.' )
b <- format( round(aux[seq(2,30, 2), 2:dim(aux)[2] ], 2) , 
             decimal.mark = ',', big.mark = '.' ) 
b <- NA_gsub( b, 1)
b <- esp_gsub(b, 1)

for (i in 1:dim(b)[1]){
        for ( j in 1:dim(b)[2]) {
                if( b[ i , j ] == ""){
                        b[ i , j ]    
                } else{
                        b[ i , j ] <- paste0('USD ', b[ i , j ] )
                }
        }
}

c <- data.frame( matrix( 0, 30, 12 ) )
m <- 0
for( i in 1:30){ 
        k <- i 
        c[ k + m,  ] <-  a[ i, ] 
        c[ k + m + 1, ] <-  b[ i, ]
        m <- i
}
c <- c[1:30,]
c <- cbind( ed, c)
c <- NA_gsub( c, 1)
c[ is.na(c)] <-""
c <- as.data.frame(c)
c[c(29), c(1:13) ] <- paste("\\textbf{", c[c(29), c(1:13) ], "}")
c[c(30), c(1:13) ] <- paste("\\textbf{", c[c(30), c(1:13) ], "}")
c[c(1:dim(c)[1]), 13 ] <- paste("\\textbf{", c[c(1:dim(c)[1]), 13 ], "}")

aux_xtable <- xtable( c )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_jefes_tiempo_aportacion_female', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# Atenciones médicas por meses de los dispensarios del SSC-----------------------------------------
message( '\tAtenciones médicas por meses de los dispensarios del SSC' )
aux <- copy( aten_med_mes )

aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_mes_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

# Atenciones médicas por meses y días de los dispensarios del SSC-----------------------------------
message( '\tAtenciones médicas por meses y días de los dispensarios del SSC' )
aux1 <- copy( aten_med_dia )

aux <- aux1[ anio==2016, -1 ]
aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_dia_2016_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

aux <- aux1[ anio==2017, -1 ]
aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_dia_2017_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

aux <- aux1[ anio==2018, -1 ]
aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_dia_2018_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

aux <- aux1[ anio==2019, -1 ]
aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_dia_2019_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

aux <- aux1[ anio==2020, -1 ]
aux_xtable <- xtable( aux, digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_dia_2020_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

# Atenciones médicas por año y provincia de los dispensarios del SSC--------------------------------
message( '\tAtenciones médicas por año y provincia de los dispensarios del SSC' )
aux <- copy( aten_med_prov )
aux <- data.table( var_num( aux, 2 ) )
aux[ , A2016P:=A2016P*100]
aux[ , A2017P:=A2017P*100]
aux[ , A2018P:=A2018P*100]
aux[ , A2019P:=A2019P*100]
aux[ , A2020P:=A2020P*100]

aux_xtable <- xtable( aux, digits = c(0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_atenciones_medicas_prov_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-1,
       sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

