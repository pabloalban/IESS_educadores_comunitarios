message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación Tablas del Capítulo de Analisis Demográfico' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_financiero.RData' ) ) 




#------------------################### ACTIVOS ###################---------------------
# Tasa de vaciación de los activos ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Activo del Fondo de SSC  ' )

aux <- copy(activos_hist_ssc)
aux[ , `Tasa crecimiento (%)`:=100*`Tasa crecimiento (%)`]
aux[ , Anio:=as.character(Anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_activos_hist_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tasa de vaciación de los activos ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Activo del Fondo de SSC  ' )


aux <- copy(activos_variacion_ssc)
for (i in 1:nrow(aux)) {
        if(i%%2 == 0){
                aux[i, ] <-aux[i, ] *100
        }
}

aux<-round(aux,2)

ltx<-data.frame( "A"= c(" ", "\\multirow{-2}{*}{Fondos Disponibles}"," ", "\\multirow{-2}{*}{Inversiones}"," ", "\\multirow{-2}{*}{Cuentas por Cobrar} ", " ", "\\multirow{-2}{*}{Propiedad Planta y Equipo}", 
                           " ", "\\multirow{-2}{*}{Deuda del Gobierno}", " ", "\\multirow{-2}{*}{Intereses por Cobrar}", " ", "\\multirow{-2}{*}{Otros Activos}",
                 " ", "\\multirow{-2}{*}{Inventarios}", " ", "\\multirow{-2}{*}{Activos   Intangibles}" , " ", "\\multirow{-2}{*}{Activos clasificados como mant}", " \\raggedright{Total Activo} "), 
                 "B"= c(" USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n", " "))

aux<-cbind(ltx,aux)

aux1<-aux[,c(1:8)]
aux2<-aux[,c(1:2,9:13)]




aux_xtable1 <- xtable( aux1, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2,2 ) )
aux_xtable2 <- xtable( aux2, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2) )

print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_activos_variacion_ssc1', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 20,
       sanitize.text.function = identity )
print( aux_xtable2,
       file = paste0( parametros$resultado_tablas, 'iess_activos_variacion_ssc2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 20,
       sanitize.text.function = identity )


# Tasa de vaciación de los activos ----------------------------------------------------------------------------
message( 'Activo del Fondo del SSC al 31 de diciembre de cada año (%) ' )
aux <- copy(activos_del_fondo_ssc)
aux <- aux[,`2011` := `2011`*100 ]
           aux <- aux[, `2012` := `2012`*100] 
           aux <- aux[, `2013` := `2013`*100] 
           aux <- aux[, `2014` := `2014`*100] 
           aux <- aux[, `2015` := `2015`*100]
           aux <- aux[, `2016` := `2016`*100] 
           aux <- aux[, `2017` := `2017`*100] 
           aux <- aux[, `2018` := `2018`*100] 
           aux <- aux[, `2019` := `2019`*100] 
           aux <- aux[, `2020` := `2020`*100]
           aux1 <- round(aux[,c(2:11)],2)
           aux<-cbind(aux[,1],aux1)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 , 2 ,2, 2, 2, 2, 2, 2  ) )


print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_activos_del_fondo_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 10,
       sanitize.text.function = identity )

# Evolución de Cuentas por Cobrar del Fondo de SSC ----------------------------------------------------------------------------
message( 'Evolución de Cuentas por Cobrar del Fondo de SSC ' )
aux <- copy(variacion_cuentas_por_cobrar_ssc[,`Tasa crecimiento (%)`:= `Tasa crecimiento (%)`*100])


aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'variacion_cuentas_por_cobrar_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tasa de vaciación de los activos ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Activo del Fondo de SSC  ' )

aux <- copy(variacion_cuentas_por_cobrar_ssc)
aux[ , `Tasa crecimiento (%)`:=100*`Tasa crecimiento (%)`]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_variacion_cuentas_por_cobrar_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )






#------------------################### PASIVOS ###################---------------------
# Tasa de vaciación de los pasivos ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Pasivo del Fondo de SSC  ' )

aux <- copy(pasivos_hist_ssc)
aux[ , `Tasa crecimiento (%)`:=100*`Tasa crecimiento (%)`]
aux[ , Anio:=as.character(Anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_hist_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tasa de vaciación de los pasivos ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Pasivo del Fondo de SSC  ' )


aux <- copy(pasivos_variacion_ssc)
for (i in 1:nrow(aux)) {
        if(i%%2 == 0){
                aux[i, ] <-aux[i, ] *100
        }
}

aux<-round(aux,2)
ltx<-data.frame( "A"= c(" ", "\\multirow{-2}{*}{Prestaciones y  Beneficios}"," ", "\\multirow{-2}{*}{Cuentas por Cobrar} ", " ", "\\multirow{-2}{*}{Pasivo Diferido}", 
                        " ", "\\multirow{-2}{*}{Deuda Gobierno Contra}", " ", "\\multirow{-2}{*}{Provisiones}", " ", "\\multirow{-2}{*}{Pasivos Corrientes}",
                        " ", "\\multirow{-2}{*}{Pasivos No Corrientes}", " \\raggedright{Total Pasivo} "), 
                 "B"= c(" USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n", " "))
aux<-cbind(ltx,aux)

aux1<-aux[,c(1:8)]
aux2<-aux[,c(1:2,9:13)]




aux_xtable1 <- xtable( aux1, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2,2 ) )
aux_xtable2 <- xtable( aux2, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2) )

print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_variacion_ssc1', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 14,
       sanitize.text.function = identity )
print( aux_xtable2,
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_variacion_ssc2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 14,
       sanitize.text.function = identity )
# Tasa de vaciación de los pasivos ----------------------------------------------------------------------------
message( 'Pasivo del Fondo del SSC al 31 de diciembre de cada año (%) ' )
aux <- copy(pasivos_del_fondo_ssc)
aux <- aux[,`2011` := `2011`*100 ]
aux <- aux[, `2012` := `2012`*100] 
aux <- aux[, `2013` := `2013`*100] 
aux <- aux[, `2014` := `2014`*100] 
aux <- aux[, `2015` := `2015`*100]
aux <- aux[, `2016` := `2016`*100] 
aux <- aux[, `2017` := `2017`*100] 
aux <- aux[, `2018` := `2018`*100] 
aux <- aux[, `2019` := `2019`*100] 
aux <- aux[, `2020` := `2020`*100]
aux1 <- round(aux[,c(2:11)],2)
aux<-cbind(aux[,1],aux1)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 , 2 ,2, 2, 2, 2, 2, 2  ) )


print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pasivos_del_fondo_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 7,
       sanitize.text.function = identity )




# Evolución de Cuentas por Pagar del Fondo de SSC ----------------------------------------------------------------------------
message( 'Evolución de Cuentas por Pagar del Fondo de SSC ' )
aux <- copy(variacion_cuentas_por_pagar_ssc[,`Tasa crecimiento (%)`:= `Tasa crecimiento (%)`*100])


aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_variacion_cuentas_por_pagar_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )




#------------------################### PATRIMONIO ###################---------------------
# Tasa de vaciación de los patrimonio ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Patrimonio del Fondo de SSC  ' )

aux <- copy(patrimonio_hist_ssc)
aux[ , `Tasa crecimiento (%)`:=100*`Tasa crecimiento (%)`]
aux[ , Anio:=as.character(Anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_hist_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tasa de vaciación del patrimonio----------------------------------------------------------------------------
message( '\tTabla de Evolución del Patrimonio del Fondo de SSC  ' )


aux <- copy(patrimonio_variacion_ssc)
for (i in 1:nrow(aux)) {
        if(i%%2 == 0){
                aux[i, ] <-aux[i, ] *100
        }
}
aux<-round(aux,2)
ltx<-data.frame( "A"= c(" ", "\\multirow{-2}{*}{Fondos Capitalizados}"," ", "\\multirow{-2}{*}{Super\\'{a}vit Revaluaci\\'{o}n} ", " ", "\\multirow{-2}{*}{Resultados}", 
                        " ", "\\multirow{-2}{*}{Reservas}", " ", "\\multirow{-2}{*}{Otros fondos Capitalizados}", " ", "\\multirow{-2}{*}{Aportes Patrimoniales}", " \\raggedright{Total Patrimonio} "), 
                 "B"= c(" USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n", " "))
aux<-cbind(ltx,aux)

aux1<-aux[,c(1:8)]
aux2<-aux[,c(1:2,9:13)]




aux_xtable1 <- xtable( aux1, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2,2 ) )
aux_xtable2 <- xtable( aux2, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2) )

print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_variacion_ssc1', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 12,
       sanitize.text.function = identity )
print( aux_xtable2,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_variacion_ssc2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 12,
       sanitize.text.function = identity )
# Tasa de vaciación del patrimonio----------------------------------------------------------------------------
message( 'Patrimonio del Fondo del SSC al 31 de diciembre de cada año (%) ' )
#patrimonio_del_fondo_ssc
# pasivos_del_fondo_ssc
aux <- copy(patrimonio_del_fondo_ssc)
aux <- aux[,`2011` := `2011`*100 ]
aux <- aux[, `2012` := `2012`*100] 
aux <- aux[, `2013` := `2013`*100] 
aux <- aux[, `2014` := `2014`*100] 
aux <- aux[, `2015` := `2015`*100]
aux <- aux[, `2016` := `2016`*100] 
aux <- aux[, `2017` := `2017`*100] 
aux <- aux[, `2018` := `2018`*100] 
aux <- aux[, `2019` := `2019`*100] 
aux <- aux[, `2020` := `2020`*100]
aux1 <- round(aux[,c(2:11)],2)
aux<-cbind(aux[,1],aux1)
aux[2, 1] <- "Super\\\'{a}vit Revaluaci\\\'{o}n"
aux[7, 1] <- "Total Patrimonio"
aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 , 2 ,2, 2, 2, 2, 2, 2  ) )


print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_del_fondo_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 6,
       sanitize.text.function = identity )


#------------------################### INGRESOS ###################---------------------
# Tasa de vaciación de los Ingresos ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Ingresos del Fondo de SSC  ' )

aux <- copy(Ingresos_hist_ssc)
aux[ , `Tasa crecimiento (%)`:=100*`Tasa crecimiento (%)`]
aux[ , Anio:=as.character(Anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Ingresos_hist_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tasa de vaciación del Ingresos----------------------------------------------------------------------------
message( '\tTabla de Evolución del Ingresos del Fondo de SSC  ' )


aux <- copy(Ingresos_variacion_ssc)
for (i in 1:nrow(aux)) {
        if(i%%2 == 0){
                aux[i, ] <-aux[i, ] *100
        }
}
aux<-round(aux,2)
ltx<-data.frame( "A"= c(" ", "\\multirow{-2}{*}{Ingresos de la  Operaci\\'{o}n}"," ", "\\multirow{-2}{*}{Ingresos Financieros} ", " ", "\\multirow{-2}{*}{Ingresos por Arriendo}", 
                        " ", "\\multirow{-2}{*}{Ingresos Extraordinarios}", " ", "\\multirow{-2}{*}{Otros Resultados Integrales}",
                        "\\raggedright{Total Ingresos} "), 
                 "B"= c(" USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n"," USD " ," \\% Participaci\\'{o}n", " "))
aux<-cbind(ltx,aux)

aux1<-aux[,c(1:8)]
aux2<-aux[,c(1:2,9:13)]

aux_xtable1 <- xtable( aux1, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2,2 ) )
aux_xtable2 <- xtable( aux2, digits = c( 0, 0, 2, 2, 2, 2 , 2 ,2) )

print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_Ingresos_variacion_ssc1', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 10,
       sanitize.text.function = identity )
print( aux_xtable2,
       file = paste0( parametros$resultado_tablas, 'iess_Ingresos_variacion_ssc2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 10,
       sanitize.text.function = identity )
# Tasa de vaciación del Ingresos----------------------------------------------------------------------------
message( 'Ingresos del Fondo del SSC al 31 de diciembre de cada año (%) ' )
aux <- copy(Ingresos_del_fondo_ssc )
aux <- aux[,`2011` := `2011`*100 ]
aux <- aux[, `2012` := `2012`*100] 
aux <- aux[, `2013` := `2013`*100] 
aux <- aux[, `2014` := `2014`*100] 
aux <- aux[, `2015` := `2015`*100]
aux <- aux[, `2016` := `2016`*100] 
aux <- aux[, `2017` := `2017`*100] 
aux <- aux[, `2018` := `2018`*100] 
aux <- aux[, `2019` := `2019`*100] 
aux <- aux[, `2020` := `2020`*100]
aux1 <- round(aux[,c(2:11)],2)
aux<-cbind(aux[,1],aux1)
aux[1, 1] <- "Ingresos de la Operaci\\\'{o}n"

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 , 2 ,2, 2, 2, 2, 2, 2  ) )


print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Ingresos_del_fondo_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 5,
       sanitize.text.function = identity )


#------------------################### INGRESOS POR APORTES ###################---------------------

# Tasa de vaciación de los Aportes ----------------------------------------------------------------------------
message( '\tTabla de Evolución del Aportes del Fondo de SSC  ' )

aux <- copy(Aportes_hist_ssc)
aux[ , `Tasa crecimiento (%)`:=100*`Tasa crecimiento (%)`]
aux[ , Anio:=as.character(Anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_Aportes_hist_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tasa de vaciación de los Aportes----------------------------------------------------------------------------
message( '\tTabla de Evolución del Aportes, contribuciones y rendimientos  del Fondo de SSC  ' )

aux <- copy(Evolucion_aportes_ssc[,`Año`:=as.character(`Año`)])

aux1<-aux[1:11,c(1:7)]
aux2<-aux[1:11,c(1,8:13)]

aux_xtable1 <- xtable( aux1, digits = c( 0, 2, 2, 2, 2 , 2 ,2,2 ) )
aux_xtable2 <- xtable( aux2, digits = c( 0, 2, 2, 2, 2 ,2 ,2,2) )

print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_Evolucion_aportes_ssc1', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
print( aux_xtable2,
       file = paste0( parametros$resultado_tablas, 'iess_Evolucion_aportes_ssc2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tasa de vaciación de los Aportes----------------------------------------------------------------------------
message( '\tTabla de Evolución del Aportes, contribuciones y rendimientos  del Fondo de SSC  ' )

aux <- copy(porcentaje_de_Aportes_ssc[,`% de Participación`:=(`% de Participación`)*100])
aux <- aux[,`% de Participación`:=round(`% de Participación`,2)]

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_porcentaje_de_Aportes_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 12,
       sanitize.text.function = identity )



#------------------################### GOBIERNO ###################---------------------

# Tabla de Cuentas por cobrar Gobierno----------------------------------------------------------------------------
message( '\tTabla de Cuentas por cobrar Gobierno' )

aux <- copy(cuentas_por_cobrar_gobierno_ssc)
ltx<-data.frame( "A"= c(" ", " "," ", "\\multirow{-4}{*}{715050131}", " ", " "," " ,"\\multirow{-4}{*}{715050132}"), 
                 "B"= c(" ", " "," ", "\\multirow{-4}{*}{40\\% Pensi\\'{o}n Invalidez}", " ", " "," " , "\\multirow{-4}{*}{40\\% Pensi\\'{o}n Vejez}"))

aux<-cbind(ltx,aux)


aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_por_cobrar_gobierno_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Resumen 40% Deuda de Gobierno periodo 2012 – 2020----------------------------------------------------------------------------
message( '\tTabla de Cuentas por cobrar Gobierno' )

aux <- copy(resumen_deuda_gobierno_ssc)


aux_xtable <- xtable( aux, digits = c( 0, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_resumen_deuda_gobierno_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de Cuentas por cobrar total Gobierno----------------------------------------------------------------------------
message( '\tTabla de Cuentas por cobrar total Gobierno' )

aux <- copy(cuentas_por_cobrar_gobierno_total_ssc)
ltx<-data.frame( "A"= c(" ", " "," ", "\\multirow{-4}{*}{715050143}", " ", " "," " ,"\\multirow{-4}{*}{715050144}"), 
                 "B"= c(" ", " "," ", "\\multirow{-4}{*}{0,30\\% Contribuci\\'{o}n SSC}", " ", " "," " , "\\multirow{-4}{*}{Contribuciones Especiales}"))

aux<-cbind(ltx,aux)


aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cuentas_por_cobrar_gobierno_total_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#------------------################### GASTOS ###################---------------------


# Evolución de los egresos prestacionales de Fondo SSC----------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales de Fondo SSC' )

aux <- copy(egresos_prestacionales_ssc[,`Variación Anual`:=`Variación Anual`*100])
aux<-aux[,`Año`:= as.character(`Año`)]

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2 , 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_egresos_prestacionales_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Evolución de los gastos de administración de Fondo SSC----------------------------------------------------------------------------
message( '\tTabla de Evolución de los gastos de administración de Fondo SSC' )

aux <- copy(gastos_de_administacion_ssc[,`Variación Anual`:=`Variación Anual`*100])
aux<-aux[,`Año`:= as.character(`Año`)]

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2 , 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_gastos_de_administacion_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC----------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC' )

aux <- copy(descripcion_gastos_egresos_ssc)
aux <- format(aux, nsmall=2, decimal.mark = ',', big.mark = ".")

aux1 <- copy(gastos_de_administacion_ssc)
aux1[,2] <-format(aux1[,2], nsmall=2, decimal.mark = ',', big.mark = ".")
aux2 <- t(aux)
aux3 <- t(aux2)
aux4 <- t(aux1[,2])
aux5 <- t(aux4)
aux5 <- rbind("Gastos de Administracion",aux5)
aux6 <- t(aux5)
nuevo <- cbind(aux2[1:12, 1], aux5[1:12,], aux2[1:12,2: 3])
nuevo2 <- as.data.table(t(nuevo))

aux_xtable <- xtable( nuevo2, digits = c( 0, 2, 2, 2 , 2 , 2, 2, 2, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_descripcion_gastos_egresos_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 3,
       sanitize.text.function = identity )

# Descripción de cuentas horizontal----------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC' )

aux <- copy(descripcion_cuentas_horizontal_ssc[,`2011`:=`2011`*100])
aux <- aux[,`2012`:=`2012`*100]
aux <- aux[,`2013`:=`2013`*100]
aux <- aux[,`2014`:=`2014`*100]
aux <- aux[,`2015`:=`2015`*100]
aux <- aux[,`2016`:=`2016`*100]
aux <- aux[,`2017`:=`2017`*100]
aux <- aux[,`2018`:=`2018`*100]
aux <- aux[,`2019`:=`2019`*100]
aux <- aux[,`2020`:=`2020`*100]
aux[, 2:10] <- round(aux[, 2:10],2)
aux <- format(aux, nsmall=2, decimal.mark = ',')

aux1 <- copy(gastos_de_administacion_ssc[,`Variación Anual`:=`Variación Anual`*100])
aux1[,4] <- round(aux1[,4],2)
aux1[,4] <- format(aux1[,4], decimal.mark = ',')
aux2 <- t(aux)
aux3 <- t(aux2)
aux4 <- t(aux1[2:11,4])
aux5 <- t(aux4)
aux5 <- rbind("Gastos de Administracion",aux5)
aux6 <- t(aux5)
nuevo <- cbind(aux2[1:11, 1], aux5[1:11,], aux2[1:11,2: 3])
nuevo2 <- as.data.table(t(nuevo))

aux_xtable <- xtable( nuevo2, digits = c( 0, 2, 2, 2 , 2 , 2, 2, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_descripcion_cuentas_horizontal_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 3,
       sanitize.text.function = identity )


# Descripción de cuentas vertical----------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC' )

aux <- copy(descripcion_cuentas_vertical_ssc[,`2010`:=`2010`*100])
aux <- aux[,`2011`:=`2011`*100]
aux <- aux[,`2012`:=`2012`*100]
aux <- aux[,`2013`:=`2013`*100]
aux <- aux[,`2014`:=`2014`*100]
aux <- aux[,`2015`:=`2015`*100]
aux <- aux[,`2016`:=`2016`*100]
aux <- aux[,`2017`:=`2017`*100]
aux <- aux[,`2018`:=`2018`*100]
aux <- aux[,`2019`:=`2019`*100]
aux <- aux[,`2020`:=`2020`*100]



aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2 , 2 , 2, 2, 2, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_descripcion_cuentas_vertical_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 3,
       sanitize.text.function = identity )

#------------------################### Comparación de ingresos por aportes versus lo gastos de administración del Fondo de SSC ###################---------------------

# Evolución de ingresos por aportes vs egresos prestacionales   ---------------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC' )

aux <- copy(ingresos_vs_egresos_ssc[,`Utilización anual`:=`Utilización anual`*100])
aux <- aux[,`valor para acumulación`:=`valor para acumulación`*100]
aux<-aux[,`AÑO`:= as.character(`AÑO`)]

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_vs_egresos_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )




# Evolución de ingresos por aportes vs los gastos de administración   ---------------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC' )

aux <- copy(ingresos_vs_gastos_ssc[,`Utilización anual`:=`Utilización anual`*100])
aux <- aux[,`valor para acumulación`:=round(`valor para acumulación`*100,2)]
aux[,2] <- format(aux[,2], nsmall = 2, decimal.mark = ',',big.mark = '.' )
aux[,3] <- format(aux[,3], nsmall = 2, decimal.mark = ',',big.mark = '.' )
aux[,4] <- format(aux[,4], nsmall = 2, decimal.mark = ',',big.mark = '.' )
aux[1:4,5] <- round(aux[1:4,5])
aux[5,5] <- round(aux[5,5],1)
aux[6:11,5] <- round(aux[6:11,5])
aux[,5] <- format(aux[,5], decimal.mark = ',')
aux[ , AÑO:=as.character(AÑO)]

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_ingresos_vs_gastos_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )




# Relación patrimonio gasto prestacional del Fondo del SSC  ---------------------------------------------------------------------------------
message( '\tTabla de Evolución de los egresos prestacionales, gastos de administración y otros gastos de Fondo SSC' )

aux <- copy(patrimonio_y_gasto_pres_ssc)

aux<-aux[,`Año`:= as.character(`Año`)]

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_patrimonio_y_gasto_pres_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()













