message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo análisis financiero SSC del IESS' )

# Carga de datos
file <- paste0( parametros$Data_seg, 'IESS_SSC_analisis_financiero.xlsx' )

#------------------################### ACTIVOS ###################---------------------

# Activo creminiento anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( 'Anio', 'Activo', 'Incremento Anual', 'Tasa crecimiento (%)')
col_tip <- c( "factor", 'numeric', 'numeric','numeric')

activos_hist_ssc <-read_excel( file,
                           sheet="Activo",
                           col_names=TRUE,
                           guess_max = 24000, range = "A1:D12")
activos_hist_ssc <- as.data.table( activos_hist_ssc)[1:11,]
setnames(activos_hist_ssc, col_nom )

# Activo creminiento anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( '2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')

activos_variacion_ssc <-read_excel( file,
                           sheet="ACTIVO (H_V)",
                           col_names=FALSE,
                           guess_max = 24000, range ="C3:M23")
activos_variacion_ssc <- as.data.table(activos_variacion_ssc)
setnames(activos_variacion_ssc, col_nom )
# Lec para el graficos porcentaje promedio de representatividad de los componentes del Activo ---------------------------------------------------------------------------------

porcentaje_promedio_representatividad_ssc <-read_excel( file,
                                    sheet="ACTIVO (H_V)",
                                    col_names=FALSE,
                                    guess_max = 24000, range ="A55:B64")

porcentaje_promedio_representatividad_ssc<-as.data.table(porcentaje_promedio_representatividad_ssc)
porcentaje_promedio_representatividad_ssc<-porcentaje_promedio_representatividad_ssc[,`...2`:=`...2`*100]


# Análisis horizontal del Activo del Fondo del Seguro Social Campesino al 31 de diciembre de cada año (%) ---------------------------------------------------------------------------------
message( '\tLeyemdo lectura horizontal del Activo del Fondo del Seguro Social' )

activos_del_fondo_ssc<-read_excel( file,
                                    sheet="ACTIVO (H_V)",
                                    col_names=T,
                                    guess_max = 24000, range ="A26:K37")
activos_del_fondo_ssc <- as.data.table(activos_del_fondo_ssc)

# Evolución de Cuentas por Cobrar del Fondo de Seguro Social Campesino---------------------------------------------------------------------------------

variacion_cuentas_por_cobrar_ssc <-read_excel( file,sheet="Ctas_x_cobrar ",
                                                        col_names=T,
                                                        guess_max = 24000, range ="B3:E14" )

variacion_cuentas_por_cobrar_ssc<-as.data.table(variacion_cuentas_por_cobrar_ssc)
variacion_cuentas_por_cobrar_ssc<-variacion_cuentas_por_cobrar_ssc[,`Año`:=as.character(`Año`)]
# variacion_cuentas_por_cobrar_ssc<-variacion_cuentas_por_cobrar_ssc[,`Cuentas por Pagar`:=as.numeric(`Cuentas por Pagar`)]
# variacion_cuentas_por_cobrar_ssc<-variacion_cuentas_por_cobrar_ssc[,`Incremento Anual`:=as.numeric(`Incremento Anual`)]
# variacion_cuentas_por_cobrar_ssc<-variacion_cuentas_por_cobrar_ssc[,`Tasa crecimiento (%)`:=as.numeric(`Tasa crecimiento (%)`)]



#------------------################### PASIVOS ###################---------------------
# Pasivo creminiento anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( 'Anio', 'Pasivo', 'Incremento Anual', 'Tasa crecimiento (%)')


pasivos_hist_ssc <-read_excel( file,
                               sheet="Pasivo",
                               col_names=TRUE,
                               guess_max = 24000, range = "B2:E13")
pasivos_hist_ssc <- as.data.table( pasivos_hist_ssc)
setnames(pasivos_hist_ssc, col_nom )
# Pasivo creminiento anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo pasivo crecimiento anual' )
col_nom <- c( '2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')

pasivos_variacion_ssc <-read_excel( file,
                                    sheet="PASIVO (H_V)",
                                    col_names=FALSE,
                                    guess_max = 24000, range ="D2:N16")
pasivos_variacion_ssc <- as.data.table(pasivos_variacion_ssc)
setnames(pasivos_variacion_ssc, col_nom )

# Lec para el graficos porcentaje promedio de representatividad de los componentes del Pasivo ---------------------------------------------------------------------------------

porcentaje_promedio_pasivos_representatividad_ssc <-read_excel( file,
                                                        sheet="PASIVO (H_V)",
                                                        col_names=FALSE,
                                                        guess_max = 24000, range ="B48:C54")

porcentaje_promedio_pasivos_representatividad_ssc<-as.data.table(porcentaje_promedio_pasivos_representatividad_ssc)
porcentaje_promedio_pasivos_representatividad_ssc<-porcentaje_promedio_pasivos_representatividad_ssc[,`...2`:=`...2`*100]


# Análisis horizontal del Pasivo del Fondo del Seguro Social Campesino al 31 de diciembre de cada año (%) ---------------------------------------------------------------------------------
message( '\tLeyemdo lectura horizontal del Pasivo del Fondo del Seguro Social' )

pasivos_del_fondo_ssc<-read_excel( file,
                                   sheet="PASIVO (H_V)",
                                   col_names=T,
                                   guess_max = 24000, range ="B19:L27")
pasivos_del_fondo_ssc <- as.data.table(pasivos_del_fondo_ssc)


# Evolución de Cuentas por Pagar del Fondo de Seguro Social Campesino---------------------------------------------------------------------------------
variacion_cuentas_por_pagar_ssc <-read_excel( file,sheet="Ctas x Pagar",
                                               col_names=T,
                                               guess_max = 24000, range ="B2:E13" )

variacion_cuentas_por_pagar_ssc<-as.data.table(variacion_cuentas_por_pagar_ssc)
variacion_cuentas_por_pagar_ssc<-variacion_cuentas_por_pagar_ssc[,`Año`:=as.character(`Año`)]
#
#-------------------------------------------------------------------------------------------------------------------------------

#------------------################### PATRIMONIO ###################---------------------
# Patrimonio variación anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( 'Anio', 'Patrimonio', 'Incremento Anual', 'Tasa crecimiento (%)')


patrimonio_hist_ssc <-read_excel( file,
                               sheet="Patrimonio",
                               col_names=TRUE,
                               guess_max = 24000, range = "B2:E13")
patrimonio_hist_ssc <- as.data.table( patrimonio_hist_ssc)
setnames(patrimonio_hist_ssc, col_nom )
# Activo creminiento anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( '2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')

patrimonio_variacion_ssc <-read_excel( file,
                                    sheet="PATRIMONIO (H_V)",
                                    col_names=FALSE,
                                    guess_max = 24000, range ="D3:N15")
patrimonio_variacion_ssc <- as.data.table(patrimonio_variacion_ssc)
setnames(patrimonio_variacion_ssc, col_nom )

# Lec para el graficos porcentaje promedio de representatividad de los componentes del Patrimonio ---------------------------------------------------------------------------------

porcentaje_promedio_patrimonio_representatividad_ssc <-read_excel( file,
                                                                sheet="PATRIMONIO (H_V)",
                                                                col_names=FALSE,
                                                                guess_max = 24000, range ="B52:C57")

porcentaje_promedio_patrimonio_representatividad_ssc<-as.data.table(porcentaje_promedio_patrimonio_representatividad_ssc)
porcentaje_promedio_patrimonio_representatividad_ssc<-porcentaje_promedio_patrimonio_representatividad_ssc[,`...2`:=`...2`*100]

# Análisis horizontal del Patrimonio del Fondo del Seguro Social Campesino al 31 de diciembre de cada año (%) ---------------------------------------------------------------------------------
message( '\tLeyemdo lectura horizontal del Patrimonio del Fondo del Seguro Social' )

patrimonio_del_fondo_ssc<-read_excel( file,
                                   sheet="PATRIMONIO (H_V)",
                                   col_names=T,
                                   guess_max = 24000, range ="B18:L25")
patrimonio_del_fondo_ssc <- as.data.table(patrimonio_del_fondo_ssc)


#------------------################### INGRESOS ###################---------------------
# Ingresos variación anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( 'Anio', 'Ingresos', 'Incremento Anual', 'Tasa crecimiento (%)')


Ingresos_hist_ssc <-read_excel( file,
                                  sheet="Ingresos",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:E13")
Ingresos_hist_ssc <- as.data.table( Ingresos_hist_ssc)
setnames(Ingresos_hist_ssc, col_nom )
# Activo creminiento anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( '2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')

Ingresos_variacion_ssc <-read_excel( file,
                                       sheet="INGRESOS (H_V)",
                                       col_names=FALSE,
                                       guess_max = 24000, range ="C4:M14")
Ingresos_variacion_ssc <- as.data.table(Ingresos_variacion_ssc)
setnames(Ingresos_variacion_ssc, col_nom )

# Lec para el graficos porcentaje promedio de representatividad de los componentes de los Ingresos ---------------------------------------------------------------------------------

porcentaje_promedio_Ingresos_representatividad_ssc <-read_excel( file,
                                                                   sheet="INGRESOS (H_V)",
                                                                   col_names=FALSE,
                                                                   guess_max = 24000, range ="A37:B41")

porcentaje_promedio_Ingresos_representatividad_ssc<-as.data.table(porcentaje_promedio_Ingresos_representatividad_ssc)
porcentaje_promedio_Ingresos_representatividad_ssc<-porcentaje_promedio_Ingresos_representatividad_ssc[,`...2`:=`...2`*100]

# Análisis horizontal del Ingresos del Fondo del Seguro Social Campesino al 31 de diciembre de cada año (%) ---------------------------------------------------------------------------------
message( '\tLeyemdo lectura horizontal del Ingresos del Fondo del Seguro Social' )

Ingresos_del_fondo_ssc<-read_excel( file,
                                      sheet="INGRESOS (H_V)",
                                      col_names=T,
                                      guess_max = 24000, range ="A17:K23")
Ingresos_del_fondo_ssc <- as.data.table(Ingresos_del_fondo_ssc)
#------------------################### Deuda de Gobierno ###################---------------------

message( '\tLeyendo Detalle de las cuentas por cobrar correspondiente al 40% de la Deuda de Gobierno correspondiente al periodo 2012-2020' )

cuentas_por_cobrar_gobierno_ssc <-read_excel( file,
                               sheet="Deuda de Gobierno",
                               col_names=FALSE,
                               guess_max = 24000, range = "E5:F12")

cuentas_por_cobrar_gobierno_ssc <- as.data.table(cuentas_por_cobrar_gobierno_ssc)
#------------------------------------------------------------------------------------------
message( '\tLeyendo Resumen 40% Deuda de Gobierno periodo 2012 – 2020' )

resumen_deuda_gobierno_ssc <-read_excel( file,
                                              sheet="Deuda de Gobierno",
                                              col_names=T,
                                              guess_max = 24000, range = "J3:K10")
resumen_deuda_gobierno_ssc <- as.data.table(resumen_deuda_gobierno_ssc)
#------------------------------------------------------------------------------------------
message( '\tLeyendo Detalle de las cuentas por cobrar correspondiente a la Deuda de Gobierno correspondiente al periodo 2012-2020' )

cuentas_por_cobrar_gobierno_total_ssc <-read_excel( file,
                                              sheet="Deuda de Gobierno",
                                              col_names=FALSE,
                                              guess_max = 24000, range = "E17:F24")

cuentas_por_cobrar_gobierno_total_ssc <- as.data.table(cuentas_por_cobrar_gobierno_total_ssc)
#------------------------------------------------------------------------------------------


#------------------################### Ingresos por aportes y contribuciones/1 ###################---------------------

# Aportes variación anual/tasa de crecimiento ---------------------------------------------------------------------------------
message( '\tLeyendo activo crecimiento anual' )
col_nom <- c( 'Anio', 'Aportes IESS', 'Incremento Anual', 'Tasa crecimiento (%)')


Aportes_hist_ssc <-read_excel( file,
                               sheet="Aportes",
                               col_names=TRUE,
                               guess_max = 24000, range = "B2:E13")
Aportes_hist_ssc <- as.data.table( Aportes_hist_ssc)
setnames(Aportes_hist_ssc, col_nom )
#------------------################### Ingresos por aportes y contribuciones/ 2 ###################---------------------
# Evolución de los aportes ---------------------------------------------------------------------------------
message( '\tEvolución de los aportes del Fondo de SSC' )
Evolucion_aportes_ssc <-read_excel( file,
                               sheet="Evolución-Aporte",
                               col_names=TRUE,
                               guess_max = 24000, range = "B3:N15")
Evolucion_aportes_ssc <- as.data.table( Evolucion_aportes_ssc)

# Evolución de los aportes ---------------------------------------------------------------------------------
message( '\tPorcentaje de Participación de Aportes y Contribuciones SSC' )
porcentaje_de_Aportes_ssc <-read_excel( file,
                                    sheet="Evolución-Aporte",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "U3:W16")
porcentaje_de_Aportes_ssc <- as.data.table(porcentaje_de_Aportes_ssc)
#------------------################### Egresos Prestacionales y Gastos de Administración ###################---------------------

# Egresos Prestacionales  ---------------------------------------------------------------------------------
message( '\tLectura Egresos Prestacionales SSC' )
egresos_prestacionales_ssc <-read_excel( file,
                                        sheet="Egresos (modific)",
                                        col_names=TRUE,
                                        guess_max = 24000, range = "C49:F60")
egresos_prestacionales_ssc  <- as.data.table(egresos_prestacionales_ssc )

# Gastos de Administración  ---------------------------------------------------------------------------------
message( '\tLectura Gastos de Administración SSC' )
gastos_de_administacion_ssc <-read_excel( file,
                                        sheet="Egresos (modific)",
                                        col_names=TRUE,
                                        guess_max = 24000, range = "C67:F78")
gastos_de_administacion_ssc  <- as.data.table(gastos_de_administacion_ssc )



# Evolución de los egresos prestacionales, gastos de administración y otros gastos  ---------------------------------------------------------------------------------
message( '\tLectura Descripción de Egresos Prestacionañes y Gastos de admin' )
descripcion_gastos_egresos_ssc <-read_excel( file,
                                          sheet="Egresos (modific)",
                                          col_names=TRUE,
                                          guess_max = 24000, range = "B2:M23")

descripcion_gastos_egresos_ssc  <- as.data.table(descripcion_gastos_egresos_ssc)
descripcion_gastos_egresos_ssc<-descripcion_gastos_egresos_ssc[`DESCRIPCIÓN DE LAS CUENTAS`%in%c("Egresos Prestacionales","Otros Gastos","Gastos de Administración (contribución)","Total Egresos")]

# Descripción de cuentas horizontal  ---------------------------------------------------------------------------------
message( '\tLectura Descripción de Egresos Prestacionañes y Gastos de admin' )
descripcion_cuentas_horizontal_ssc<-read_excel( file,
                                             sheet="Egresos (modific)",
                                             col_names=TRUE,
                                             guess_max = 24000, range = "B26:L38")

descripcion_cuentas_horizontal_ssc <- as.data.table(descripcion_cuentas_horizontal_ssc)
descripcion_cuentas_horizontal_ssc<-descripcion_cuentas_horizontal_ssc[`DESCRIPCIÓN DE LAS CUENTAS`%in%c("Egresos Prestacionales","Otros Gastos","Gastos de Administración (contribución)","Total Egresos")]

# Descripción de cuentas vertical  ---------------------------------------------------------------------------------
message( '\tLectura Descripción de Egresos Prestacionañes y Gastos de admin' )
descripcion_cuentas_vertical_ssc <-read_excel( file,
                                             sheet="Egresos (modific)",
                                             col_names=TRUE,
                                             guess_max = 24000, range = "B41:M45")

descripcion_cuentas_vertical_ssc  <- as.data.table(descripcion_cuentas_vertical_ssc)
#------------------################### Comparación de ingresos por aportes versus lo gastos de administración del Fondo de SSC ###################---------------------

# Evolución de ingresos por aportes vs egresos prestacionales   ---------------------------------------------------------------------------------
message( '\tLectura Ingresos vs egresos prestacionales SSC' )
ingresos_vs_egresos_ssc <-read_excel( file,
                                               sheet="Ing_vs_Gast",
                                               col_names=TRUE,
                                               guess_max = 24000, range = "B2:I13")

ingresos_vs_egresos_ssc  <- as.data.table(ingresos_vs_egresos_ssc)
# Evolución de ingresos por aportes vs los gastos de administración   ---------------------------------------------------------------------------------
message( '\tLectura Ingresos vs Gastos Adm SSC' )
ingresos_vs_gastos_ssc<-read_excel( file,
                                               sheet="Ing_vs_Gast",
                                               col_names=TRUE,
                                               guess_max = 24000, range = "B21:G32")

ingresos_vs_gastos_ssc <- as.data.table(ingresos_vs_gastos_ssc)
# Relación patrimonio gasto prestacional del Fondo del SSC  ---------------------------------------------------------------------------------
message( '\tLectura patrimonio gasto prestacional del Fondo del SSC' )
patrimonio_y_gasto_pres_ssc <-read_excel( file,
                                               sheet="Ing_vs_Gast",
                                               col_names=TRUE,
                                               guess_max = 24000, range = "C38:F49")

patrimonio_y_gasto_pres_ssc  <- as.data.table(patrimonio_y_gasto_pres_ssc)

# Porcentaje promedio de representatividad de los egresos prestacionales del Fondo del SSC  ---------------------------------------------------------------------------------
message( '\tLectura Porcentaje promedio de representatividad de los egresos prestacionales' )
porcentaje_promedio_egresos_pres_ssc <-read_excel( file,
                                          sheet="Egresos (modific)",
                                          col_names=FALSE,
                                          guess_max = 24000, range = "B91:C93")

porcentaje_promedio_egresos_pres_ssc  <- as.data.table(porcentaje_promedio_egresos_pres_ssc)


#------------------------------------------------------------------------------------------
lista <- c('activos_hist_ssc','activos_variacion_ssc', 'porcentaje_promedio_representatividad_ssc', 'activos_del_fondo_ssc', 'variacion_cuentas_por_cobrar_ssc','variacion_cuentas_por_pagar_ssc',
           'pasivos_hist_ssc', 'pasivos_variacion_ssc', 'porcentaje_promedio_pasivos_representatividad_ssc', 'pasivos_del_fondo_ssc',
           'patrimonio_hist_ssc','patrimonio_variacion_ssc',
           'porcentaje_promedio_patrimonio_representatividad_ssc', 'patrimonio_del_fondo_ssc'
            ,'Ingresos_hist_ssc','Ingresos_variacion_ssc',
           'porcentaje_promedio_Ingresos_representatividad_ssc', 'Ingresos_del_fondo_ssc' ,'Aportes_hist_ssc'
           ,'cuentas_por_cobrar_gobierno_ssc', 'cuentas_por_cobrar_gobierno_total_ssc' ,'resumen_deuda_gobierno_ssc','Evolucion_aportes_ssc','porcentaje_de_Aportes_ssc', 'egresos_prestacionales_ssc'
           ,'gastos_de_administacion_ssc','descripcion_gastos_egresos_ssc','descripcion_cuentas_horizontal_ssc'
           ,'descripcion_cuentas_vertical_ssc','ingresos_vs_egresos_ssc','ingresos_vs_gastos_ssc','patrimonio_y_gasto_pres_ssc', 'porcentaje_promedio_egresos_pres_ssc')


save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_financiero.RData' ) )

###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()



