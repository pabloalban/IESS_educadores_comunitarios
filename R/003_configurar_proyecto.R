# Parámetros globales R ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tConfiguración global de R' )

options( scipen = 99 )
setNumericRounding( 2 )
options( stringsAsFactors = FALSE )

# Parámetros ---------------------------------------------------------------------------------------
message( '\tCreando entorno de parámetros' )

# Entorno con parámetros
parametros <- new.env()

# User name
parametros$user <- Sys.getenv( 'USER' )

parametros$fec_eje <- Sys.Date()

# Operating system name
parametros$opsys <- Sys.info()[[1]]

# Hostname
parametros$hostname <- Sys.info()[[4]]

#Servidor de datos
parametros$data_server <- 'Y:/IESS_2020/'

# local
#parametros$data_server <- paste0( getwd(), '/' )


# Directorio de trabajo
parametros$work_dir <- paste0( getwd(), '/' )

# Setting Time Zone
parametros$time_zone <- "America/Guayaquil"

# Colores IESS
parametros$iess_blue <- rgb( 0, 63, 138, maxColorValue = 255 )
parametros$iess_green <- rgb( 0, 116, 53, maxColorValue = 255 )
parametros$iess_total <- rgb( 138, 5, 81, maxColorValue = 255 )
parametros$female <- rgb( 220, 20, 60, maxColorValue = 255 )
parametros$male <- rgb( 0, 139, 139, maxColorValue = 255 )
# Calcular balance
# parametros$calcular_balance <- FALSE

parametros$mont_prop_afi <- 0.1275

# Direcciones globables  ---------------------------------------------------------------------------
message( '\tEstableciendo directorios globales' )
parametros$empresa <- 'IESS'

message( '\tConfiguración seguro' )
message( '\t\tLas opciones son: IVM, SAL, RTR, DES, SSC, CES, TNRH')
parametros$seguro <- readline( prompt = '\tIngrese seguro: ' )
if ( !( parametros$seguro %in% c( 'IVM', 'SAL', 'RTR', 'DES', 'SSC', 'CES','TNRH' ) ) ) {
  stop( 'El seguro ingresado no está entre las opciones' )
}

# Parametro realizar análisis demográfico
if ( parametros$seguro == 'IVM' ) {
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE
} else if (parametros$seguro == 'SAL'){
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE
} else if (parametros$seguro == 'RTR'){
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE
} else if (parametros$seguro == 'DES'){
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE
} else if (parametros$seguro == 'CES'){
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE
} else if (parametros$seguro == 'SSC'){
  parametros$hacer_ana_dem <- TRUE
  parametros$calcular_balance <- TRUE
}else if (parametros$seguro == 'TNRH'){
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE
}


# Configuraciones particulares por seguro ----------------------------------------------------------
parametros$fec_fin <- ymd( '2020-12-31' )
parametros$anio_ini <- 2020
parametros$anio <- 2021 # Año del estudio
parametros$edad_max <- 105

# Incluir casos según corresponda
if ( parametros$seguro == 'SAL' ) {
  parametros$horizonte <- 10 # en años
  parametros$fec_ini <- ymd( '2010-01-01' ) # fecha inicio del periodo de observación
  # #Patrimonio
  # load( file = paste0( parametros$RData_seg, 'IESS_SAL_analisis_financiero.RData' ) )
  # parametros$reserva_ini <- Patrimonio[11,2] #2403773950.63 # reserva inicial
} else
if ( parametros$seguro == 'IVM' ) {
  parametros$horizonte <- 40 # en años
  parametros$fec_ini <- ymd( '2013-01-01' ) # fecha inicio del periodo de observación
} else if ( parametros$seguro == 'RTR' ) {
  parametros$horizonte <- 40 # en años

} else if ( parametros$seguro == 'DES' ) {
  parametros$horizonte <- 40 # en años

} else if ( parametros$seguro == 'CES' ) {
  parametros$horizonte <- 40 # en años
  parametros$ana_dem <- paste0( parametros$work_dir, 'R/ces/300_analisis_demografico_ces.R' )
} else if ( parametros$seguro == 'CES' ) {
  parametros$horizonte <- 40 # en años
  parametros$ana_dem <- paste0( parametros$work_dir, 'R/tnrh/300_analisis_demografico_tnrh.R' )
} else if ( parametros$seguro == 'SSC' ) {
  parametros$horizonte <- 20 # en años
  parametros$fec_ini <- ymd( '2012-01-01' ) # fecha inicio del periodo de observación
  parametros$ana_dem <- paste0( parametros$work_dir, 'R/SSC/313_analisis_demografico_ssc.R' )
  parametros$anio <- 2020 # Año del estudio
  parametros$anio_ini <- 2021 # Año inicial de la proyección
  parametros$anio_fin <- parametros$anio + parametros$horizonte  # Año fin de la proyección
  parametros$edad_max <- 105
  if(parametros$hostname %in% c('PCUIOMTDAIE6382' #Cristian G.
                                )){
    parametros$ilo_out <- 'C:/Users/cristian.guatemal/Downloads/'
  }else if(parametros$hostname %in% c('PCUIOMTDAI3L2S' #Priscila
                                      )){
    parametros$ilo_out <-'C:/Users/Usuario01/Downloads'
    
  }
} else if ( parametros$seguro == "TNRH" ){
  parametros$horizonte <- 40 # en años
}
  
  #   } else {
#   parametros$horizonte <- 40 # en años
#   parametros$fec_ini <- ymd( '2013-01-01' ) # fecha inicio del periodo de observación
#   parametros$ana_dem <- paste0( parametros$work_dir, 'R/311_analisis_demografico.R' )
#   


# Variables automáticas ----------------------------------------------------------------------------
parametros$Data <- paste0( parametros$data_server, 'Data/' )
parametros$RData <- paste0( parametros$data_server, 'RData/' )
parametros$SQL <- paste0( parametros$data_server, 'SQL/' )
parametros$RSQL <- paste0( parametros$data_server, 'RSQL/' )
parametros$Data_seg <- paste0( parametros$Data, parametros$seguro, '/' )
parametros$RData_seg <- paste0( parametros$RData, parametros$seguro, '/' )
parametros$SQL_seg <- paste0( parametros$SQL, parametros$seguro, '/' )
parametros$RSQL_seg <- paste0( parametros$RSQL, parametros$seguro, '/' )
parametros$Driver <- paste0( parametros$data_server, 'Drivers/oracle', '/' )

parametros$reportes <- paste0( parametros$work_dir, 'Reportes/' )
parametros$resultados <- paste0( parametros$work_dir, 'Resultados/' )
parametros$reporte_seguro <- paste0( parametros$work_dir, 'Reportes/Reporte_', 
                                     parametros$seguro, '/' )
if ( parametros$seguro == 'SAL' ) {
  #Patrimonio
  load( file = paste0( parametros$RData_seg, 'IESS_SAL_analisis_financiero.RData' ) )
  parametros$reserva_ini <- Patrimonio[11,2] #2403773950.63 # reserva inicial
}

if ( parametros$seguro == 'IVM' ) {
  #parametros$calculo_balance <- paste0( parametros$work_dir, 'R/ivm/303_calculo_escenarios_balance_ivm.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/IVM/600_reporte_latex_ivm.R' )
} else if ( parametros$seguro == 'SAL' ) {
  parametros$calculo_balance <- paste0( parametros$work_dir, 'R/ivm/303_calculo_escenarios_balance_ivm.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/sal/600_reporte_latex_sal.R' )
}else if ( parametros$seguro == 'SSC' ) {
  parametros$calculo_balance <- paste0( parametros$work_dir, 'R/SSC/309_calculo_escenarios_balance_ssc.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/SSC/600_reporte_latex_ssc.R' )
  parametros$reporte_script_conv <- paste0( parametros$reporte_seguro, 'reporte_script.R' )
} else if ( parametros$seguro == 'RTR' ) {
  parametros$calculo_balance <- paste0( parametros$work_dir, 'R/rtr/305_calculo_escenarios_balance_rtr.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/rtr/600_reporte_latex_rtr.R' )

} else if ( parametros$seguro == 'DES' ) {
  parametros$calculo_balance <- paste0( parametros$work_dir, 'R/des/310_calculo_escenarios_balance_des.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/des/600_reporte_latex_des.R' )

} else if ( parametros$seguro == 'CES' ) {
  parametros$calculo_balance <- paste0( parametros$work_dir, 'R/ces/304_calculo_escenarios_balance_ces.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/ces/600_reporte_latex_ces.R' )

} else if ( parametros$seguro == 'TNRH' ) {
  parametros$calculo_balance <- paste0( parametros$work_dir, 'R/tnrh/304_calculo_escenarios_balance_tnrh.R' )
  parametros$reporte_genera <- paste0( parametros$work_dir, 'R/tnrh/600_reporte_latex_tnrh.R' )
}

parametros$reporte_script <- paste0( parametros$reporte_seguro, 'reporte.R' )
parametros$reporte_script_latex <- paste0( parametros$reporte_seguro, 'reporte_script.R' )
parametros$reporte_nombre <- paste0( parametros$empresa, '_', 
                                     parametros$seguro, '_estudio_actuarial' )
parametros$reporte_latex <- paste0( parametros$reporte_nombre, '.tex' )
parametros$reporte_latex_script <- paste0( parametros$reporte_nombre, '_script','.tex' )
parametros$resultado_seguro <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/' )
parametros$resultado_tablas <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/tablas/' )
parametros$resultado_graficos <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                         format( parametros$fec_eje, '%Y_%m_%d' ), '/graficos/' )

parametros$graf_modelo_1 <- 'R/401_graf_plantilla.R'
parametros$graf_ext <- '.png'

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

