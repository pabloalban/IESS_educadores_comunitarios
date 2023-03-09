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
parametros$data_server <- 'Y:/IESS_educadores_comunitarios/'

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
parametros$female <- rgb( 255, 204, 255, maxColorValue = 255 )
parametros$male <- rgb( 51, 153, 255, maxColorValue = 255 )
# Calcular balance
# parametros$calcular_balance <- FALSE

parametros$mont_prop_afi <- 0.1275

# Direcciones globables  ---------------------------------------------------------------------------
message( '\tEstableciendo directorios globales' )
parametros$empresa <- 'IESS'


# Parametro realizar análisis demográfico
parametros$hacer_ana_dem <- FALSE
parametros$calcular_balance <- FALSE

# Configuraciones particulares por seguro ----------------------------------------------------------
parametros$fec_fin <- ymd( '2020-12-31' )
parametros$anio_ini <- 2020
parametros$anio <- 2021 # Año del estudio
parametros$edad_max <- 105

# Incluir casos según corresponda
parametros$seguro <- 'ECO'
parametros$horizonte <- 40 # en años
parametros$fec_ini <- ymd( '2010-01-01' ) # fecha inicio del periodo de observación

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

parametros$calculo_balance <- paste0( parametros$work_dir, 'R/ECO/304_calculo_escenarios_balance_eco.R' )
parametros$reporte_genera <- paste0( parametros$work_dir, 'R/ECO/600_reporte_latex_eco.R' )



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
