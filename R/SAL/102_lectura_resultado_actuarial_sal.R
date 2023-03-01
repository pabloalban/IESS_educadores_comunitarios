message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo agregados proyectados financieros de salud' )
message( '\t[RPT_TRE] y [RPT_TFR] Table Revenue and Expenditure' )

col_nom <- c("Year", "salary_mass", "contributions", "goverment_trans",
             "interest_re", "copagment", "others", "total",
             "health", "cash","total_b", "admin", "other", "total_t"
             )
#-------------------------------------------------------------------------------
# Escenario 1 (BASE)
escenario_1 <- 'ESCENARIO_BASE'
file <- paste0( parametros$Data_seg, '/OUTPUT/', escenario_1, '/RESULTADOS_ACTUARIALES/',
              '2105_rpt_tre_49___male__50_.xlsx' )
column_types <- rep("numeric", 14)

agregado_financiero <- read_excel( file, sheet = 'RPT_TRE', col_types = column_types )
agregado_financiero <- as.data.table( agregado_financiero[ 8:17, 1:ncol( agregado_financiero ) ] ) 

setnames( agregado_financiero, col_nom )

# AGREGO COLUMNAS A FALTA DE VALORES DESAGREGADOS EN LOS RESULTADOS DEL MODELO
# Agrego columna para aportes por menores de 18 anios del SGO
agregado_financiero[ , aporte_menores_18 := 0 ]

# Agrego aporte extension de cobertura
agregado_financiero[ , aporte_extension_cob := 0 ]

# Agrego beneficio pensionistas
agregado_financiero[ , beneficio_pensionistas := 0 ]

# Agrego beneficio menores de 18 
agregado_financiero[ , beneficio_menores_18 := 0 ]

# Agrego beneficio extension cobertura
agregado_financiero[ , beneficio_extension_cob := 0 ]

# [RPT_TFR]
file <- paste0( parametros$Data_seg, '/OUTPUT/', escenario_1, '/RESULTADOS_ACTUARIALES/',
                '2105_rpt_tfr______50_.xlsx' )
column_types <- rep("numeric", 5 )
col_nom <- c("Year", "financial_resul", "PAYG", "begin_year_reserve",
             "reserve_coef"
             )

resultado_financiero <- read_excel( file, sheet = 'RPT_TFR', col_types = column_types )
resultado_financiero <- as.data.table( resultado_financiero[ 7:16, 1:ncol( resultado_financiero ) ] ) 

setnames( resultado_financiero, col_nom )
agregado_financiero[ , financial_resul := resultado_financiero$financial_resul ]
agregado_financiero[ , begin_year_reserve := resultado_financiero$begin_year_reserve ]

save( agregado_financiero, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_nominal_escenario_1.RData' ) )
