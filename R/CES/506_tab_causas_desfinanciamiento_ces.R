message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tablas de las casusas de desfinanciamiento' )
#Función de tildes a latex--------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_CES_causas_desfinanciamiento.RData' ) )

# Tabla causas totales de desfinanciamiento---------------------------------------------------------
message( '\tTabla total desfinanciamiento' )
aux <- copy( causa_desf_total )
aux <- as.data.table( aux )
aux <- aux[ ,list(concepto,capital,lucro,total)]
#aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
aux_xtable <- tildes_a_latex(aux_xtable)
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_CES_total', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))

# Reducción de aportes por Desempleo----------------------------------------------------------------
message( '\tTabla reducción de aportes por Desempleo' )
aux <- copy( causa_desf_CD515 )
aux$tasa_promedio_de_interes <- aux$tasa_promedio_de_interes * 100
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_CES_Desfi_CD515', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))


# Tabla desinversiones------------------------------------------------------------------------------
message( '\tTabla desinversiones' )
aux <- ( causa_desf_desinversiones )
aux$periodo <- format(aux$periodo, "%b/%Y")
aux$periodo <- as.character(aux$periodo)
aux[nrow(aux),1] <- "Total"
aux$rentabilidad_neta<-aux$rentabilidad_neta*100
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 6, 6, 2,2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_CES_desinversiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))
# Tabla evolución de las tasas de aportación--------------------------------------------------------
message( '\tTabla evolución de las tasas de aportación' )
aux <- ( comparacion_primas )
aux$ano <- format(aux$ano, "%d-%b-%Y")
aux$ano <- as.character(aux$ano)
aux <- aux %>% 
        select(-etiqueta) %>%
        mutate( c_d_515 = ifelse( is.na(c_d_515), NA, paste0( 100 * c_d_515, '\\%' ) ),
                c_d_501 = ifelse( is.na(c_d_501), NA, paste0( 100 * c_d_501, '\\%' ) ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_CES_tasas', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = NULL)

#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()