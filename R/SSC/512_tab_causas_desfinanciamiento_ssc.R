message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura tablas de las casusas de desfinanciamiento' )
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_causas_desfinanciamiento.RData' ) )

# Tabla causas totales de desfinanciamiento----------------------------------------------------
message( '\tTabla total desfinanciamiento' )
aux <- copy( causa_desf_total )
aux <- as.data.table( aux )
aux <- aux[ , print_names := c( 'Ausencia contribuci\\\'{o}n del Estado', 
                                'Desinversiones','Total') ]
aux <- aux[ ,list(print_names,capital,lucro,total)]
#aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_SSC_causas_desfinanciamiento_total', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1, nrow(aux)), 
       sanitize.text.function = identity )

# Tabla aportes del estado----------------------------------------------------
message( '\tTabla aportes del estado' )
aux <- copy( causa_desf_estado ) %>%
        select( -i ) %>%
        mutate( tasa_de_rendimiento_neto = tasa_de_rendimiento_neto * 100 )
        

aux_xtable <- xtable( aux, digits = c( 0, 0, rep(2, 8) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_SSC_deuda_estado_desf', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux)-1,
       sanitize.text.function = identity)


# Tabla desinversiones-----------------------------------------------------------------------------
message( '\tTabla desinversiones' )
aux <- ( causa_desf_desinversiones )
aux$periodo<- as.Date(aux$periodo,"%d/%m/%Y")
aux$periodo <- format(aux$periodo, "%b/%Y")
aux$periodo <- as.character(aux$periodo)
aux$periodo[nrow(aux)] <- 'Total'

aux$rentabilidad_neta<-aux$rentabilidad_neta*100
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 6, 6, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_SSC_desinversiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))

# Tabla desinversiones-----------------------------------------------------------------------------
message( '\tTabla desinversiones anuales' )
aux <- ( causa_desf_desinversiones_anual )
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2 ,2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_SSC_desinversiones_anual', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux)-1),
                         command = c(paste("\\hline \n"))))


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()