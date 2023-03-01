message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población del TNRH del fondo del IVM' )

#file
file <- paste0( parametros$Data_seg, 'IESS_TNRH_analisis_demografico.xlsx' )


# Población afiliada TNRH --------------------------------------------------
message( '\tLeyendo población afiliada TNRH del IESS' )
col_nom <- c( 'anio', '92', '93', '94', '95', '96', 'TOTAL', 'Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

afi_acti_tnrh<-read_excel(file,sheet="NumAfi"
                          ,col_names=TRUE,guess_max = 24000)
afi_acti_tnrh <- as.data.table( afi_acti_tnrh )[2:7,]
setnames(afi_acti_tnrh, col_nom )


# Población afiliados activos (2)TNRH ------------------------------------------
message( '\tLeyendo población afiliada (2) TNRH del IESS' )
col_nom <- c( 'anio', 'Afiliados_Activos', 'Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric')

afi_acti2_tnrh<-read_excel(file,sheet="NumAfi (2)"
                           ,col_names=TRUE,guess_max = 24000)
afi_acti2_tnrh <- as.data.table( afi_acti2_tnrh )[1:6,1:3]
setnames(afi_acti2_tnrh, col_nom )


# PoblaciÃ³n afiliada por edad y sexo TNRH -------------------------------
message( '\tLeyendo población afiliada por sexo y edad activa inicial TNRH del IESS' )
col_nom <- c( 'sexo', 'edad', 'n' )
col_tip <- c( 'character', 'numeric', 'numeric')

pob_afi_edad_sexo_tnrh<-read_excel(file,sheet="NumAfiSexoEdad"
                                   ,col_names=TRUE,guess_max = 24000)
pob_afi_edad_sexo_tnrh <- as.data.table( pob_afi_edad_sexo_tnrh )[1:79,]
setnames( pob_afi_edad_sexo_tnrh, col_nom )


# Masa Salarial -----------------------------------------------------------
message( '\tLeyendo masa salarial de TNRH del IESS' )

col_nom <- c( 'anio', 'Masa_Anual', 'Masa_Salarial','Crecimiento_anual','Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric' )

masa_salarial_tnrh<-read_excel(file,sheet="Masa_Salarial"
                               ,col_names=TRUE,guess_max = 24000)
masa_salarial_tnrh <- as.data.table( masa_salarial_tnrh )[1:6,1:5]
setnames( masa_salarial_tnrh, col_nom )

# Guardando ---------------------------------------------------------------

lista <- c('afi_acti_tnrh', 'afi_acti2_tnrh','pob_afi_edad_sexo_tnrh','masa_salarial_tnrh')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_TNRH_analisis_demografico.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

