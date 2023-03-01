message( paste( rep('-', 100 ), collapse = '' ) )

# Porcentaje de desempleo --------------------------------------------------------------------------
message( '\tLeyendo porcentaje de desempleo' )

col_nom <- c( 'periodo1', 'indice1', 'periodo2', 'indice2')
col_tip <- c( 'character', 'numeric', 'character', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_contexto_demografico.xlsx' )

porc_desempleo<-read_excel( file, sheet = "porc_desempleo", col_names = TRUE, guess_max = 24000 )
porc_desempleo <- as.data.table( porc_desempleo )
setnames( porc_desempleo, col_nom )

# Desempleo Rural y Urbano--------------------------------------------------------------------------
message( '\tLeyendo desempleo Rural y Urbano' )

col_nom <- c( 'periodo1', 'PU1', 'PR1', 'periodo2', 'PU2', 'PR2')
col_tip <- c( 'character', 'numeric', 'numeric', 'character', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_contexto_demografico.xlsx' )

urb_rur_desempleo<-read_excel( file, sheet = "urb_rur_desempleo", col_names = TRUE, 
                               guess_max = 24000 )
urb_rur_desempleo <- as.data.table( urb_rur_desempleo )
setnames( urb_rur_desempleo, col_nom )

# Desempleo por sexo--------------------------------------------------------------------------------
message( '\tLeyendo desempleo por sexo' )

col_nom <- c( 'periodo1', 'H1', 'M1', 'periodo2', 'H2', 'M2')
col_tip <- c( 'character', 'numeric', 'numeric', 'character', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_contexto_demografico.xlsx' )

sexo_desempleo<-read_excel(file,sheet="sexo_desempleo"
                           ,col_names=TRUE,guess_max = 24000 )
sexo_desempleo <- as.data.table( sexo_desempleo )
setnames( sexo_desempleo, col_nom )

#Desempleo por sexo---------------------------------------------------------------------------------
message( '\tLeyendo desempleo por rango de edad' )

col_nom <- c( 'periodo', 'E1', 'E2', 'E3', 'E4', 'E5')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_contexto_demografico.xlsx' )

edad_desempleo<-read_excel( file, sheet = "edad_desempleo", col_names = TRUE, guess_max = 24000 )
edad_desempleo <- as.data.table( edad_desempleo )
setnames( edad_desempleo, col_nom )

# Pirámides de Desempleo por sexo-------------------------------------------------------------------
message( '\tLeyendo pirámides de desempleo por sexo' )

col_nom <- c( 'edad', 'H1', 'M1', 'H2', 'M2', 'H3', 'M3')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_contexto_demografico.xlsx' )

pira_edad_desempleo<-read_excel( file, sheet = "prob_proy_sexo", col_names = TRUE, 
                                 guess_max = 24000 )
pira_edad_desempleo <- as.data.table( pira_edad_desempleo )
setnames( pira_edad_desempleo, col_nom )

# PEA por sexo--------------------------------------------------------------------------------------
message( '\tLeyendo pirámides de desempleo por sexo' )

col_nom <- c( 'edad', 'H1', 'M1', 'H2', 'M2', 'H3', 'M3')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_contexto_demografico.xlsx' )

pea_desempleo<-read_excel( file, sheet = "PEA", col_names = TRUE, guess_max = 24000 )
pea_desempleo <- as.data.table( pea_desempleo )
setnames( pea_desempleo, col_nom )

# Guardando datos ----------------------------------------------------------------------------------
message( '\tGuardando datos' )
save( porc_desempleo, 
      urb_rur_desempleo, 
      sexo_desempleo, 
      edad_desempleo, 
      pira_edad_desempleo, 
      pea_desempleo,
      file = paste0( parametros$RData_seg, 'IESS_DES_estadistica_desempleo.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()