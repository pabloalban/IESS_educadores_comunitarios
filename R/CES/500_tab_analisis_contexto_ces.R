message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura porcentaje de desempleo' )

# Carga de datos-----------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_DES_estadistica_desempleo.RData' ) ) 
# Tabla porcentaje de desempleo-----------------------------------------------------
aux <- copy( porc_desempleo )
aux[ , indice1:=100*indice1]
aux[ , indice2:=100*indice2]
aux[ , periodo1:=paste0(substr(format(as.Date(periodo1),"%B"),1,3),"-",
                        substr(format(as.Date(periodo1),"%Y"),3,4))]
aux[ , periodo2:=paste0(substr(format(as.Date(periodo2),"%B"),1,3),"-",
                        substr(format(as.Date(periodo2),"%Y"),3,4))]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_porc_desempleo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla  de desempleo urbano y rural-----------------------------------------------------
message( '\tLectura  desempleo rural y urbano' )
aux <- copy( urb_rur_desempleo )
aux[ , PU1:=100*PU1]
aux[ , PR1:=100*PR1]
aux[ , PU2:=100*PU2]
aux[ , PR2:=100*PR2]
aux[ , periodo1:=paste0(substr(format(as.Date(periodo1),"%B"),1,3),"-",
                        substr(format(as.Date(periodo1),"%Y"),1,4))]
aux[ , periodo2:=paste0(substr(format(as.Date(periodo2),"%B"),1,3),"-",
                        substr(format(as.Date(periodo2),"%Y"),1,4))]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 0, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_urb_rur_desempleo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de desempleo por sexo-----------------------------------------------------
message( '\tLectura  índice de población desempleada por sexo' )
aux <- copy( sexo_desempleo )
aux[ , H1:=100*H1]
aux[ , M1:=100*M1]
aux[ , H2:=100*H2]
aux[ , M2:=100*M2]
aux[ , periodo1:=paste0(substr(format(as.Date(periodo1),"%B"),1,3),"-",
                        substr(format(as.Date(periodo1),"%Y"),1,4))]
aux[ , periodo2:=paste0(substr(format(as.Date(periodo2),"%B"),1,3),"-",
                        substr(format(as.Date(periodo2),"%Y"),1,4))]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 0, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_sexo_desempleo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de desempleo por rangos de edad-----------------------------------------------------
message( '\tLectura  desempleo por rangos de edad' )
aux <- copy( edad_desempleo )
aux[ , E1:=100*E1]
aux[ , E2:=100*E2]
aux[ , E3:=100*E3]
aux[ , E4:=100*E4]
aux[ , E5:=100*E5]
aux[ , periodo:=paste0(substr(format(as.Date(periodo),"%B"),1,3),"-",
                       substr(format(as.Date(periodo),"%Y"),1,4))]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_edad_desempleo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de pirámides de desempleo por sexo y edad----------------------------------------------------
message( '\tLectura  pirámides de desempleo por sexo y edad' )
aux <- copy( pira_edad_desempleo )
aux[ , edad:=as.character(edad)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pir_edad_desempleo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 21,
       sanitize.text.function = identity )

# Tabla de pirámides de desempleo por sexo y edad----------------------------------------------------
message( '\tLectura  PEA' )

aux <- copy( pea_desempleo )
aux[ , edad:=as.character(edad)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pea_desempleo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 18,
       sanitize.text.function = identity )

################################################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()