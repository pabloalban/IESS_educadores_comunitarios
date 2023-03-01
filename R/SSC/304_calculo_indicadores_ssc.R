message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) )

# Cálculo de la  tasa de cobertura -----------------------------------------------------------------
message( '\tCálculo de la tasa de cobertura ' )
aux <- copy( acum_dem )
aux <- aux[ ,  list( l2 = sum( l2, na.rm=T), l1 = sum( l1, na.rm=T) ), by = t ]
aux[ , tc:= l2/l1 * 100 ]
indicadores <- merge( data.table( expand.grid( t=2012:2040 ) ), aux[, list( t, tc)], by=c('t'), all.x=T)
# Cálculo de la  carga pensional -------------------------------------------------------------------
message( '\tCálculo de la  carga pensional' )
aux <- copy( acum_dem )
aux <- aux[ ,  list( l2 = sum( l2, na.rm=T), l3 = sum( l3, na.rm=T), l4 = sum( l4, na.rm=T),
                     l8 = sum( l8, na.rm=T), l9 = sum( l9, na.rm=T)), 
            by = t ]
aux[ , pen:= l3 + l4 + l8 + l9 ]
aux[ , cpt:= l2/pen  ]
indicadores <- merge( indicadores, aux[, list( t, cpt)], by=c('t'), all.x=T)

aux <- copy( acum_dem )
aux <- aux[ sexo=='Female' ,  list( l2 = sum( l2, na.rm=T), l3 = sum( l3, na.rm=T), l4 = sum( l4, na.rm=T),
                     l8 = sum( l8, na.rm=T), l9 = sum( l9, na.rm=T)), 
            by = t ]
aux[ , pen:= l3 + l4 + l8 + l9 ]
aux[ , cpf:= l2/pen  ]
indicadores <- merge( indicadores, aux[, list( t, cpf)], by=c('t'), all.x=T)

aux <- copy( acum_dem )
aux <- aux[ sexo=='Male' ,  list( l2 = sum( l2, na.rm=T), l3 = sum( l3, na.rm=T), l4 = sum( l4, na.rm=T),
                                    l8 = sum( l8, na.rm=T), l9 = sum( l9, na.rm=T)), 
            by = t ]
aux[ , pen:= l3 + l4 + l8 + l9 ]
aux[ , cpm:= l2/pen  ]
indicadores <- merge( indicadores, aux[, list( t, cpm)], by=c('t'), all.x=T)



lista <- c('indicadores' )

save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_indicadores.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()