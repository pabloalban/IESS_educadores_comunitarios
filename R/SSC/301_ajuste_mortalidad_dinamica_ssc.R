message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019.RData' ) )
#load( paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019_v2.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'onu_ecu_mort_din', 'iess_mort_afi_proy', 'iess_mort_inac_proy',
                                 'iess_mort_vej_proy', 'iess_mort_vej_inac_proy','iess_mort_inv_proy' ) ) ] )

aux_onu <- onu_ecu_mort_din[ t >= 2020, list( t, sexo, x, q_onu_x = qx, vx  ) ]
aux_iess <- iess_mort_afi_proy[ , list( t = 2020, sexo, x = edad, qx) ]

iess_mort_din <- merge( aux_onu, 
                        aux_iess[ , list( sexo, x, qx ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_inac_proy[ , list( sexo, x = edad, q_inac_x = qinacx  ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_vej_proy[ , list( sexo, x = edad, qvx ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_vej_inac_proy[ , list( sexo, x = edad, qv_inac_x = qvinax ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_inv_proy[ , list( sexo, x = edad, qix ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

setorder( iess_mort_din, t, sexo, x )

message( '\tAjustando probabilidad por variaciones' )
iess_mort_din[ is.na( qx ), qx := q_onu_x ] 
iess_mort_din[ qx < 0 , qx := 0 ] 
iess_mort_din[ is.na( q_inac_x ) | q_inac_x < 0 , q_inac_x := 0 ]
iess_mort_din[ is.na( qvx ) | qvx < 0 , qvx := 0 ]
iess_mort_din[ is.na( qv_inac_x ) | qv_inac_x < 0, qv_inac_x := 0 ]
iess_mort_din[ is.na( qix ) | qix < 0, qix := 0 ]

iess_mort_din[ qx > 1, qx := 1 ]
iess_mort_din[ q_inac_x > 1 , q_inac_x := 1 ]
iess_mort_din[ qvx > 1 , qvx := 1 ]
iess_mort_din[ qv_inac_x > 1, qv_inac_x := 1 ]
iess_mort_din[ qix > 1, qix := 1 ]

iess_mort_din[ t == 2020, vx := 1 ]

# Con variación ------------------------------------------------------------------------------------
tipo <- c( 'qx', 'q_inac_x','qvx', 'qv_inac_x','qix')
xx <- expand.grid(sexo = c('F', 'M'), x = 0:105, 
                  t = min(unique(iess_mort_din$t)):max(unique(iess_mort_din$t)))
xx <- as.data.table( xx )
setorder( xx, sexo, t )
for( i in 1: length(tipo) ){  # i<-1
  x <- dcast.data.table( iess_mort_din[ t==2020 & x<=105], sexo + x ~ t, value.var = tipo[i])
  y <- dcast.data.table( iess_mort_din[ x<=105], sexo + x ~ t, value.var = 'vx')
  res <- as.data.table( x[ , c(1,2)])
  t <- c( x[, 3] * y[,3])
  res[, `2020`:= t ]
  y <- as.data.frame( y[ , -3] )
  x <- as.data.frame( x )
  res <- as.data.frame( res )
  nm <- c('sexo', 'x', as.character( seq(min(unique(iess_mort_din$t)), max(unique(iess_mort_din$t)))  ))
  for ( j in 3: dim(y)[2]) {  
    aux <- res[ , j ] * y[ , j]
    res <- cbind( res, aux )
    colnames( res )[ j+1 ] <- nm[ j+1]
  }
  #-------------------------------------------------------------------------------
  b <- as.data.frame( melt( res, id.vars = c('sexo', 'x'), value.name = tipo[i], variable.name = 't'))
  setorder( b, sexo, t)
  b <- b[ , c( tipo[i]) ]
  xx <- data.table( cbind( xx, b ))
  eval( expr = parse( text = paste0( 'xx[ ,', tipo[i], ':= b]'  ) ) )
  eval( expr = parse( text = paste0( 'xx[ , b:= NULL]'  ) ) )
}

iess_mort_din <- merge( xx, 
                        iess_mort_din[ , list(sexo, x, t, q_onu_x)],
                        by=c('sexo', 't', 'x'), all.x=TRUE)

iess_mort_din[ qx > 1 , qx := 1 ]
iess_mort_din[ qv_inac_x > 1, qv_inac_x := 1 ]
iess_mort_din[ qvx > 1, qvx := 1 ]
iess_mort_din[ qix > 1, qix := 1 ]

iess_mort_din[ , px := 1 - qx ]
iess_mort_din[ , p_inac_x := 1 - q_inac_x ] 
iess_mort_din[ , pvx := 1 - qvx ]
iess_mort_din[ , pv_inac_x := 1 - qv_inac_x ]
iess_mort_din[ , pix := 1 - qix ]

save( iess_mort_din, 
      file = paste0( parametros$RData_seg, 'IESS_SSC_tabla_mortalidad_dinamica.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()