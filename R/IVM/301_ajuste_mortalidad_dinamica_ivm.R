message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019.RData' ) )
#load( paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019_v2.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_suavizamiento_tablas_mortalidad.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'onu_ecu_mort_din', 'iess_mort_afi_proy',
                                 'iess_mort_inac_proy', 'iess_mort_vej_proy','iess_mort_vej_inac_proy',
                                 'iess_mort_inv_proy', 'iess_mort_dis_proy','iess_mort_orf_proy',
                                 'iess_mort_viu_proy' ) ) ] )

# #Interpolación  de vx ----------------------------------------------------------
# anio <- min(unique(onu_ecu_mort_din$t)):max(unique(onu_ecu_mort_din$t))
# ch_sexo <- c( "F", "M" )
# suavi_vx <- NULL
# for( j in 1:2) {
# for( i in 1:length( anio )){ 
# aux_suavi_vx <- data.table( x = 0:max(unique(onu_ecu_mort_din$x)) )
# smooth_model <- lm(  vx ~ bs( x , df= 8 ,degree = 7
#                                    , knots = c(84.5)
#                                    #, Boundary.knots = range(edad)
#                                   ), 
#                           data = onu_ecu_mort_din[ t == as.numeric(anio[i]) & sexo == as.character(ch_sexo[j]) ] )
# 
# aux_suavi_vx[  , vxs := predict( object = smooth_model, 
#                             newdata = aux_suavi_vx) ]
# aux_suavi_vx[ , t := anio[i]]
# aux_suavi_vx[ , sexo:= ch_sexo[j]]
# suavi_vx <- rbind( suavi_vx, aux_suavi_vx )
# }
# 
# }
# #-------------------------------------------------------------------------------
# onu_ecu_mort_din <- merge( onu_ecu_mort_din,
#                            suavi_vx,
#                            by=c('t', 'sexo', 'x'), all.x = TRUE)


aux_onu <- onu_ecu_mort_din[ t >= 2020, list( t, sexo, x, q_onu_x = qx, vx  ) ]
# --------------------------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad de no afiliados' )

aux <- aux_onu[ sexo == 'F' & t == 2020 & x<=105 ]
#aux[ x==103, q_onu_x:=0.85]
aux[ x==104, q_onu_x:=0.95]
aux[ x==105, q_onu_x:=1]

tasa_mort_noafi_proy_f <- data.table( x= 0:105 )

ud_smooth_model_f <- lm(  q_onu_x ~ bs( x , df= 8 ,degree = 6
                                       , knots = c(95)
                                       #, Boundary.knots = range(edad)
                        ), 
                        #weights = N_exp,
                        data = aux[ sexo == 'F' & is.finite( q_onu_x ) ] )

summary(ud_smooth_model_f)
tasa_mort_noafi_proy_f[ , qx := predict( object = ud_smooth_model_f, 
                                       newdata = tasa_mort_noafi_proy_f) ]

tasa_mort_noafi_proy_f[ , sexo:='F']
tasa_mort_noafi_proy_f[ qx > 1 , qx := 1 ]
# plot(aux[t==2020&sexo=="F"]$x, aux[t==2020&sexo=="F"]$q_onu_x, ylim = c( -0.5, 1 ));points(tasa_mort_noafi_proy_f$x, tasa_mort_noafi_proy_f$qx, col="red", type = "l")

aux <- aux_onu[ sexo == 'M' & t == 2020 & x<=105 ]
#aux[ x==103, q_onu_x:=0.85]
aux[ x==104, q_onu_x:=0.95]
aux[ x==105, q_onu_x:=1]

tasa_mort_noafi_proy_m <- data.table( x= 0:105 )

ud_smooth_model_m <- lm(  q_onu_x ~ bs( x , df= 7 ,degree = 6
                                        , knots = c(97.7)
                                        #, Boundary.knots = range(edad)
                                      ), 
                                      #weights = N_exp,
                                      data = aux[ sexo == 'M' & is.finite( q_onu_x ) ] )

summary(ud_smooth_model_m)
tasa_mort_noafi_proy_m[ , qx := predict( object = ud_smooth_model_m, 
                                         newdata = tasa_mort_noafi_proy_m) ]
tasa_mort_noafi_proy_m[ , sexo:='m']
tasa_mort_noafi_proy_m[ qx > 1 , qx := 1 ]
# plot(aux[t==2020&sexo=="M"]$x, aux[t==2020&sexo=="M"]$q_onu_x, ylim = c( -0.5, 1 ));points(tasa_mort_noafi_proy_m$x, tasa_mort_noafi_proy_m$qx, col="red", type = "l")
# --------------------------------------------------------------------------------------------------
aux_onu <- aux_onu[ t==2020 & sexo=='F' & x>=80 & x<=105, q_onu_x:=tasa_mort_noafi_proy_f[ x>=80 ]$qx ]
aux_onu <- aux_onu[ t==2020 & sexo=='M' & x>=80 & x<=105, q_onu_x:=tasa_mort_noafi_proy_m[ x>=80 ]$qx ]

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

iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_dis_proy[ , list( sexo, x = edad, qdx ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_orf_proy[ , list( sexo, x = edad, qox ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )
iess_mort_din <- merge( iess_mort_din, 
                        iess_mort_viu_proy[ , list( sexo, x = edad, qwx ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

setorder( iess_mort_din, t, sexo, x )

message( '\tAjustando probabilidad por variaciones' )
iess_mort_din[ is.na( qx ), qx := q_onu_x ] # solución temporal hasta completar la tabla del IESS
iess_mort_din[ qx < 0 , qx := 0 ] 
iess_mort_din[ is.na( q_inac_x ) | q_inac_x < 0 , q_inac_x := 0 ]
iess_mort_din[ is.na( qvx ) | qvx < 0 , qvx := 0 ]
iess_mort_din[ is.na( qv_inac_x ) | qv_inac_x < 0, qv_inac_x := 0 ]
iess_mort_din[ is.na( qix ) | qix < 0, qix := 0 ]
iess_mort_din[ is.na( qox ) | qox < 0, qox := 0 ]
iess_mort_din[ is.na( qwx ) | qwx < 0, qwx := 0 ]

iess_mort_din[ qx > 1, qx := 1 ]
iess_mort_din[ q_inac_x > 1 , q_inac_x := 1 ]
iess_mort_din[ qvx > 1 , qvx := 1 ]
iess_mort_din[ qv_inac_x > 1, qv_inac_x := 1 ]
iess_mort_din[ qix > 1, qix := 1 ]
iess_mort_din[ qox > 1, qox := 1 ]
iess_mort_din[ qwx > 1, qwx := 1 ]

iess_mort_din[ t == 2020, vx := 1 ]

# Con variación ----------------------------------------------------------------
tipo <- c( 'qx', 'q_inac_x', 'qvx', 'qv_inac_x', 'qix', 'qdx','qox', 'qwx', 'q_onu_x')
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
                        iess_mort_din[ , list(sexo, x, t)],
                        by=c('sexo', 't', 'x'), all.x=TRUE)

iess_mort_din[ qx > 1 , qx := 1 ]
# iess_mort_din[ , q_inac_x := vx * q_inac_x ]
# iess_mort_din[ , qvx := vx * qvx ]
iess_mort_din[ qv_inac_x > 1, qv_inac_x := 1 ]
iess_mort_din[ qix > 1, qix := 1 ]
iess_mort_din[ qdx > 1, qdx := 1 ]
iess_mort_din[ qox > 1, qox := 1 ]
iess_mort_din[ qwx > 1, qwx := 1 ]

iess_mort_din[ x==17 & t!=2020 & sexo =='F', qox := iess_mort_din[ x==17 & t==2020 & sexo =='F']$qox]
iess_mort_din[ x==17 & t!=2020 & sexo =='M', qox := iess_mort_din[ x==17 & t==2020 & sexo =='M']$qox]
iess_mort_din[ x==18 & t!=2020 & sexo =='F', qox := iess_mort_din[ x==18 & t==2020 & sexo =='F']$qox]
iess_mort_din[ x==18 & t!=2020 & sexo =='M', qox := iess_mort_din[ x==18 & t==2020 & sexo =='M']$qox]
iess_mort_din[ x==20 & t!=2020 & sexo =='F', qox := iess_mort_din[ x==20 & t==2020 & sexo =='F']$qox]
iess_mort_din[ x==20 & t!=2020 & sexo =='M', qox := iess_mort_din[ x==20 & t==2020 & sexo =='M']$qox]
iess_mort_din[ x==21 & t!=2020 & sexo =='F', qox := iess_mort_din[ x==21 & t==2020 & sexo =='F']$qox]
iess_mort_din[ x==21 & t!=2020 & sexo =='M', qox := iess_mort_din[ x==21 & t==2020 & sexo =='M']$qox]

iess_mort_din[ , px := 1 - qx ] 
iess_mort_din[ , p_inac_x := 1 - q_inac_x ] 
iess_mort_din[ , pvx := 1 - qvx ] 
iess_mort_din[ , pv_inac_x := 1 - qv_inac_x ] 
iess_mort_din[ , pix := 1 - qix ] 
iess_mort_din[ , pdx := 1 - qdx ] 
iess_mort_din[ , pox := 1 - qox ] 
iess_mort_din[ , pwx := 1 - qwx ] 

message( '\tGuardando resultados' )
save( iess_mort_din, 
      file = paste0( parametros$RData_seg, 'IESS_IVM_tabla_mortalidad_dinamica.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
