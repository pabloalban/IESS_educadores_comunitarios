message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tSuavizamiento de las tasa de muerte de afiliados y pensionistas' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_tasas_mortalidad_estimadas.RData' ) )

# Alisando tasa mortalidad de jefes activos --------------------------------------------------------
message( '\tAlisando tasa mortalidad de jefes activos' )

tasa_mort_afi <- copy( prob_mue_act )
tasa_mort_afi[ sexo == 'F' & edad==100, qx_est:=1.6]
tasa_mort_afi[ sexo == 'F' & edad==98, qx_est:=0.95]
tasa_mort_afi_proy_f <- data.table( edad = 15:105 )

ud_smooth_model_f <- lm(  qx_est ~ bs( edad, df= 8, degree = 6
                                       #, knots = c( 60.5)
                                       ), 
                          weights = N_exp,
                          data = tasa_mort_afi[ sexo == 'F' & is.finite( qx_est ) & edad<=105] )

summary(ud_smooth_model_f)
tasa_mort_afi_proy_f[ , qx := predict( object = ud_smooth_model_f, 
                                       newdata = tasa_mort_afi_proy_f ) ]

tasa_mort_afi_proy_f[ , sexo:='F']
tasa_mort_afi_proy_f[ qx > 1 , qx := 1 ]
# plot(tasa_mort_afi[sexo=="F"]$edad, tasa_mort_afi[sexo=="F"]$qx_est, ylim = c( -0.5, 1 ));points(tasa_mort_afi_proy_f$edad, tasa_mort_afi_proy_f$qx, col="red", type = "l")
# plot(tasa_mort_afi_proy_f$edad, tasa_mort_afi_proy_f$qx, col="red")

tasa_mort_afi <- copy( prob_mue_act )
tasa_mort_afi[ sexo == 'M' & edad==100, qx_est:=1.9]
tasa_mort_afi[ sexo == 'M' & edad==105, qx_est:=3.5]
tasa_mort_afi[ sexo == 'M' & edad==108, qx_est:=3.5]
tasa_mort_afi_proy_m <- data.table( edad = 15:105 )

ud_smooth_model_m <- lm(  qx_est ~ bs( edad, df= 12, degree = 7
                                       , knots = c( 65.5)
                        ), 
                        weights = N_exp,
                        data = tasa_mort_afi[ sexo == 'M' & is.finite( qx_est ) & edad<=109] )

summary(ud_smooth_model_m)
tasa_mort_afi_proy_m[ , qx := predict( object = ud_smooth_model_m, 
                                       newdata = tasa_mort_afi_proy_m ) ]

tasa_mort_afi_proy_m[ , sexo:='M']
tasa_mort_afi_proy_m[ qx > 1 , qx := 1 ]
tasa_mort_afi_proy_m[ edad==105 , qx := 1 ]
# plot(tasa_mort_afi[sexo=="M"]$edad, tasa_mort_afi[sexo=="M"]$qx_est, ylim = c( -0.5, 1 ));points(tasa_mort_afi_proy_m$edad, tasa_mort_afi_proy_m$qx, col="red", type = "l")
# plot(tasa_mort_afi_proy_m$edad, tasa_mort_afi_proy_m$qx, col="red")

aux <- rbind( tasa_mort_afi_proy_f, tasa_mort_afi_proy_m)
iess_mort_afi_proy <- aux

iess_mort <- merge( tasa_mort_afi, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

# Para afiliados inactivos -------------------------------------------------------------------------
tasa_mort_afi_inac <- copy( prob_mue_inac )
# Alisando afiliados -------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad de afiliados inactivos' )
#tasa_mort_afi[ sexo == 'F' & edad==105, qx_est:=1]
tasa_mort_afi_inac_proy_f <- data.table( edad = 15:105 )

udi_smooth_model_f <- lm(  qx_est ~ bs( edad , df= 4 ,degree = 4
                                        , knots = c(85.0) 
                                        #, Boundary.knots = range(edad)
                        ), 
                        weights = N_exp,
                        data = tasa_mort_afi_inac[ sexo == 'F' & is.finite( qx_est ) & edad <= 90] )

summary(udi_smooth_model_f)
tasa_mort_afi_inac_proy_f[ , qinacx := predict( object = udi_smooth_model_f, 
                                                newdata = tasa_mort_afi_inac_proy_f ) ]

tasa_mort_afi_inac_proy_f[ , sexo:='F']
#tasa_mort_afi_inac_proy_f[ qinacx < 0, qinacx:= 0.0000639950 ]
tasa_mort_afi_inac_proy_f[ qinacx > 1, qinacx:= 1 ]
# plot(tasa_mort_afi_inac[sexo=="F"]$edad, tasa_mort_afi_inac[sexo=="F"]$qx_est, ylim = c( -0.5, 1 ));points(tasa_mort_afi_inac_proy_f$edad, tasa_mort_afi_inac_proy_f$qinacx, col="red", type = "l")
# plot(tasa_mort_afi_inac_proy_f$edad, tasa_mort_afi_inac_proy_f$qinacx, col="red", ylim=c(-0.25, 1.25))

tasa_mort_afi_inac_proy_m <- data.table( edad = 15:105 )

udi_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 4, degree = 2
                                        , knots = c( 84.65)
                                        #, Boundary.knots = range(0,)
                          ), 
                          weights = N_exp,
                          data = tasa_mort_afi_inac[ sexo == 'M' & is.finite( qx_est ) & edad <=87 ] )

summary(udi_smooth_model_m)
tasa_mort_afi_inac_proy_m[ , qinacx := predict( object = udi_smooth_model_m, 
                                                newdata = tasa_mort_afi_inac_proy_m ) ]
tasa_mort_afi_inac_proy_m[ , sexo:='M']
#tasa_mort_afi_inac_proy_m[ qinacx < 0, qinacx:=  0.0001502700 ]
tasa_mort_afi_inac_proy_m[ qinacx > 1, qinacx:= 1 ]
# plot(tasa_mort_afi_inac[sexo=="M"]$edad, tasa_mort_afi_inac[sexo=="M"]$qx_est, ylim = c(-0.5,1));points(tasa_mort_afi_inac_proy_m$edad, tasa_mort_afi_inac_proy_m$qinacx, col="red", type='l')
# plot(tasa_mort_afi_inac_proy_m$edad, tasa_mort_afi_inac_proy_m$qinacx, col="red")

aux <- rbind( tasa_mort_afi_inac_proy_f, tasa_mort_afi_inac_proy_m)
iess_mort_inac_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_afi_inac[ , list( sexo, edad, Ninac_exp = N_exp, Ninac_mue =N_mue,
                                                           qinacx_est = qx_est)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

# Para Pensionistas --------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad de pensionistas de vejez' )
tasa_mort_ben <- copy( prob_mue_ben )
tasa_mort_ben[ sexo == 'F' & edad ==100, qx_est:= 1.5]

tasa_mort_vej_proy_f <- data.table( edad = 60:105 )

udv_smooth_model_f <- lm(  qx_est ~ bs( edad , df = 8, degree = 5
                                        #, knots = c( 89.9)
                                        #, Boundary.knots = range(edad)
                        ), 
                        weights = N_exp,
                        data = tasa_mort_ben[ tipo=='VEJEZ' & sexo == 'F' & is.finite( qx_est ) &
                                                edad <=95] )

summary(udv_smooth_model_f)
tasa_mort_vej_proy_f[ , qvx := predict( object = udv_smooth_model_f, 
                                        newdata = tasa_mort_vej_proy_f ) ]

tasa_mort_vej_proy_f[ , sexo:='F']
tasa_mort_vej_proy_f[ edad <= 60, qvx:=0 ]
tasa_mort_vej_proy_f[ qvx>1, qvx:=1 ]
# plot(tasa_mort_ben[ tipo=='VEJEZ'& sexo=="F"]$edad, tasa_mort_ben[tipo=='VEJEZ'& sexo=="F"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_vej_proy_f$edad, tasa_mort_vej_proy_f$qvx, col="red", type="l")
# plot(tasa_mort_vej_proy_f$edad, tasa_mort_vej_proy_f$qvx, col="red")

tasa_mort_ben <- copy( prob_mue_ben )
tasa_mort_ben[ sexo == 'M' & edad ==100, qx_est:= 2.0]

tasa_mort_vej_proy_m <- data.table( edad = 60:105 )

udv_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 8, degree = 7
                                        #, knots = c( 89.9)
                                        #, Boundary.knots = range(edad)
                        ), 
                        weights = N_exp,
                        data = tasa_mort_ben[ tipo=='VEJEZ' & sexo == 'M' & is.finite( qx_est ) &
                                                edad <=96] )

summary(udv_smooth_model_m)
tasa_mort_vej_proy_m[ , qvx := predict( object = udv_smooth_model_m, 
                                        newdata = tasa_mort_vej_proy_m ) ]

tasa_mort_vej_proy_m[ , sexo:='M']
tasa_mort_vej_proy_m[ qvx>1, qvx:=1 ]
# plot(tasa_mort_ben[ tipo=='VEJEZ'& sexo=="M"]$edad, tasa_mort_ben[tipo=='VEJEZ'& sexo=="M"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_vej_proy_m$edad, tasa_mort_vej_proy_m$qvx, col="red", type="l")
# plot(tasa_mort_vej_proy_m$edad, tasa_mort_vej_proy_m$qvx, col="red")

aux <- rbind( tasa_mort_vej_proy_f, tasa_mort_vej_proy_m)
iess_mort_vej_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_ben[ tipo=='VEJEZ', list( sexo, edad, Nv_exp = N_exp, Nv_mue =N_mue,
                                                                   Nv_sal=N_sal, qvx_est=qx_est, qvx_est_sal=qx_est_sal)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

# Alisando tasa de mortalidad de pensionistas de vejez e inactivos------------------------------------------
message( '\tAlisando tasa de mortalidad de pensionistas de vejez e inactivos' )
tasa_mort_vej_inac_proy_f <- data.table( edad = 15:105 )

vej_inac <- merge( tasa_mort_ben[ tipo=='VEJEZ'][ , c('edad','sexo', 'N_exp','N_mue','N_sal','qx_est','qx_est_sal')],
                   tasa_mort_afi_inac[, list( edadin = edad, 
                                              sexoin = sexo, 
                                              Nin_exp = N_exp,
                                              Nin_mue = N_mue,
                                              qxin_est = qx_est)],
                   by.x=c('sexo', 'edad'), by.y=c( 'sexoin', 'edadin'), all.x=TRUE)

vej_inac[ , Nvi_exp:= N_exp + Nin_exp]
vej_inac[ , Nvi_mue:= N_mue + Nin_mue]
vej_inac[ , qvix_est:= Nvi_mue/Nvi_exp ]

udvina_smooth_model_f <- lm(  qvix_est ~ bs( edad , df = 5, degree = 5
                                             , knots = c( 90.1)
                                             #, Boundary.knots = range(edad)
                            ), 
                            #weights = N_exp,
                            data = vej_inac[ sexo == 'F' & is.finite( qvix_est ) & edad<=97] )

summary(udvina_smooth_model_f)
tasa_mort_vej_inac_proy_f[ , qvinax := predict( object = udvina_smooth_model_f, 
                                                newdata = tasa_mort_vej_inac_proy_f ) ]

tasa_mort_vej_inac_proy_f[ , sexo:='F']
#tasa_mort_vej_inac_proy_f[ qvinax < 0 , qvinax := 0.10092613 ]
tasa_mort_vej_inac_proy_f[ qvinax > 1 , qvinax := 1 ]
# plot(vej_inac[ sexo=="F"]$edad, vej_inac[sexo=="F"]$qvix_est, ylim=c(-0.25,1));points(tasa_mort_vej_inac_proy_f$edad, tasa_mort_vej_inac_proy_f$qvinax, col="red", type='l')
# plot(tasa_mort_vej_inac_proy_f$edad, tasa_mort_vej_inac_proy_f$qvinax, col="red")

tasa_mort_vej_inac_proy_m <- data.table( edad = 15:105 )
udvina_smooth_model_m <- lm(  qvix_est ~ bs( edad , df = 4, degree = 4
                                             , knots = c( 91.65)
                                             #, Boundary.knots = range(edad)
                            ), 
                            #weights = N_exp,
                            data = vej_inac[ sexo == 'M' & is.finite( qvix_est ) & edad<=94] )

summary(udvina_smooth_model_m)
tasa_mort_vej_inac_proy_m[ , qvinax := predict( object = udvina_smooth_model_m, 
                                                newdata = tasa_mort_vej_inac_proy_m ) ]

tasa_mort_vej_inac_proy_m[ , sexo:='M']
#tasa_mort_vej_inac_proy_m[ qvinax < 0 , qvinax:= 0.0005017653]
tasa_mort_vej_inac_proy_m[ qvinax > 1 , qvinax:= 1]
# plot(vej_inac[ sexo=="M"]$edad, vej_inac[sexo=="M"]$qvix_est, ylim=c(-0.25,1));points(tasa_mort_vej_inac_proy_m$edad, tasa_mort_vej_inac_proy_m$qvinax, col="red", type='l')
# plot(tasa_mort_vej_inac_proy_m$edad, tasa_mort_vej_inac_proy_m$qvinax, col="red")

aux <- rbind( tasa_mort_vej_inac_proy_f, tasa_mort_vej_inac_proy_m)
iess_mort_vej_inac_proy <- aux

iess_mort <- merge( iess_mort, vej_inac[ , list( sexo, edad, Nvi_exp, Nvi_mue,
                                                 qvix_est)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

# Alisando tasa mortalidad de pensionistas de invalidez --------------------------------------------
message( '\tAlisando tasa mortalidad de pensionistas de invalidez' )
tasa_mort_ben <- copy( prob_mue_ben )
tasa_mort_ben[ edad==102, qx_est:=2.5]

tasa_mort_inv_proy_f <- data.table( edad = 20:105 )

udi_smooth_model_f <- lm(  qx_est ~ bs( edad , df = 5, degree = 3
                                          # , knots = c(95.5)
                                        #, Boundary.knots = range(edad)
                        ), 
                        weights = N_exp,
                        data = tasa_mort_ben[ tipo == "INVALIDEZ" & sexo=="M" 
                                                  & is.finite( qx_est ) & edad<=105 ]  )

summary(udi_smooth_model_f)
tasa_mort_inv_proy_f[ , qix := predict( object = udi_smooth_model_f, 
                                        newdata = tasa_mort_inv_proy_f ) ]

tasa_mort_inv_proy_f[ , sexo:='F']
tasa_mort_inv_proy_f[ edad==105, qix:= 1 ]
# plot(tasa_mort_ben[tipo == "INVALIDEZ" & sexo=="M" ]$edad, tasa_mort_ben[tipo == "INVALIDEZ" & sexo=="M" ]$qx_est, ylim=c(-0.25,1));points(tasa_mort_inv_proy_f$edad, tasa_mort_inv_proy_f$qix, col="red", type='l')
# plot(tasa_mort_inv_proy_f$edad, tasa_mort_inv_proy_f$qix, col="red")

tasa_mort_inv_proy_m <- data.table( edad = 20:105 )
tasa_mort_inv_proy_m <- tasa_mort_inv_proy_m[ , qix:=tasa_mort_inv_proy_f$qix]
tasa_mort_inv_proy_m[, sexo:='M']

aux <- rbind( tasa_mort_inv_proy_f, tasa_mort_inv_proy_m)
iess_mort_inv_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_ben[ tipo=='INVALIDEZ' & sexo=="M", list( sexo, edad, Ni_exp = N_exp, Ni_mue =N_mue,
                                                                       Ni_sal=N_sal, qix_est=qx_est, qix_est_sal=qx_est_sal)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

lista <- c('iess_mort', 
           'iess_mort_afi_proy', 'iess_mort_inac_proy','iess_mort_vej_proy', 
           'iess_mort_vej_inac_proy','iess_mort_inv_proy')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
