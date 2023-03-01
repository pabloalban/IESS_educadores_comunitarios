message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tSuavizamiento de las probabilidades de muerte de afiliados y pensionistas' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_tablas_mortalidad_estimadas.RData' ) )

# Para afiliados activos ---------------------------------------------------------------------------
tasa_mort_afi <- copy( prob_mue_act )
# Alisando afiliados -------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad de afiliados activos' )
#tasa_mort_afi[ sexo == 'F' & edad==105, qx_est:=1]
tasa_mort_afi_proy_f <- data.table( edad = 15:105 )

ud_smooth_model_f <- lm(  qx_est ~ bs( edad , df= 8 ,degree = 7
                                       , knots = c(84.5)
                                       #, Boundary.knots = range(edad)
                                      ), 
                          weights = N_exp,
                          data = tasa_mort_afi[ sexo == 'F' & is.finite( qx_est ) & edad <= 100] )

summary(ud_smooth_model_f)
tasa_mort_afi_proy_f[ , qx := predict( object = ud_smooth_model_f, 
                                       newdata = tasa_mort_afi_proy_f ) ]

tasa_mort_afi_proy_f[ , sexo:='F']
tasa_mort_afi_proy_f[ qx > 1 , qx := 1 ]
# plot(tasa_mort_afi[sexo=="F"]$edad, tasa_mort_afi[sexo=="F"]$qx_est, ylim = c( -0.5, 1 ));points(tasa_mort_afi_proy_f$edad, tasa_mort_afi_proy_f$qx, col="red", type = "l")
# plot(tasa_mort_afi_proy_f$edad, tasa_mort_afi_proy_f$qx, col="red")

tasa_mort_afi_proy_m <- data.table( edad = 15:105 )

ud_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 5, degree = 4
                                       , knots = c( 80.15)
                                       #, Boundary.knots = range(0,)
                                      ), 
                           weights = N_exp,
                           data = tasa_mort_afi[ sexo == 'M' & is.finite( qx_est ) & edad <=93 ] )

summary(ud_smooth_model_m)
tasa_mort_afi_proy_m[ , qx := predict( object = ud_smooth_model_m, 
                                       newdata = tasa_mort_afi_proy_m ) ]
tasa_mort_afi_proy_m[ , sexo:='M']
tasa_mort_afi_proy_m[ qx > 1 , qx := 1 ]
# plot(tasa_mort_afi[sexo=="M"]$edad, tasa_mort_afi[sexo=="M"]$qx_est, ylim = c(-0.5,1));points(tasa_mort_afi_proy_m$edad, tasa_mort_afi_proy_m$qx, col="red", type='l')
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

udi_smooth_model_f <- lm(  qx_est ~ bs( edad , df= 4 ,degree = 3
                                       , knots = c(83.39) 
                                       #, Boundary.knots = range(edad)
                                      ), 
                          weights = N_exp,
                          data = tasa_mort_afi_inac[ sexo == 'F' & is.finite( qx_est ) & edad <= 90] )

summary(udi_smooth_model_f)
tasa_mort_afi_inac_proy_f[ , qinacx := predict( object = udi_smooth_model_f, 
                                       newdata = tasa_mort_afi_inac_proy_f ) ]

tasa_mort_afi_inac_proy_f[ , sexo:='F']
tasa_mort_afi_inac_proy_f[ qinacx < 0, qinacx:= 0.0000639950 ]
tasa_mort_afi_inac_proy_f[ qinacx > 1, qinacx:= 1 ]
# plot(tasa_mort_afi_inac[sexo=="F"]$edad, tasa_mort_afi_inac[sexo=="F"]$qx_est, ylim = c( -0.5, 1 ));points(tasa_mort_afi_inac_proy_f$edad, tasa_mort_afi_inac_proy_f$qinacx, col="red", type = "l")
# plot(tasa_mort_afi_inac_proy_f$edad, tasa_mort_afi_inac_proy_f$qinacx, col="red", ylim=c(-0.25, 1.25))

tasa_mort_afi_inac_proy_m <- data.table( edad = 15:105 )

udi_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 4, degree = 3
                                       , knots = c( 84.9)
                                       #, Boundary.knots = range(0,)
                                      ), 
                          weights = N_exp,
                          data = tasa_mort_afi_inac[ sexo == 'M' & is.finite( qx_est ) & edad <=90 ] )

summary(udi_smooth_model_m)
tasa_mort_afi_inac_proy_m[ , qinacx := predict( object = udi_smooth_model_m, 
                                       newdata = tasa_mort_afi_inac_proy_m ) ]
tasa_mort_afi_inac_proy_m[ , sexo:='M']
tasa_mort_afi_inac_proy_m[ qinacx < 0, qinacx:=  0.0001502700 ]
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


# Para Pensionistas
tasa_mort_ben <- copy( prob_mue_ben )
# Alisando pensionistas -----------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad de pensionistas de vejez' )
tasa_mort_vej_proy_f <- data.table( edad = 48:105 )

udv_smooth_model_f <- lm(  qx_est ~ bs( edad , df = 7, degree = 6
                                        , knots = c( 89.9)
                                        #, Boundary.knots = range(edad)
                                      ), 
                           weights = N_exp,
                           data = tasa_mort_ben[ tipo=='VEJEZ' & sexo == 'F' & is.finite( qx_est ) & edad<=98] )

summary(udv_smooth_model_f)
tasa_mort_vej_proy_f[ , qvx := predict( object = udv_smooth_model_f, 
                                        newdata = tasa_mort_vej_proy_f ) ]

tasa_mort_vej_proy_f[ , sexo:='F']
tasa_mort_vej_proy_f[ edad <=50, qvx:=0 ]
tasa_mort_vej_proy_f[ qvx > 1, qvx := 1 ]
# plot(tasa_mort_ben[ tipo=='VEJEZ'& sexo=="F"]$edad, tasa_mort_ben[tipo=='VEJEZ'& sexo=="F"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_vej_proy_f$edad, tasa_mort_vej_proy_f$qvx, col="red", type="l")
# plot(tasa_mort_vej_proy_f$edad, tasa_mort_vej_proy_f$qvx, col="red")

tasa_mort_ben[ tipo=='VEJEZ' & sexo == 'M' & edad==105, qx_est:=1]
tasa_mort_vej_proy_m <- data.table( edad = 40:105 )

udv_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 9, degree = 7
                                        , knots = c( 90.095)
                                        #, Boundary.knots = range(edad)
                                       ), 
                            weights = N_exp,
                            data = tasa_mort_ben[ tipo=='VEJEZ' & sexo == 'M' & is.finite( qx_est ) 
                                                  & edad>=50 & edad<=95] )

summary(udv_smooth_model_m)
tasa_mort_vej_proy_m[ , qvx := predict( object = udv_smooth_model_m, 
                                        newdata = tasa_mort_vej_proy_m ) ]

tasa_mort_vej_proy_m[ , sexo:='M']
tasa_mort_vej_proy_m[ edad <= 49, qvx := 0 ] 
tasa_mort_vej_proy_m[ qvx > 1, qvx := 1 ] 
# plot(tasa_mort_ben[ tipo=='VEJEZ'& sexo=="M"]$edad, tasa_mort_ben[tipo=='VEJEZ'& sexo=="M"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_vej_proy_m$edad, tasa_mort_vej_proy_m$qvx, col="red", type="l")
# plot(tasa_mort_vej_proy_m$edad, tasa_mort_vej_proy_m$qvx, col="red")

aux <- rbind( tasa_mort_vej_proy_f, tasa_mort_vej_proy_m)
iess_mort_vej_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_ben[ tipo=='VEJEZ', list( sexo, edad, Nv_exp = N_exp, Nv_mue =N_mue,
                                                                   Nv_sal=N_sal, qvx_est=qx_est, qvx_est_sal=qx_est_sal)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)


# Alisando tasa de mortalidad de pensionistas e inactivos-------------------------------------------
message( '\tAlisando tasa de mortalidad de pensionistas e inactivos' )
tasa_mort_vej_inac_proy_f <- data.table( edad = 15:105 )

vej_inac <- merge( tasa_mort_ben[ tipo=='VEJEZ'][ , c('edad','sexo', 'N_exp','N_mue','N_sal','qx_est','qx_est_sal')],
                   tasa_mort_afi_inac[, list( edadin=edad, 
                                              sexoin=sexo, 
                                              Nin_exp=N_exp,
                                              Nin_mue= N_mue,
                                              qxin_est=qx_est)],
                   by.x=c('sexo', 'edad'), by.y=c( 'sexoin', 'edadin'), all.x=TRUE)

vej_inac[ , Nvi_exp:= N_exp + Nin_exp]
vej_inac[ , Nvi_mue:= N_mue + Nin_mue]
vej_inac[ , qvix_est:= Nvi_mue/Nvi_exp ]

udvina_smooth_model_f <- lm(  qvix_est ~ bs( edad , df = 6, degree = 5
                                        , knots = c( 86.81)
                                        #, Boundary.knots = range(edad)
                                       ), 
                          #weights = N_exp,
                          data = vej_inac[ sexo == 'F' & is.finite( qvix_est ) & edad<=93] )

summary(udvina_smooth_model_f)
tasa_mort_vej_inac_proy_f[ , qvinax := predict( object = udvina_smooth_model_f, 
                                        newdata = tasa_mort_vej_inac_proy_f ) ]

tasa_mort_vej_inac_proy_f[ , sexo:='F']
tasa_mort_vej_inac_proy_f[ qvinax < 0 , qvinax := 0.00039560986 ]
tasa_mort_vej_inac_proy_f[ qvinax > 1 , qvinax := 1 ]
# plot(vej_inac[ sexo=="F"]$edad, vej_inac[sexo=="F"]$qvix_est, ylim=c(-0.25,1));points(tasa_mort_vej_inac_proy_f$edad, tasa_mort_vej_inac_proy_f$qvinax, col="red", type='l')
# plot(tasa_mort_vej_inac_proy_f$edad, tasa_mort_vej_inac_proy_f$qvinax, col="red")

tasa_mort_vej_inac_proy_m <- data.table( edad = 15:105 )
udvina_smooth_model_m <- lm(  qvix_est ~ bs( edad , df = 4, degree = 3
                                             , knots = c( 90.33)
                                             #, Boundary.knots = range(edad)
                                           ), 
                              #weights = N_exp,
                               data = vej_inac[ sexo == 'M' & is.finite( qvix_est ) & edad<=94] )

summary(udvina_smooth_model_m)
tasa_mort_vej_inac_proy_m[ , qvinax := predict( object = udvina_smooth_model_m, 
                                                newdata = tasa_mort_vej_inac_proy_m ) ]

tasa_mort_vej_inac_proy_m[ , sexo:='M']
tasa_mort_vej_inac_proy_m[ qvinax < 0 , qvinax:= 0.0005017653]
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

# Alisando tasa mortalidad de pensionistas de invalidez-discapaidad --------------------------------
message( '\tAlisando tasa mortalidad de pensionistas de invalidez' )
tasa_mort_ben[ tipo%in%c('INVALIDEZ','DISCAPACIDAD'), tipo1:='INV-DIS']
tasa_mort_inv_proy_f <- data.table( edad = 20:105 )

aux_tabla <- tasa_mort_ben[ tipo1=='INV-DIS',
                            list( N_exp=sum(N_exp, na.rm=T), N_mue=sum(N_mue, na.rm=T),
                                  N_sal=sum(N_sal, na.rm=T), qx_est=sum(qx_est, na.rm=T), 
                                  qx_est_sal=sum(qx_est_sal, na.rm=T)), by=list(edad, sexo, tipo1)]

udi_smooth_model_f <- lm(  qx_est ~ bs( edad , df = 8, degree = 5
                                        , knots = c( 40.5,72.5, 87.8)
                                        #, Boundary.knots = range(edad)
                                       ), 
                          weights = N_exp,
                          data = aux_tabla[ tipo1=='INV-DIS' & sexo == 'F' & is.finite( qx_est ) & edad<=100] )

summary(udi_smooth_model_f)
tasa_mort_inv_proy_f[ , qix := predict( object = udi_smooth_model_f, 
                                        newdata = tasa_mort_inv_proy_f ) ]

tasa_mort_inv_proy_f[ , sexo:='F']
tasa_mort_inv_proy_f[ qix > 1, qix:= 1 ]
# plot(aux_tabla[ tipo1=='INV-DIS' & sexo=="F"]$edad, aux_tabla[ tipo1=='INV-DIS'& sexo=="F"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_inv_proy_f$edad, tasa_mort_inv_proy_f$qix, col="red", type='l')
# plot(tasa_mort_inv_proy_f$edad, tasa_mort_inv_proy_f$qix, col="red")

tasa_mort_inv_proy_m <- data.table( edad = 20:105 )

udi_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 7, degree = 3
                                        , knots = c( 0.5, 17.5,88.02 )
                                        #, Boundary.knots = range(edad)
                                      ), 
                            #weights = N_exp,
                            data = aux_tabla[ tipo1=='INV-DIS' & sexo == 'M' & is.finite( qx_est ) 
                                               & edad <= 96 & edad!=70] )

summary(udi_smooth_model_m)
tasa_mort_inv_proy_m[ , qix := predict( object = udi_smooth_model_m, 
                                        newdata = tasa_mort_inv_proy_m ) ]

tasa_mort_inv_proy_m[ , sexo:='M']
tasa_mort_inv_proy_m[ qix > 1, qix := 1 ]
# plot(aux_tabla[ tipo1=='INV-DIS'& sexo=="M"]$edad, aux_tabla[tipo1=='INV-DIS' & sexo=="M"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_inv_proy_m$edad, tasa_mort_inv_proy_m$qix, col="red", type='l')
# plot(tasa_mort_inv_proy_m$edad, tasa_mort_inv_proy_m$qix, col="red")


aux <- rbind( tasa_mort_inv_proy_f, tasa_mort_inv_proy_m)
iess_mort_inv_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_ben[ tipo=='INVALIDEZ', list( sexo, edad, Ni_exp = N_exp, Ni_mue =N_mue,
                                                                       Ni_sal=N_sal, qix_est=qx_est, qix_est_sal=qx_est_sal)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)


# Alisando tasa mortalidad de pensionistas de discapacidad -----------------------------------------
message( '\tAlisando tasa mortalidad de pensionistas de discapacidad' )
tasa_mort_dis_proy_f <- data.table( edad = 15:105 )

aux <- copy( prob_mue_ben )
aux[ , tipo:=as.character(tipo)]
aux[ tipo=='DISCAPACIDAD' & edad==37 & sexo=='F', qx_est := 0.15]
aux[ tipo=='DISCAPACIDAD' & edad==70 & sexo=='F', qx_est := 0.10]
aux[ tipo=='DISCAPACIDAD' & edad==95 & sexo=='F', qx_est := 0.3 ]
aux[ tipo=='DISCAPACIDAD' & edad==100 & sexo=='F', qx_est := 0.70 ]
aux[ tipo=='DISCAPACIDAD' & edad==105 & sexo=='F', qx_est := 1 ]


udd_smooth_model_f <- lm(  qx_est ~ bs( edad , df = 4, degree = 4
                                        #, knots = c( 89.9)
                                        #, Boundary.knots = range(edad)
                        ),
                        data = aux[ tipo=='DISCAPACIDAD' & is.finite( qx_est ) & sexo=='F' & edad >=21] )

summary(udd_smooth_model_f)
tasa_mort_dis_proy_f[ , qdx := predict( object = udd_smooth_model_f, 
                                           newdata = tasa_mort_dis_proy_f )  ]

tasa_mort_dis_proy_f[ , sexo:='F']
tasa_mort_dis_proy_f[ edad <= 34, qdx := 0 ] 
tasa_mort_dis_proy_f[ qdx > 1, qdx := 1 ] 
# plot(aux[ tipo=='DISCAPACIDAD'& sexo=="F"]$edad, aux[tipo=='DISCAPACIDAD'& sexo=="F"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_dis_proy_f$edad, tasa_mort_dis_proy_f$qdx, col="red", type="l")
# plot(tasa_mort_dis_proy_f$edad, tasa_mort_dis_proy_f$qdx, col="red")

tasa_mort_dis_proy_m <- data.table( edad = 15:105 )

aux[ tipo=='DISCAPACIDAD' & edad==35 & sexo=='M', qx_est := 0.012 ]
aux[ tipo=='DISCAPACIDAD' & edad==70 & sexo=='M', qx_est := 0.04 ]
aux[ tipo=='DISCAPACIDAD' & edad==95 & sexo=='M', qx_est := 0.3 ]
aux[ tipo=='DISCAPACIDAD' & edad==100 & sexo=='M', qx_est := 0.70 ]
aux[ tipo=='DISCAPACIDAD' & edad==105 & sexo=='M', qx_est := 1 ]

udd_smooth_model_m <- lm(  qx_est ~ bs( edad , df = 5, degree = 4
                                        #, knots = c( 90.095)
                                        #, Boundary.knots = range(edad)
                         ), 
                        data = aux[ tipo=='DISCAPACIDAD' & sexo == 'M' & is.finite( qx_est ) ] )

summary(udd_smooth_model_m)
tasa_mort_dis_proy_m[ , qdx := predict( object = udd_smooth_model_m, 
                                        newdata = tasa_mort_dis_proy_m ) ]

tasa_mort_dis_proy_m[ , sexo:='M']
tasa_mort_dis_proy_m[ edad <= 34, qdx := 0 ] 
tasa_mort_dis_proy_m[ qdx > 1, qdx := 1 ] 
plot(aux[ tipo=='DISCAPACIDAD'& sexo=="M"]$edad, aux[tipo=='DISCAPACIDAD'& sexo=="M"]$qx_est, ylim=c(-0.25,1));points(tasa_mort_dis_proy_m$edad, tasa_mort_dis_proy_m$qdx, col="red", type="l")
# plot(tasa_mort_dis_proy_m$edad, tasa_mort_dis_proy_m$qdx, col="red")

aux <- rbind( tasa_mort_dis_proy_f, tasa_mort_dis_proy_m)
iess_mort_dis_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_ben[ tipo=='DISCAPACIDAD', list( sexo, edad, Nd_exp = N_exp, Nd_mue =N_mue,
                                                                   Nd_sal=N_sal, qdx_est=qx_est, qdx_est_sal=qx_est_sal)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)


# Alisando tasa mortalidad de pensionistas de orfandad --------------------------------------------
aux <- tasa_mort_ben[ tipo=='ORFANDAD' & sexo == 'F' & is.finite( qx_est_sal )   & edad <= 105 ]

aux[ edad <= 40 & qx_est_sal >= 0.10, N_exp := 0.0 ]

aux[ edad == 105, qx_est_sal := 0.99 ]
aux[ edad == 104, qx_est_sal := 0.7 ]
aux[ edad == 103, qx_est_sal := 0.5 ]
aux[ edad == 102, qx_est_sal := 0.2 ]

aux[ edad == 105, N_exp := 1000 ]
aux[ edad == 104, N_exp := 100 ]
aux[ edad == 103, N_exp := 100 ]
aux[ edad == 102, N_exp := 90 ]

message( '\tAlisando tasa mortalidad de pensionistas de orfandad' )
tasa_mort_orf_proy_f <- data.table( edad = 0:105 )

udo_smooth_model_f <- lm(  qx_est_sal ~ bs( edad , df = 8, degree = 6
                                        , knots = c(21.5, 30.5, 95.5)
                                        #knots = c(21.5, 30.5, 78.5, 90.5, 100.5 )
                                        #, Boundary.knots = range(edad)
                                        ), 
                          weights = N_exp,
                            # data = tasa_mort_ben[ tipo=='ORFANDAD' & sexo == 'F' & is.finite( qx_est_sal )
                            #                     & edad>=15  & edad<=90]
                          data = aux
                          )

summary(udo_smooth_model_f)
tasa_mort_orf_proy_f[ , qox := predict( object = udo_smooth_model_f, 
                                        newdata = tasa_mort_orf_proy_f ) ]

tasa_mort_orf_proy_f[ , sexo:='F']
tasa_mort_orf_proy_f[ edad == 17 , qox := aux[ edad==17 ]$qx_est_sal ]
tasa_mort_orf_proy_f[ edad == 18 , qox := aux[ edad==18 ]$qx_est_sal ]
tasa_mort_orf_proy_f[ edad == 20 , qox := aux[ edad==20 ]$qx_est_sal ]
tasa_mort_orf_proy_f[ edad == 21 , qox := aux[ edad==21 ]$qx_est_sal ]
tasa_mort_orf_proy_f[ edad == 105 , qox := 1 ]

# plot(aux[ tipo=='ORFANDAD'& sexo=="F"]$edad, aux[tipo=='ORFANDAD' & sexo=="F"]$qx_est_sal, ylim=c(-0.25,1));points(tasa_mort_orf_proy_f$edad, tasa_mort_orf_proy_f$qox, col="red", type='l')
# plot(tasa_mort_orf_proy_f$edad, tasa_mort_orf_proy_f$qox, col="red", ylim=c(-1,2))

########################################################################################################
aux <- tasa_mort_ben[ tipo=='ORFANDAD' & sexo == 'M' & is.finite( qx_est_sal )  & edad<= 105 ]
aux[ edad <= 40 & qx_est_sal >= 0.10, N_exp := 0.0 ]
aux[ edad > 40 & qx_est_sal == 0.0, N_exp := 0.0 ]

tasa_mort_orf_proy_m <- data.table( edad = 0:105 )

udo_smooth_model_m <- lm(  qx_est_sal ~ bs( edad , df = 8, degree = 3
                                        , knots = c( 25.5, 40.5, 71.36 )
                                        #, Boundary.knots = range(edad)
                                       ), 
                           weights = N_exp,
                           data = aux )

summary(udo_smooth_model_m)
tasa_mort_orf_proy_m[ , qox := predict( object = udo_smooth_model_m, 
                                        newdata = tasa_mort_orf_proy_m ) ]

tasa_mort_orf_proy_m[ , sexo:='M']
tasa_mort_orf_proy_m[ edad == 17 , qox := aux[ edad==17 ]$qx_est_sal ]
tasa_mort_orf_proy_m[ edad == 18 , qox := aux[ edad==18 ]$qx_est_sal ]
tasa_mort_orf_proy_m[ edad == 20 , qox := aux[ edad==20 ]$qx_est_sal ]
tasa_mort_orf_proy_m[ edad == 21 , qox := aux[ edad==21 ]$qx_est_sal ]
tasa_mort_orf_proy_m[ qox > 1, qox:= 1 ]
# plot(aux[ tipo=='ORFANDAD'& sexo=="M"]$edad, aux[tipo=='ORFANDAD' & sexo=="M"]$qx_est_sal, ylim=c(-0.25,1));points(tasa_mort_orf_proy_m$edad, tasa_mort_orf_proy_m$qox, col="red", type='l')
# plot(tasa_mort_orf_proy_m$edad, tasa_mort_orf_proy_m$qox, col="red", ylim=c(-1,2))

aux <- rbind( tasa_mort_orf_proy_f, tasa_mort_orf_proy_m)
iess_mort_orf_proy <- aux

iess_mort <- merge( iess_mort, 
                    tasa_mort_ben[ tipo=='ORFANDAD', 
                                   list( sexo, edad, 
                                         No_exp = N_exp, 
                                         No_mue =N_mue, 
                                         No_sal = N_sal, 
                                         qox_est = qx_est, 
                                         qox_est_sal = qx_est_sal) 
                                   ],
                    by.x=c('edad', 'sexo'), 
                    by.y=c('edad', 'sexo'), 
                    all.x=TRUE )

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)


# Alisando tasa mortalidad de pensionistas de viudedad  --------------------------------------------
message( '\tAlisando tasa mortalidad de pensionistas de viudedad' )
tasa_mort_viu_proy_f <- data.table( edad = 14:105 )

aux <- tasa_mort_ben[ tipo == 'VIUDEDAD' & sexo == 'F' & is.finite( qx_est_sal ) & edad >= 21 & edad <= 105 ]
aux[ edad == 24, N_exp := 1000 ]
aux[ edad == 22, N_exp := 3000 ]
aux[ edad >= 25 & edad <= 50, N_exp := 0 ]
aux[ edad == 105, qx_est_sal := 0.99 ]
aux[ edad == 104, qx_est_sal := 0.7 ]
aux[ edad == 103, qx_est_sal := 0.5 ]
aux[ edad == 102, qx_est_sal := 0.3 ]

aux[ edad == 105, N_exp := 8000 ]
aux[ edad == 104, N_exp := 1000 ]
aux[ edad == 103, N_exp := 1000 ]
aux[ edad == 102, N_exp := 1000 ]

udw_smooth_model_f <- lm(  qx_est_sal ~ bs( edad , df = 8, degree = 4
                                        , knots = c(95.5 )
                                        #, Boundary.knots = range(edad)
                                        ), 
                                        weights = N_exp,
                                        data = aux 
                           )

summary(udw_smooth_model_f)
tasa_mort_viu_proy_f[ , qwx := predict( object = udw_smooth_model_f, 
                                        newdata = tasa_mort_viu_proy_f ) ]

tasa_mort_viu_proy_f[ , sexo:='F']
tasa_mort_viu_proy_f[ edad==105, qwx:= 1 ]
# plot( aux$edad, aux$qx_est_sal, ylim=c(-0.25,1));points(tasa_mort_viu_proy_f$edad, tasa_mort_viu_proy_f$qwx, col="red", type='l')
# plot(tasa_mort_viu_proy_f$edad, tasa_mort_viu_proy_f$qwx, col="red", ylim=c(-1,2))

##############################################################################
tasa_mort_viu_proy_m <- data.table( edad = 14:105 )
aux <- tasa_mort_ben[ tipo=='VIUDEDAD' & sexo == 'M' & is.finite( qx_est_sal ) & edad>= 33 & edad <= 105 ]

#aux[ edad <= 50, N_exp := 0 ]
aux[ edad == 51, N_exp := 1000 ]
aux[ edad == 47, N_exp := 1000 ]
aux[ edad <= 50 & qx_est_sal >= 0.020, N_exp := 0.0 ]

aux[ edad == 105, qx_est_sal := 0.99 ]
aux[ edad == 104, qx_est_sal := 0.8 ]
aux[ edad == 103, qx_est_sal := 0.7 ]
aux[ edad == 102, qx_est_sal := 0.6 ]
aux[ edad == 101, qx_est_sal := 0.5 ]
aux[ edad == 100, qx_est_sal := 0.4 ]
aux[ edad == 40, qx_est_sal := 0.01 ]
aux[ edad == 42, qx_est_sal := 0.01 ]

aux[ edad == 105, N_exp := 1000 ]
aux[ edad == 104, N_exp := 1000 ]
aux[ edad == 103, N_exp := 1000 ]
aux[ edad == 102, N_exp := 1000 ]
aux[ edad == 101, N_exp := 1000 ]
aux[ edad == 100, N_exp := 1000 ]

udw_smooth_model_m <- lm(  qx_est_sal ~ bs( edad , df = 18, degree = 3
                                        , knots = c( 89.5 )
                                        #, Boundary.knots = range(edad)
                                        ), 
                                        weights = N_exp,
                                        data = aux )

summary(udw_smooth_model_m)
tasa_mort_viu_proy_m[ , qwx := predict( object = udw_smooth_model_m, 
                                        newdata = tasa_mort_viu_proy_m ) ]

tasa_mort_viu_proy_m[ , sexo:='M']
tasa_mort_viu_proy_m[ edad == 105, qwx:= 1 ]
# plot( aux$edad, aux$qx_est_sal, ylim=c(-0.25,1));points(tasa_mort_viu_proy_m$edad, tasa_mort_viu_proy_m$qwx, col="red", type='l')
# plot(tasa_mort_viu_proy_m$edad, tasa_mort_viu_proy_m$qwx, col="red", ylim=c(-1,2))

aux <- rbind( tasa_mort_viu_proy_f, tasa_mort_viu_proy_m)
iess_mort_viu_proy <- aux

iess_mort <- merge( iess_mort, tasa_mort_ben[ tipo=='VIUDEDAD', list( sexo, edad, Nw_exp = N_exp, Nw_mue =N_mue,
                                                                      Nw_sal=N_sal, qwx_est=qx_est, qwx_est_sal=qx_est_sal)],
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort <- merge( iess_mort, aux,
                    by.x=c('edad', 'sexo'), by.y=c('edad', 'sexo'), all.x=TRUE)

iess_mort[ is.na(iess_mort), ] <- NA
setorder( iess_mort, sexo, edad)

lista <- c('iess_mort',
           'iess_mort_afi_proy', 'iess_mort_inac_proy', 'iess_mort_vej_proy',
           'iess_mort_vej_inac_proy', 'iess_mort_inv_proy', 'iess_mort_dis_proy',
           'iess_mort_orf_proy', 'iess_mort_viu_proy')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_IVM_suavizamiento_tablas_mortalidad.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()