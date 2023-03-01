message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'ONU_life_table_survivors_2019.RData' ) )
# 
# # Preparando información -------------------------------------------------------
# # Extrayendo solo la información del Ecuador
# # onu_ecu_surv <- onu_survivors[ country_code == 218, list( period, sexo, x, lx ) ]
# # onu_ecu_surv[ , t := as.numeric( substr( period, 1, 4 ) ) ]
# # onu_ecu_surv <- dcast.data.table( onu_ecu_surv, t + sexo ~ x, value.var = "lx")
# # onu_ecu_surv_pro <- onu_ecu_surv[ t >= 2015 & t <= 2065]
# # 
# # ch_sexo <- c('F', 'M')
# # for ( i in 1:2) { #i <- 1
# # onu_ecu_surv <- onu_ecu_surv_pro[ sexo==ch_sexo[i], 1:15 ] # Hasta los 50 años
# # # No se incluye la edad de 1 año
# # onu_ecu_surv <- as.matrix( onu_ecu_surv[ , -c('t', 'sexo', '1')] )
# # 
# # # Nodos originales
# # x <- c( 0,seq(0,50,5)[-1] )
# # y <- seq( 2015, 2065, 5)
# # # Nodos  a evaluar
# # xp <- seq(0,50,1)
# # yp <- c(seq(2015, 2065, 1))
# # 
# # obj <- list( x= x, y= y, z= onu_ecu_surv)
# # loc <- make.surface.grid( list( xp,yp))
# # Res <-interp.surface( obj, loc)
# # 
# # aux_1 <- matrix(Res, 51)
# # colnames(aux_1) <- as.character(xp)
# # 
# # onu_ecu_surv <- onu_ecu_surv_pro[ sexo==ch_sexo[i], c(1,2, 14:24) ] # De 50 a 100 años
# # onu_ecu_surv <- as.matrix( onu_ecu_surv[ , -c('t', 'sexo')] )
# # 
# # # Nodos originales
# # x <- c( 50,seq(50,100,5)[-1] )
# # y <- seq( 2015, 2065, 5)
# # # Nodos a evaluar
# # xp <- seq(50,100,1)
# # yp <- c(seq(2015, 2065, 1))
# # 
# # obj <- list( x= x, y= y, z= onu_ecu_surv)
# # loc <- make.surface.grid( list( xp,yp))
# # Res <-interp.surface( obj, loc)
# # 
# # aux_2 <- matrix(Res, 51)
# # colnames(aux_2) <- as.character(xp)
# # 
# # if (i == 1){
# #   inter_onu_f <- data.table( cbind( aux_1, aux_2[, -1] ) )
# # }
# # if(i == 2){
# #   inter_onu_m <- data.table( cbind( aux_1, aux_2[, -1] ) )
# # }
# # 
# # }
# # 
# # inter_onu_f[ , t:= seq( 2015, 2065,1)]
# # inter_onu_f[ , sexo:='F']
# # inter_onu_f <- inter_onu_f[ , c('t', 'sexo', as.character(seq(0,100,1)))]
# # 
# # 
# # inter_onu_m[ , t:=seq(2015,2065,1)]
# # inter_onu_m[ , sexo:='M']
# # inter_onu_m <- inter_onu_m[ , c('t', 'sexo', as.character(seq(0,100,1)))]
# # 
# # inter_onu <- rbind( inter_onu_f, inter_onu_m)
# 
# # ##### Codigo Priscila ##########################################################
# file <- paste0( parametros$Data_seg, 'IESS_IVM_interpolacion_onu_pg.xlsx' )
# 
# inter_onu  <- read_excel( file,sheet="Hoja1"
#                            ,col_names=TRUE,guess_max = 24000)
# inter_onu  <- as.data.table( inter_onu  )
# colnames( inter_onu ) <- c('t', 'sexo', as.character(seq(0,100,1)) )
# # ################################################################################
# 
# onu_ecu_mort_din <- melt.data.table( inter_onu,
#                                      id.vars = c('t', 'sexo'),
#                                      value.name = "lx", variable.name = "x")
# 
# onu_ecu_mort_din[ , x := as.integer(as.character(x)) ]
# setorder( onu_ecu_mort_din, t, sexo, x )
# 
# # Cálculo de variaciones de la mortalidad ----------------------------------------------------------
# message( '\tCalculando probabilidad de muerte' )
# onu_ecu_mort_din[ , lxs := shift( lx, 1, fill = 0, type = 'lead' ), by = list( t, sexo ) ]
# onu_ecu_mort_din[ , px := lxs / lx ]
# onu_ecu_mort_din[ , qx := 1 - px ]
# onu_ecu_mort_din[ , ex_onu := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
# onu_ecu_mort_din[ , ex_onu := ex_onu / lx - 0.5 ]
# 
# message( '\tCalculando variación de la probabilidad de muerte' )
# setorder( onu_ecu_mort_din, sexo, x, t )
# onu_ecu_mort_din[ , qxs := shift( qx, 1, fill = 0, type = 'lead' ), by = list( sexo, x ) ]
# onu_ecu_mort_din[ , vx := qxs / qx ]
# onu_ecu_mort_din[ , qxs := NULL ]
# 
# onu_ecu_mort_din[ , exs := shift( ex_onu, 1, fill = 0, type = 'lead' ), by = list( sexo, x ) ]
# onu_ecu_mort_din[ , vex := exs / ex_onu ]
# onu_ecu_mort_din[ , exs := NULL ]
# 
# setorder( onu_ecu_mort_din, t, sexo, x )


#### Código ONU estándar #####################################################
load( paste0( parametros$RData, 'ONU_estandar_interpolado_qx_ecuador.RData' ) )
inter_onu <- copy( mod_std_ecu_onu )
inter_onu <- melt.data.table( inter_onu,
                              id.vars = c( 'sexo', 'x' ),
                              value.name = 'qx',
                              variable.name = 't' )
# inter_onu <- dcast.data.table( inter_onu, sexo + x ~ t, value.var = 'qx' )
onu_ecu_mort_din <- inter_onu[ x <= 105]

message( '\tCalculando variación de la probabilidad de muerte' )
setorder( onu_ecu_mort_din, sexo, x, t )
onu_ecu_mort_din[ , qxs := shift( qx, 1, fill = 0, type = 'lead' ), by = list( sexo, x ) ]
onu_ecu_mort_din[ , vx := qxs / qx ]
onu_ecu_mort_din[ , qxs := NULL ]
onu_ecu_mort_din[ , t:= as.numeric( as.character(t) )]

################################################################################

message( '\tGuardando resultados interpolados' )
save( onu_ecu_mort_din,
      file = paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019_v2.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
