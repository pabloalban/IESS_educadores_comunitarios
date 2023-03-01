message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tSelección del mejor modelo de vida estadar para Ecuador según la ONU' )

# Lectura lx para Ecuador según la ONU
load( paste0( parametros$RData, 'ONU_life_table_survivors_2019.RData' ) )

# # Caso de error mínimo entre familias y años -----------------------------------
# # Preparando información -------------------------------------------------------
# # Extrayendo solo la información del Ecuador
# onu_ecu_surv <- onu_survivors[ country_code == 218, list( period, sexo, x, lx ) ]
# onu_ecu_surv[ , t := as.numeric( substr( period, 1, 4 ) ) ]
# onu_ecu_surv <- onu_ecu_surv[ t>=2015]
# 
# onu_std_life <- copy( onu_estandar_life[ , c( 'Type', 'Sex', 'E0', 'age', 'lx1') ] )
# onu_std_life[ Sex=='Female', sexo:='F']
# onu_std_life[ Sex=='Male', sexo:='M']
# onu_std_life[ , lxstd:= lx1]
# onu_std_life[ , x:= age]
# onu_std_life[ , Sex := NULL]
# onu_std_life[ , lx1 := NULL]
# onu_std_life[ , age := NULL]
# onu_std_life <- onu_std_life[ x%in%unique( onu_ecu_surv$x)]
# 
# aux <- expand.grid( t = unique(onu_ecu_surv$t), sexo = c('F', 'M'),
#                     Type = unique( onu_std_life$Type), x = unique(onu_std_life$x)    )
# aux <- as.data.table( aux )
# comp_lx <- merge( aux, onu_ecu_surv, by= c('t','sexo', 'x') )
# 
# n <- length(unique(onu_ecu_surv$t))
# 
# onu_std_life <- do.call( "rbind", replicate( n, onu_std_life, simplify = FALSE))
# onu_std_life[ , t:= sort(rep( unique(onu_ecu_surv$t), dim(onu_std_life)[1]/n  )) ]
# 
# onu_ecu_std <- merge.data.table( onu_std_life, comp_lx , by = c('Type', 't', 'sexo', 'x') )
# setorder( onu_ecu_std, Type, t, sexo, E0)
# onu_ecu_std[ , dif:= ( lxstd - lx )^2 ]
# onu_ecu_std[ , p_dif1:= (lx/lxstd - 1) ]
# onu_ecu_std[ , p_dif2:= ( lxstd/lx - 1) ]
# 
# onu_ecu_std[ , se:= sqrt( sum( dif, na.rm =T) ) , by = list( Type, t, sexo, E0 ) ]
# # onu_ecu_std[ , mean_p_dif1:= mean( p_dif1, na.rm=T ) , by = list( t, sexo, E0 ) ]
# # onu_ecu_std[ , mean_p_dif2:= mean( p_dif2, na.rm=T ) , by = list( t, sexo, E0 ) ]
# # onu_ecu_std[ , max_p_dif1:= max( p_dif1, na.rm=T ) , by = list( t, sexo, E0 ) ]
# # onu_ecu_std[ , max_p_dif2:= max( p_dif2, na.rm=T ) , by = list( t, sexo, E0 ) ]
# 
# 
# onu_ecu_std[ , minse:= min( se ) , by = list( t, sexo ) ]
# # onu_ecu_std[ , min_mean_p_dif1:= min( mean_p_dif1, na.rm=T ) , by = list( t, sexo ) ]
# # onu_ecu_std[ , min_mean_p_dif2:= min( mean_p_dif2,na.rm =T ) , by = list( t, sexo ) ]
# # onu_ecu_std[ , min_max_p_dif1:= min( max_p_dif1,na.rm =T ) , by = list( t, sexo ) ]
# # onu_ecu_std[ , min_max_p_dif2:= min( max_p_dif2,na.rm =T ) , by = list( t, sexo ) ]
# setorder( onu_ecu_std, t, sexo)
# 
# # choose_opt <- onu_ecu_std[ min_max_p_dif2 == max_p_dif2 ]
# choose_opt <- onu_ecu_std[ se==minse]
# 
# choose_opt <- choose_opt[ , list( Type = unique(Type)), by=list( t, sexo, E0) ]
# setorder( choose_opt, sexo, t)
# # write.xlsx( choose_opt , 'choose_opt.xlsx' )
# 
# aux <- copy( onu_estandar_life)
# aux[ Sex=='Female', sexo:='F']
# aux[ Sex=='Male', sexo:='M']
# aux[ , lx:= lx1]
# aux[ , x:= age]
# aux[ , Sex := NULL]
# aux[ , lx1 := NULL]
# aux[ , age := NULL]
# 
# llenado_2 <- NULL
# for ( j in 1:dim(choose_opt)[1]) {
#     e <- choose_opt[ j , ]$E0
#     tp <- choose_opt[ j , ]$Type
#     sx <- choose_opt[ j , ]$sexo
#     ti <- choose_opt[ j , ]$t
#     res <- aux[ Type== tp & E0==e & sexo==sx]
#     res[ , t:= ti]
#     llenado_2 <- rbind( llenado_2, res)
# }
# # write.xlsx(  llenado_2 , 'modelo1.xlsx' )

# Caso de error mínimo entre familias ------ -----------------------------------
# Preparando información -------------------------------------------------------
# Extrayendo solo la información del Ecuador
onu_ecu_surv <- onu_survivors[ country_code == 218, list( period, sexo, x, lx ) ]
onu_ecu_surv[ , t := as.numeric( substr( period, 1, 4 ) ) ]
onu_ecu_surv <- onu_ecu_surv[ t>=2015]

onu_ecu_ex <- copy( onu_ex[ country_code == 218, list( t, sexo, ex ) ] )
onu_ecu_ex[ , t := as.numeric( substr( t, 1, 4 ) ) ]
onu_ecu_ex <- onu_ecu_ex[ t>=2015]
setorder( onu_ecu_ex, sexo)
onu_ecu_ex[ , ex_int:= round( ex , 0 )]

onu_std_life <- copy( onu_estandar_life[ , c( 'Type', 'Sex', 'E0', 'age', 'lx1') ] )
onu_std_life[ Sex=='Female', sexo:='F']
onu_std_life[ Sex=='Male', sexo:='M']
onu_std_life[ , lxstd:= lx1]
onu_std_life[ , x:= age]
onu_std_life[ , ex2 := rev( cumsum( rev( lxstd ) ) ), by = list( Type, E0, sexo ) ]
onu_std_life[ , ex2 := ex2 / lxstd ]
onu_std_life[ x==0, e02 := ex2, by=list(Type, E0, sexo) ]
onu_std_life[ , Sex := NULL]
onu_std_life[ , lx1 := NULL]
onu_std_life[ , age := NULL]
onu_std_life <- onu_std_life[ x%in%unique( onu_ecu_surv$x)]

aux <- expand.grid( t = unique(onu_ecu_surv$t), sexo = c('F', 'M'),
                    Type = unique( onu_std_life$Type), x = unique(onu_std_life$x)    )
aux <- as.data.table( aux )
comp_lx <- merge( aux, onu_ecu_surv, by= c('t','sexo', 'x') )

n <- length(unique(onu_ecu_surv$t))

onu_std_life <- do.call( "rbind", replicate( n, onu_std_life, simplify = FALSE))
onu_std_life[ , t:= sort(rep( unique(onu_ecu_surv$t), dim(onu_std_life)[1]/n  )) ]

onu_ecu_std <- merge.data.table( onu_std_life, comp_lx , by = c('Type', 't', 'sexo', 'x') )
setorder( onu_ecu_std, Type, t, sexo, E0)
onu_ecu_std[ , dif:= ( lxstd - lx )^2 ]

onu_ecu_std[ , se:= sqrt( sum( dif, na.rm =T) ) , by = list( Type, sexo ) ]

onu_ecu_std[ , minse:= min( se ) , by = list( sexo ) ]
setorder( onu_ecu_std, t, sexo)

choose_opt <- onu_ecu_std[ se==minse]

choose_opt <- choose_opt[ !is.na(e02), list( Type = unique(Type), e02), by=list( t, sexo, E0) ]
setorder( choose_opt, sexo, t)

choose_opt <- merge.data.table(choose_opt,
                               onu_ecu_ex[ , list(t, sexo, e0_onu=ex)],
                               by.x=c('t', 'sexo'), by.y=c('t', 'sexo'), all.x=T)

choose_opt[ , dif_ex:= abs( e0_onu - e02 ) ]
choose_opt[ , min_dif_ex:= min(dif_ex),  by=list( t, sexo)]

choose_opt <- choose_opt[ dif_ex == min_dif_ex ]
setorder(choose_opt , Type, t, sexo)

aux <- copy( onu_estandar_life)
aux[ Sex=='Female', sexo:='F']
aux[ Sex=='Male', sexo:='M']
aux[ , lx:= lx1]
aux[ , x:= age]
aux[ , Sex := NULL]
aux[ , lx1 := NULL]
aux[ , age := NULL]

llenado_2 <- NULL
for ( j in 1:dim(choose_opt)[1]) {
    e <- choose_opt[ j , ]$E0
    tp <- choose_opt[ j , ]$Type
    sx <- choose_opt[ j , ]$sexo
    ti <- choose_opt[ j , ]$t
    res <- aux[ Type== tp & E0==e & sexo==sx]
    res[ , t:= ti]
    llenado_2 <- rbind( llenado_2, res)
}
# write.xlsx(  llenado_2 , 'modelo2.xlsx' )

modelo_ecu <- dcast( llenado_2, sexo + x ~ t, value.var = 'qx1', fun.aggregate = sum )
modelo_ecu_std <-  modelo_ecu[ , -c( 1, 2 ) ] 

inter_anio <- function( data, anio_ini, anio_fin ){
  res <- data.frame( ix = 1:dim(data)[1])
  nm <- as.character( seq( anio_ini, anio_fin, 1 ) )
  s1 <- c(4,3,2,1)
  s2 <- c(1,2,3,4)
  for ( j in 1: (dim(data)[2]-1) ){ 
    rel <- data.frame( ix = 1:dim(data)[1])
  for ( i in 1:4) {
    aux <- NULL
    aux <- data.frame( exp( ( s1[i]*log(data[ , j ] ) + s2[i]*log(data[ , j+1 ] ) )/ 5 ) )
    rel <- cbind( rel, aux )
  }
  rel <- cbind( data[ , j ], rel[ , -1] )
  posi <- which( nm == colnames(data)[j] )
  colnames( rel ) <- nm[ posi: c(posi + 4) ]
  res <- cbind( res, rel )
  }
  res <- cbind( res, data[ , j+1 ] )
  colnames( res )[ dim(res)[2] ] <- nm[length( nm )]
  return( as.data.table( res[ , -1] ) )
}

anio_ini <- 2015
anio_fin <- 2095

mod_std_ecu_onu <- inter_anio( modelo_ecu_std, anio_ini, anio_fin )
mod_std_ecu_onu[ , sexo:= modelo_ecu$sexo ]
mod_std_ecu_onu[ , x:= modelo_ecu$x ]
orden <- c( 'sexo', 'x', 
            c(colnames( mod_std_ecu_onu)[ colnames( mod_std_ecu_onu) !='sexo' & colnames( mod_std_ecu_onu) !='x']) )
mod_std_ecu_onu <- as.data.frame(mod_std_ecu_onu)
mod_std_ecu_onu <- as.data.table( mod_std_ecu_onu[ , c(orden)  ] )

save( mod_std_ecu_onu,
      file = paste0( parametros$RData, 'ONU_estandar_interpolado_qx_ecuador.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

