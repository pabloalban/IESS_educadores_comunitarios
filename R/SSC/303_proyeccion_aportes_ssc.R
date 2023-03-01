message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_input_proyeccion_aportes.RData' ) )

# Cálculo de la proyección del  Aporte del 0,3% del Estado -----------------------------------------
message( '\tCálculo de la proyección del  Aporte del 0,3% del Estado' )

densi_dep <- merge( data.table( expand.grid( sexo = c('H', 'M'), x = 0:120 ) ),
                masa_sgo[, list(sexo, edad,masa_sgo)],
                by.x=c('sexo','x'), by.y=c('sexo', 'edad'), all.x=TRUE )
densi_dep <- merge( densi_dep,
                masa_dep[ ,list(sexo, edad, masa_dep)],
                by.x=c('sexo','x'), by.y=c('sexo', 'edad'), all.x=TRUE)

densi_dep[ , var:= masa_dep/masa_sgo]
densi_dep[ sexo=='H', sexo:='Male']
densi_dep[ sexo=='M', sexo:='Female']

# Lectura de la Masa Salarial de los resultados del Seguro IVM -------------------------------------
setwd(paste0( parametros$Data, 'IVM/OUTPUT/ILO_OUT_ALL' ))
arch <- list.files()

sexo <- c('Male', 'Female')
Masa_ILO <- NULL
for( k in 1:2) { # k <- 1
  
csact <- paste0(parametros$Data, 'IVM/OUTPUT/ILO_OUT_ALL/', 
               arch[match( TRUE, c(str_detect( arch, "csact")==T & str_detect( arch, sexo[k])==T &
                                     str_detect( arch, ".csv")==T))] )
csact <- as.data.table( read.csv( file = csact, header = FALSE, sep = ",", dec = ".") )[ -c(1,2), -c(1)]
col_names <- c('x', as.character(seq(2021,2060)))
setnames( csact, col_names )
csact <- merge.data.frame( data.table( x = 15:70), csact, by = c('x'), all.x = TRUE )
csact[ is.na( csact ) ] <- 0

cent <- paste0(parametros$Data, 'IVM/OUTPUT/ILO_OUT_ALL/', 
                  arch[match( TRUE, c(str_detect( arch, "cent")==T & str_detect( arch, sexo[k])==T &
                                        str_detect( arch, ".csv")==T))] )
cent <- as.data.table( read.csv( file = cent, header = FALSE, sep = ",", dec = ".") )[ -c(1,2), -1]
col_names <- c('x', as.character(seq(2021,2060)))
setnames( cent, col_names )
cent <- merge.data.frame( data.table( x = 15:70), cent, by = c('x'), all.x = TRUE )
cent[ is.na( cent ) ] <- 0

sal <- paste0(parametros$Data, 'IVM/OUTPUT/ILO_OUT_ALL/', 
               arch[match( TRUE, c(str_detect( arch, ",sal,")==T & str_detect( arch, sexo[k])==T &
                                     str_detect( arch, ".csv")==T))] )
sal <- as.data.table( read.csv( file = sal, header = FALSE, sep = ",", dec = ".") )[ -c(1,2), -1]
col_names <- c('x', as.character(seq(2021,2060)))
setnames( sal, col_names )
sal <- merge.data.frame( data.table( x = 15:70), sal, by = c('x'), all.x = TRUE )
sal[ is.na( sal ) ] <- 0

aux <- data.frame( x = 15:70, matrix( 0, 56, 20 ) )
for ( j in 1:parametros$horizonte ) { # j <-1
     aux[ , j+1 ] <- sal[ , j+1 ]*( cent[, j+1] + csact[ , j+1 ] )
    }
setnames( aux, c('x', seq( 2021, 2040 ) ) )
aux <- as.data.table( aux )
aux[ , sex:= sexo[k]]

Masa_ILO <- rbind( Masa_ILO, aux )

}

Masa_ILO <- data.table( melt( Masa_ILO, id.vars = c('x','sex'), variable.name = c('anio'), value.name = c('masa') ))
Masa_ILO <- Masa_ILO[ , list( x, sexo = sex, anio, masa )]
Masa_ILO[ , t:= as.numeric( as.character(anio) ) ]
Masa_ILO[ , t := t - parametros$anio ]
Masa_ILO <- Masa_ILO[ , list( t, sexo, x, masa )]

Masa_sgo_dep <- merge( Masa_ILO, densi_dep, by.x=c('sexo', 'x'), by.y=c('sexo', 'x'), all.x=TRUE)
Masa_sgo_dep[ , proy_masa_dep := var * masa ]
masa_proy <- Masa_sgo_dep[ , list(t, sexo, x, M = masa, MD = proy_masa_dep )]


# Lectura de los aportes de los jefes de familia del Modelo ILO/PENSIONS ---------------------------
message( '\tLectura de los aportes de los jefes de familia del Modelo ILO/PENSIONS' )
setwd( paste0( parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/') )
arch <- list.files()

sexo <- c('Male', 'Female')
Masa_ILO_SSC <- NULL

for( k in 1:2) { # k <- 1
  
  csact <- paste0(parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/', 
                  arch[match( TRUE, c(str_detect( arch, "csact")==T & str_detect( arch, sexo[k])==T &
                                        str_detect( arch, ".csv")==T))] )
  csact <- as.data.table( read.csv( file = csact, header = FALSE, sep = ",", dec = ".") )[ -c(1,2), -c(1)]
  col_names <- c('x', as.character(seq(2021,2040)))
  setnames( csact, col_names )
  csact <- merge.data.frame( data.table( x = 15:80), csact, by = c('x'), all.x = TRUE )
  csact[ is.na( csact ) ] <- 0
  
  cent <- paste0(parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/', 
                 arch[match( TRUE, c(str_detect( arch, "cent")==T & str_detect( arch, sexo[k])==T &
                                       str_detect( arch, ".csv")==T))] )
  cent <- as.data.table( read.csv( file = cent, header = FALSE, sep = ",", dec = ".") )[ -c(1,2), -1]
  col_names <- c('x', as.character(seq(2021,2040)))
  setnames( cent, col_names )
  cent <- merge.data.frame( data.table( x = 15:80), cent, by = c('x'), all.x = TRUE )
  cent[ is.na( cent ) ] <- 0
  
  sal <- paste0(parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/', 
                arch[match( TRUE, c(str_detect( arch, ",sal,")==T & str_detect( arch, sexo[k])==T &
                                      str_detect( arch, ".csv")==T))] )
  sal <- as.data.table( read.csv( file = sal, header = FALSE, sep = ",", dec = ".") )[ -c(1,2), -1]
  col_names <- c('x', as.character(seq(2021,2040)))
  setnames( sal, col_names )
  sal <- merge.data.frame( data.table( x = 15:80), sal, by = c('x'), all.x = TRUE )
  sal[ is.na( sal ) ] <- 0
  
  aux <- data.frame( x = 15:80, matrix( 0, 66, 20 ) )

  for (j in 1:parametros$horizonte ) { # j <- 1
    aux[ , j+1 ] <- sal[ , j+1 ]*( cent[, j+1] + csact[ , j+1 ] )
  }
  setnames( aux, c('x', seq( 2021, 2040 ) ) )
  aux <- as.data.table( aux )
  aux[ , sex:= sexo[k]]
  
  Masa_ILO_SSC <- rbind( Masa_ILO_SSC, aux )
  
}

Masa_ILO_SSC <- data.table( melt( Masa_ILO_SSC, id.vars = c('x','sex'), variable.name = c('anio'), value.name = c('MS') ))
Masa_ILO_SSC <- Masa_ILO_SSC[ , list( x, sexo = sex, anio, MS )]
Masa_ILO_SSC[ , t:= as.numeric( as.character(anio) ) ]
Masa_ILO_SSC[ , t :=  t - parametros$anio]
Masa_ILO_SSC <- Masa_ILO_SSC[ , list( t, sexo, x, MS )]

aportes <- data.table( expand.grid( t = 1:parametros$horizonte,
                                    x = 0:105, sexo=c('Male', 'Female') ) )
aportes <- merge( aportes, masa_proy[ , list( t = t , sexo, x, M, MD )],
                  by.x=c('t', 'sexo', 'x'), by.y=c('t', 'sexo', 'x'), all.x=TRUE)

aportes <- merge( aportes, Masa_ILO_SSC[ , list( t, sexo, x , MS)],  
                  by.x=c('t', 'sexo', 'x'), by.y=c('t', 'sexo', 'x'), all.x=TRUE)

# Carga de datos --------------------------------------------------------------- -------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_contexto_economico.RData' ) )

message( '\tCálculo de la proyección del  Aporte ISSPOL' )

aux <- copy( aporte_issfa_isspol[ , list(anio, isspol )] )
aum <- sum( aux$isspol[ 33:44 ] ) - sum( aux$isspol[ 21:32 ] )
dif <- ( sum( aux$isspol[ 33:44 ] ) - sum( aux$isspol[ 21:32 ] ) + sum( aux$isspol[ 9:20 ] ) - sum( aux$isspol[ 21:32 ] ) ) / ( - sum( aux$isspol[ 9:20 ] ) + sum( aux$isspol[ 21:32 ] ) )

isspol_proy <- data.table()
isspol_proy[, t:= c(0:40) ]
isspol_proy[, aporte_isspol_anio := 0 ]
isspol_proy[ 1, 2] <- sum( aux$isspol[ 33:44 ] )

for (i in 1:40 ){
  isspol_proy[ i+1, 2] <- isspol_proy[ i, 2 ] + aum
  if( aum > 0 ){
    aum <- aum * ( 1 + dif )
  }
  else{
    aum <- 0
  }
}
isspol_proy <- isspol_proy[ t > 0]


aportes <- merge( aportes, isspol_proy[ , list( t, A_isspol = aporte_isspol_anio/( 2*106) ) ],  
                  by.x=c('t'), by.y=c('t'), all.x=TRUE)

message( '\tCálculo de la proyección del Aporte SEGUROS PRIVADOS' )
aux <- copy( aporte_sp )
aum <- mean( aux$aporte )

segu_proy <- data.table()
segu_proy[, t := c(0:40) ]
segu_proy[, aporte_seg_anio := 0 ]
segu_proy[ 1, 2 ] <- sum( aux$aporte[ 11:2 ] )

for (i in 1:40 ){
  segu_proy[ i+1, 2] <- aum
}
segu_proy <- segu_proy[ t > 0 ]

aportes <- merge( aportes, segu_proy[ , list( t, A_sp = aporte_seg_anio/( 2*106) ) ],  
                  by.x=c('t'), by.y=c('t'), all.x=TRUE)

message( '\tCálculo de la proyección del Aporte ISSFA' )
aux <- copy( issfa_proy )

aportes <- merge( aportes, aux[ , list( t = anio - parametros$anio, A_issfa_RT = RT/( 2*106) * 1000, 
                                        A_issfa_NS = NS/( 2*106) * 1000,
                                        A_issfa_Total = Total/( 2*106) * 1000 ) ],  
                  by.x=c('t'), by.y=c('t'), all.x=TRUE)

lista <- c('masa_proy', 'Masa_ILO_SSC', 'aportes' )


save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_proyeccion_aportes.RData' ) )

setwd( parametros$work_dir )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()