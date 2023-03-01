message( paste( rep('-', 100 ), collapse = '' ) )
# Código tomado del 303.R del SSC
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( "Y:/IESS_2020/RData/SSC/", 'IESS_SSC_input_proyeccion_aportes.RData' ) )

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
  for ( j in 1:40 ) { # j <-1 parametros$horizonte
    aux[ , j+1 ] <- sal[ , j+1 ]*( cent[, j+1] + csact[ , j+1 ] )
  }
  setnames( aux, c('x', seq( 2021, 2060 ) ) )
  aux <- as.data.table( aux )
  aux[ , sex:= sexo[k]]
  
  Masa_ILO <- rbind( Masa_ILO, aux )
  
}

Masa_ILO <- data.table( melt( Masa_ILO, id.vars = c('x','sex'), variable.name = c('anio'), value.name = c('masa') ))
Masa_ILO <- Masa_ILO[ , list( x, sexo = sex, anio, masa )]
Masa_ILO[ , t:= as.numeric( as.character(anio) ) ]
Masa_ILO[ , t := t - parametros$anio ]
Masa_ILO <- Masa_ILO[ , list( t, sexo, x, masa )]

Masa_ILO <- Masa_ILO[ , list( masa = sum( masa ) ), by = 't' ]

# Lectura masa salarial voluntario historico
file <- paste0(parametros$Data, 'RTR/IESS_RTR_masa_salarial_voluntario_historico.xlsx' )
masa_vol_his <-as.data.table( read_excel( file, sheet = 1 ) )
masa_vol_his[ , porcentaje := masa_voluntario / masa_sgo ]
# tasa <- round( mean( masa_vol_his$porcentaje ), 4 )
tasa <- round( masa_vol_his$porcentaje[5], 4 )

# Proyeccion masa salarial de voluntarios del SGO
masa_vol_proy <- Masa_ILO[ , list( t, masa = tasa * masa ) ]


save( list = c('Masa_ILO', 'masa_vol_proy' ),
      file = paste0( parametros$RData, 'RTR/IESS_RTR_masasal_sgo_voluntarios.RData' ) )

