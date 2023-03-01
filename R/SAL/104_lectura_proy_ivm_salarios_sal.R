message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo proyeccion salarios IVM por edad y sexo' )

# Se toma la Proyeccion Salarios del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_sal_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_sal_sgo_female_50_.xlsx' )
proy_mujeres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_mujeres <- as.data.table( proy_mujeres )
# Unificacion
n <- dim( proy_hombres )[2]

proy_hombres <- proy_hombres[ , 2:n ]
proy_hombres <- as.data.table( melt(proy_hombres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "sal" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "sal" ) )
proy_mujeres[ , sexo := 'F' ]

sal_proy <- rbind( proy_hombres, proy_mujeres )
sal_proy$t <- as.numeric( sal_proy$t )
setnames( sal_proy, '...2', 'x' )

# Masa salarial anual
sal_proy[ , sal := sal * 12 ]
sal_proy$x <- as.numeric( sal_proy$x )

sal_proy[ is.na( sal_proy ) ] <- 0

# Factor de correccion
fac <- data.table( t = c(1:10), f = c(0.90624047778,
                                      0.91470649656,
                                      0.91527817740,
                                      0.91535823694,
                                      0.91610949142,
                                      0.91730097365,
                                      0.91862664894,
                                      0.91961134156,
                                      0.92062356237,
                                      0.92173192635
                                      ) 
                   )

sal_proy <- merge( sal_proy,
               fac,
               by = c( 't' ), 
               all.x = TRUE
               )
sal_proy[ , sal := f * sal ]

# PROYECCION DE SALARIOS -------------------------------------------------------
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
Masa_ILO[ , t := t + 1 ]
#-------------------------------------------------------------------------------
lista <- c('sal_proy', 'Masa_ILO')
# ----
save( list = lista, 
      file = paste0( parametros$RData_seg, 'IESS_proyeccion_salarios_escenario_1_ivm.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
