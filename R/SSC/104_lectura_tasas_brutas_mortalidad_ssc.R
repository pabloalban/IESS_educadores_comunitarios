message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLeyendo insumos para el cálculo de tasas brutas de mortalidad del SSC')

# Mortalidad de jefes activos ----------------------------------------------------------------------
message( '\tMortalidad de jefes activos' )
file <- paste0( parametros$Data_seg, 'Input/Escenario_Base/Demographic/Transition Probabilities/',
                'IESS_SSC_matrices_qact.xlsx' )
col_nom <- c( 'anio', 'sexo', 'edad', 'n_afi', 'nada', 
              'anio_1', 'sexo_1', 'edad_1', 'n_afi_1', 'nada_1',
              'anio_mue', 'sexo_mue', 'edad_mue', 'n_afi_mue')

data <- read_excel( file,
                   sheet = "Edad",
                   col_names = TRUE,
                   guess_max = 24000 )

datos <- as.data.table( data )
setnames( datos, col_nom )

datos <- datos[ -c(1), -c('nada', 'nada_1')]
datos[ , anio:= as.numeric(anio)]
datos[ , edad:= as.numeric(edad)]
datos[ , n_afi:= as.numeric(n_afi)]
datos[ , anio_mue:= as.numeric(anio_mue)]
datos[ , edad_mue:= as.numeric(edad_mue)]
datos[ , n_afi_mue:= as.numeric(n_afi_mue)]

# Verificación de totales
#datos[ , list( n_afi= sum(n_afi, na.rm=T)), by=list(anio)]
#datos[ , list(  n_afi_mue= sum( n_afi_mue, na.rm=T)), by=list(anio_mue)]

edad_merge <- expand.grid( t = 2012:2020, sexo = c( 'H', 'M' ), edad = 0:120 )

exp_afi <- datos[ , c('anio', 'sexo', 'edad', 'n_afi')]
n_fal <- datos[ , c('anio_mue', 'sexo_mue', 'edad_mue', 'n_afi_mue')]

exp_afi <- merge( edad_merge,
                  exp_afi, 
                  by.x=c('t', 'sexo','edad'), by.y=c( 'anio', 'sexo','edad'), all.x= TRUE)
n_ir <- exp_afi

n_fal <- merge( edad_merge,
                n_fal, 
                by.x=c('t' ,'sexo', 'edad'), by.y=c( 'anio_mue', 'sexo_mue','edad_mue'), all.x= TRUE)

datos_comp <- data.table( cbind( exp_afi, n_fal ) )[, c('t', 'sexo', 'edad', 'n_afi', 'n_afi_mue')]

prob_mue_act <- datos_comp[ , list( N_exp = sum( n_afi, na.rm=T), 
                                    N_mue = sum( n_afi_mue, na.rm=T)), by=list(edad, sexo)]

prob_mue_act[ , qx_est:= N_mue/N_exp]
prob_mue_act[ sexo=='M', sexo:="F"]
prob_mue_act[ sexo=='H', sexo:="M"]

# Mortalidad de jefes inactivos ----------------------------------------------------------------------
message( '\tMortalidad de jefes inactivos' )
file <- paste0( parametros$Data_seg, 'Input/Escenario_Base/Demographic/Transition Probabilities/',
                'IESS_SSC_matrices_qinact.xlsx' )

data <- read_excel( file,
                    sheet = "Edad_Inactivos",
                    col_names = FALSE,
                    guess_max = 24000 )

inac <- as.data.frame( data )
anio <- as.character( 2012:2020 )
datos_inac <- NULL

for ( i in 1:9) { # i<- 1
  aux <- inac[ , c(which(inac[1,]==anio[i]): c(which(inac[1,]==anio[i])+2))][ -c(1,2),]
  aux[, "anio"] <- anio[i]
  colnames( aux ) <- c('sexo', 'edad', 'n_afi', 'anio')
  datos_inac <-rbind(  datos_inac, aux)
}
datos_inac <- as.data.table( datos_inac )
datos_inac[ , anio:= as.numeric(anio)]
datos_inac[ , edad:= as.numeric(edad)]
datos_inac[ , n_afi:= as.numeric(n_afi)]
datos_inac <- datos_inac[ sexo%in%c('H','M')]

data <- read_excel( file,
                   sheet = "Edad_Inactivos_Muertos",
                   col_names = FALSE,
                   guess_max = 24000,
                   na=""
                   )
inac_mue <- as.data.frame( data )
anio <- as.character( 2012:2020 )
datos_inac_mue <- NULL
for ( i in 1:9) { # i<- 1
  aux <- inac_mue[ , c(which(inac_mue[1,]==paste0( 'MUERTOS EN EL AÑO ',anio[i])): c(which(inac_mue[1,]==paste0( 'MUERTOS EN EL AÑO ',anio[i]))+2))][ -c(1,2),]
  aux[, "anio_mue"] <- anio[i]
  colnames( aux ) <- c('sexo_mue', 'edad_mue', 'n_afi_mue', 'anio_mue')
  datos_inac_mue <-rbind(  datos_inac_mue, aux)
}
datos_inac_mue <- as.data.table( datos_inac_mue )
datos_inac_mue[ , anio_mue:= as.numeric(anio_mue)]
datos_inac_mue[ , edad_mue:= as.numeric(edad_mue)]
datos_inac_mue[ , n_afi_mue:= as.numeric(n_afi_mue)]

edad_merge <- expand.grid( t = 2012:2020, sexo = c( 'H', 'M' ), edad = 0:110 )

exp_afi <- datos_inac[ sexo%in%c('H', 'M') , c('anio', 'sexo', 'edad', 'n_afi')]
n_fal <- datos_inac_mue[ sexo_mue%in%c('H', 'M'), c('anio_mue', 'sexo_mue', 'edad_mue', 'n_afi_mue')]

exp_afi <- merge( edad_merge,
                  exp_afi, 
                  by.x=c('t', 'sexo','edad'), by.y=c( 'anio', 'sexo','edad'), all.x= TRUE)
n_fal <- merge( edad_merge,
                n_fal, 
                by.x=c('t' ,'sexo', 'edad'), by.y=c( 'anio_mue', 'sexo_mue','edad_mue'), all.x= TRUE)

datos_comp <- data.table( cbind( exp_afi, n_fal ) )[, c('t', 'sexo', 'edad', 'n_afi', 'n_afi_mue')]

prob_mue_inac <- datos_comp[ , list( N_exp = sum( n_afi, na.rm=T), 
                                     N_mue = sum( n_afi_mue, na.rm=T)), by=list(edad, sexo)]
prob_mue_inac[ , qx_est:= N_mue/N_exp]
prob_mue_inac[ sexo=='M', sexo:="F"]
prob_mue_inac[ sexo=='H', sexo:="M"]

# Mortalidad de pensionistas -----------------------------------------------------------------------
message( '\tMortalidad de pensionistas' )
file <- paste0( parametros$Data_seg, 'Input/Escenario_Base/Demographic/Transition Probabilities/',
                'IESS_SSC_matrices_qben.xlsx' )
col_nom <- c( 'anio', 'tipo', 'sexo', 'edad', 'n_ben', 'nada', 
              'anio_mue', 'tipo_mue','sexo_mue', 'edad_mue', 'n_ben_mue', 'nada_mue',
              'anio_sal', 'tipo_sal', 'sexo_sal', 'edad_sal', 'n_ben_sal')

data_ben <- read_excel( file,
                       sheet = "Edad",
                       col_names = TRUE,
                       guess_max = 24000,
                       na="" )
datos <- as.data.table( data_ben )

posi_col <- which( colnames( datos) =='PENSIONISTAS DE TODO EL AÑO')
datos <- datos[ , posi_col:c(dim(datos)[2])]
setnames( datos, col_nom )
datos <- datos[ -c(1), -c('nada', 'nada_mue')]
datos[ , anio:= as.numeric(anio)]
datos[ , edad:= as.numeric(edad)]
datos[ , n_ben:= as.numeric(n_ben)]
datos[ , anio_mue:= as.numeric(anio_mue)]
datos[ , edad_mue:= as.numeric(edad_mue)]
datos[ , n_ben_mue:= as.numeric(n_ben_mue)]
datos[ , anio_sal:= as.numeric(anio_sal)]
datos[ , edad_sal:= as.numeric(edad_sal)]
datos[ , n_ben_sal:= as.numeric(n_ben_sal)]

#Verificacion de totales
#datos[ , list( n_ben= sum(n_ben, na.rm=T)), by=list(anio,tipo)]
#datos[ , list( n_ben_mue= sum(n_ben_mue, na.rm=T)), by=list( anio_mue,tipo_mue)]

edad_merge_ben <- expand.grid( t = 2012:2020, tipo = c('INVALIDEZ', 'VEJEZ' ),
                               sexo = c( 'H', 'M' ), edad = 0:120 )


exp_ben <- datos[ , c('anio', 'tipo','sexo', 'edad', 'n_ben')]
# exp_ben[ , list( n_ben= sum(n_ben, na.rm=T)), by=list(anio,tipo)]
n_ben_fal <- datos[ , c('anio_mue', 'tipo_mue','sexo_mue', 'edad_mue', 'n_ben_mue')]
# n_ben_fal[ , list( n_ben_mue= sum(n_ben_mue, na.rm=T)), by=list( anio_mue,tipo_mue)]
n_ben_sal <- datos[ , c('anio_sal', 'tipo_sal','sexo_sal', 'edad_sal', 'n_ben_sal')]

exp_ben <- merge( edad_merge_ben,
                  exp_ben, 
                  by.x=c('t', 'tipo' ,'sexo','edad'), by.y=c( 'anio', 'tipo','sexo','edad'), all.x= TRUE)
n_ben_fal <- merge( edad_merge_ben,
                    n_ben_fal, 
                    by.x=c('t','tipo' ,'sexo', 'edad'), by.y=c( 'anio_mue', 'tipo_mue','sexo_mue','edad_mue'), all.x= TRUE)

n_ben_sal <- merge( edad_merge_ben,
                    n_ben_sal, 
                    by.x=c('t','tipo' ,'sexo', 'edad'), by.y=c( 'anio_sal', 'tipo_sal','sexo_sal','edad_sal'), all.x= TRUE)

datos_comp_ben <- data.table( cbind( exp_ben, n_ben_fal, n_ben_sal ) )[ , c('t', 'tipo','sexo', 'edad', 'n_ben', 'n_ben_mue', 'n_ben_sal')]
prob_mue_ben <- datos_comp_ben[ , list( N_exp = sum( n_ben, na.rm=T), 
                                        N_mue = sum( n_ben_mue, na.rm=T),
                                        N_sal = sum( n_ben_sal, na.rm=T)), 
                                by=list(tipo,edad, sexo)]
prob_mue_ben[ , qx_est:= N_mue/N_exp]
prob_mue_ben[ , qx_est_sal:= N_sal/N_exp]
#plot( prob_mue_ben[ tipo=='INVALIDEZ']$qx_est )
prob_mue_ben[ sexo=='M', sexo:="F"]
prob_mue_ben[ sexo=='H', sexo:="M"]


lista <- c('prob_mue_act', 'prob_mue_inac','prob_mue_ben' )
# write.xlsx( prob_inv , 'prob_inv.xlsx' )

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_tasas_mortalidad_estimadas.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()