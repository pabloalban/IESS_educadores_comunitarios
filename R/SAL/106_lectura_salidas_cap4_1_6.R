message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población asegurada' )

file <- paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 'IESS_IVM_poblacion_asegurada.xlsx' )


# Grafico 1 ---------------------------------------------------------------

col_nom <- c('Des','Descp',	'Total', 'Por')

prop_afi_act_inac <-read_excel(file,sheet="G 3.1"
                        ,col_names=TRUE,guess_max = 24000)
prop_afi_act_inac <- as.data.table( prop_afi_act_inac )[-5,]
setnames( prop_afi_act_inac, col_nom )


# Grafico 2 ---------------------------------------------------------------

col_nom <- c( 'edad', 'num', 'num_neg', 'nada', 'cot', 'num_cot')

cotizantes <-read_excel(file,sheet="G 3.2"
                        ,col_names=TRUE,guess_max = 24000)
cotizantes <- as.data.table( cotizantes )
setnames( cotizantes, col_nom )


# Grafico 3 ---------------------------------------------------------------

col_nom <- c('edad', 'num', 'num_neg', 'nada', 'cot', 'num_cot')

activ_cotiz_fem <-read_excel(file,sheet="G 3.3"
                               ,col_names=TRUE,guess_max = 24000)
activ_cotiz_fem <- as.data.table( activ_cotiz_fem)
setnames( activ_cotiz_fem, col_nom )


# Grafico 4 ---------------------------------------------------------------

col_nom <- c('edad', 'num', 'num_neg', 'nada', 'cot', 'num_cot')

inac_cotiz_masc <-read_excel(file,sheet="G 3.4"
                        ,col_names=TRUE,guess_max = 24000)
inac_cotiz_masc <- as.data.table( inac_cotiz_masc)
setnames( inac_cotiz_masc, col_nom )


# Grafico 5 ----------------------------------------------------------------

col_nom <- c('edad', 'num', 'num_neg', 'nada', 'cot', 'num_cot')

inac_cotiz_fem <-read_excel(file,sheet="G 3.5"
                        ,col_names=TRUE,guess_max = 24000)
inac_cotiz_fem <- as.data.table( inac_cotiz_fem)
setnames( inac_cotiz_fem, col_nom )


# Grafico 6 ---------------------------------------------------------------

col_nom <- c( 'edad', 'h_num', 'h_por', 'm_num', 'm_por')
entradas_sis <-read_excel(file,sheet="G 3.6"
                          ,col_names=TRUE,guess_max = 24000)
entradas_sis <- as.data.table( entradas_sis )[-1,]
setnames( entradas_sis, col_nom )


# Grafico 7 ---------------------------------------------------------------

col_nom <- c( 'sexo_h', 'edad_h', 'male', 'sexo_m', 'edad_m', 'female')

salarios <-read_excel(file,sheet="G 3.7"
                      ,col_names=TRUE,guess_max = 24000)
salarios <- as.data.table( salarios )
setnames( salarios, col_nom )

# Grafico 8 --------------------------------------------------------------

col_nom <- c( 'nada1', 'nada2', 'nada3', 'nada4', 'nada5', 'tipo_pens', 'porcentaje')

tipo_pensi_sexo <-read_excel(file,sheet="G 3.8"
                      ,col_names=TRUE,guess_max = 24000)
tipo_pensi_sexo <- as.data.table( tipo_pensi_sexo)
setnames( tipo_pensi_sexo, col_nom )


# Tabla 3.1 ---------------------------------------------------------------

col_nom <- c( 'pension', 'masculino', 'femenino', 'total')

edad_promedio <-read_excel(file,sheet="C 3.1"
                             ,col_names=TRUE,guess_max = 24000)
edad_promedio <- as.data.table( edad_promedio)
setnames( edad_promedio, col_nom )


# Grafico 9.1 ---------------------------------------------------------------

col_nom <- c( 'edad', 'num_vejz', 'num_inval', 'num_viud', 'num_orfan')

pensio_edad_sexo_h <-read_excel(file,sheet="G 3.9 H"
                           ,col_names=TRUE,guess_max = 24000)
pensio_edad_sexo_h <- as.data.table( pensio_edad_sexo_h)
setnames( pensio_edad_sexo_h, col_nom )

# Grafico 9.2 ---------------------------------------------------------------

col_nom <- c( 'edad', 'num_vejz', 'num_inval', 'num_viud', 'num_orfan')

pensio_edad_sexo_m <-read_excel(file,sheet="G 3.9 M"
                              ,col_names=TRUE,guess_max = 24000)
pensio_edad_sexo_m <- as.data.table( pensio_edad_sexo_m)
setnames( pensio_edad_sexo_m, col_nom )


# Grafico 10 --------------------------------------------------------------

col_nom <- c( 'nada1', 'nada2', 'nada3', 'nada4', 'nada5', 'tipo_pens', 'porcentaje')

benef_pensi_sexo <-read_excel(file,sheet="G 3.10"
                             ,col_names=TRUE,guess_max = 24000)
benef_pensi_sexo <- as.data.table( benef_pensi_sexo)
setnames( benef_pensi_sexo, col_nom )


# Grafico 11.1 ------------------------------------------------------------

col_nom <- c( 'edad', 'mascu_vejz', 'femen_vejz', 'mascu_inv', 'femen_inv')

val_prom_benef_sex_1 <-read_excel(file,sheet="G 3.11 Parte 1"
                                ,col_names=TRUE,guess_max = 24000)
val_prom_benef_sex_1   <- as.data.table( val_prom_benef_sex_1 )
setnames( val_prom_benef_sex_1 , col_nom )

# Grafico 11.2 ------------------------------------------------------------

col_nom <- c( 'edad', 'mascu_viud', 'femen_viud', 'mascu_orfan', 'femen_orfan')

val_prom_benef_sex_2 <-read_excel(file,sheet="G 3.11 Parte 2"
                                ,col_names=TRUE,guess_max = 24000)
val_prom_benef_sex_2  <- as.data.table( val_prom_benef_sex_2)
setnames( val_prom_benef_sex_2 , col_nom )


# Grafico 12 --------------------------------------------------------------

col_nom <- c( 'mascu_vejez', 'mascu_inval', 'mascu_viudez', 'mascu_orfand', 'nada', 'fem_vejez', 'fem_inval', 'fem_viudez', 'fem_orfand')

val_prom_benef__tipo_sex <-read_excel(file,sheet="G 3.12"
                                ,col_names=TRUE,guess_max = 24000)
val_prom_benef__tipo_sex  <- as.data.table( val_prom_benef__tipo_sex )
setnames( val_prom_benef__tipo_sex , col_nom )


# Graficos pendientes  -------------------------------------------------

#3.2 (burbujas)

col_nom <- c( 'sexo', 'edad', 'impos_años', 'imposiciones', 'n_afiliados', 'riesgo1', 'riesgo3', 'nada', 'nada', 'nada', 'nada')

afi_cotiz_riesgo_mascul <-read_excel(file,sheet="3.2M"
                                     ,col_names=TRUE,guess_max = 24000)
afi_cotiz_riesgo_mascul  <- as.data.table(afi_cotiz_riesgo_mascul)
setnames(afi_cotiz_riesgo_mascul , col_nom )


#3.2 (pastel)

col_nom <- c( 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'riesgos', 'nada', 'porcentaje')

pastel_afi_cotiz_riesgo_mascul <-read_excel(file,sheet="3.2M"
                                            ,col_names=TRUE,guess_max = 24000)
pastel_afi_cotiz_riesgo_mascul  <- as.data.table(pastel_afi_cotiz_riesgo_mascul)
setnames(pastel_afi_cotiz_riesgo_mascul, col_nom )

#-----------------------------------------------------------------------------

#3.3 (burbujas)

col_nom <- c( 'sexo', 'edad', 'impos_años', 'imposiciones', 'n_afiliados', 'riesgo1', 'riesgo3', 'nada', 'nada', 'nada', 'nada', 'nada')

distri_activ_cotiz_riesgo_femenin <-read_excel(file,sheet="3.3F"
                                     ,col_names=TRUE,guess_max = 24000)
distri_activ_cotiz_riesgo_femenin  <- as.data.table(distri_activ_cotiz_riesgo_femenin)
setnames(distri_activ_cotiz_riesgo_femenin , col_nom )


#3.3 (pastel)

col_nom <- c( 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'riesgos', 'nada', 'porcentaje', 'nada')

pastel_distri_activ_cotiz_riesgo_femenin <-read_excel(file,sheet="3.3F"
                                            ,col_names=TRUE,guess_max = 24000)
pastel_distri_activ_cotiz_riesgo_femenin  <- as.data.table(pastel_distri_activ_cotiz_riesgo_femenin)
setnames(pastel_distri_activ_cotiz_riesgo_femenin, col_nom )

#--------------------------------------------------------------------------

#3.4 (burbujas)

col_nom <- c( 'sexo', 'edad', 'impos_años', 'imposiciones', 'n_afiliados', 'riesgo1', 'riesgo3', 'nada', 'nada', 'nada', 'nada','nada')

distri_inact_cotiz_riesgo_mascul <-read_excel(file,sheet="3.4M"
                                      ,col_names=TRUE,guess_max = 24000)
distri_inact_cotiz_riesgo_mascul  <- as.data.table(distri_inact_cotiz_riesgo_mascul)
setnames(distri_inact_cotiz_riesgo_mascul , col_nom )


#3.4 (pastel)

col_nom <- c( 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'riesgos', 'nada', 'porcentaje', 'nada')

pastel_distri_inact_cotiz_riesgo_mascul <-read_excel(file,sheet="3.4M"
                                             ,col_names=TRUE,guess_max = 24000)
pastel_distri_inact_cotiz_riesgo_mascul  <- as.data.table(pastel_distri_inact_cotiz_riesgo_mascul)
setnames(pastel_distri_inact_cotiz_riesgo_mascul, col_nom )

#------------------------------------------------------------------------------------

#3.5 (burbujas)

col_nom <- c( 'sexo', 'edad', 'impos_años', 'imposiciones', 'n_afiliados', 'riesgo1', 'riesgo3', 'nada', 'nada', 'nada', 'nada')

distri_inact_cotiz_riesgo_femen <-read_excel(file,sheet="3.5F"
                                              ,col_names=TRUE,guess_max = 24000)
distri_inact_cotiz_riesgo_femen  <- as.data.table(distri_inact_cotiz_riesgo_femen)
setnames(distri_inact_cotiz_riesgo_femen , col_nom )


#3.5 (pastel)

col_nom <- c( 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'nada', 'riesgos', 'nada', 'porcentaje', 'nada')

pastel_distri_inact_cotiz_riesgo_femen <-read_excel(file,sheet="3.5F"
                                                     ,col_names=TRUE,guess_max = 24000)
pastel_distri_inact_cotiz_riesgo_femen  <- as.data.table(pastel_distri_inact_cotiz_riesgo_femen)
setnames(pastel_distri_inact_cotiz_riesgo_femen, col_nom )

# CARGA SALIDAS ----------------------------------------------------------------
# COBERTURA Y CARGA  Y PENSIONISTAS --------------------------------------------
file <- paste0( 'Y:/IESS_2020/Data/IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_rpt_mdat_sgo_male_50_.xlsx' )
tasa_cob <- read_excel( file, sheet="RPT_MDAT"
                        ,col_names = FALSE
                        ,guess_max = 24000
                        ,skip = 8 )

tasa_cob <- as.data.table( tasa_cob )
tasa_cob_female <- as.data.table( tasa_cob[ 1:40, ] )
cp_female <- tasa_cob_female[ , list(`...1`, `...8`) ]
jv_female <- tasa_cob_female[ , list(`...1`, `...4`) ]
tasa_cob_female <- tasa_cob_female[ , 1:3 ]

tasa_cob_male <- as.data.table( tasa_cob[ 43:dim(tasa_cob)[1], ] )
cp_male <- tasa_cob_male[ ,  list(`...1`, `...8`) ]
jv_male <- tasa_cob_male[ , list(`...1`, `...4`) ]
tasa_cob_male <- tasa_cob_male[ , 1:3 ]

col_names <- c('year', 'employedLF', 'ActiContri' )

setnames( tasa_cob_female, col_names )
setnames( tasa_cob_male, col_names )

col_names <- c('year', 'pensionistas' )

setnames( cp_female, col_names )
setnames( cp_male, col_names )

col_names <- c('year', 'jv' )

setnames( jv_female, col_names )
setnames( jv_male, col_names )

tasa_cob <- data.table( year = tasa_cob_female$year
                        , tcv = ( tasa_cob_female$ActiContri + tasa_cob_male$ActiContri ) / ( tasa_cob_female$employedLF + tasa_cob_male$employedLF )
                        , cap_female = (tasa_cob_female$ActiContri / cp_female$pensionistas )
                        , cap_male = (tasa_cob_male$ActiContri / cp_male$pensionistas )
                        , cap = (tasa_cob_female$ActiContri + tasa_cob_male$ActiContri)/( cp_female$pensionistas + cp_male$pensionistas ) 
                        , activos_f_m = tasa_cob_female$ActiContri + tasa_cob_male$ActiContri
                        , fuerza_la_f_m = tasa_cob_female$employedLF + tasa_cob_male$employedLF
                        , jv_female = jv_female$jv
                        , jv_male = jv_male$jv
                        , jv_f_m = jv_female$jv + jv_male$jv
)

rm( tasa_cob_female, tasa_cob_male, cp_female, cp_male )

# ACTIVOS E INACTIVOS ----------------------------------------------------------
file <- paste0( 'Y:/IESS_2020/Data/IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_tact_sgo_female_50_.xlsx' )

act_female <- read_excel( file, sheet="Tact"
                          ,col_names = FALSE
                          ,guess_max = 24000
                          ,skip = 2 
)
act_female <- as.data.table( act_female[ , 2:3 ] )
setnames( act_female, c('Anio', 'Valor') )
act_female[ , tipo := 'Activ_female' ]

file <- paste0( 'Y:/IESS_2020/Data/IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_tact_sgo_male_50_.xlsx')

act_male <- read_excel( file, sheet="Tact"
                        ,col_names = FALSE
                        ,guess_max = 24000
                        ,skip = 2 )

act_male <- as.data.table( act_male[ , 2:3 ] )
setnames( act_male, c('Anio', 'Valor') )
act_male[ , tipo := 'Activ_male' ]

act <- rbind( act_female, act_male )

#
file <- paste0( 'Y:/IESS_2020/Data/IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_inactx__female_2020_.xlsx' )

inac_female <- read_excel( file, sheet="inactx"
                           ,col_names = FALSE
                           ,guess_max = 24000
                           #,skip = 0
                           , n_max = 2
)

inac_female <- as.data.table( t( inac_female ) )
setnames( inac_female, c( 'Valor', 'Anio' ) )
inac_female <- inac_female[ 3:43, ]
inac_female$Valor <- as.numeric( inac_female$Valor )
inac_female[ , tipo := 'Inact_f' ]
inac_female <- inac_female[ , list( Anio, Valor, tipo ) ]

file <- paste0( 'Y:/IESS_2020/Data/IVM/OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/'
                , 'base_inactx__male_2020_.xlsx')

inac_male <- read_excel( file, sheet="inactx"
                         ,col_names = FALSE
                         ,guess_max = 24000
                         #,skip = 2 
                         , n_max = 2
)

inac_male <- as.data.table( t( inac_male ) )
setnames( inac_male, c( 'Valor', 'Anio' ) )
inac_male <- inac_male[ 3:43, ]
inac_male$Valor <- as.numeric( inac_male$Valor )
inac_male[ , tipo := 'Inact_m' ]
inac_male <- inac_male[ , list( Anio, Valor, tipo ) ]

aux <- merge( act_male, inac_male, all.x = TRUE, by = 'Anio' )
aux[ , Valor := Valor.x + Valor.y ]
aux[ , tipo := 'Afi_m']
aux <- aux[ , list( Anio, Valor, tipo ) ]

aux2 <- merge( act_female, inac_female, all.x = TRUE, by = 'Anio' )
aux2[ , Valor := Valor.x + Valor.y ]
aux2[ , tipo := 'Afi_f']
aux2 <- aux2[ , list( Anio, Valor, tipo ) ]

aux3 <- data.table( Anio = act$Anio, Valor = aux$Valor + aux2$Valor, tipo = 'Acti_f_m' )
aux4 <- data.table( Anio = act$Anio, Valor = inac_female$Valor + inac_male$Valor, tipo = 'Inac_f_m' )
aux5 <- data.table( Anio = act$Anio, Valor = aux3$Valor + aux4$Valor, tipo = 'Afi_f_m' )

act <- rbind( act, inac_female, inac_male, aux, aux2, aux3, aux4, aux5 )

# Media de crecimiento de afiliados activos f+m
actvivos <- data.table(Anio = act_male$Anio, Valor = act_male$Valor + act_female$Valor )
actvivos[ , tasa := 100 * ( Valor / shift( x = Valor, n = 1 ) - 1 ) ]
media <- mean( actvivos$tasa , na.rm = TRUE )

inactvivos <- data.table(Anio = inac_male$Anio, Valor = inac_male$Valor + inac_female$Valor )
pctjact_2020 <- actvivos[Anio==2020]$Valor/(actvivos[Anio==2020]$Valor+inactvivos[Anio==2020]$Valor)
pctjinac_2020 <- 1 - pctjact_2020

pctjact_2060 <- actvivos[Anio==2060]$Valor/(actvivos[Anio==2060]$Valor+inactvivos[Anio==2060]$Valor)
pctjinac_2060 <- 1 - pctjact_2060


# Graficando pensionistas de invalid----------------------------------------------------------------
message( '\tGraficando pensionistas de invalidez' )
setwd(paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS'))
arch <- list.files()

proy_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_dis_")==T & str_detect( arch, "_female_")==T))] ),
                                    header = FALSE, sep = ",", dec = "."))
proy_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_dis_")==T & str_detect( arch, "_male_")==T))] ),
                                    header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', seq(2021,2060) )

setnames( proy_fem, col_nam )
p_fem <- t( proy_fem[ 1, c(as.character(seq(2021,2060)))] )
p_fem <- data.frame( anio = c(rownames(p_fem)), valor = p_fem[ ,1] )

setnames( proy_mal, col_nam )
p_mal <- t( proy_mal[ 1, c(as.character(seq(2021,2060)))] )
p_mal <- data.frame( anio = c(rownames(p_mal)), valor = p_mal[ ,1] )



his_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                  arch[match( TRUE, c(str_detect( arch, "_hdisp_")==T & str_detect( arch, "_female_")==T))] ),
                                   header = FALSE, sep = ",", dec = "."))
his_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                  arch[match( TRUE, c(str_detect( arch, "_hdisp_")==T & str_detect( arch, "_male_")==T))] ),
                                   header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', 'valor' )
setnames( his_fem, col_nam )
h_fem <- his_fem[ -c(1,2) , c('anio', 'valor' )]
h_fem[ , valor:= as.numeric(valor)]
setnames( his_mal , col_nam)
h_mal <- his_mal[ -c(1,2), c('anio', 'valor' ) ]
h_mal[ , valor:= as.numeric(valor)]

mal <- rbind( h_mal, p_mal )
fem <- rbind( h_fem, p_fem )

pen_dis <- as.data.table( merge( mal, fem , by = c('anio')) )
colnames( pen_dis ) <- c('anio', 'hombres', 'mujeres')
pen_dis[ , total:= hombres + mujeres ]
pen_dis[ , anio:= as.numeric( anio) ]

# Graficando pensionistas de viudez ----------------------------------------------------------------
message( '\tGraficando pensionistas de viudez' )

proy_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_wid_")==T & str_detect( arch, "_female_")==T))] ),
                                    header = FALSE, sep = ",", dec = "."))
proy_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_wid_")==T & str_detect( arch, "_male_")==T))] ),
                                    header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', seq(2021,2060) )

setnames( proy_fem, col_nam )
p_fem <- t( proy_fem[ 1, c(as.character(seq(2021,2060)))] )
p_fem <- data.frame( anio = c(rownames(p_fem)), valor = p_fem[ ,1] )

setnames( proy_mal, col_nam)
p_mal <- t( proy_mal[ 1, c(as.character(seq(2021,2060)))] )
p_mal <- data.frame( anio = c(rownames(p_mal)), valor = p_mal[ ,1] )


his_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                  arch[match( TRUE, c(str_detect( arch, "_hwp_")==T & str_detect( arch, "_female_")==T))] ),
                                   header = FALSE, sep = ",", dec = "."))
his_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                  arch[match( TRUE, c(str_detect( arch, "_hwp_")==T & str_detect( arch, "_male_")==T))] ),
                                   header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', 'valor' )
setnames( his_fem, col_nam )
h_fem <- his_fem[ -c(1,2) , c('anio', 'valor' )]
h_fem[ , valor:= as.numeric(valor)]
setnames( his_mal , col_nam)
h_mal <- his_mal[ -c(1,2), c('anio', 'valor' ) ]
h_mal[ , valor:= as.numeric(valor)]

mal <- rbind( h_mal, p_mal )
fem <- rbind( h_fem, p_fem )

pen_viu <- as.data.table( merge( mal, fem , by = c('anio')) )
colnames( pen_viu) <- c('anio', 'hombres', 'mujeres')
pen_viu[ , total:= hombres + mujeres ]
pen_viu <- pen_viu[ anio >= 2012]
pen_viu[ , anio:= as.numeric( anio) ]

# Graficando pensionistas de orfandad ----------------------------------------------------------------
message( '\tGraficando pensionistas de orfandad' )

proy_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_orph_")==T & str_detect( arch, "_female_")==T))] ),
                                    header = FALSE, sep = ",", dec = "."))
proy_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_orph_")==T & str_detect( arch, "_male_")==T))] ),
                                    header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', seq(2021,2060) )

setnames( proy_fem, col_nam )
p_fem <- t( proy_fem[ 1, c(as.character(seq(2021,2060)))] )
p_fem <- data.frame( anio = c(rownames(p_fem)), valor = p_fem[ ,1] )

setnames( proy_mal, col_nam)
p_mal <- t( proy_mal[ 1, c(as.character(seq(2021,2060)))] )
p_mal <- data.frame( anio = c(rownames(p_mal)), valor = p_mal[ ,1] )


his_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                  arch[match( TRUE, c(str_detect( arch, "_horp_")==T & str_detect( arch, "_female_")==T))] ),
                                   header = FALSE, sep = ",", dec = "."))
his_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                  arch[match( TRUE, c(str_detect( arch, "_horp_")==T & str_detect( arch, "_male_")==T))] ),
                                   header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', 'valor' )
setnames( his_fem, col_nam )
h_fem <- his_fem[ -c(1,2) , c('anio', 'valor' )]
h_fem[ , valor:= as.numeric(valor)]
setnames( his_mal , col_nam)
h_mal <- his_mal[ -c(1,2), c('anio', 'valor' ) ]
h_mal[ , valor:= as.numeric(valor)]

mal <- rbind( h_mal, p_mal )
fem <- rbind( h_fem, p_fem )

pen_orf <- as.data.table( merge( mal, fem , by = c('anio')) )
colnames( pen_orf ) <- c('anio', 'hombres', 'mujeres')
pen_orf[ , total:= hombres + mujeres ]
pen_orf <- pen_orf[ anio >= 2012]
pen_orf[ , anio:= as.numeric( anio) ]

# Nuevos Pensionistas Histórico --------------------------------------------------------------------
message( '\tNuevos Pensionistas Histórico' )
file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
               'hist_nuevos_pensionistas.xlsx' )

his_pen <- as.data.table(read_xlsx( file,
                                    sheet = 'Hoja1',
                                    col_names = TRUE,
                                    guess_max = 24000,
                                    na="") 
                         )

col_nam <- c('anio', 'valor', 'sexo', 'tipo' )
setnames( his_pen, col_nam )
his_pen[, sexo:=as.character(sexo)]
his_pen[ sexo=='2', sexo:='M']
his_pen[ sexo=='1', sexo:='H']
his_pen[ tipo=='INV-DIS', tipo:='INVALIDEZ']
his_pen[ tipo=='VUIDEDAD', tipo:='VIUDEDAD']

# Nuevos Pensionistas Vejez ------------------------------------------------------------------------
message( '\tVejez' )

proy_fem_vej <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                       arch[match( TRUE, c(str_detect( arch, "_nret_")==T & str_detect( arch, "_female_")==T))] ),
                                        header = FALSE, sep = ",", dec = "."))
proy_mal_vej <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                       arch[match( TRUE, c(str_detect( arch, "_nret_")==T & str_detect( arch, "_male_")==T))] ),
                                        header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', 'valor')

setnames( proy_fem_vej, col_nam )
proy_fem_vej <- proy_fem_vej[ anio %in%seq(2021,2060), -1]
proy_fem_vej <- proy_fem_vej[ , list( anio, valor, sexo='M', tipo='vejez')]

setnames( proy_mal_vej, col_nam)
proy_mal_vej <- proy_mal_vej[ anio %in%seq(2021,2060), -1]
proy_mal_vej <- proy_mal_vej[ , list( anio, valor, sexo='H', tipo='vejez')]

proy_vej <- rbind(proy_fem_vej, proy_mal_vej)

# Invalidez-Discapacidad ---------------------------------------------------------------------------
message( '\t\tInvalidez-Discapacidad' )

proy_fem_inv <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                       arch[match( TRUE, c(str_detect( arch, "_ndisx_")==T & str_detect( arch, "_female_")==T))] ),
                                        header = FALSE, sep = ",", dec = "."))
proy_mal_inv <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                       arch[match( TRUE, c(str_detect( arch, "_ndisx_")==T & str_detect( arch, "_male_")==T))] ),
                                        header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', seq(2021,2060) )

setnames( proy_fem_inv, col_nam )
p_fem <- t( proy_fem_inv[ 1, c(as.character(seq(2021,2060)))] )
p_fem <- as.data.table( data.frame( anio = c(rownames(p_fem)), valor = p_fem[ ,1] ) )

setnames( proy_mal_inv, col_nam)
p_mal <- t( proy_mal_inv[ 1, c(as.character(seq(2021,2060)))] )
p_mal <- as.data.table( data.frame( anio = c(rownames(p_mal)), valor = p_mal[ ,1] ) )

proy_inv <- rbind( p_fem[ , list(anio, valor, sexo='M', tipo='invalidez')],
                   p_mal[ , list(anio, valor, sexo='H', tipo='invalidez')] )

# Viudedad -----------------------------------------------------------------------------------------
message( '\tViudedad' )

proy_fem_viu_act <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_nwidactgx_")==T & str_detect( arch, "_female_")==T))] ),
                                            header = FALSE, sep = ",", dec = "."))
proy_mal_viu_act <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_nwidactgx_")==T & str_detect( arch, "_male_")==T))] ),
                                            header = FALSE, sep = ",", dec = ".") )

proy_fem_viu_pen <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_nwidpenx_")==T & str_detect( arch, "_female_")==T))] ),
                                            header = FALSE, sep = ",", dec = "."))
proy_mal_viu_pen <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_nwidpenx_")==T & str_detect( arch, "_male_")==T))] ),
                                            header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', seq(2021,2060) )

setnames( proy_fem_viu_act, col_nam )
p_fem_act <- t( proy_fem_viu_act[ 1, c(as.character(seq(2021,2060)))] )
p_fem_act <- as.data.table( data.frame( anio = c(rownames(p_fem_act)), valor = p_fem_act[ ,1] ) )

setnames( proy_mal_viu_act, col_nam)
p_mal_act <- t( proy_mal_viu_act[ 1, c(as.character(seq(2021,2060)))] )
p_mal_act <- as.data.table( data.frame( anio = c(rownames(p_mal_act)), valor = p_mal_act[ ,1] ) )

setnames( proy_fem_viu_pen, col_nam )
p_fem_pen <- t( proy_fem_viu_pen[ 1, c(as.character(seq(2021,2060)))] )
p_fem_pen <- as.data.table( data.frame( anio = c(rownames(p_fem_pen)), valor = p_fem_pen[ ,1] ) )

setnames( proy_mal_viu_pen, col_nam)
p_mal_pen <- t( proy_mal_viu_pen[ 1, c(as.character(seq(2021,2060)))] )
p_mal_pen <- as.data.table( data.frame( anio = c(rownames(p_mal_pen)), valor = p_mal_pen[ ,1] ) )


proy_viu <- rbind( p_fem_act[ , list(anio, valor, sexo='M', tipo='viudedad')],
                   p_mal_act[ , list(anio, valor, sexo='H', tipo='viudedad')],
                   p_fem_pen[ , list(anio, valor, sexo='M', tipo='viudedad')],
                   p_mal_pen[ , list(anio, valor, sexo='H', tipo='viudedad')])

# Orfandad-----------------------------------------------------------------------------------------
message( '\t\tOrfandad' )

proy_fem_orf_act <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_norphactsgx_")==T & str_detect( arch, "_female_")==T))] ),
                                            header = FALSE, sep = ",", dec = "."))
proy_mal_orf_act <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_norphactsgx_")==T & str_detect( arch, "_male_")==T))] ),
                                            header = FALSE, sep = ",", dec = ".") )


proy_fem_orf_pen <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_norphpensx_")==T & str_detect( arch, "_female_")==T))] ),
                                            header = FALSE, sep = ",", dec = "."))
proy_mal_orf_pen <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                           arch[match( TRUE, c(str_detect( arch, "_norphpensx_")==T & str_detect( arch, "_male_")==T))] ),
                                            header = FALSE, sep = ",", dec = ".") )

col_nam <- c('por', 'anio', seq(2021,2060) )

setnames( proy_fem_orf_act, col_nam )
p_fem_act <- t( proy_fem_orf_act[ 1, c(as.character(seq(2021,2060)))] )
p_fem_act <- as.data.table( data.frame( anio = c(rownames(p_fem_act)), valor = p_fem_act[ ,1] ) )

setnames( proy_mal_orf_act, col_nam)
p_mal_act <- t( proy_mal_orf_act[ 1, c(as.character(seq(2021,2060)))] )
p_mal_act <- as.data.table( data.frame( anio = c(rownames(p_mal_act)), valor = p_mal_act[ ,1] ) )

setnames( proy_fem_orf_pen, col_nam )
p_fem_pen <- t( proy_fem_orf_pen[ 1, c(as.character(seq(2021,2060)))] )
p_fem_pen <- as.data.table( data.frame( anio = c(rownames(p_fem_pen)), valor = p_fem_pen[ ,1] ) )

setnames( proy_mal_orf_pen, col_nam)
p_mal_pen <- t( proy_mal_viu_pen[ 1, c(as.character(seq(2021,2060)))] )
p_mal_pen <- as.data.table( data.frame( anio = c(rownames(p_mal_pen)), valor = p_mal_pen[ ,1] ) )

proy_orf <- rbind( p_fem_act[ , list(anio, valor, sexo='M', tipo='orfandad')],
                   p_mal_act[ , list(anio, valor, sexo='H', tipo='orfandad')],
                   p_fem_pen[ , list(anio, valor, sexo='M', tipo='orfandad')],
                   p_mal_pen[ , list(anio, valor, sexo='H', tipo='orfandad')])

#Graficando tasa de cobertura ----------------------------------------------------------------------
message( '\tGraficando tasa de cobertura' )

proy_fem <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_pen_rap60cr_")==T & str_detect( arch, "_female_")==T))] ),
                                    header = FALSE, sep = ",", dec = "."))
proy_mal <-as.data.table( read.csv( file = paste0( parametros$Data_seg, 'OUTPUT/ESCENARIO_BASE/PROYECCIONES_DEMOGRAFICAS/', 
                                                   arch[match( TRUE, c(str_detect( arch, "_pen_rap60cr_")==T & str_detect( arch, "_male_")==T))] ),
                                    header = FALSE, sep = ",", dec = "."))

col_nam <- c('por', 'anio', 'valor')

setnames( proy_fem, col_nam )
proy_fem <- proy_fem[ anio %in%seq(2021,2060), -1]
proy_fem <- proy_fem[ , list( anio, valor, sexo='M')]

setnames( proy_mal, col_nam )
proy_mal <- proy_mal[ anio %in%seq(2021,2060), -1]
proy_mal <- proy_mal[ , list( anio, valor, sexo='H')]
proy_cob <- rbind( proy_fem, proy_mal )



# GUARDAR ----------------------------------------------------------------------
lista <- c( 'tasa_cob', 'act', 'media', 'pctjact_2020', 'pctjinac_2020',
            'pctjact_2060', 'pctjinac_2060', 'pen_dis', 'pen_viu', 'pen_orf', 'his_pen',
            'proy_vej', 'proy_inv', 'proy_viu', 'proy_orf',
            'proy_cob', 'entradas_sis', 'salarios', 'cotizantes', 'prop_afi_act_inac', 'inac_cotiz_masc',
            'activ_cotiz_fem', 'inac_cotiz_fem', 'tipo_pensi_sexo', 'edad_promedio', 'pensio_edad_sexo_h',
            'pensio_edad_sexo_m', 'benef_pensi_sexo', 'val_prom_benef_sex_1', 'val_prom_benef_sex_2', 'val_prom_benef__tipo_sex',
            'afi_cotiz_riesgo_mascul', 'pastel_afi_cotiz_riesgo_mascul', 'distri_activ_cotiz_riesgo_femenin',
            'pastel_distri_activ_cotiz_riesgo_femenin', 'distri_inact_cotiz_riesgo_mascul', 'pastel_distri_inact_cotiz_riesgo_mascul',
            'distri_inact_cotiz_riesgo_femen', 'pastel_distri_inact_cotiz_riesgo_femen')

save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_IVM_salidas_demograficos.RData' ) )
setwd( parametros$work_dir )

message( paste( rep('-', 100 ), collapse = '' ) )
gc()


