message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de las inversiones de desempleo y cesantía' )
#Archivos en texto a subir--------------------------------------------------------------------------
file_2015_2016 <- paste0( 'D://Datos (D)//Bases//', 'actividad_2015_2016.txt' )
file_2017_2018 <- paste0( 'D://Datos (D)//Bases//', 'actividad_2017_2018.txt' )
file_2019_2020 <- paste0( 'D://Datos (D)//Bases//', 'actividad_2019_2020.txt' )
file_dic_ene <- paste0( 'D://Datos (D)//Bases//', 'actividad_diciembre.txt' )
file_inventario <- paste0( 'D://Datos (D)//Bases//', 'inventario_afi.txt' )

#Inventario de afiliados----------------------------------------------------------------------------
inv_afi <- (read.table(file_inventario,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                       stringsAsFactors = FALSE, fill = TRUE)) %>% clean_names()

inv_afi <- inv_afi %>%
  filter( imposiciones > 0 ) %>%
  mutate( fecha_nac = as.Date(fechanacimiento, "%d/%m/%Y")) %>%
  mutate( edad = round((as.Date("30/06/2020","%d/%m/%Y") - fecha_nac)/360,0) ) %>%
  mutate( sexo = if_else( cod_genero=='2','F','M' ) ) %>%
  select( cedula:=afiliados, sexo, edad)

#Actividad 2015 a 2016------------------------------------------------------------------------------

act_15_16 <- (read.table(file_2015_2016,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                       stringsAsFactors = FALSE, fill = TRUE, colClasses = c('character',
                                                                             'integer',
                                                                             'integer') )) %>%
  clean_names() %>%
  select( cedula:=afiliados, anio, mes)

#Actividad 2017 a 2018------------------------------------------------------------------------------

act_17_18 <- (read.table(file_2017_2018,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                         stringsAsFactors = FALSE, fill = TRUE, colClasses = c('character',
                                                                               'integer',
                                                                               'integer') )) %>%
  clean_names() %>%
  select( cedula:=afiliados, anio, mes)

#Actividad 2019 a 2020------------------------------------------------------------------------------

act_19_20 <- (read.table(file_2019_2020,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                         stringsAsFactors = FALSE, fill = TRUE, colClasses = c('character',
                                                                               'integer',
                                                                               'integer') )) %>%
  clean_names() %>%
  select( cedula:=afiliados, anio, mes)


#Actividad diciembre - enero------------------------------------------------------------------------

act_dic_ene <- (read.table(file_dic_ene,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                         stringsAsFactors = FALSE, fill = TRUE, colClasses = c('character',
                                                                               'integer',
                                                                               'integer') )) %>%
  clean_names() %>%
  select( cedula:=afiliados, anio, mes)


#Guardar resultados en un Rdata---------------------------------------------------------------------
message( '\tGuardando Rdatas' )
save( inv_afi, file = paste0( 'D://Datos (D)//Bases//', 'IESS_inventario_afi.RData' ) )

save( act_15_16, file = paste0( 'D://Datos (D)//Bases//', 'IESS_act_15_16.RData' ) )

save( act_17_18, file = paste0( 'D://Datos (D)//Bases//', 'IESS_act_17_18.RData' ) )

save( act_19_20, file = paste0( 'D://Datos (D)//Bases//', 'IESS_act_19_20.RData' ) )

save( act_dic_ene, file = paste0( 'D://Datos (D)//Bases//', 'IESS_act_dic_ene.RData' ) )

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()