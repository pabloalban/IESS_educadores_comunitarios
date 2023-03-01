message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los avisos de salida' )
#Archivos en texto a subir--------------------------------------------------------------------------
file <- paste0( 'D://Datos (D)//Salidas_2015_2020//', 'Resultados_Salidas_2015_2020.tsv' )

#Inventario de afiliados----------------------------------------------------------------------------
avisos_salida <- (read.table(file,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                       stringsAsFactors = FALSE,
                       quote="",
                       colClasses = c('integer',
                                      'integer',
                                      'character',
                                      'character',
                                      'integer',
                                      'character',
                                      'character',
                                      'character',
                                      'character',
                                      'character',
                                      'character',
                                      'character'),
                       fill = TRUE)) %>% clean_names() 


tipo_salida <- avisos_salida %>% select( cod_causal,
                                         causal ) %>%
  distinct( cod_causal,.keep_all = TRUE) %>% 
  dplyr::filter( nchar(causal) > 0 )

avisos_salida <- avisos_salida %>%
  select( anio, mes, cedula:=cedula_afi, edad, genero, sector, fecininov, cod_causal  ) %>%
  mutate( fecininov   = as.Date(fecininov, "%d/%m/%Y"))

avisos_salida <- avisos_salida %>% 
  mutate( mes = month(fecininov) + 1 ) %>%
  mutate( anio = year( fecininov ) ) %>%
  mutate( anio = ifelse( mes == '13', (anio + 1), anio ) ) %>%
  mutate( mes = ifelse( mes == '13', 1, mes ) )

#Guardar resultados en un Rdata---------------------------------------------------------------------
message( '\tGuardando Rdatas' )
save( avisos_salida,
      tipo_salida, file = paste0( 'D://Datos (D)//Bases//', 'IESS_avisos_salida.RData' ) )
#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
