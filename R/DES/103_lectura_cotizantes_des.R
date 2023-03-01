message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los cotizantes al seguro de desempleo y cesantia' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_cotizantes_masa_salarial_ces_des.xls' )

#Caraga de recursos administrados por el BIESS------------------------------------------------------
aux <- read_excel(file,
                             sheet = 1,
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names() %>%
  filter( edad < 115, edad > 15 )


cotizantes_ces_des <- aux %>% mutate( cotizantes = numero ) %>% select( -numero )
  

cotizantes_ces_des$genero <- gsub( '2', 'F', cotizantes_ces_des$genero )
cotizantes_ces_des$genero <- gsub( '1', 'M', cotizantes_ces_des$genero )


# Evolución histórica de cotizantes a cesantía y desempleo------------------------------------------
evo_anual_cotizantes_ces_des <- cotizantes_ces_des %>%
  group_by( aniper, genero) %>%
  mutate( cotizantes = round( sum( cotizantes, na.rm = TRUE ), 0 ) ) %>%
  ungroup() %>%
  distinct( aniper, genero, .keep_all = TRUE ) %>%
  select( -edad ) %>%
  arrange( aniper, genero ) %>% 
  reshape2::dcast( .,
                   aniper ~ genero,
                   value.var = "cotizantes" ) %>%
  mutate( total = rowSums(.[2:3]),
          incremento = total-lag( total ),
          tasa_crecimiento = ( ( total-lag( total) ) / lag( total ) ) )

# Cotizantes al SGO por año ------------------------------------------------------------------------
file_cot_sgo <- paste0( parametros$Data_seg, 'IESS_SGO_cotizantes.txt' )

cotizantes_sgo <- read.table( file_cot_sgo, 
                              dec = ".", 
                              header = TRUE, 
                              sep = "\t", 
                              na.strings = "NA", 
                              stringsAsFactors = FALSE)

cotizantes_sgo$genero <- gsub( '2', 'F', cotizantes_sgo$genero )
cotizantes_sgo$genero <- gsub( '1', 'M', cotizantes_sgo$genero )
cotizantes_sgo[ is.na( cotizantes_sgo$cotizantes_sgo ), ]$cotizantes_sgo <- 0

aux <- cotizantes_sgo %>% 
  group_by(anio, genero) %>%
  top_n(n = 1, wt = cotizantes_sgo) %>%
  ungroup() %>%
  mutate(edad_max_freq = edad) %>%
  select(anio, genero, edad_max_freq)

cotizantes_sgo <- left_join( cotizantes_sgo, aux, by = c( 'anio', 'genero' ) )
cotizantes_sgo[ which( cotizantes_sgo$edad < 15 ), ]$edad <- cotizantes_sgo[ which( cotizantes_sgo$edad < 15 ), ]$edad_max_freq
cotizantes_sgo[ which( cotizantes_sgo$edad > 115 ), ]$edad <- cotizantes_sgo[ which( cotizantes_sgo$edad >115 ), ]$edad_max_freq
cotizantes_sgo <- cotizantes_sgo %>%
  group_by( anio, genero, edad ) %>%
  mutate( cotizantes_sgo = sum( cotizantes_sgo, na.rm = TRUE ) ) %>%
  ungroup() %>%
  distinct( anio, edad, genero, .keep_all = TRUE ) %>%
  select( -edad_max_freq ) %>%
  arrange( edad, anio, genero )


# Guardar los data.frames en un Rdata---------------------------------------------------------------
message( '\tGuardando cotizantes de cesantía, desempleo y sgo' )

save( cotizantes_sgo, cotizantes_ces_des, evo_anual_cotizantes_ces_des,
      file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_historicos.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
