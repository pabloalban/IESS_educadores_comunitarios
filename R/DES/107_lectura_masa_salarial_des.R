message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de la masa salarial de los cotizantes al seguro de desempleo y cesantia' )

#Evolución de masa salarial-------------------------------------------------------------------------
file_cot_ces_des <- paste0( parametros$Data_seg, 'IESS_DES_cotizantes.txt' )

aux<-read.table(file_cot_ces_des, dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                stringsAsFactors = FALSE)

aux$genero<-gsub("0","1",aux$genero)
evo_masa_sal_ces_des<-aux %>%
  group_by(anio,genero) %>%
  mutate(masa_salarial=sum(12*masa_mensual,na.rm = TRUE)) %>%
  select(anio,edad, genero, masa_salarial) %>%  
  ungroup() %>%
  distinct(anio,genero, .keep_all = TRUE)%>%
  arrange(anio,genero) %>% 
  reshape2::dcast(.,
                  anio ~ genero,
                  value.var = "masa_salarial") %>%
  mutate(total= rowSums(.[2:3]),
         incremento=total-lag(total),
         tasa_crecimiento= ((total-lag(total))/lag(total)))





message( '\tLectura de la masa salaria de los cotizantes del seguro de desempleo y cesantia' )

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
evo_masa_sal_ces_des <- cotizantes_ces_des %>%
  group_by( aniper, genero) %>%
  mutate( masa_salarial = sum( 12 * masamensual , na.rm = TRUE ) )  %>%
  ungroup() %>%
  distinct( aniper, genero, .keep_all = TRUE ) %>%
  select( -edad ) %>%
  arrange( aniper, genero ) %>% 
  reshape2::dcast( .,
                   aniper ~ genero,
                   value.var = "masa_salarial" ) %>%
  mutate( total = rowSums(.[2:3]),
          incremento = total-lag( total ),
          tasa_crecimiento = ( ( total-lag( total) ) / lag( total ) ) )


# Guardar los data.frames en un Rdata---------------------------------------------------------------
message( '\tGuardando masa salarial de los cotizantes de cesantía, desempleo' )

save( evo_masa_sal_ces_des,
      file = paste0( parametros$RData_seg, 'IESS_CES_DES_masa_salarial_historico.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ] )
gc()
