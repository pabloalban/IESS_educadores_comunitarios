message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de bases' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_educadores_comunitarios.xlsx' )

file_afiliados_sgo<-paste0(parametros$Data_seg, 'DatosPersonales.xlsx' )

#load( file = paste0( parametros$RData_seg, 'IESS_Reg_Civil.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo tabla de cotizaciones' )
#Carga de cotizantes al SGO (exposición)------------------------------------------------------------
file2<- paste0( parametros$Data_seg, 'Aportes_Edu_Com.txt' )
planillas_sgo<-read.table(file2, dec = ".",
                         header = TRUE,
                         sep = ";",
                         na.strings = "NA",
                         colClasses = c('character',
                                        'integer',
                                        'integer',
                                        'numeric'),
                         fill = TRUE ) %>% clean_names() %>%
  mutate( tipo_pla = 'sgo', 
          cedula = numafi ) %>%
  dplyr::select(-numafi)

afiliados_sgo <-  read_excel( file_afiliados_sgo,
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names() %>%
  dplyr::select(cedula,
                fecha_nacimiento,
                sexo,
                fecha_defuncion,
                vej_fec_der,
                vej_valor_pen ) %>%
  mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%d/%m/%Y"),
          fecha_defuncion  = as.Date( fecha_defuncion , "%d/%m/%Y"),
          vej_fec_der  = as.Date( vej_fec_der , "%d/%m/%Y") )


#Carga de datos-------------------------------------------------------------------------------------

planillas_edu <- read_excel(file,
                            #sheet = 'Deuda_consolidada',
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0) %>% clean_names()


planillas_edu <-  planillas_edu %>%
                  mutate( fecha_pago = as.Date( fecha_pago, "%YY-%mm-%dd"),
                          fecha_pla = as.Date( paste0(anio,"/",mes,"/1"), "%Y/%m/%d" ) ) %>%
                  mutate( ta_per = 0.0864,
                          ta_pat = 0.0110 ) %>%
                  mutate( ta_per = ifelse( fecha_pla >= as.Date( "2003-01-01", "%Y-%m-%d"), 0.08, ta_per ) ) %>%
                  mutate( ta_per = ifelse( fecha_pla >= as.Date( "2005-11-01", "%Y-%m-%d"), 0.0864, ta_per ) ) %>%
                  mutate( ta_pat = ifelse( fecha_pla >= as.Date( "2003-01-01", "%Y-%m-%d"), 0.0115, ta_per ) ) %>%
                  mutate( ta_pat = ifelse( fecha_pla >= as.Date( "2005-11-01", "%Y-%m-%d"), 0.010, ta_per ) ) %>%
                  mutate( ta_ivm = ta_per + ta_pat ) %>%
                  mutate( apor_per = ta_per * sueldo,
                          apor_pat = ta_pat * sueldo,
                          apor_ivm = ta_ivm * sueldo ) %>%
                  group_by( cedula ) %>%
                  mutate( imposiciones_edu = as.integer(sum(dias, na.rm = TRUE)/30 ) ) %>%
                  mutate( fecha_sal_edu = max( fecha_pla ) ) %>%
                  ungroup()

aux_1 <- planillas_edu %>%
  dplyr::select( cedula,
                 provincia,
                 imposiciones_edu,
                 fecha_sal_edu ) %>%
  distinct( cedula, .keep_all = TRUE )


planillas_sgo <- anti_join( planillas_sgo, planillas_edu, by=c('cedula', 'anio', 'mes', 'sueldo') ) %>%
  group_by( cedula ) %>%
  mutate( imposiciones_sgo = n() ) %>%
  mutate( planilla = as.Date( paste0(anio,"-",mes,"-01"), "%Y-%m-%d" ) ) %>%
  mutate( fecha_sal_sgo = max( planilla ) ) %>%
  ungroup() 
  
aux_2 <- planillas_sgo %>%
  dplyr::select( cedula,
                 imposiciones_sgo,
                 fecha_sal_sgo ) %>%
  distinct( cedula, .keep_all = TRUE )
  
                  
edu_comunitarios <- afiliados_sgo %>% 
  left_join(., aux_1, by='cedula') %>%
  left_join(., aux_2, by='cedula') %>%
  mutate( fecha_sal = pmax( fecha_sal_edu , fecha_sal_sgo, na.rm = TRUE)) %>%
  group_by(cedula) %>%
  mutate( imposiciones_tot =  sum( imposiciones_edu, imposiciones_sgo , na.rm = TRUE)) %>%
  ungroup() %>%
  mutate( edad = as.integer((today() - fecha_nacimiento )/360) )



#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando balance de desempleo' )

save( planillas_edu,
      planillas_sgo,
      edu_comunitarios,
      file = paste0( parametros$RData_seg, 'IESS_ECO_aportes.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()