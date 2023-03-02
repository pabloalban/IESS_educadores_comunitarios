message(paste(rep("-", 100), collapse = ""))

#---------------------------------------------------------------------------------------------------
message("\tCargando datos")
load(paste0(parametros$RData_seg, "IESS_ECO_tablas_estadisticas.RData"))
load(paste0(parametros$RData_seg, "IESS_ECO_aportes.RData"))

#Tasas de interes actuarial-------------------------------------------------------------------------

tasa_actuarial <- data.frame( anio = c( 1947:2022 ),
                              ta = 0.04 ) %>%
  mutate( ta = if_else( anio >= 2020,
                        0.0625,
                        ta ) ) %>%
  mutate( anio = as.integer( anio ) )

#Calculo de intereses de Educadores Comunitarios----------------------------------------------------

message("\tCalculando intereses")

intereses <- planillas_edu %>%
  group_by( anio ) %>%
  mutate( masa = sum( sueldo, na.rm = TRUE ),
          apor_ivm = sum( apor_ivm, na.rm = TRUE ),
          apor_per = sum( apor_per, na.rm =  TRUE ),
          apor_pat = sum( apor_pat, na.rm =  TRUE ) ) %>%
  ungroup( ) %>%
  distinct( anio, cedula , .keep_all = TRUE ) %>%
  group_by( anio ) %>%
  mutate( educadores = n() ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  dplyr::select( anio,
                 educadores,
                 masa,
                 apor_per,
                 apor_pat,
                 apor_ivm ) %>%
  rbind( ., data.frame( anio = 2009:2021,
                        educadores = rep( 0, 13 ),
         masa = rep( 0, 13 ), 
         apor_per = rep( 0, 13 ),
         apor_pat = rep( 0, 13 ),
         apor_ivm = rep( 0, 13 ) ) ) %>%
  left_join( ., tasa_actuarial, by = 'anio' ) %>%
  mutate( ta1 = 1 + ta ) %>%
  mutate( ta_lead = lead( ta1, default = 1 ) ) %>%
  arrange( -anio ) %>%
  mutate( i = cumprod( ta_lead ) ) %>%
  mutate( interes = (i * apor_ivm) - apor_ivm ) %>%
  arrange( anio )


#Tabla de resumen de resultados---------------------------------------------------------------------
aux <- intereses %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  filter( anio == 'Total') %>%
  dplyr::select( -anio,
                 -educadores,
                 -ta,
                 -ta1,
                 -ta_lead,
                 -i ) %>%
  t( . )

aux <- data.frame( concepto = c( 'Masa Salarial 1941 a 2008',
                                 'Aportes Personales a IVM',
                                 'Aportes Patronales a IVM',
                                 'Total aportes a IVM',
                                 'Lucro Cesante'),
                   valores = as.numeric( aux ) ) 

resumen <- aux

#Guardar en un Rdata--------------------------------------------------------------------------------

message( '\tGuardando tablas' )

save( intereses,
      resumen,
      file = paste0( parametros$RData_seg, 'IESS_ECO_intereses.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()