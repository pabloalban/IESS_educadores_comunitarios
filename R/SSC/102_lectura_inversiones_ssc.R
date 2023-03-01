message( paste( rep('-', 100 ), collapse = '' ) )

# message( '\tLectura del portafolio de inversiones' )
# library("xlsx")
# # Carga de datos -----------------------------------------------------------------------------------
# file_inversiones <- paste0( parametros$RData_seg, 'IESS_SSC_inversiones.RData' )
# load( file = file_inversiones )
# 
# #Archivo en Excel-----------------------------------------------------------------------------------
# file_xlxs<-paste0(parametros$resultado_seguro, 'IESS_SSC_inversiones.xlsx' )
# 
# message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )
# 
# 
# #Guardar recursos administrados por el BIESS--------------------------------------------------------
# 
# write.xlsx( as.data.frame( recurs_adm_biess ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "recurs_adm_biess", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# 
# write.xlsx( as.data.frame( inver_corte ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "inver_corte", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# 
# 
# write.xlsx( as.data.frame( rendimientos_netos ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "rendimientos_netos", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# 
# write.xlsx( as.data.frame(  ingresos ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "ingresos", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# 
# write.xlsx( as.data.frame(  gastos_opera ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "gastos_opera", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# write.xlsx( as.data.frame(  inv_instrumento ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "inv_instrumento", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# 
# 
# write.xlsx( as.data.frame(  detalle_bonos ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_bonos", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# write.xlsx( as.data.frame(  detalle_bonos_40 ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_bonos_40", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# 
# write.xlsx( as.data.frame(  detalle_obligaciones ),
#             file = paste0( parametros$resultado_seguro , 'IESS_SSC_inversiones.xlsx' ),
#             sheetName = "detalle_obligaciones", 
#             col.names = TRUE,
#             row.names = FALSE, 
#             append = TRUE)
# 
# message( '\tLectura de las inversiones de SSC' )

#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_SSC_inversiones.xlsx' )


#Caraga de recursos administrados por el BIESS------------------------------------------------------
recurs_adm_biess <- read_excel(file,
                               sheet = 1,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names()

inver_corte <- read_excel(file,
                          sheet = 2,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0) %>% clean_names()

rendimientos_netos <- read_excel(file,
                                 sheet = 3,
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0) %>% clean_names() %>%
   mutate(corte_a=as.Date(corte_a,"%Y-%m-%d"))

ingresos <- read_excel(file,
                       sheet = 4,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

gastos_opera <- read_excel(file,
                           sheet = 5,
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0) %>% clean_names()

inv_instrumento <- read_excel(file,
                              sheet = 6,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0) %>% clean_names()

detalle_bonos <- read_excel(file,
                            sheet = 7,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0) %>% clean_names()

detalle_bonos_40 <- read_excel(file,
                               sheet = 8,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names() %>%
   mutate(fecha_colocacion=as.Date(fecha_colocacion,"%d/%m/%Y"),
          vencimiento=as.Date(vencimiento,"%d/%m/%Y"),
          pago_del_periodo=as.Date(pago_del_periodo,"%d/%m/%Y"))

detalle_obligaciones <- read_excel(file,
                                   sheet = 9,
                                   col_names = TRUE,
                                   col_types = NULL,
                                   na = "",
                                   skip = 0) %>% clean_names()


recuperacion_bonos <- read_excel(file,
                                sheet = 10,
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0) %>% clean_names() %>%
   mutate(fecha_cupon =as.Date(fecha_cupon ,"%d/%m/%Y"),
          fecha_vcmto=as.Date(fecha_vcmto,"%d/%m/%Y"))


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando inversiones en un solo data.frame' )

save( recurs_adm_biess,
      inver_corte,
      rendimientos_netos,
      ingresos,
      gastos_opera,
      inv_instrumento,
      detalle_bonos,
      detalle_bonos_40,
      detalle_obligaciones,
      recuperacion_bonos,
      file = paste0( parametros$RData_seg, 'IESS_SSC_inversiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()