message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de causas de desfnanciamiento' )

message( '\tReconstrucción del archivo xlsx' )
#library("xlsx")
#detach("package:XLConnect", unload = TRUE)
# Carga de datos -----------------------------------------------------------------------------------
# file <- paste0( parametros$RData_seg, 'IESS_SSC_causas_desfinanciamiento.RData' )
# load( file = file )
# 
# #Archivo en Excel-----------------------------------------------------------------------------------
# file_xlxs<-paste0(parametros$resultado_seguro, 'IESS_SSC_causas_desfinanciamiento0.xlsx' )
# 
# message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )
# 
# 
# #Guardar recursos administrados por el BIESS--------------------------------------------------------
# 
# write.xlsx( as.data.frame( causa_desf_desinversiones ),
#             file = file_xlxs,
#             sheetName = "causa_desf_desinversiones",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = FALSE)
# 
# 
# 
# write.xlsx( as.data.frame( causa_desf_desinversiones_anual ),
#             file = file_xlxs,
#             sheetName = "causa_desf_desinversiones_anual",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# 
# 
# write.xlsx( as.data.frame( causa_desf_estado ),
#             file = file_xlxs,
#             sheetName = "causa_desf_estado",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# 
# write.xlsx( as.data.frame( causa_desf_total ),
#             file = file_xlxs,
#             sheetName = "causa_desf_total",
#             col.names = TRUE,
#             row.names = FALSE,
#             append = TRUE)
# 
# message( '\tLectura de las inversiones de SSC' )
# 
# 
# 
# 
# 
# 
# message( paste( rep('-', 100 ), collapse = '' ) )
# 
# message( '\tLectura de causas de desfinanciamiento' )
# 




#Cargando información financiera--------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_SSC_causas_desfinanciamiento.xlsx' )


#Caraga de recursos administrados por el BIESS------------------------------------------------------
causa_desf_desinversiones <- read_excel(file,
                                sheet = 1,
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0) %>% clean_names() %>%
  mutate( periodo = as.Date( periodo, "%d/%m/%Y")) %>%
  filter( capital_desinvertido > 0 )

causa_desf_desinversiones_anual <- read_excel(file,
                                              sheet = 2,
                                              col_names = TRUE,
                                              col_types = NULL,
                                              na = "",
                                              skip = 0) %>% clean_names()

causa_desf_estado <- read_excel(file,
                                sheet = 3,
                                col_names = TRUE,
                                col_types = NULL,
                                na = "",
                                skip = 0) %>% clean_names()



causa_desf_total <- read_excel(file,
                               sheet = 4,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names()



#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando balance de desempleo' )

save( causa_desf_estado,
      causa_desf_desinversiones,
      causa_desf_total,
      causa_desf_desinversiones_anual,
      #deuda_estado,
      file = paste0( parametros$RData_seg, 'IESS_SSC_causas_desfinanciamiento.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()