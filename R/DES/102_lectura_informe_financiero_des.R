message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los estados financieros de desempleo' )

#Cargando información financiera--------------------------------------------------------------------
file_rec_admi_BIESS<-paste0(parametros$Data_seg, 'IESS_inv_des.xls' )
file_balances_IESS<-paste0(parametros$Data_seg, 'IESS_balance_financiero_des.xls' )
file_ingresos<-paste0(parametros$Data_seg, 'IESS_DES_ingresos_DNFTSD.xls' )

#Caraga de recursos administrados por el BIESS------------------------------------------------------
rec_admi_BIESS <- read_excel(file_rec_admi_BIESS,
                              sheet = 1,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0) %>% clean_names()

activos <- read_excel(file_balances_IESS,
                             sheet = 1,
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names()

cuentas_cobrar <- read_excel(file_balances_IESS,
                          sheet = 2,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0) %>% clean_names()

pasivo <- read_excel(file_balances_IESS,
                     sheet = 3,
                     col_names = TRUE,
                     col_types = NULL,
                     na = "",
                     skip = 0) %>% clean_names()

cuentas_pagar <- read_excel(file_balances_IESS,
                           sheet = 4,
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0) %>% clean_names()

patrimonio <- read_excel(file_balances_IESS,
                         sheet = 5,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "",
                         skip = 0) %>% clean_names()

ingresos <- read_excel(file_balances_IESS,
                       sheet = 6,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

gastos <- read_excel(file_balances_IESS,
                       sheet = 7,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

fondos_disponibles <- read_excel(file_balances_IESS,
                        sheet = 8,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0) %>% clean_names()

pasivos_no_corrientes <- read_excel(file_balances_IESS,
                                 sheet = 9,
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0) %>% clean_names()

fondo_cap<- read_excel(file_balances_IESS,
                                    sheet = 10,
                                    col_names = TRUE,
                                    col_types = NULL,
                                    na = "",
                                    skip = 0) %>% clean_names()

resultados<- read_excel(file_balances_IESS,
                       sheet = 11,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()


ingresos_DNFTSD <- read_excel(file_ingresos,
                       sheet = 1,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()


#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando balance de desempleo' )

save( rec_admi_BIESS,
      activos,
      cuentas_cobrar,
      pasivo,
      cuentas_pagar,
      patrimonio,
      ingresos,
      gastos,
      ingresos_DNFTSD,
      fondos_disponibles,
      pasivos_no_corrientes,
      fondo_cap,
      resultados,
      file = paste0( parametros$RData_seg, 'IESS_DES_balances_financieros.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()