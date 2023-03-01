message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo variables macroeconomicas IVM del IESS' )

#file
file <- paste0( parametros$Data, 'IESS_IVM_variables_macroeconomicas.xlsx' )


# IPC IVM --------------------------------------------------
message( '\tLeyendo  IPC IVM del IESS' )
col_nom <- c( 'Anio', 'IPC', 'inflacion')
col_tip <- c( 'character', 'numeric', 'numeric')

IPC_ivm <-read_excel(file,sheet=1
                        ,col_names=TRUE,guess_max = 24000)
IPC_ivm <- as.data.table( IPC_ivm)[1:21,]
setnames( IPC_ivm, col_nom )

# IPC GRAFICO----------------------------------------------
message( '\tLeyendo  IPC IVM del IESS' )
col_nom <- c( 'Anio', 'IPC', 'inflacion')
col_tip <- c( 'numeric', 'numeric', 'numeric')

IPCgraf_ivm <-read_excel(file,sheet=1
                     ,col_names=TRUE,guess_max = 24000)
IPCgraf_ivm <- as.data.table( IPCgraf_ivm)[1:21,]
setnames(IPCgraf_ivm, col_nom )
# SBU IVM --------------------------------------------------
message( '\tLeyendo  SBU IVM del IESS')
col_nom <- c( 'Anio', 'SBU', 'Tasa de crecimiento (%)')
col_tip <- c( 'character', 'numeric', 'numeric')

SBU_ivm<-read_excel(file,sheet=2
                    ,col_names=TRUE,guess_max = 24000)
SBU_ivm <- as.data.table( SBU_ivm )[1:20,]
setnames(SBU_ivm, col_nom )

# Salario Promedio IVM --------------------------------------------------
message( '\tLeyendo  Salario promedio IVM del IESS')
col_nom <- c( 'Anio', 'Salario_declarado_promedio', 'Incremento anual (USD)', 'Tasa de crecimiento (%)')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

Salprom_ivm<-read_excel(file,sheet=3
                    ,col_names=TRUE,guess_max = 24000)
Salprom_ivm <- as.data.table( Salprom_ivm )[1:11,]
setnames(Salprom_ivm, col_nom )

#PIB IVM --------------------------------------------------
message( '\tLeyendo  PIB IVM del IESS' )
col_nom <- c( 'Anio', 'Crecimiento_real_del_PIB ', 'Anio', 'Crecimiento_real_del_PIB' )
col_tip <- c( 'character', 'numeric', 'character', 'numeric')

PIB_ivm<-read_excel(file,sheet=4
                    ,col_names=TRUE,guess_max = 24000)
PIB_ivm <- as.data.table(PIB_ivm )[1:30,]
setnames(PIB_ivm, col_nom )

#Tasa de crecimiento --------------------------------------------
message( '\tLeyendo  PIB IVM del IESS' )
col_nom <- c( 'Anio', 'CrecimientoPIB' )
col_tip <- c( 'character', 'numeric')

crec_PIB_ivm<-read_excel(file,sheet=9
                    ,col_names=TRUE,guess_max = 24000)
crec_PIB_ivm <- as.data.table(crec_PIB_ivm)[1:60,]
setnames(crec_PIB_ivm, col_nom )

#PIB vs. IPC IVM --------------------------------------------------
message( '\tLeyendo  PIB vs. IPC IVM  del IESS' )
col_nom <- c( 'Anio', 'CrecimientoPIB', 'inflacion')
col_tip <- c( 'character', 'numeric', 'numeric')

PIBvsIPC_ivm<-read_excel(file,sheet=5
                    ,col_names=TRUE,guess_max = 24000)
PIBvsIPC_ivm <- as.data.table(PIBvsIPC_ivm )[1:21,]
setnames(PIBvsIPC_ivm, col_nom )

#Pensiones mínimas-------------------------------------------------------

message( '\tLeyendo pensiones minimas IVM  del IESS' )
col_nom <- c( '', '', '2010', '2011', '2012','2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

penmin_ivm<-read_excel(file,sheet=7
                         ,col_names=TRUE,guess_max = 24000)
penmin_ivm <- as.data.table(penmin_ivm )[1:6,]
setnames(penmin_ivm, col_nom )

#Pensiones maximas -------------------------------------------------------

message( '\tLeyendo pensiones maximas IVM  del IESS' )
col_nom <- c( '', '', '2010', '2011', '2012','2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

penmax_ivm<-read_excel(file,sheet=8
                       ,col_names=TRUE,guess_max = 24000)
penmax_ivm <- as.data.table(penmax_ivm )[1:7,]
setnames(penmax_ivm, col_nom )


#Tabla de las hipótesis macroeconomicas -------------------------------------------------------

message( '\tLeyendo tabla hipótesis macroeconómicas IVM  del IESS')
hip_macro<-read_excel(file,sheet='hip_macro',col_names=TRUE,guess_max = 24000)
hip_macro <- as.data.table(hip_macro)

# Guardando ---------------------------------------------------------------

lista <- c('IPC_ivm', 'IPCgraf_ivm', 'SBU_ivm', 'Salprom_ivm', 'PIB_ivm', 'PIBvsIPC_ivm', 'penmin_ivm', 'penmax_ivm', 'crec_PIB_ivm','hip_macro')

save( list=lista,file = paste0( parametros$RData_seg, 'IESS_IVM_variables_macroeconomicas.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()


