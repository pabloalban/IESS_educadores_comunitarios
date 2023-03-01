message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo agregados proyectados financieros TNRH' )

col_nom <- c("anio","masa_salarial", "contribuciones", "intereses","Otros","total1","beneficios_vejez","beneficios_invalidez",
             "beneficios_viudedad","beneficios_orfandad","lumpsum","total2","gastos_administrativos","gastos_otros","total3",
             "resultados","paygr","reserva","coeficiente_reserva")
#--------------------------------------------------------------------------------------------------------------------------------------
#Escenario 1 0%
file<-paste0(parametros$Data_seg, 'IESS_TNRH_agregado_financiero_escenario_1.xlsx' )
column_types <- rep("numeric", 19)

agregado_financiero <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_financiero<-rbind(rep(0,ncol(agregado_financiero)),agregado_financiero)
agregado_financiero<-as.data.table(agregado_financiero[c(1,5:44),1:ncol(agregado_financiero)]) 

#agregado_financiero<-as.data.table(agregado_financiero[4:43,2:ncol(agregado_financiero)]) 
setnames(agregado_financiero,col_nom)
save(agregado_financiero, file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_financiero_escenario_1.RData' ) )

#Escenario 2 31%
file<-paste0(parametros$Data_seg, 'IESS_TNRH_agregado_financiero_escenario_2.xlsx' )
column_types <- rep("numeric", 19)

agregado_financiero <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_financiero<-rbind(rep(0,ncol(agregado_financiero)),agregado_financiero)
agregado_financiero<-as.data.table(agregado_financiero[c(1,5:44),1:ncol(agregado_financiero)]) 

#agregado_financiero<-as.data.table(agregado_financiero[4:43,2:ncol(agregado_financiero)]) 
setnames(agregado_financiero,col_nom)
save(agregado_financiero, file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_financiero_escenario_2.RData' ) )

#Escenario 3 40%
file<-paste0(parametros$Data_seg, 'IESS_TNRH_agregado_financiero_escenario_3.xlsx' )
column_types <- rep("numeric", 19)

agregado_financiero <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_financiero<-rbind(rep(0,ncol(agregado_financiero)),agregado_financiero)
agregado_financiero<-as.data.table(agregado_financiero[c(1,5:44),1:ncol(agregado_financiero)]) 

#agregado_financiero<-as.data.table(agregado_financiero[4:43,2:ncol(agregado_financiero)]) 
setnames(agregado_financiero,col_nom)
save(agregado_financiero, file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_financiero_escenario_3.RData' ) )
