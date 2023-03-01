message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo agregados proyectados demograficos TNRH' )

col_nom <- c("anio","fuerza_laboral_empleada","activos_contribuyentes","retiro1","discapacidad1","viudedad","orfandad","total1",
             "retiro2","discapacidad2","muerte","total2")
#--------------------------------------------------------------------------------------------------------------------------------------
#Escenario base
file<-paste0(parametros$Data_seg, 'IESS_TNRH_agregado_demografico_escenario_1.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)

#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_demografico_escenario_1.RData' ) )

#Escenario 2
file<-paste0(parametros$Data_seg, 'IESS_TNRH_agregado_demografico_escenario_2.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)

#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_demografico_escenario_2.RData' ) )


#Escenario 3
file<-paste0(parametros$Data_seg, 'IESS_TNRH_agregado_demografico_escenario_3.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)

#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_demografico_escenario_3.RData' ) )