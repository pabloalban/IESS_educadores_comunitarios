message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo agregados proyectados demograficos IVM' )

col_nom <- c("anio","fuerza_laboral_empleada","activos_contribuyentes","retiro1","discapacidad1","viudedad","orfandad","total1",
             "retiro2","discapacidad2","muerte","total2")
#--------------------------------------------------------------------------------------------------------------------------------------
#Escenario base
file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_1.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)

#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_1.RData' ) )

#Escenario pesimista

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_2.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)


#Masculino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_2.RData' ) )

#Escenario alternativo

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_3.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)


#Masculino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)



save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_3.RData' ) )

#Escenario reforma1

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_4.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)


#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_4.RData' ) )

#Escenario reforma2

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_5.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)


#Masculino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)

save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_5.RData' ) )

#Escenario reforma3

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_6.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)

#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_6.RData' ) )

#Escenario reforma4

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_7.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)


#Masculino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_7.RData' ) )

#Escenario reforma5

file<-paste0(parametros$Data_seg, 'IESS_IVM_agregado_demografico_escenario_8.xlsx' )
column_types <- rep("numeric", 12)

#Femenino
agregado_demografico<- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_F<-as.data.table(agregado_demografico[4:43,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_F,col_nom)


#Masculino
agregado_demografico <- read_excel(file,sheet = 1,col_types =column_types) 
agregado_demografico_M<-as.data.table(agregado_demografico[46:85,1:ncol(agregado_demografico)]) 
setnames(agregado_demografico_M,col_nom)


save(agregado_demografico_F,agregado_demografico_M, file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_escenario_.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
