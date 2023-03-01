message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo análisis financiero SSC del IESS' )

# Carga de datos
file <- paste0( parametros$Data_seg, 'Grapher/','base_q_sgo_50_.xlsx' )
#######################------------------------   2   (base_q_sgo_50_) -----------------###########################
# Lectura de base_q_sgo__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_q_sgo__female_50_ssc<- read_excel(file,
                                       sheet="base_q_sgo__female_50_",
                                       col_names=TRUE,
                                       guess_max = 24000, range = "B2:AQ108")


# Lectura de base_q_sgo__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_q_sgo__male_50_ssc<- read_excel(file,
                                     sheet="base_q_sgo__male_50_",
                                     col_names=TRUE,
                                     guess_max = 24000, range = "B2:AQ108")



### Nombrar columnas mujer
names_mujer<-data.table(fila1=colnames(base_q_sgo__female_50_ssc),
                        fila2= as.vector(rep(c("m"),dim(base_q_sgo__female_50_ssc)[2])))
names_mujer<-names_mujer[2:dim(base_q_sgo__female_50_ssc)[2]]

names_mujer[,names_mujer:=paste0(fila1,"_",fila2)]
names_mujer<-names_mujer[,3]
names_mujer<-c("Edad",names_mujer$names_mujer)
names(base_q_sgo__female_50_ssc) <- (names_mujer)
base_q_sgo__female_50_ssc<-as.data.table(base_q_sgo__female_50_ssc)
base_q_sgo__female_50_ssc[,`Edad`:= as.numeric(`Edad`)]

### Nombrar columnas hombre
names_hombre<-data.table(fila1=colnames(base_q_sgo__male_50_ssc),
                         fila2= as.vector(rep(c("h"),dim(base_q_sgo__male_50_ssc)[2])))
names_hombre<-names_hombre[2:dim(base_q_sgo__male_50_ssc)[2]]

names_hombre[,names_hombre:=paste0(fila1,"_",fila2)]
names_hombre<-names_hombre[,3]
names_hombre<-c("Edad",names_hombre$names_hombre)
names(base_q_sgo__male_50_ssc) <- (names_hombre)
base_q_sgo__male_50_ssc<-as.data.table(base_q_sgo__male_50_ssc)
base_q_sgo__male_50_ssc[,`Edad`:= as.numeric(`Edad`)]

base_q_sgo_50_ssc<-merge(base_q_sgo__female_50_ssc,base_q_sgo__male_50_ssc,by="Edad")



####---------------------------------------------------------------------------------------------------------------
# Lectura de base_q_sgo__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_q_sgo__female_50_ssc<- read_excel(file,
                                       sheet="base_q_sgo__female_50_",
                                       col_names=TRUE,
                                       guess_max = 24000, range = "B2:AQ108")


# Lectura de base_q_sgo__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_q_sgo__male_50_ssc<- read_excel(file,
                                     sheet="base_q_sgo__male_50_",
                                     col_names=TRUE,
                                     guess_max = 24000, range = "B2:AQ108")






###---------------------- Particion nueva data

aux<-data.frame(rep(2020:2060,106))
colnames(aux)<-"year"
aux %>% group_by(year)
aux<-as.data.frame(aux[order(aux$year),])
colnames(aux)<-"year"

new_data<- base_q_sgo__female_50_ssc[,1]
d<-base_q_sgo__female_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0

for (i in 3:42)
{
  d<-base_q_sgo__female_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}



base_q_sgo__female_50_ssc1<-new_data_f

names(base_q_sgo__female_50_ssc1)<-c("Age","Prob_m")



#---------------------
new_data<- base_q_sgo__male_50_ssc[,1]
d<-base_q_sgo__male_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0


for (i in 3:42)
{
  d<-base_q_sgo__male_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}


base_q_sgo__male_50_ssc1<-new_data_f
names(base_q_sgo__male_50_ssc1)<-c("Age","Prob_h")

base_q_sgo_50_ssc1<-cbind(base_q_sgo__female_50_ssc1,base_q_sgo__male_50_ssc1)
base_q_sgo_50_ssc1<-base_q_sgo_50_ssc1[,c(1,2,4)]
base_q_sgo_50_ssc1<-cbind(aux,base_q_sgo_50_ssc1)







# Carga de datos
file <- paste0( parametros$Data_seg, 'Grapher/','base_qi_50_.xlsx' )
#######################------------------------   2   (base_qi_50_) -----------------###########################
# Lectura de base_qi__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_qi__female_50_ssc<- read_excel(file,
                                    sheet="base_qi__female_50_",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "B2:AQ108")


# Lectura de base_qi__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_qi__male_50_ssc<- read_excel(file,
                                  sheet="base_qi__male_50_",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:AQ108")



### Nombrar columnas mujer
names_mujer<-data.table(fila1=colnames(base_qi__female_50_ssc),
                        fila2= as.vector(rep(c("m"),dim(base_qi__female_50_ssc)[2])))
names_mujer<-names_mujer[2:dim(base_qi__female_50_ssc)[2]]

names_mujer[,names_mujer:=paste0(fila1,"_",fila2)]
names_mujer<-names_mujer[,3]
names_mujer<-c("Edad",names_mujer$names_mujer)
names(base_qi__female_50_ssc) <- (names_mujer)
base_qi__female_50_ssc<-as.data.table(base_qi__female_50_ssc)
base_qi__female_50_ssc[,`Edad`:= as.numeric(`Edad`)]

### Nombrar columnas hombre
names_hombre<-data.table(fila1=colnames(base_qi__male_50_ssc),
                         fila2= as.vector(rep(c("h"),dim(base_qi__male_50_ssc)[2])))
names_hombre<-names_hombre[2:dim(base_qi__male_50_ssc)[2]]

names_hombre[,names_hombre:=paste0(fila1,"_",fila2)]
names_hombre<-names_hombre[,3]
names_hombre<-c("Edad",names_hombre$names_hombre)
names(base_qi__male_50_ssc) <- (names_hombre)
base_qi__male_50_ssc<-as.data.table(base_qi__male_50_ssc)
base_qi__male_50_ssc[,`Edad`:= as.numeric(`Edad`)]

base_qi_50_ssc<-merge(base_qi__female_50_ssc,base_qi__male_50_ssc,by="Edad")



####---------------------------------------------------------------------------------------------------------------
# Lectura de base_qi__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_qi__female_50_ssc<- read_excel(file,
                                    sheet="base_qi__female_50_",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "B2:AQ108")


# Lectura de base_qi__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_qi__male_50_ssc<- read_excel(file,
                                  sheet="base_qi__male_50_",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:AQ108")






###---------------------- Particion nueva data

aux<-data.frame(rep(2020:2060,106))
colnames(aux)<-"year"
aux %>% group_by(year)
aux<-as.data.frame(aux[order(aux$year),])
colnames(aux)<-"year"

new_data<- base_qi__female_50_ssc[,1]
d<-base_qi__female_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0

for (i in 3:42)
{
  d<-base_qi__female_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}



base_qi__female_50_ssc1<-new_data_f

names(base_qi__female_50_ssc1)<-c("Age","Prob_m")



#---------------------
new_data<- base_qi__male_50_ssc[,1]
d<-base_qi__male_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0


for (i in 3:42)
{
  d<-base_qi__male_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}


base_qi__male_50_ssc1<-new_data_f
names(base_qi__male_50_ssc1)<-c("Age","Prob_h")

base_qi_50_ssc1<-cbind(base_qi__female_50_ssc1,base_qi__male_50_ssc1)
base_qi_50_ssc1<-base_qi_50_ssc1[,c(1,2,4)]
base_qi_50_ssc1<-cbind(aux,base_qi_50_ssc1)



# Carga de datos
file <- paste0( parametros$Data_seg, 'Grapher/','base_qo_50_.xlsx' )
#######################------------------------   3   (base_qo_50_) -----------------###########################
# Lectura de base_qo__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_qo__female_50_ssc<- read_excel(file,
                                    sheet="base_qo__female_50_",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "B2:AQ108")


# Lectura de base_qo__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_qo__male_50_ssc<- read_excel(file,
                                  sheet="base_qo__male_50_",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:AQ108")



### Nombrar columnas mujer
names_mujer<-data.table(fila1=colnames(base_qo__female_50_ssc),
                        fila2= as.vector(rep(c("m"),dim(base_qo__female_50_ssc)[2])))
names_mujer<-names_mujer[2:dim(base_qo__female_50_ssc)[2]]

names_mujer[,names_mujer:=paste0(fila1,"_",fila2)]
names_mujer<-names_mujer[,3]
names_mujer<-c("Edad",names_mujer$names_mujer)
names(base_qo__female_50_ssc) <- (names_mujer)
base_qo__female_50_ssc<-as.data.table(base_qo__female_50_ssc)
base_qo__female_50_ssc[,`Edad`:= as.numeric(`Edad`)]

### Nombrar columnas hombre
names_hombre<-data.table(fila1=colnames(base_qo__male_50_ssc),
                         fila2= as.vector(rep(c("h"),dim(base_qo__male_50_ssc)[2])))
names_hombre<-names_hombre[2:dim(base_qo__male_50_ssc)[2]]

names_hombre[,names_hombre:=paste0(fila1,"_",fila2)]
names_hombre<-names_hombre[,3]
names_hombre<-c("Edad",names_hombre$names_hombre)
names(base_qo__male_50_ssc) <- (names_hombre)
base_qo__male_50_ssc<-as.data.table(base_qo__male_50_ssc)
base_qo__male_50_ssc[,`Edad`:= as.numeric(`Edad`)]

base_qo_50_ssc<-merge(base_qo__female_50_ssc,base_qo__male_50_ssc,by="Edad")



####---------------------------------------------------------------------------------------------------------------
# Lectura de base_qo__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_qo__female_50_ssc<- read_excel(file,
                                    sheet="base_qo__female_50_",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "B2:AQ108")


# Lectura de base_qo__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_qo__male_50_ssc<- read_excel(file,
                                  sheet="base_qo__male_50_",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:AQ108")






###---------------------- Particion nueva data

aux<-data.frame(rep(2020:2060,106))
colnames(aux)<-"year"
aux %>% group_by(year)
aux<-as.data.frame(aux[order(aux$year),])
colnames(aux)<-"year"

new_data<- base_qo__female_50_ssc[,1]
d<-base_qo__female_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0

for (i in 3:42)
{
  d<-base_qo__female_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}



base_qo__female_50_ssc1<-new_data_f

names(base_qo__female_50_ssc1)<-c("Age","Prob_m")



#---------------------
new_data<- base_qo__male_50_ssc[,1]
d<-base_qo__male_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0


for (i in 3:42)
{
  d<-base_qo__male_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}


base_qo__male_50_ssc1<-new_data_f
names(base_qo__male_50_ssc1)<-c("Age","Prob_h")

base_qo_50_ssc1<-cbind(base_qo__female_50_ssc1,base_qo__male_50_ssc1)
base_qo_50_ssc1<-base_qo_50_ssc1[,c(1,2,4)]
base_qo_50_ssc1<-cbind(aux,base_qo_50_ssc1)




# Carga de datos
file <- paste0( parametros$Data_seg, 'Grapher/','base_qw_50_.xlsx' )
#######################------------------------   4   (base_qw_50_) -----------------###########################
# Lectura de base_qw__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_qw__female_50_ssc<- read_excel(file,
                                    sheet="base_qw__female_50_",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "B2:AQ108")


# Lectura de base_qw__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_qw__male_50_ssc<- read_excel(file,
                                  sheet="base_qw__male_50_",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:AQ108")



### Nombrar columnas mujer
names_mujer<-data.table(fila1=colnames(base_qw__female_50_ssc),
                        fila2= as.vector(rep(c("m"),dim(base_qw__female_50_ssc)[2])))
names_mujer<-names_mujer[2:dim(base_qw__female_50_ssc)[2]]

names_mujer[,names_mujer:=paste0(fila1,"_",fila2)]
names_mujer<-names_mujer[,3]
names_mujer<-c("Edad",names_mujer$names_mujer)
names(base_qw__female_50_ssc) <- (names_mujer)
base_qw__female_50_ssc<-as.data.table(base_qw__female_50_ssc)
base_qw__female_50_ssc[,`Edad`:= as.numeric(`Edad`)]

### Nombrar columnas hombre
names_hombre<-data.table(fila1=colnames(base_qw__male_50_ssc),
                         fila2= as.vector(rep(c("h"),dim(base_qw__male_50_ssc)[2])))
names_hombre<-names_hombre[2:dim(base_qw__male_50_ssc)[2]]

names_hombre[,names_hombre:=paste0(fila1,"_",fila2)]
names_hombre<-names_hombre[,3]
names_hombre<-c("Edad",names_hombre$names_hombre)
names(base_qw__male_50_ssc) <- (names_hombre)
base_qw__male_50_ssc<-as.data.table(base_qw__male_50_ssc)
base_qw__male_50_ssc[,`Edad`:= as.numeric(`Edad`)]

base_qw_50_ssc<-merge(base_qw__female_50_ssc,base_qw__male_50_ssc,by="Edad")



####---------------------------------------------------------------------------------------------------------------
# Lectura de base_qw__female_50_ ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Mujer' )

base_qw__female_50_ssc<- read_excel(file,
                                    sheet="base_qw__female_50_",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "B2:AQ108")


# Lectura de base_qw__male_50_  ---------------------------------------------------------------------------------
message( '\tLeyendo datos de tasa de mortalidad por edad Hombre' )

base_qw__male_50_ssc<- read_excel(file,
                                  sheet="base_qw__male_50_",
                                  col_names=TRUE,
                                  guess_max = 24000, range = "B2:AQ108")






###---------------------- Particion nueva data

aux<-data.frame(rep(2020:2060,106))
colnames(aux)<-"year"
aux %>% group_by(year)
aux<-as.data.frame(aux[order(aux$year),])
colnames(aux)<-"year"

new_data<- base_qw__female_50_ssc[,1]
d<-base_qw__female_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0

for (i in 3:42)
{
  d<-base_qw__female_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}



base_qw__female_50_ssc1<-new_data_f

names(base_qw__female_50_ssc1)<-c("Age","Prob_m")



#---------------------
new_data<- base_qw__male_50_ssc[,1]
d<-base_qw__male_50_ssc[,2]
new_data_0<-cbind(new_data,d)
new_data_f<-new_data_0


for (i in 3:42)
{
  d<-base_qw__male_50_ssc[,i]
  new_data_0<-cbind(new_data,d)
  colnames(new_data_f)<-names(new_data_0)
  new_data_f<-rbind(new_data_f,new_data_0)
}


base_qw__male_50_ssc1<-new_data_f
names(base_qw__male_50_ssc1)<-c("Age","Prob_h")

base_qw_50_ssc1<-cbind(base_qw__female_50_ssc1,base_qw__male_50_ssc1)
base_qw_50_ssc1<-base_qw_50_ssc1[,c(1,2,4)]
base_qw_50_ssc1<-cbind(aux,base_qw_50_ssc1)















#-----------------------------
lista<-c('base_q_sgo_50_ssc','base_q_sgo_50_ssc1'
         ,'base_qi_50_ssc','base_qi_50_ssc', 'base_qo_50_ssc','base_qo_50_ssc', 'base_qw_50_ssc','base_qw_50_ssc')

save( list=lista,
      file = paste0( parametros$RData_seg, 'tasa_de_mortalidad.RData' ) )


remove("aux","new_data","new_data_0","new_data_f","base_q_sgo__male_50_ssc1","base_q_sgo__female_50_ssc1"
       ,"base_qi__female_50_ssc1","base_qi__male_50_ssc1","base_qo__female_50_ssc1","base_qo__male_50_ssc1","base_qw__female_50_ssc1","base_qw__male_50_ssc1")
