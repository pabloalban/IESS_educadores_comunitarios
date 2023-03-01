message( paste( rep('-', 100 ), collapse = '' ) )

var_num <- function( A, col_ini ){
  A <- as.data.frame( A )
  for (i in col_ini:dim(A)[2]) {
    A[ , i ] <- as.numeric( A[ , i ])
  }
  return( data.frame( A ) )
}

message( '\tLeyendo recopilación de información ILO/PENSIONS' )
##link descargar
# https://www.cepal.org/es/temas/proyecciones-demograficas/estimaciones-proyecciones-excel

# Context: Demographic -----------------------------------------------------------------------------
message( '\t\tContext: Demographic ' )
dir_lec <- 'Input/Escenario_Base/Context Demographic, Economic, and Labour Market/Demographic and Labour Market/Fuentes Externas/'
file <- paste0( parametros$Data_seg, dir_lec,'08_ECU.xlsx' )

pob_ecu <-read_excel( file,
                      sheet="poblacio  urbana-rural",
                      col_names=TRUE,
                      guess_max = 24000 )
pob_ecu  <- as.data.table( pob_ecu )
colnom <- c( 'sexo',t( pob_ecu[ 6, -1 ] ))
ind <- which( pob_ecu[ , 2] =='Población Rural /  Rural  population')
pob_rural <- rbind( pob_ecu[ind+25], pob_ecu[ind+48])
setnames(pob_rural, colnom)

pea_ecu <-read_excel( file,
                      sheet="Poblacion econo activa",
                      col_names=TRUE,
                      guess_max = 24000 )
pea_ecu  <- as.data.table( pea_ecu )
colnom <- c( 'sexo',t( pea_ecu[ 6, -1 ] ))
ind <- which( pea_ecu[ , 2] =='Población económicamente activa rural /  Rural economically active population')
pea_rural <- rbind( pea_ecu[ind+24], pea_ecu[ind+45])
setnames(pea_rural, colnom)

pob_rural <- as.data.table( dcast( melt(pob_rural, id.vars='sexo', variable.name = 'anio',
                                        value.var = 'num'), anio~sexo))
pob_rural[ , "Hombres/ Males" :=as.numeric(`Hombres/ Males`)]
pob_rural[ , "Mujeres/ Females" :=as.numeric(`Mujeres/ Females`)]
pob_rural[ , anio :=as.numeric(as.character(anio))]
pob_rural_mol <- pob_rural[ anio > parametros$anio & anio <= parametros$anio+parametros$horizonte ]
pob_rural_hist <- pob_rural[ anio >= year(parametros$fec_ini) & anio <= parametros$anio ]

pea_rural <- as.data.table( dcast( melt(pea_rural, id.vars='sexo', variable.name = 'anio',
                                        value.var = 'num'), anio~sexo))
pea_rural[ , "Hombres/ Males" :=as.numeric(`Hombres/ Males`)]
pea_rural[ , "Mujeres/ Females" :=as.numeric(`Mujeres/ Females`)]
pea_rural[ , anio :=as.numeric(as.character(anio))]
pea_rural_mol <- pea_rural[ anio > parametros$anio & anio <= parametros$anio+parametros$horizonte ]
pea_rural_hist <- pea_rural[ anio >= year(parametros$fec_ini) & anio <= parametros$anio ]

colnom <- c( 'sexo',t( pob_ecu[ 6, -1 ] ))
ind <- which( pob_ecu[ , 2] =='Población Rural /  Rural  population')
ini <- ind+38
fin <- ind+46
pob_rural_60_m <- pob_ecu[ini:fin]
pob_rural_60_m <- as.data.table( var_num( pob_rural_60_m, 2) )
pob_rural_60_m <- t(c(sexo='Hombres/ Males', colSums(pob_rural_60_m[,2:dim(pob_rural_60_m)[2]]) ))
colnames(pob_rural_60_m) <- colnom

ini <- ind+61
fin <- ind+69
pob_rural_60_f <- pob_ecu[ini:fin]
pob_rural_60_f <- as.data.table( var_num( pob_rural_60_f, 2) )
pob_rural_60_f <- t(c(sexo='Mujeres/ Females', colSums(pob_rural_60_f[,2:dim(pob_rural_60_f)[2]]) ))
colnames(pob_rural_60_f) <- colnom

pob_rural_60 <- data.table( rbind( pob_rural_60_m, pob_rural_60_f ))
pob_rural_60 <- as.data.table( var_num( pob_rural_60, 2) )
setnames(pob_rural_60, colnom)

pob_rural_60 <- as.data.table( dcast( melt(pob_rural_60, id.vars='sexo', variable.name = 'anio',
                                           value.var = 'num'), anio~sexo))

pob_rural_60[ , "Hombres/ Males" :=as.numeric(`Hombres/ Males`)]
pob_rural_60[ , "Mujeres/ Females" :=as.numeric(`Mujeres/ Females`)]
pob_rural_60[ , anio :=as.numeric(as.character(anio))]
pob_rural_60_mol <- pob_rural_60[ anio > parametros$anio & anio <= parametros$anio+parametros$horizonte ]
pob_rural_60_hist <- pob_rural_60[anio >= year(parametros$fec_ini) & anio <= parametros$anio]

ini <- ind+39
fin <- ind+46
pob_rural_65_m <- pob_ecu[ini:fin]
pob_rural_65_m <- as.data.table( var_num( pob_rural_65_m, 2) )
pob_rural_65_m <- t(c(sexo='Hombres/ Males', colSums(pob_rural_65_m[,2:dim(pob_rural_65_m)[2]]) ))
colnames(pob_rural_65_m) <- colnom

ini <- ind+62
fin <- ind+69
pob_rural_65_f <- pob_ecu[ini:fin]
pob_rural_65_f <- as.data.table( var_num( pob_rural_65_f, 2) )
pob_rural_65_f <- t(c(sexo='Mujeres/ Females', colSums(pob_rural_65_f[,2:dim(pob_rural_65_f)[2]]) ))
colnames(pob_rural_65_f) <- colnom

pob_rural_65 <- data.table( rbind( pob_rural_65_m, pob_rural_65_f ))
pob_rural_65 <- as.data.table( var_num( pob_rural_65, 2) )
setnames(pob_rural_65, colnom)

pob_rural_65 <- as.data.table( dcast( melt(pob_rural_65, id.vars='sexo', variable.name = 'anio',
                                           value.var = 'num'), anio~sexo))

pob_rural_65[ , "Hombres/ Males" :=as.numeric(`Hombres/ Males`)]
pob_rural_65[ , "Mujeres/ Females" :=as.numeric(`Mujeres/ Females`)]
pob_rural_65[ , anio :=as.numeric(as.character(anio))]
pob_rural_65_mol <- pob_rural_65[ anio > parametros$anio & anio <= parametros$anio+parametros$horizonte ]
pob_rural_65_hist <- pob_rural_65[anio >= year(parametros$fec_ini) & anio <= parametros$anio  ]

# Recopilación de información mercado laboral
##link descargar
#https://www.ecuadorencifras.gob.ec/empleo-agosto-2021/
dir_lec <- 'Input/Escenario_Base/Context Demographic, Economic, and Labour Market/Demographic and Labour Market/Fuentes Externas/'
file <- paste0( parametros$Data_seg, dir_lec,'Tabulados_Mercado_Laboral.xlsx' )
tasa_desemp <-read_excel( file,
                          sheet="2. Tasas",
                          col_names=TRUE,
                          guess_max = 24000 )
tasa_desemp<- as.data.table( tasa_desemp )
col_nom<-c('encuesta','periodo','indicador','nacional','urbana','rural','hombre','mujer')
setnames(tasa_desemp,col_nom)

aux<-tasa_desemp[  periodo %in% c("dic-07","dic-08","dic-09","dic-10","dic-11","dic-12",
                                  "dic-13","dic-14","dic-15","dic-16","dic-17","dic-18",
                                  "dic-19","dic-20","dic-21") ]
tasa_desemp <-aux[indicador=="Desempleo (%)"]
tasa_desemp_hist <- tasa_desemp[ periodo %in% c("dic-12",
                                                "dic-13","dic-14","dic-15","dic-16","dic-17","dic-18",
                                                "dic-19","dic-20") ] 


# Matriz [NATPOP] National population (s,t). -------------------------------------------------------
message( '\t\t[NATPOP] National population (s,t)' )
dir_lec_int <- 'Input/Escenario_Base/Context Demographic, Economic, and Labour Market/Demographic and Labour Market/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_natpop_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'NATPOP', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'NATPOP', pob_rural_mol[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="NATPOP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )


write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_natpop_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'NATPOP', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'NATPOP', pob_rural_mol[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="NATPOP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [POP65OVER] Population aged 65 and over (s,t).---------------------------------------------
message( '\t\t[POP65OVER] Population aged 65 and over (s,t).' )

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_pop65over_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'POP65OVER', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'POP65OVER', pob_rural_65_mol[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="POP65OVER",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_pop65over_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'POP65OVER', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'POP65OVER', pob_rural_65_mol[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="POP65OVER",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [POPACT] Active national population (s,t).-------------------------------------------------
message( '\t\t[POPACT] Active national population (s,t)' )
file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_popact_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'POPACT', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'POPACT', pea_rural_mol[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="POPACT",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_popact_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'POPACT', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'POPACT', pea_rural_mol[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="POPACT",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )


write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [POP60OVER] Population aged 60 and over (s,t).---------------------------------------------
message( '\t\t[POP60OVER] Population aged 60 and over (s,t).' )

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_pop60over_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'POP60OVER', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'POP60OVER', pob_rural_60_mol[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="POP60OVER",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_pop60over_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'POP60OVER', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'POP60OVER', pob_rural_60_mol[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="POP60OVER",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [Partr] Participation rate in the labour force (s,t). -------------------------------------
message( '\t\t[Partr] Participation rate in the labour force (s,t)' )
part <- merge( pob_rural, pea_rural, by=c('anio') )
part[  , Hombres:= `Hombres/ Males.y`/`Hombres/ Males.x`]
part[  , Mujeres:= `Mujeres/ Females.y`/`Mujeres/ Females.x`]
part_mol <- part[ anio > parametros$anio & anio <= parametros$anio+parametros$horizonte, 
                  list( anio,  Hombres, Mujeres ) ]
part_hist <- part[ anio >= year(parametros$fec_ini) & anio <= parametros$anio, 
                   list( anio,  Hombres, Mujeres ) ]


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_partr_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'Partr', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'Partr', part_mol[ , "Hombres"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="Partr",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_partr_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'Partr', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'Partr', part_mol[ , "Mujeres"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="Partr",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [unemrate] Unemployment rate (s,t). -------------------------------------------------------
message( '\t\t[unemrate] Unemployment rate (s,t)' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_unemrate_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'unemrate', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData(wb, 'unemrate', data.table(rep(median(as.numeric(tasa_desemp$rural))*0.01,parametros$horizonte)), colNames = FALSE,
          startCol = 3,
          startRow = 3,)
aux_des <- data.table( anio = seq(parametros$anio+1, parametros$anio+parametros$horizonte ),
                       des=rep(median(as.numeric(tasa_desemp$rural))*0.01,parametros$horizonte))

saveWorkbook(wb, file_wr, overwrite = T)


aux <- read_excel( file_wr,
                   sheet="unemrate",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_unemrate_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'unemrate', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData(wb, 'unemrate', data.table(rep(median(as.numeric(tasa_desemp$rural))*0.01,parametros$horizonte)), colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="unemrate",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [cov] Coverage rate as percentage of employed labour force (s,g,t).------------------------
message( '\t\t[cov] Coverage rate as percentage of employed labour force (s,g,t)' )
#Proyecciones SGO
file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_tact_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )
afi_male <- read_excel( file_wr,
                        sheet="Tact",
                        col_names=FALSE,
                        guess_max = 24000 )
afi_male <- as.data.table( afi_male)
afi_male <- afi_male[ -c(1:2), -1]
afi_male <- data.table( var_num( afi_male, 2))
setnames(afi_male, c("anio", "Male")) 

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_tact_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )
afi_female <- read_excel( file_wr,
                          sheet="Tact",
                          col_names=FALSE,
                          guess_max = 24000 )
afi_female <- as.data.table( afi_female )
afi_female <- afi_female[ -c(1:2), -1]
afi_female <- data.table( var_num( afi_female, 2))
setnames(afi_female, c("anio", "Female")) 

afi_sgo <- as.data.table( merge( afi_male, afi_female) )
afi_sgo[ , Total:= Male + Female ]
afi_sgo[ , s_total:= shift(Total, 1, "lag", fill=NA) ]
afi_sgo[ , vari:= Total/s_total - 1 ]

ssc_ini <- 405769 # Número de jefes de familia en todo el año 2020
afi_sgo[ anio==parametros$anio , cre:= 1   ]
afi_sgo[ anio!=parametros$anio, cre:= cumprod( 1 + vari )   ]
afi_sgo[ , ssc:= cre * ssc_ini   ]

pond <- 0.670337902971703 - 0.04 #Promedio geometrico histórico del porcentaje de mujeres sobre el total
cov_fem <- data.table( female = afi_sgo[anio> parametros$anio & anio<= parametros$anio_fin]$ssc*(1-pond)/(pob_rural_mol$`Mujeres/ Females`* part_mol$Mujeres*(1-aux_des$des)))
cov_mas <- data.table( male = afi_sgo[anio> parametros$anio & anio<= parametros$anio_fin]$ssc*pond/(pob_rural_mol$`Hombres/ Males`* part_mol$Hombres*(1-aux_des$des)))

cov <- cbind(anio=seq(parametros$anio+1,parametros$anio_fin ), cov_fem,cov_mas)

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_cov_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'cov', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData(wb, 'cov', cov[ , "male"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="cov",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_cov_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'cov', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData(wb, 'cov', cov[ , "female"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="cov",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

#Cálculo mediante suavizamiento
#Hombres
cov <- data.table( t = 2021:2040, pob = c(
  0.141408825914765,
  0.141887834,
  0.142100833,
  0.144208245,
  0.146371914,
  NA,
  NA,
  NA,
  NA,
  NA,
  0.17,
  NA,
  NA,
  NA, NA, NA,NA,NA,
  NA,
  0.19574086)
)


cov_sua <- data.table( t = 2021:2040 )
ud_smooth_model_f <- lm(  pob ~ bs( t, df= 4, degree = 3
                                    #, knots = c( 2021.5)
), 
data = cov[  is.finite( pob )] )

summary(ud_smooth_model_f)
cov_sua[ , cov_s := predict( object = ud_smooth_model_f,newdata = cov_sua ) ]

# plot(cov$t, cov$pob, ylim = c( 0,0.25));points(cov_sua$t, cov_sua$cov_s, col="red", type = "l")
# plot(cov_sua$t, cov_sua$cov_s, col="red")
# 
# write.xlsx( cov_sua, paste0( parametros$Data_seg, 'sua_cov_male.xlsx') )

#Mujeres
cov <- data.table( t = 2021:2040, pob = c( 0.137020162285628,
                                           0.139904333443535,
                                           0.142868571066076,
                                           0.146155428025113,
                                           0.14950320859809,
                                           0.152716016940904,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           NA,
                                           0.159, NA,NA,NA,
                                           NA,
                                           NA,
                                           NA, 
                                           0.1659128194740434
)
)


cov_sua <- data.table( t = 2021:2040 )
ud_smooth_model_f <- lm(  pob ~ bs( t, df= 3, degree = 3
                                    #, knots = c( 2020.5)
), 
data = cov[  is.finite( pob )] )

summary(ud_smooth_model_f)
cov_sua[ , cov_s := predict( object = ud_smooth_model_f,newdata = cov_sua ) ]

# plot(cov$t, cov$pob, ylim = c( 0,0.25));points(cov_sua$t, cov_sua$cov_s, col="red", type = "l")
# plot(cov_sua$t, cov_sua$cov_s, col="red")
# 
# write.xlsx( cov_sua, paste0( parametros$Data_seg, 'sua_cov_female.xlsx') )

# Context: Economic  -------------------------------------------------------------------------------
message( '\t\tContext: Economic ' )
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )

dir_lec_int <- 'Input/Escenario_Base/Context Demographic, Economic, and Labour Market/Economic/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# Matriz [gdp] GDP Growth rate (t).-----------------------------------------------------------------
message( '\t\t[gdp] GDP Growth rate (t)' )
hip_macro_cre <- as.data.table( copy( tasas_macro_crec ) )
hip_macro_cre <- data.table( rbind( data.frame( anio=2020, t_pib=NA, t_sal=NA, t_sbu=NA, ta=NA, tp=NA,
                                                inf=NA), hip_macro_cre ) )
hip_macro_cre[ anio== 2020 | anio== 2021, t_pib:= 0.0181]

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_gdp_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'gdp', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'gdp', hip_macro_cre[ anio >= parametros$anio & anio <= parametros$anio + parametros$horizonte , "t_pib"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="gdp",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [inf] Inflation rate past and future (t).--------------------------------------------------
message( '\t\t[inf] Inflation rate past and future (t).' )
# hip_macro_prod <- as.data.table( copy( tasas_macro_pred ) )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_inf_")==T & str_detect( arch, ".xlsx")==T))] )

# wb <- loadWorkbook(file_wr)
# deleteData(wb, sheet = 'inf', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)
# 
# writeData(wb, 'inf', hip_macro_prod[ anio >= 2010 & anio <= parametros$anio + parametros$horizonte , "inflación_prom"], colNames = FALSE,
#           startCol = 3,
#           startRow = 3,)
# saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="inf",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [maxretpen] Maximum old-age pension amount (t).--------------------------------------------
dir_lec_int <- 'Input/Escenario_Base/Statutory Rules/Benefit Formulas/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()
message( '\t\t[maxretpen] Maximum old-age pension amount (t)' )
cre_sbu <- as.data.table( copy( tasas_macro_crec ) )
cre_sbu <- data.table( rbind( data.frame( anio = c(2018,2019,2020),
                                          t_pib = c(NA, NA, NA),
                                          t_sal = c(NA, NA, NA),
                                          t_sbu =c(NA, NA, NA),
                                          ta = c(NA, NA, NA),
                                          tp = c(NA, NA, NA),
                                          inf = c(NA, NA, NA)),
                              cre_sbu ) )

cre_sbu[ anio==2018, t_sbu:=0]
cre_sbu[ anio==2019, t_sbu:= 394/386 - 1 ]
cre_sbu[ anio==2020, t_sbu:= 400/394 - 1]
cre_sbu[ anio==2021, t_sbu:= 400/400 - 1 ]
cre_sbu[ , pen_ssc:=  100 * cumprod( 1 + t_sbu) ]
cre_sbu[ , sbu:=  386 * cumprod( 1 + t_sbu) ]

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_maxretpen_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'maxretpen', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'maxretpen', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "pen_ssc"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="maxretpen",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# message( '\t\t[minretpen] Minimum old-age pension amount (t)' )
# write.table(aux, str_replace_all( str_replace_all( file_wr, "maxretpen", "minretpen"),  "xlsx","\\csv"),
#             na = "", quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [flatret] Flat amount component of the old-age benefit (t).--------------------------------
message( '\t\t[flatret] Flat amount component of the old-age benefit (t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_flatret_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'flatret', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'flatret', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "pen_ssc"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="flatret",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [maxdispen] Maximum disability pension amount (t).-----------------------------------------
message( '\t\t[maxdispen] Maximum disability pension amount (t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_maxdispen_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'maxdispen', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'maxdispen', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "pen_ssc"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="maxdispen",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [flatdis] Flat amount component of the disability pension benefit (t).---------------------
message( '\t\t[flatdis] Flat amount component of the disability pension benefit (t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_flatdis_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'flatdis', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'flatdis', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "pen_ssc"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="flatdis",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [maxsurvpen] Maximum survivorship pension amount (t).--------------------------------------
message( '\t\t[maxsurvpen] Maximum survivorship pension amount (t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_maxsurvpen_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'maxsurvpen', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

cre_sbu[ , viu:= 0.40*pen_ssc]
cre_sbu[ , orf_20:= 0.20*pen_ssc]
cre_sbu[ , orf_75:= 0.75*0.225*sbu]
cre_sbu[ , max_mont:= pmax(viu, orf_20,  orf_75)]
cre_sbu[ , min_mont:= pmin(viu, orf_20,  orf_75)]

writeData(wb, 'maxsurvpen', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "max_mont"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="maxsurvpen",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [minsurvpen] Minimum survivorship pension amount (t).--------------------------------------
message( '\t\t[minsurvpen] Minimum survivorship pension amount (t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_minsurvpen_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'minsurvpen', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

cre_sbu[ , viu:= 0.40*pen_ssc]
cre_sbu[ , orf_20:= 0.20*pen_ssc]
cre_sbu[ , orf_75:= 0.75*0.225*sbu]
cre_sbu[ , max_mont:= pmax(viu, orf_20,  orf_75)]
cre_sbu[ , min_mont:= pmin(viu, orf_20,  orf_75)]

writeData(wb, 'minsurvpen', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "min_mont"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="minsurvpen",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [flatsurv] Flat amount component of the survivors benefit (t).----------------------------
message( '\t\t[flatsurv] Flat amount component of the survivors benefit (t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_flatsurv_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'flatsurv', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'flatsurv', cre_sbu[ anio > parametros$anio & anio <= parametros$anio + parametros$horizonte , "max_mont"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="flatsurv",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [asg] Salary growth rate assumption (g,t).-------------------------------------------------
dir_lec_int <- 'Input/Escenario_Base/Financial/Financial Assumptions/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

message( '\t\t[asg] Salary growth rate assumption (g,t).' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_asg_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'asg', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'asg', cre_sbu[ anio >= parametros$anio & anio <= parametros$anio + parametros$horizonte , "t_sbu"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="asg",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [adjben] Benefit adjustment rate (t). -----------------------------------------------------
message( '\t\t[adjben] Benefit adjustment rate (t)' )

file_wr <-paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_adjben_")==T & str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'adjben', cols = 3, rows = 3:c(3+parametros$horizonte), gridExpand = TRUE)

writeData(wb, 'adjben', cre_sbu[ anio >= parametros$anio & anio <= parametros$anio + parametros$horizonte , "t_sbu"], colNames = FALSE,
          startCol = 3,
          startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="adjben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [HTP] Total national population (s,t). ----------------------------------------------------
message( '\t\t[HTP] Total national population (s,t).' )
dir_lec_int <- 'Input/Escenario_Base/Historical/Historical Demographic/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_htp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HTP', cols = 3, rows = 3:11, gridExpand = TRUE)

writeData( wb, 'HTP', pob_rural_hist[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HTP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )


write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_htp_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HTP', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HTP', pob_rural_hist[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HTP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [HRAP60] Population aged 60 and over (s,t).------------------------------------------------
message( '\t\t[HRAP60] Population aged 60 and over (s,t).' )

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hrap60_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HRAP60', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HRAP60', pob_rural_60_hist[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HRAP60",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hrap60_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HRAP60', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HRAP60', pob_rural_60_hist[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HRAP60",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [HRAP65] Population aged 65 and over (s,t).------------------------------------------------
message( '\t\t[HRAP65] Population aged 65 and over (s,t).' )

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hrap65_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HRAP65', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HRAP65', pob_rural_65_hist[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HRAP65",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hrap65_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HRAP65', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HRAP65', pob_rural_65_hist[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HRAP65",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"),na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# Matriz [HWEP] Working age population (s,t).-------------------------------------------------------
message( '\t\t[HWEP] Working age population (s,t).' )
file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hwep_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HWEP', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HWEP', pea_rural_hist[ , "Hombres/ Males"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HWEP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hwep_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HWEP', cols = 3, rows = 3:c(3+parametros$horizonte-1), gridExpand = TRUE)

writeData( wb, 'HWEP', pea_rural_hist[ , "Mujeres/ Females"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HWEP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )


write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# Matriz [HLF] Employed labour force (s,t).---------------------------------------------------------
message( '\t\t[HLF] Employed labour force (s,t).' )

emplo_hist <- merge( pob_rural_hist, part_hist , by.x=c('anio'), by.y=c('anio') )
emplo_hist[ , emp_hom:= `Hombres/ Males`* Hombres * (1 - as.numeric(tasa_desemp_hist$rural)/100)]
emplo_hist[ , emp_muj:= `Mujeres/ Females`* Mujeres * (1 - as.numeric(tasa_desemp_hist$rural)/100)]

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hlf_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HLF', cols = 3, rows = 3:11, gridExpand = TRUE)

writeData( wb, 'HLF', emplo_hist[ , "emp_hom"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HLF",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "_hlf_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
deleteData(wb, sheet = 'HLF', cols = 3, rows = 3:11, gridExpand = TRUE)

writeData( wb, 'HLF', emplo_hist[ , "emp_muj"], colNames = FALSE,
           startCol = 3,
           startRow = 3,)
saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="HLF",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )


write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

setwd( parametros$work_dir )

message( paste( rep('-', 100 ), collapse = '' ) )
gc()


