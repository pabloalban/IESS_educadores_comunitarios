message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo recopilación de información otros colaboradores' )

dir_lec_int <- 'Input/Escenario_Base/Demographic/Base Year/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# Modificaciones del Icact para correcto carga en ILO/PENSIONS -------------------------------------
icact_fem <-read_excel( 'Icact_Prev_Female_KT.xlsx',
                           sheet=1,
                           col_names=TRUE,
                           guess_max = 24000 )

icact_fem <- as.data.table( icact_fem )
icact_fem <- icact_fem[ Edad<=80]

pri_tip <- data.table(nombre= as.character(seq(1, 792)))
posi <- NULL
nom <- NULL
for (i in 1:length(pri_tip$nombre)) {
  posi[i]<- ifelse( pri_tip$nombre[i]%in%colnames(icact_fem) ==F, i, 0)
  nom[i] <- ifelse( pri_tip$nombre[i]%in%colnames(icact_fem) ==F, pri_tip$nombre[i], "" )
}
posi <- posi[posi!=0] 
nom <- nom[nom!=""] 

completacion <- data.frame( matrix( 0, nrow = dim(icact_fem)[1], ncol = length(posi) ) )
colnames( completacion ) <- nom

if( dim(icact_fem)[2]== dim(pri_tip)[1]){
  icact_fem <- data.table( icact_fem )
}else{
  icact_fem <- data.table( cbind( icact_fem, completacion) )
}

icact_fem[is.na(icact_fem)] <- 0

icact_fem <- icact_fem[ , c(as.character(seq(1, 792)))]

icact_mal <-read_excel( 'Icact_Prev_Male_KT.xlsx',
                        sheet=1,
                        col_names=TRUE,
                        guess_max = 24000 )

icact_mal <- as.data.table( icact_mal )
icact_mal <- icact_mal[ Edad<=80]

pri_tip <- data.table(nombre= as.character(seq(1, 792)))
posi <- NULL
nom <- NULL
for (i in 1:length(pri_tip$nombre)) {
  posi[i]<- ifelse( pri_tip$nombre[i]%in%colnames(icact_mal) ==F, i, 0)
  nom[i] <- ifelse( pri_tip$nombre[i]%in%colnames(icact_mal) ==F, pri_tip$nombre[i], "" )
}
posi <- posi[posi!=0] 
nom <- nom[nom!=""] 

completacion <- data.frame( matrix( 0, nrow = dim(icact_mal)[1], ncol = length(posi) ) )
colnames( completacion ) <- nom

if( dim(icact_mal)[2]== dim(pri_tip)[1]){
  icact_mal <- data.table( icact_mal )
}else{
  icact_mal <- data.table( cbind( icact_mal, completacion) )
}

icact_mal[is.na(icact_mal)] <- 0
icact_mal <- icact_mal[ , c(as.character(seq(1, 792)))]

# Modificaciones del ICinact para correcto carga en ILO/PENSIONS -----------------------------------
icinact_fem <-read_excel( 'Icinact_Prev_Female_KT.xlsx',
                        sheet=1,
                        col_names=TRUE,
                        guess_max = 24000 )

icinact_fem <- as.data.table( icinact_fem )
icinact_fem <- icinact_fem[ Edad<=80]

pri_tip <- data.table( nombre= as.character(seq(1, 780)))
posi <- NULL
nom <- NULL
for (i in 1:length(pri_tip$nombre)) {
  posi[i]<- ifelse( pri_tip$nombre[i]%in%colnames(icinact_fem) ==F, i, 0)
  nom[i] <- ifelse( pri_tip$nombre[i]%in%colnames(icinact_fem) ==F, pri_tip$nombre[i], "" )
}
posi <- posi[posi!=0] 
nom <- nom[nom!=""] 

completacion <- data.frame( matrix( 0, nrow = dim(icinact_fem)[1], ncol = length(posi) ) )
colnames( completacion ) <- nom

if( dim(icinact_fem)[2]== dim(pri_tip)[1]){
  icinact_fem <- data.table( icinact_fem )
}else{
  icinact_fem <- data.table( cbind( icinact_fem, completacion) )
}

icinact_fem[is.na(icinact_fem)] <- 0
icinact_fem <- icinact_fem[ , c(as.character(seq(1, 780)))] 

icinact_mal <-read_excel( 'Icinact_Prev_Male_KT.xlsx',
                        sheet=1,
                        col_names=TRUE,
                        guess_max = 24000 )

icinact_mal <- as.data.table( icinact_mal )
icinact_mal <- icinact_mal[ Edad<=80]

pri_tip <- data.table(nombre= as.character(seq(1, 780)))
posi <- NULL
nom <- NULL
for (i in 1:length(pri_tip$nombre)) {
  posi[i]<- ifelse( pri_tip$nombre[i]%in%colnames(icinact_mal) ==F, i, 0)
  nom[i] <- ifelse( pri_tip$nombre[i]%in%colnames(icinact_mal) ==F, pri_tip$nombre[i], "" )
}
posi <- posi[posi!=0] 
nom <- nom[nom!=""] 

completacion <- data.frame( matrix( 0, nrow = dim(icinact_mal)[1], ncol = length(posi) ) )
colnames( completacion ) <- nom

if( dim(icinact_mal)[2]== dim(pri_tip)[1]){
  icinact_mal <- data.table( icinact_mal )
}else{
  icinact_mal <- data.table( cbind( icinact_mal, completacion) )
}

icinact_mal[is.na(icinact_mal)] <- 0
icinact_mal <- icinact_mal[ , c(as.character(seq(1, 780)))]

# [ICact] Distribution of past credits (in months)  for the initial cohort of active contributors (s,g,x,c)-----------------------------------------
message( '\t\t[ICact] Distribution of past credits (in months)  for the initial cohort of active contributors (s,g,x,c).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_icact_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'ICact', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'ICact', icact_fem, colNames = FALSE,
          startCol = 3,
          startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="ICact",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_icact_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'ICact', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'ICact', icact_mal, colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="ICact",
                   col_names=FALSE,
                   guess_max = 24000 )

aux  <- as.data.table( aux )
write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [ICinact] Distribution of past credits (in months) for the initial cohort of inactive contributors (s,x,c)
message( '\t\t[ICinact] Distribution of past credits (in months) for the initial cohort of inactive contributors (s,x,c)' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "icinact")==T & str_detect( arch, "female")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'ICinact', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'ICinact', icinact_fem, colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="ICinact",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_icinact_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'ICinact', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'ICinact', icinact_mal, colNames = FALSE,
           startCol = 3,
           startRow = 3,)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="ICinact",
                   col_names=FALSE,
                   guess_max = 24000 )

aux  <- as.data.table( aux )
write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [Ioldage] Initial cohort of old-age beneficiaries (s,x).------------------------------------------
message( '\t\t[Ioldage] Initial cohort of old-age beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ioldage_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Ioldage",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ioldage_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Ioldage",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [Idis] Initial cohort of disability beneficiaries (s,x).------------------------------------------
message( '\t\t[Idis] Initial cohort of disability beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_idis_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Idis",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_idis_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Idis",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [Iwid] Initial cohort of widows/ers beneficiaries (s,x).------------------------------------------
message( '\t\t[Iwid] Initial cohort of widows/ers beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iwid_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iwid",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iwid_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iwid",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# [Iorph] Initial cohort of orphan beneficiaries (s,x).------------------------------------------
message( '\t\t[Iorph] Initial cohort of orphan beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iorph_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iorph",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iorph_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iorph",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


dir_lec_int <- 'Input/Escenario_Base/Demographic/Transition Probabilities/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# [er] Probability of leaving the active contributing population for any reason but death, retirement ...
message( '\t\t[er] Probability of leaving the active contributing population for any reason but death, retirement' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_er_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="er",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_er_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="er",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [rp] Probability that a given entry on a group has a past history of contributions (s,g,x,t).
message( '\t\t[rp] Probability that a given entry on a group has a past history of contributions (s,g,x,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_rp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="rp",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_rp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="rp",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [f] Distribution of the total entries per age (s,g,x,t).
message( '\t\t[f] Distribution of the total entries per age (s,g,x,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_f_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="f",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_f_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="f",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [ret] Probability of taking retirement once attained the retirement conditions for active contributors (s,g,x,t).
message( '\t\t[ret] Probability of taking retirement once attained the retirement conditions for active contributors (s,g,x,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ret_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="ret",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ret_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="ret",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [reti] Probability of taking retirement once attained the retirement conditions for inactive contributors (s,x,t).
message( '\t\t[reti] Probability of taking retirement once attained the retirement conditions for inactive contributors (s,x,t). (s,g,x,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_reti_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="reti",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_reti_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="reti",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

dir_lec_int <- 'Input/Escenario_Base/Demographic/Family Structure/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# [fwid] Expected number of surviving spouses after the death of an active contributor (s,g,x,y).
message( '\t\t[fwid] Expected number of surviving spouses after the death of an active contributor (s,g,x,y).' )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_fwid_")==T & str_detect( arch, "_male_")==T &
                                      str_detect( arch, "_input")==T & str_detect( arch, ".csv")==T))] )

aux_viu <- as.data.table( read.table( file_wr, sep = "\t", dec = '.', header = TRUE) )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_fwid_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'fwid', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'fwid', aux_viu[ , -c(1) ], colNames = FALSE,
           startCol = 3,
           startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="fwid",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_fwid_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, "_input")==T & str_detect( arch, ".csv")==T))] )

aux_viu <- as.data.table( read.table( file_wr, sep = "\t", dec = '.', header = TRUE) )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_fwid_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'fwid', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'fwid', aux_viu[ , -c(1) ], colNames = FALSE,
           startCol = 3,
           startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="fwid",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [forph] Expected number of surviving children after the death of an active contributor (s,g,x,y).
message( '\t\t[forph] Expected number of surviving children after the death of an active contributor (s,g,x,y).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_forph_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, "_input")==T & str_detect( arch, ".csv")==T))] )

aux_orf <- as.data.table( read.table( file_wr, sep = "\t", dec = '.', header = TRUE) )


file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_forph_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'forph', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'forph', aux_orf[ , -c(1) ], colNames = FALSE,
           startCol = 3,
           startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="forph",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_forph_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, "_input")==T & str_detect( arch, ".csv")==T))] )

aux_orf <- as.data.table( read.table( file_wr, sep = "\t", dec = '.', header = TRUE) )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_forph_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
wb <- loadWorkbook(file_wr)
deleteData(wb, sheet = 'forph', cols = 3, rows = 3, gridExpand = TRUE)

writeData( wb, 'forph', aux_orf[ , -c(1) ], colNames = FALSE,
           startCol = 3,
           startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="forph",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [fwidi] Expected number of surviving spouses after the death of a inactive member (s,x,y)..
message( '\t\t[fwidi] Expected number of surviving spouses after the death of a inactive member (s,x,y).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_fwidi_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="fwidi",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_fwidi_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="fwidi",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [forphi] Expected number of surviving children after the death of an inactive contributor (s,x,y).
message( '\t\t[forphi] Expected number of surviving children after the death of an inactive contributor (s,x,y).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_forphi_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="forphi",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_forphi_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="forphi",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


dir_lec_int <- 'Input/Escenario_Base/Financial/Base Year/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# [Isal] Average salary of initial contributors (s,g,x).
message( '\t\t[Isal] Average salary of initial contributors (s,g,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_isal_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Isal",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_isal_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Isal",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [Ioldage_ben] Average pension benefit for the initial cohort of old-age pension beneficiaries (s,x)
message( '\t\t[Ioldage_ben] Average pension benefit for the initial cohort of old-age pension beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ioldage_ben_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Ioldage_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ioldage_ben_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Ioldage_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# [Idis_ben] Average pension benefit for the initial cohort of disability pension beneficiaries (s,x).
message( '\t\t[Idis_ben] Average pension benefit for the initial cohort of disability pension beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_idis_ben_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Idis_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_idis_ben_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Idis_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [Iwid_ben] Average pensions benefit for the initial cohort of widow(er) pension beneficiaries (s,x).
message( '\t\t[Iwid_ben] Average pensions benefit for the initial cohort of widow(er) pension beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iwid_ben_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iwid_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iwid_ben_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iwid_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [Iorph_ben] Average pensions benefit for the initial cohort of orphan pension beneficiaries (s,x).
message( '\t\t[Iorph_ben] Average pensions benefit for the initial cohort of orphan pension beneficiaries (s,x).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iorph_ben_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iorph_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_iorph_ben_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="Iorph_ben",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


dir_lec_int <- 'Input/Escenario_Base/Historical/Historical Demographic/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# [HAC] Total active contributors (t). -------------------------------------------------------------
message( '\t\t[HAC] Total active contributors (t)' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hac_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HAC",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hac_male <- aux[ -c(1,2), c(2,3) ]
hac_male[ , sexo:='Male']
setnames(hac_male, c('t', 'l2', 'sexo'))
hac_male[ , t:=as.numeric( t )]
hac_male[ , l2:=as.numeric( l2 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hac_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HAC",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hac_female <- aux[ -c(1,2), c(2,3) ]
setnames(hac_female, c('t', 'l2'))
hac_female[ , sexo:='Female']
hac_female[ , t:=as.numeric( t )]
hac_female[ , l2:=as.numeric( l2 )]

hac <- rbind( hac_female, hac_male)
hac <- merge( data.table( expand.grid( t =2012:2020, sexo=c('Male', 'Female')  ) ) ,
              hac, by=c('t', 'sexo'), all.x=T)
setorder(hac, sexo, t)

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HOAP] Old-age pensioners by sex (s,t). ----------------------------------------------------------
message( '\t\t[HOAP] Old-age pensioners by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hoap_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOAP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hoap_male <- aux[ -c(1,2), c(2,3) ]
setnames(hoap_male, c('t', 'l3'))
hoap_male[ , sexo:='Male']
hoap_male[ , t:=as.numeric( t )]
hoap_male[ , l3:=as.numeric( l3 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hoap_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOAP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hoap_female <- aux[ -c(1,2), c(2,3) ]
setnames(hoap_female, c('t', 'l3'))
hoap_female[ , sexo:='Female']
hoap_female[ , t:=as.numeric( t )]
hoap_female[ , l3:=as.numeric( l3 )]

hoap <- rbind( hoap_female, hoap_male)
hoap <- merge( data.table( expand.grid( t =2012:2020, sexo=c('Male', 'Female')  ) ) ,
               hoap, by=c('t', 'sexo'), all.x=T)
setorder( hoap, sexo, t)

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

pob_hist <- merge( hac, hoap, by=c('t','sexo'), all.x = T )
setorder( pob_hist, sexo, t)

# [HDISP] Disability pensioners by sex (s,t).-------------------------------------------------------
message( '\t\t[HDISP] Disability pensioners by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hdisp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HDISP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hdisp_male <- aux[ -c(1,2), c(2,3) ]
setnames( hdisp_male, c('t', 'l4'))
hdisp_male[ , sexo:='Male']
hdisp_male[ , t:=as.numeric( t )]
hdisp_male[ , l4:=as.numeric( l4 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hdisp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HDISP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hdisp_female <- aux[ -c(1,2), c(2,3) ]
setnames( hdisp_female, c('t', 'l4'))
hdisp_female[ , sexo:='Female']
hdisp_female[ , t:=as.numeric( t )]
hdisp_female[ , l4:=as.numeric( l4 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

hdisp <- rbind( hdisp_female, hdisp_male)
pob_hist <- merge( pob_hist, hdisp, by=c('t','sexo'), all.x = T )
setorder( pob_hist, sexo, t)

# [HWP] Widow/er pensioners by sex (s,t).-------------------------------------------------------
message( '\t\t[HWP] Widow/er pensioners by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hwp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HWP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hwp_male <- aux[ -c(1,2), c(2,3) ]
setnames( hwp_male, c('t', 'l9'))
hwp_male[ , sexo:='Male']
hwp_male[ , t:=as.numeric( t )]
hwp_male[ , l9:=as.numeric( l9 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hwp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HWP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
hwp_female <- aux[ -c(1,2), c(2,3) ]
setnames( hwp_female, c('t', 'l9'))
hwp_female[ , sexo:='Female']
hwp_female[ , t:=as.numeric( t )]
hwp_female[ , l9:=as.numeric( l9 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

hwp <- rbind( hwp_female, hwp_male)
pob_hist <- merge( pob_hist, hwp, by=c('t','sexo'), all.x = T )
setorder( pob_hist, sexo, t)

# [HORP] Orphan pensioners by sex (s,t).------------------------------------------------------------
message( '\t\t[HORP] Orphan pensioners by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_horp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HORP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
horp_male <- aux[ -c(1,2), c(2,3) ]
setnames( horp_male, c('t', 'l8'))
horp_male[ , sexo:='Male']
horp_male[ , t:=as.numeric( t )]
horp_male[ , l8:=as.numeric( l8 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_horp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HORP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )
horp_female <- aux[ -c(1,2), c(2,3) ]
setnames( horp_female, c('t', 'l8'))
horp_female[ , sexo:='Female']
horp_female[ , t:=as.numeric( t )]
horp_female[ , l8:=as.numeric( l8 )]

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

horp <- rbind( horp_female, horp_male)
pob_hist <- merge( pob_hist, horp, by=c('t','sexo'), all.x = T )
setorder( pob_hist, sexo, t)

dir_lec_int <- 'Input/Escenario_Base/Historical/Historical Financial/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

# [HOAEXP] Expenditure on old-age by sex (s,t). -------------------------------------------------------------
message( '\t\t[HOAEXP] Expenditure on old-age by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hoaexp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOAEXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hoaexp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOAEXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HDISEXP] Expenditure on disability by sex (s,t).-------------------------------------------------------------
message( '\t\t[HDISEXP] Expenditure on disability by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hdisexp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HDISEXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hdisexp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HDISEXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HWIEXP] Expenditure on widow/er by sex (s,t).-------------------------------------------------------------
message( '\t\t[HWIEXP] Expenditure on widow/er by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hwiexp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HWIEXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hwiexp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HWIEXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HOREXP] Expenditure on orphans by sex (s,t).-------------------------------------------------------------
message( '\t\t[HOREXP] Expenditure on orphans by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_horexp_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOREXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_horexp_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOREXP",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HOA_Pav] Average old-age pension by sex (s,t).-------------------------------------------------------------
message( '\t\t[HOA_Pav] Average old-age pension by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hoa_pav_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOA_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hoa_pav_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOA_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HDIS_Pav] Average disability pension by sex (s,t).-----------------------------------------------
message( '\t\t[HDIS_Pav] Average disability pension by sex (s,t)..' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hdis_pav_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HDIS_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hdis_pav_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HDIS_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HWI_Pav] Average widow/er pension by sex (s,t).-----------------------------------------------
message( '\t\t[HWI_Pav] Average widow/er pension by sex (s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hwi_pav_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HWI_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hwi_pav_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HWI_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [HOR_Pav] Average orphan pension by sex(s,t).-----------------------------------------------
message( '\t\t[HOR_Pav] Average orphan pension by sex(s,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hor_pav_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOR_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_hor_pav_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )
aux <- read_excel( file_wr,
                   sheet="HOR_Pav",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

lista <- c('pob_hist')
save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_outputs_historico_ssc.RData' ) )

setwd( parametros$work_dir )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()