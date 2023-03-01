message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'IVM/', 'IESS_IVM_tabla_mortalidad_dinamica.RData' ) )
iess_mort_din_ivm <- iess_mort_din

load( paste0( parametros$RData_seg, 'IESS_SSC_tabla_mortalidad_dinamica.RData' ) )
message( '\tGenerando tablas de mortalidad para cada estado' )

# Mortalidad de jefes activos ----------------------------------------------------------------------
message( '\tMortalidad de jefes activos' )
afi_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, qx, px ) ]
setorder( afi_mor, t, sexo, x )
afi_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
afi_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
afi_mor[ , dx := lx * qx ]
afi_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
afi_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad de jefes inactivos' )
afi_mor_inac <- iess_mort_din[ x >= 15, list( t, sexo, x, qx = q_inac_x, px = p_inac_x ) ]
setorder( afi_mor_inac, t, sexo, x )
afi_mor_inac[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
afi_mor_inac[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
afi_mor_inac[ , dx := lx * qx ]
afi_mor_inac[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
afi_mor_inac[ , ex := ex / lx - 0.5 ]

# Mortalidad pensionistas por vejez ----------------------------------------------------------------
message( '\tMortalidad pensionistas por vejez' )
pen_vej_mor <- iess_mort_din[ x >= 65, list( t, sexo, x, qx = qvx, px = pvx ) ]
setorder( pen_vej_mor, t, sexo, x )
pen_vej_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_vej_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_vej_mor[ , dx := lx * qx ]
pen_vej_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_vej_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por vejez y afiliados inactivos' )
pen_vej_inac_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, qx = qv_inac_x, px = pv_inac_x ) ]
setorder( pen_vej_inac_mor, t, sexo, x )
pen_vej_inac_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_vej_inac_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_vej_inac_mor[ , dx := lx * qx ]
pen_vej_inac_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_vej_inac_mor[ , ex := ex / lx - 0.5 ]

# Mortalidad pensionistas por invalidez ------------------------------------------------------------
message( '\tMortalidad pensionistas por invalidez' )
pen_inv_mor <- iess_mort_din[ x >= 20, list( t, sexo, x, qx = qix, px = pix ) ]
setorder( pen_inv_mor, t, sexo, x )
pen_inv_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_inv_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_inv_mor[ , dx := lx * qx ]
pen_inv_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_inv_mor[ , ex := ex / lx - 0.5 ]

# Mortalidad no afiliados --------------------------------------------------------------------------
message( '\tMortalidad no afiliados' )
noafi_mor <- iess_mort_din[ x >= 0, list( t, sexo, x, qx = q_onu_x, px = 1 - q_onu_x ) ]
setorder( noafi_mor, t, sexo, x )
noafi_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
noafi_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
noafi_mor[ , dx := lx * qx ]
noafi_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
noafi_mor[ , ex := ex / lx - 0.5 ]

#Parte IVM
message( '\tMortalidad y otras salidas para pensionistas por orfandad' )
pen_orf_mor <- iess_mort_din_ivm[ x >= 0, list( t, sexo, x, qx = qox, px = pox ) ]
setorder( pen_orf_mor, t, sexo, x )
pen_orf_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_orf_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_orf_mor[ , dx := lx * qx ]
pen_orf_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_orf_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad  y otras salidas para pensionistas por viudedad' )
pen_viu_mor <- iess_mort_din_ivm[ x >= 14, list( t, sexo, x, qx = qwx, px = pwx ) ]
setorder( pen_viu_mor, t, sexo, x )
pen_viu_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_viu_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_viu_mor[ , dx := lx * qx ]
pen_viu_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_viu_mor[ , ex := ex / lx - 0.5 ]


lista <- c('afi_mor', 'afi_mor_inac','pen_vej_mor', 'pen_vej_inac_mor','pen_inv_mor', 'noafi_mor', 
           'pen_orf_mor', 'pen_viu_mor')

# Probability of death for active members (s,g,x,t)[q] ---------------------------------------------
message( '\t\tProbability of death for active members (s,g,x,t)[q] ' )
dir_lec_int <- 'Input/Escenario_Base/Demographic/Transition Probabilities/'
setwd(paste0( parametros$Data_seg, dir_lec_int ))
arch <- list.files()

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_q_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'q', cols = 3:c(3+parametros$edad_max), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'q', dcast.data.table( afi_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                 sexo=='M'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 18)

writeData( wb, 'q', matrix(0, 15,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="q",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_q_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'q', cols = 3:c(3+parametros$edad_max), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'q', dcast.data.table( afi_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                 sexo=='F'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3,startRow = 18)

writeData( wb, 'q', matrix(0, 15,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="q",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# [qd] Probability of Death For a Disability Pensioner (s,x,t). ------------------------------------
message( '\t\t[qd] Probability of Death For a Disability Pensioner (s,x,t).' )
file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qd_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qd', cols = 3:c( 3 + parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qd', dcast.data.table( pen_inv_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                 sexo=='M'],sexo + x ~ t, value.var = 'qx' )[,-c('sexo', 'x', '2040')]
           , colNames = FALSE,startCol = 3, startRow = 8)

writeData( wb, 'qd', matrix(0, 5,  parametros$horizonte )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qd",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qd_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qd', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qd', dcast.data.table( pen_inv_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                 sexo=='F'],sexo + x ~ t, value.var = 'qx' )[,-c('sexo', 'x', '2040')]
           , colNames = FALSE,startCol = 3,startRow = 8)

writeData( wb, 'qd', matrix(0, 5,  parametros$horizonte )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qd",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# [qi] Probability of death for an inactive contributor or an old-age pensioner (s,x,t). -----------
message( '\t\t[qi] Probability of death for an inactive contributor or an old-age pensioner (s,x,t)' )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qi_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qi', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

x <- dcast.data.table( pen_vej_inac_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                           sexo=='M'],sexo + x ~ t, value.var = 'qx' )

writeData( wb, 'qi', dcast.data.table( pen_vej_inac_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                 sexo=='M'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 18)

writeData( wb, 'qi', matrix(0, 15,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qi",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qi_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qi', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qi', dcast.data.table( pen_vej_inac_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                      sexo=='F'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 18)

writeData( wb, 'qi', matrix(0, 15,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qi",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# [qo] Probability of death (including other reasons for exit such as turning the majority of age) for an orphan (s,x,t). -----------
message( '\t\t[qo] Probability of death (including other reasons for exit such as turning the majority of age) for an orphan (s,x,t).' )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qo_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qo', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qo', dcast.data.table( pen_orf_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                      sexo=='M'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qo",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qo_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qo', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qo', dcast.data.table( pen_orf_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                      sexo=='F'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qo",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)


# [qw] Probability of death (including other reasons for exit such as marriage) for a widow(er) (s,x,t). -----------
message( '\t\t[qw] Probability of death (including other reasons for exit such as marriage) for a widow(er) (s,x,t).' )

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qw_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qw', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qw', dcast.data.table( pen_viu_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                      sexo=='M'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 17)
writeData( wb, 'qw', matrix(0, 14,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 3)


saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qw",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_qw_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )
openxlsx::deleteData(wb, sheet = 'qw', cols = 3:c(3+parametros$horizonte), rows = 3:108, gridExpand = TRUE)

writeData( wb, 'qw', dcast.data.table( pen_viu_mor[ t>= parametros$anio & t <= parametros$anio + parametros$horizonte & 
                                                      sexo=='F'],sexo + x ~ t, value.var = 'qx' )[,-c(1,2)]
           , colNames = FALSE,startCol = 3, startRow = 17)

writeData( wb, 'qw', matrix(0, 14,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 3)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="qw",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

# [ir] Probability of incapacitating disability (s,g,x,t).------------------------------------------
message( '\t\t[ir] Probability of incapacitating disability (s,g,x,t)' )

file_wr <- paste0( parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ir_")==T & str_detect( arch, "_male_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )


openxlsx::deleteData(wb, sheet = 'ir', cols = 3:c(3+parametros$horizonte), rows = 3:68, gridExpand = TRUE)

ir <- as.data.table( read.csv( file = 'Y:/IESS_2020/Data/IVM/INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/ir_form_hombres.csv',
                               sep=";", dec = ","))



writeData( wb, 'ir', matrix( ir[ X>=15 & X<=69]$form, 55, parametros$horizonte + 1), colNames = FALSE,startCol = 3, startRow = 3)
writeData( wb, 'ir', matrix(ir[ X==69]$form, 11,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 58)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="ir",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)

file_wr <- paste0(parametros$Data_seg, dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, "_ir_")==T & str_detect( arch, "_female_")==T &
                                        str_detect( arch, ".xlsx")==T))] )

wb <- loadWorkbook( file_wr  )


openxlsx::deleteData(wb, sheet = 'ir', cols = 3:c(3+parametros$horizonte), rows = 3:68, gridExpand = TRUE)

ir <- as.data.table( read.csv( file = 'Y:/IESS_2020/Data/IVM/INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/ir_form_mujeres.csv',
                               sep=";", dec = ","))



writeData( wb, 'ir', matrix( ir[ X>=15 & X<=69]$form, 55, parametros$horizonte + 1), colNames = FALSE,startCol = 3, startRow = 3)
writeData( wb, 'ir', matrix(ir[ X==69]$form, 11,  parametros$horizonte + 1 )
           , colNames = FALSE,startCol = 3, startRow = 58)

saveWorkbook(wb, file_wr, overwrite = T)

aux <- read_excel( file_wr,
                   sheet="ir",
                   col_names=FALSE,
                   guess_max = 24000 )
aux  <- as.data.table( aux )

write.table(aux, str_replace_all( file_wr, "xlsx","\\csv"), na = "",
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = FALSE)
#---------------------------------------------------------------------------------------------------
setwd( parametros$work_dir )

save( list = lista ,
      file = paste0( parametros$RData_seg, 'IESS_SSC_tablas_mortalidad_todos_estados.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()