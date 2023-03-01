message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo proyeccion Pensiones IVM por edad y sexo' )

# Se toma la Proyeccion Pensiones JV del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_oldage_ben_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_oldage_ben_sgo_female_50_.xlsx' )
proy_mujeres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_mujeres <- as.data.table( proy_mujeres )
# Unificacion
n <- dim( proy_hombres )[2]

proy_hombres <- proy_hombres[ , 2:n ]
proy_hombres <- as.data.table( melt(proy_hombres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P3" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P3" ) )
proy_mujeres[ , sexo := 'F' ]

proy_jv <- rbind( proy_hombres, proy_mujeres )
proy_jv$t <- as.numeric( proy_jv$t )
setnames( proy_jv, '...2', 'x' )

# Pension JV anual
proy_jv[ , P3 := P3 * 12 ]
proy_jv$x <- as.numeric( proy_jv$x )

proy_jv[ is.na( proy_jv ) ] <- 0

# Se toma la Proyeccion Pensiones IN del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_dis_ben__male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_dis_ben__female_50_.xlsx' )
proy_mujeres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_mujeres <- as.data.table( proy_mujeres )
# Unificacion
n <- dim( proy_hombres )[2]

proy_hombres <- proy_hombres[ , 2:n ]
proy_hombres <- as.data.table( melt(proy_hombres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P4" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P4" ) )
proy_mujeres[ , sexo := 'F' ]

proy_ji <- rbind( proy_hombres, proy_mujeres )
proy_ji$t <- as.numeric( proy_ji$t )
setnames( proy_ji, '...2', 'x' )

# Pension IN anual
proy_ji[ , P4 := P4 * 12 ]
proy_ji$x <- as.numeric( proy_ji$x )

proy_ji[ is.na( proy_ji ) ] <- 0

# Se toma la Proyeccion Pensiones VO del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_wid_ben__male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_wid_ben__female_50_.xlsx' )
proy_mujeres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_mujeres <- as.data.table( proy_mujeres )
# Unificacion
n <- dim( proy_hombres )[2]

proy_hombres <- proy_hombres[ , 2:n ]
proy_hombres <- as.data.table( melt(proy_hombres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P6" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P6" ) )
proy_mujeres[ , sexo := 'F' ]

proy_vo <- rbind( proy_hombres, proy_mujeres )
proy_vo$t <- as.numeric( proy_vo$t )
setnames( proy_vo, '...2', 'x' )

# Pension VO anual
proy_vo[ , P6 := P6 * 12 ]
proy_vo$x <- as.numeric( proy_vo$x )

proy_vo[ is.na( proy_vo ) ] <- 0

# Se toma la Proyeccion Pensiones OR del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_orph_ben__male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_orph_ben__female_50_.xlsx' )
proy_mujeres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_mujeres <- as.data.table( proy_mujeres )
# Unificacion
n <- dim( proy_hombres )[2]

proy_hombres <- proy_hombres[ , 2:n ]
proy_hombres <- as.data.table( melt(proy_hombres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P67" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "P67" ) )
proy_mujeres[ , sexo := 'F' ]

proy_or <- rbind( proy_hombres, proy_mujeres )
proy_or$t <- as.numeric( proy_or$t )
setnames( proy_or, '...2', 'x' )

# Pension VO anual
proy_or[ , P67 := P67 * 12 ]
proy_or$x <- as.numeric( proy_or$x )

proy_or[ is.na( proy_or ) ] <- 0

# Se toma el SBU proyectado del Escenario base de IVM ----
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )
sbu <- as.data.table( tasas_macro_pred )
sbu <- sbu[, list( t = anio - 2020, sbu ) ][ t > 0 ]

# Unificado proyeccion ----
ben_proy <- merge( proy_jv, 
               proy_ji,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

ben_proy <- merge( ben_proy,
               proy_vo,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

ben_proy <- merge( ben_proy,
               proy_or,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

ben_proy <- merge( ben_proy,
                   sbu,
                   by = c( 't' ), 
                   all.x = TRUE, 
                   all.y = TRUE )

ben_proy[ is.na( ben_proy ) ] <- 0

# ----
save( ben_proy, 
      file = paste0( parametros$RData_seg, 'IESS_proyeccion_beneficios_escenario_1_ivm.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
