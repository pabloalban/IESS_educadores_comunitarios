message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo proyeccion demografica IVM por edad y sexo' )

# Se toma la Proyeccion Activos del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_actgx_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_actgx_sgo_female_50_.xlsx' )
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
            value.name    = "l2" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
            id.vars = "...2",
            variable.name = "t",
            value.name    = "l2" ) )
proy_mujeres[ , sexo := 'F' ]

proy <- rbind( proy_hombres, proy_mujeres )
proy$t <- as.numeric( proy$t )
proy[ , t := t - 1 ] # Se rectifica porque la proyeccion comienza en 2020
setnames( proy, '...2', 'x' )

# Se toma la Proyeccion JV del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_oldage_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_oldage_sgo_female_50_.xlsx' )
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
                                    value.name    = "l3" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "l3" ) )
proy_mujeres[ , sexo := 'F' ]

proy_jv <- rbind( proy_hombres, proy_mujeres )
proy_jv$t <- as.numeric( proy_jv$t ) # No se rectifica porque la proyeccion comienza en 2021
setnames( proy_jv, '...2', 'x' )

# Se toma la Proyeccion IN del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_dis_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_dis_sgo_female_50_.xlsx' )
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
                                    value.name    = "l4" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "l4" ) )
proy_mujeres[ , sexo := 'F' ]

proy_in <- rbind( proy_hombres, proy_mujeres )
proy_in$t <- as.numeric( proy_in$t ) # No se rectifica porque la proyeccion comienza en 2021
setnames( proy_in, '...2', 'x' )

# Se toma la Proyeccion VO del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_wid_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_wid_sgo_female_50_.xlsx' )
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
                                    value.name    = "l6" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "l6" ) )
proy_mujeres[ , sexo := 'F' ]

proy_vo <- rbind( proy_hombres, proy_mujeres )
proy_vo$t <- as.numeric( proy_vo$t ) # No se rectifica porque la proyeccion comienza en 2021
setnames( proy_vo, '...2', 'x' )

# Se toma la Proyeccion OR del Escenario base de IVM ----
# Hombres 
file <- paste0( parametros$Data_seg, 'base_orph_sgo_male_50_.xlsx' )
proy_hombres <- read_excel( file, 
                            sheet = 1,
                            skip = 1,
                            col_names = TRUE )
proy_hombres <- as.data.table( proy_hombres )
# Mujeres
file <- paste0( parametros$Data_seg, 'base_orph_sgo_female_50_.xlsx' )
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
                                    value.name    = "l67" ) )
proy_hombres[ , sexo := 'M' ]

proy_mujeres <- proy_mujeres[ , 2:n ]
proy_mujeres <- as.data.table( melt(proy_mujeres, 
                                    id.vars = "...2",
                                    variable.name = "t",
                                    value.name    = "l67" ) )
proy_mujeres[ , sexo := 'F' ]

proy_or <- rbind( proy_hombres, proy_mujeres )
proy_or$t <- as.numeric( proy_or$t ) # No se rectifica porque la proyeccion comienza en 2021
setnames( proy_or, '...2', 'x' )

# Unificado proyeccion ----
proy <- merge( proy, 
               proy_jv,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

proy <- merge( proy, 
               proy_in,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

proy <- merge( proy,
               proy_vo,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

proy <- merge( proy,
               proy_or,
               by = c( 't', 'sexo', 'x' ), 
               all.x = TRUE, 
               all.y = TRUE )

proy[ is.na( proy ) ] <- 0

# ----
save( proy, 
      file = paste0( parametros$RData_seg, 'IESS_SAL_proy_ivm_demografia.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

