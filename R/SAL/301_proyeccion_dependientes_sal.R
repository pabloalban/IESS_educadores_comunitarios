message( paste( rep('-', 100 ), collapse = '' ) )

# Descripción estados:
# 1 = Individuos en la PEA no activos
# 2 = Activos
# 2_cot = Activos cotizantes
# 2_ces = Activos cesantes
# 3 = vejez
# 4 = invalidez
# 5 = Muertos
# 6 = Viudedad
# 67= Orfandad
# 7 = Hijos de cotizantes
# 8 = Cónjuges no asegurados de cotizantes

# Carga de proyeccion de IVM ----
message( '\tCarga de datos demograficos de IVM' )
load( paste0( parametros$RData_seg, 'IESS_SAL_proy_ivm_demografia.RData' ) )
pob_proy <- proy
pob_proy$x <- as.numeric( pob_proy$x )

# Incluyendo proyección de dependientes ------------------------------------------------------------
message( '\tIncluyendo dependientes' )
load( paste0( parametros$RData, 'INEC_censo_iess_fertilidad_alisado_2010.RData' ) )
aux <- cen_iess_hij_alis[ , list( x, z = y, sexo, sexo_dep, qh = q ) ]
aux[ , sexo := as.character( sexo ) ]
aux[ , sexo_dep := as.character( sexo_dep ) ]
aux[ sexo == 'M', sexo := 'F' ]
aux[ sexo == 'H', sexo := 'M' ]
aux[ sexo_dep == 'M', sexo_dep := 'F' ]
aux[ sexo_dep == 'H', sexo_dep := 'M' ]
pob_proy_dep <- merge( pob_proy[ , list( sexo, x, t, l2 ) ],
                       aux,
                       by = c( 'sexo', 'x' ),
                       all.x = TRUE, allow.cartesian = TRUE )

pob_proy_dep[ is.na( qh ), qh := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( z ) ]

pob_proy_dep <- merge( pob_proy_dep,
                       cen_iess_cony_alis[ , list( x, y, qc = q ) ], by = c( 'x' ),
                       all.x = TRUE, allow.cartesian = TRUE )
gc()
pob_proy_dep[ is.na( qc ), qc := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( y ) ]
pob_proy_dep[ sexo == 'F', sexo_cony := 'M' ]
pob_proy_dep[ sexo == 'M', sexo_cony := 'F' ]

pob_proy_dep <- merge( pob_proy_dep,
                       aux[ , list( y = x, z, sexo_cony = sexo, sexo_dep, qhc = qh ) ],
                       by = c( 'sexo_cony', 'y', 'sexo_dep', 'z' ) )

pob_proy_dep <- pob_proy_dep[ y - z >= 15 ]
pob_proy_dep <- pob_proy_dep[ y - z <= 50 ]
pob_proy_dep <- pob_proy_dep[ x - z >= 15 ]
pob_proy_dep <- pob_proy_dep[ x - z <= 70 ]
gc()

pob_proy_dep <- pob_proy_dep[ y <= 105, list( l7 = sum( l2 * qh * qhc * ( 1 - 0.5 * qc ) ) ),
                              by = list( t, sexo = sexo_dep, x = z ) ]
gc()

pob_proy_cony <- merge( pob_proy[ , list( sexo, x, t, l2 ) ],
                        cen_iess_cony_alis[ , list( x, y, qc = q ) ], by = c( 'x' ),
                        all.x = TRUE, allow.cartesian = TRUE )

pob_proy_cony[ is.na( qc ), qc := 0 ]
pob_proy_cony <- pob_proy_cony[ !is.na( y ) ]
pob_proy_cony <- pob_proy_cony[ y <= 105, list( l8 = sum( l2 * qc ) ),
                                by = list( t, sexo, x = y ) ]

pob_proy <- merge( pob_proy, pob_proy_dep, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
pob_proy <- merge( pob_proy, pob_proy_cony, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
pob_proy[ is.na( l7 ), l7 := 0 ]
pob_proy[ is.na( l8 ), l8 := 0 ]


# Calibración de dependientes hijos menores de 18
# Del historico, 2021 iguala al promedio de los ultimos 4 años por sexo
# Los datos estan en el excel Beneficios
cal_dep_menores_f <- 0.48282
cal_dep_menores_m <- 0.47084

pob_proy[ sexo == 'F', l7 := cal_dep_menores_f * l7 ]
pob_proy[ sexo == 'M', l7 := cal_dep_menores_m * l7 ]


#----
message( '\tGuardando proyección de población' )
save( pob_proy,
      file = paste0( parametros$RData, 'IESS_proyeccion_poblacion_salud.RData' ) )
#----
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c(  'parametros' ) ) ] )
gc()
