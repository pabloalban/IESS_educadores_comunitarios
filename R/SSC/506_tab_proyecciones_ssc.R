message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData')) 

#Tabla de la proyección de la población desagregada por sexo, en cada estado -----------------------
message( '\tTabla de la proyección de la población desagregada por sexo, en cada estado' )

aux_f <- acum_dem[ sexo == 'Female' & t >parametros$anio, 
                   list( t, l1_f = l1, l2_f = l2, l3_f = l3, l4_f = l4, l8_f = l8, l9_f =l9 ) ]

aux_m <- acum_dem[ sexo == 'Male' & t > parametros$anio, 
                   list( t, l1_m = l1, l2_m = l2, l3_m = l3, l4_m = l4, l8_m = l8, l9_m =l9 ) ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[ , t := as.character( t ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 12 ) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

#Tabla de la proyección de la nueva población desagregada por sexo, en cada estado -----------------
message( '\tTabla de la proyección de la población desagregada por sexo, en cada estado' )
aux_f <- new_acum_dem[ sexo == 'Female' & t >parametros$anio, 
                   list( t, l12_f = l12, l23_f = l23, l24_f = l24, l25_f = l25, l35_f = l35, l45_f =l45,
                         l85_f = l85, l95_f = l95) ]

aux_m <- new_acum_dem[ sexo == 'Male' & t >parametros$anio, 
                   list( t, l12_m = l12, l23_m = l23, l24_m = l24, l25_m = l25, l35_m = l35, l45_m =l45,
                             l85_m = l85, l95_m = l95) ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[ , t := as.character( t ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 16 ) ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_new_pob_proy_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

#Tabla de la proyección de los dependientes del SSC ------------------------------------------------
message( '\tTabla de la proyección de los dependientes del SSC' )

aux_df <- acum_dem[ sexo == 'Female' & t >parametros$anio, 
                   list( t, l6_f = l6, l7_f = l7 ) ]
aux_nf <- new_acum_dem[ sexo == 'Female' & t >parametros$anio, 
                       list( t, l65_f = l65, l75_f = l75 ) ]
aux_f <- merge( aux_df, aux_nf, by = c( 't' ) )
aux_f <- aux_f[ , list( t, l6_f, l65_f, l7_f, l75_f )]

aux_dm <- acum_dem[ sexo == 'Male' & t >parametros$anio, 
                    list( t, l6_m = l6, l7_m = l7 ) ]
aux_nm <- new_acum_dem[ sexo == 'Male' & t >parametros$anio, 
                        list( t, l65_m = l65, l75_m = l75 ) ]
aux_m <- merge( aux_dm, aux_nm, by = c( 't' ) )
aux_m <- aux_m[ , list( t, l6_m, l65_m, l7_m, l75_m )]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[ , t := as.character( t ) ]

xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_dep_proy_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )



message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()