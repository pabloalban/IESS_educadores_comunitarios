message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )
#Función de tildes a latex--------------------------------------------------------------------------
#source( 'R/502_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

escenarios_lista <- paste0( 'escenario_', 1:4 )

for ( i in 1:length( escenarios_lista ) ) { # i<-3
  escenario <- escenarios_lista[i]
  load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', escenario, '.RData' ) )
  
  # Balance corriente ------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, 
                                A, 
                                A_est, 
                                B, 
                                # G, 
                                V_cor ) ]
  aux <- aux[ t > 2020 ]
  aux[, t := as.character( t ) ]
  # write.xlsx( aux, paste0('C:/Users/jendry.toapanta/Downloads/','iess_balance_corriente_', escenario, '.xlsx') )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0 ) ) #c( 0, 0, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex',
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, 
                                A2,
                                A7,
                                # A_afi,
                                A8, 
                                A_est, 
                                A ) ]
  aux <- aux[ t >2020 ]
  aux[, t := as.character( t ) ]
  # write.xlsx( aux, paste0('C:/Users/jendry.toapanta/Downloads/','iess_balance_aportes_', escenario, '.xlsx') )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0 ) ) #c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE,
         hline.after = NULL, sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, 
                                B2,
                                B7,
                                B8,
                                B_cat,
                                B_pen = B3 + B4 + B6, 
                                B9,
                                B ) ]
  aux <- aux[ t > 2020 ]
  aux[, t := as.character( t )]
  # write.xlsx( aux, paste0('C:/Users/jendry.toapanta/Downloads/','iess_balance_beneficios_', escenario, '.xlsx') )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0, 0 ) ) #c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico  ------------------------------------------------------------------------
  # Balance dinámico (actuarial) -------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A_afi_vap, A_est_vap, B_vap, G_vap, V ) ]
  aux[, anio := as.character( anio )]
  aux <- aux[ t > 0 ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0 ) ) #c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (aportes) ---------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, 
                                A2_vap, A7_vap, A8_vap,
                                A_afi_vap, A_est_vap, A_vap ) ]
  aux[, anio := as.character( anio )]
  aux <- aux[ t > 0 ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0, 0 ) ) #c( 0, 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_aportes_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (beneficios) ------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, 
                                B2_vap, 
                                B346_vap = B3_vap + B4_vap + B6_vap, 
                                B7_vap, B8_vap, B9_vap, B_vap ) ]
  aux[, anio := as.character( anio )]
  aux <- aux[ t > 0 ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0, 0 ) ) #c( 0, 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------
  if( i < 5 ){
    aux <- balance_anual[ t == max(t), 
                          list( V0, 
                                A2_vap, 
                                A7_vap, 
                                A8_vap,
                                A_2_8_vap = A_afi_vap + A8_vap,
                                A_est_cat_vap, #aporte para las catastroficas
                                A_est_pen = A_est_3_vap + A_est_4_vap + A_est_6_vap, #atenciones pensionsts
                                A_est_vap, 
                                # A_vap,
                                Atot_vap = A_est_vap + A_afi_vap + A8_vap,
                                activo = V0 + A_vap,
                                B2_vap, 
                                B3_vap, 
                                B4_vap, 
                                B6_vap, 
                                B7_vap, 
                                B8_vap,
                                #B_cat = B_est_cat_vap, #beneficios catastroficas
                                B9_vap, 
                                B_vap, 
                                # G_vap, 
                                pasivo = B_vap + G_vap, 
                                V ) 
    ]
    aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
    aux2 <- data.table( V0 = 'Reserva inicial', 
                        A2_vap = 'Aporte de activos', 
                        A7_vap = 'Aportes para hijos menores de 18',
                        A8_vap = 'Aportes por extensi\\\'{o}n de cobertura y viudas',
                        A_2_8_vap = 'Aportes de afiliados',
                        A_est_cat_vap = 'Aportes por enfermedades catastr\\\'{o}ficas',
                        A_est_pen = 'Aporte estatal para pensionistas',
                        A_est_vap = 'Aporte estatal', 
                        Atot_vap = 'Aportes totales', 
                        activo = 'Activo actuarial', 
                        B2_vap = 'Beneficios afiliados cotizantes', 
                        B3_vap = 'Beneficios pensionistas vejez', 
                        B4_vap = 'Beneficios pensionistas invalidez', 
                        B6_vap = 'Beneficios pensionistas montep\\\'{i}o', 
                        B7_vap = 'Beneficios de hijos menores de 18 a$\\tilde{\\text{n}}$os',
                        B8_vap = 'Beneficios por extensi\\\'{o}n de cobertura',
                        #B_cat = 'Beneficios por enfermedades catastr\\\'{o}ficas ',
                        B9_vap = 'Pago de subsidios',
                        B_vap = 'Beneficios totales', 
                        # G_vap = 'Gastos administrativos',
                        pasivo = 'Pasivo actuarial',
                        V = 'Balance actuarial' )
    aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
    aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
    setnames(aux, c('item', 'descripcion', 'valor'))
    # write.xlsx( aux, paste0('C:/Users/jendry.toapanta/Downloads/','iess_bal_act_vap_', escenario, '.xlsx') )
    xtb_aux <- xtable( aux[ , list(descripcion, valor)], digits = c( 0, 0, 0 ) )
    #xtb_aux <- tildes_a_latex(xtb_aux)
    print( xtb_aux,
           file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
           type = 'latex', 
           include.colnames = FALSE, include.rownames = FALSE, 
           format.args = list( decimal.mark = ',', big.mark = '.' ), 
           only.contents = TRUE, 
           #hline.after = c(1, 4, 5, 6, 7, 8, 15, 17, 18), sanitize.text.function = identity )
           hline.after = c(1, 4, 5, 7, 8, 9, 10, 17, 19, 20), sanitize.text.function = identity )
  }
  else{
    aux <- balance_anual[ t == max(t), 
                          list( V0, 
                                A_vap,
                                activo = A_vap,
                                B_vap, 
                                G_vap, 
                                pasivo = B_vap + G_vap, 
                                V ) 
    ]
    aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
    aux2 <- data.table( V0 = 'Reserva inicial', 
                        A_vap = 'Aporte de activos', 
                        activo = 'Activo actuarial', 
                        B_vap = 'Beneficios afiliados cotizantes', 
                        G_vap = 'Gastos administrativos',
                        pasivo = 'Pasivo actuarial',
                        V = 'Balance actuarial' )
    aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
    aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
    setnames(aux, c('item', 'descripcion', 'valor'))
    xtb_aux <- xtable( aux[ , list(descripcion, valor)], digits = c( 0, 0, 2 ) )
    #xtb_aux <- tildes_a_latex(xtb_aux)
    print( xtb_aux,
           file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
           type = 'latex', 
           include.colnames = FALSE, include.rownames = FALSE, 
           format.args = list( decimal.mark = ',', big.mark = '.' ), 
           only.contents = TRUE, 
           hline.after = c(1, 2, 3, 5, 6 ), sanitize.text.function = identity )  
  }
  
  rm( aportes, beneficios, balance_anual )
}

# message( paste( rep('-', 100 ), collapse = '' ) )
# rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
# gc()

# Tablas de la masa salarial ---------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, paste0( 'IESS_proyeccion_salarios_', 'escenario_1', '.RData' ) ) )
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c(  'parametros', 'sal_proy'
                                  , 'pob_proy' ) ) ] )

# Salario
sal_proy_tot_sex <- sal_proy[ , list( mas = sum( sal )
), 
by = list( t, sexo, x ) ]
setorder( sal_proy_tot_sex, t, sexo )

# Poblacion
pob_proy_tot_sex <- pob_proy[ , list( 
  l2 = sum( l2 ),
  l2_cot = sum( l2_cot )
), 
by = list( t, sexo, x ) ]

setorder( pob_proy_tot_sex, t, sexo )

# Union
sal_pob <- merge( pob_proy_tot_sex, sal_proy_tot_sex, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
sal_pob[ is.na( mas), mas := 0 ]
sal_pob[ , mas_sal := mas * l2_cot ]

# Masa Salarial
masa_salarial <- sal_pob[ , list( mas_sal = sum( mas_sal )
),  by = list( t, sexo ) ]

aux <- merge( masa_salarial[ sexo == 'F' ][ , list( t, mujeres = mas_sal  ) ]
              , masa_salarial[ sexo == 'M' ][ , list( t, hombres = mas_sal  ) ]
              , by = 't', all.x = TRUE )
aux[ , total := mujeres + hombres ]
aux <- aux[ t <= parametros$horizonte & 0 < t ]
aux <- aux[ , t := t + parametros$anio_ini ]
aux$t <- as.character( aux$t )

xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial', '.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, 
       sanitize.text.function = identity )



message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
