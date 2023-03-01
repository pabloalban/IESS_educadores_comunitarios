message( paste( rep('-', 100 ), collapse = '' ) )

diccionario_ms <- 
  data.table( item = c('t','M', 'MS','MD', 'M_vap', 'MS_vap','MD_vap' ),
              descripcion = c('Año del Horizonte de Proyección',
                              'Masa Salarial de Afiliados al SGO',
                              'Masa Salarial del SSC',
                              'Masa Salarial de Afiliados al SGO bajo Relación de Dependientes',
                              'Masa Salarial VAP',
                              'Masa Salarial del SSC VAP',
                              'Masa Salarial de Afiliados al SGO bajo Relación de Dependientes VAP') )

diccionario_bal_corriente <- 
  data.table( item = c( 'A', 'Int', 'AI','A_est','G', 'Exc_G','B', 'V_cor', 'V_cap'), 
              descripcion = c('Aportes de afiliados + Estado + Otros Aportes', 'Interés de la reserva',
                              'Aporte incluido interés de la reserva','Aportes del Estado',
                              'Gasto administrativo', 
                              'Excedente de Gasto Administrativo',
                              'Beneficios Totales',
                              'Balance Corriente', 'Balance Capitalizado' ) )

diccionario_apo_corriente <-
  data.table( item = c('A2', 'A_sgo', 'A_afi',
                       'A_est_pen', 'A_est_rel_dep', 'A_est_cat', 'A_est_pen_sal', 'A_est_fij', 'A_est',
                       'A_issfa', 'A_isspol', 'A_seg_pri', 'A_otr'), 
              descripcion = c('Aportes Jefes de Familia SSC', 'Aporte Activos SGO', 'Total Aporte Afiliados',
                              'Aporte del Estado para Pensiones',
                              'Aporte del Estado del 0,3%',
                              'Aporte del Estado para Enfermedades Catastróficas',
                              'Aporte del Estado para Salud de Pensionistas',
                              'Aaporte del Estado Fijo',
                              'Total Aporte del Estado',
                              'Aporte del ISSFA', 'Aporte del ISSPOL', 'Aporte de los Seguros Privados',
                              'Total Aportes Otros' ) )

diccionario_ben_corriente <- 
  data.table( item = c('B3', 'B4', 'B8', 'B9', 'B_pen', 'B_aux', 'B_pen_sal', 'B_cot_dep_sal','B_sal',
                       'B_pen_sal_cat', 'B_cot_dep_sal_cat','B_sal_cat' ), 
              descripcion = c('Beneficios Pensionistas por Vejez', 'Beneficios Pensionistas por Invalidez', 
                              'Beneficios Pensionistas de Montepío por Orfandad', 'Beneficios Pensionistas de Montepío por Viudedad', 
                              'Beneficios Total por Pensiones', 
                              'Beneficios por auxilio de funerales', 
                              'Beneficios de Salud a Pensionistas',
                              'Beneficios de salud de Cotizantes y Dependientes',
                              'Beneficios de Salud Total al nucleo familiar',
                              'Beneficios de Salud a Pensionistas por Enfermedad Catastrófica',
                              'Beneficios de Salud de Cotizantes y Dependientes por Enfermedad Catastrófica',
                              'Beneficios de Salud al nucleo familiar por Enfermedades Catastróficas' ) )

diccionario_bal_dinamico <- 
  data.table( item = c('A_vap', 'Int_vap', 'AI_vap','A_est_vap', 'B_vap', 'G_vap', 'Exc_G_vap', 'V0', 'V'), 
              descripcion = c( 'Aportes Afiliados + Estado + Otro Aportes VAP', 'Interés reserva VAP',
                               'Aportes incluido interés de la reserva VAP',
                               'Aporte estatal VAP', 'Beneficios VAP', 'Gasto administrativo VAP', 'Excedente de Gasto administrativo VAP', 
                               'Reserva inicial', 'Balance actuarial') )

diccionario_apo_dinamico <- 
  data.table( item = c( 'A2_vap', 'A_sgo_vap', 'A_afi_vap',
                        'A_est_pen_vap', 'A_est_rel_dep_vap', 'A_est_cat_vap', 'A_est_pen_sal_vap', 'A_est_fij_vap', 'A_est_vap',
                        'A_issfa_vap', 'A_isspol_vap', 'A_seg_pri_vap', 'A_otr_vap' ), 
              descripcion = c('Aportes Jefes de Familia SSC VAP', 'Aporte Activos SGO VAP', 'Total Aporte Afiliados VAP',
                              'Aporte del Estado para Pensiones VAP',
                              'Aporte del Estado del 0,3% VAP',
                              'Aporte del Estado para Enfermedades Catastróficas VAP',
                              'Aporte del Estado para Salud de Pensionistas VAP',
                              'Aaporte del Estado Fijo VAP',
                              'Total Aporte del Estado VAP',
                              'Aporte del ISSFA VAP', 'Aporte del ISSPOL VAP', 'Aporte de los Seguros Privados VAP',
                              'Total Aportes Otros VAP') )

diccionario_ben_dinamico <- 
  data.table( item = c('B3_vap', 'B4_vap', 'B8_vap', 'B9_vap', 'B_pen_vap', 'B_aux_vap', 'B_pen_sal_vap', 'B_cot_dep_sal_vap','B_sal_vap',
                       'B_pen_sal_cat_vap', 'B_cot_dep_sal_cat_vap','B_sal_cat_vap'), 
              descripcion = c('Beneficios Pensionistas por Vejez VAP', 'Beneficios Pensionistas por Invalidez VAP', 
                              'Beneficios Pensionistas de Montepío por Orfandad VAP', 'Beneficios Pensionistas de Montepío por Viudedad VAP', 
                              'Beneficios Total por Pensiones VAP', 
                              'Beneficios por auxilio de funerales VAP', 
                              'Beneficios de Salud a Pensionistas VAP',
                              'Beneficios de Salud a Cotizantes y Dependientes VAP',
                              'Beneficios de Salud al nucleo familiar VAP',
                              'Beneficios de Salud a Pensionistas por Enfermedad Catastrófica VAP',
                              'Beneficios de Salud a Cotizantes y Dependientes por Enfermedad Catastrófica VAP ',
                              'Beneficios de Salud al nucleo familiar por Enfermedades Catastróficas VAP') )

diccionario_tot <- do.call('rbind', list(diccionario_ms, diccionario_bal_corriente, diccionario_apo_corriente,
                                         diccionario_ben_corriente, diccionario_bal_dinamico, diccionario_apo_dinamico, 
                                         diccionario_ben_dinamico))