message( '\tEstableciendo información para la configuración del reporte parte SAL' )

REP <- new.env()

#Parametros generales
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )

REP$inflacion <- format( hip_macro_resumen[6,2], digits = 2, nsmall = 2, big.mark = '.', 
                decimal.mark = ',', format = 'f' )
REP$cre_salarios <- format( hip_macro_resumen[2,2], digits = 2, nsmall = 2, big.mark = '.', 
                         decimal.mark = ',', format = 'f' )
REP$cre_sbu <- format( hip_macro_resumen[3,2], digits = 2, nsmall = 2, big.mark = '.', 
                            decimal.mark = ',', format = 'f' )

# Horizonte de estudio
REP$horizonte <- format( parametros$horizonte
                         ,digits = 0
                         , nsmall = 0
                         , big.mark = '.'
                         , decimal.mark = ','
                         , format = 'f' )
# reserva inicial
REP$reserva_inicial_sal <- format( parametros$reserva_ini, 
                       digits = 2, nsmall = 0, big.mark = '.',
                       decimal.mark = ',', format = 'f' )

# ------------------------------------------------------------------------------.
# PROYECCIONES DEMOGRAFICAS Y FINANCIERAS ----
load( paste0( "Y:/IESS_2020/RData/IVM/" ,'IESS_IVM_salidas_demograficos.RData' ) )

REP$media_crecimiento_afi_act <- format( media,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$max_tasa_cob <- format( 100 * tasa_cob[ year == 2060 ]$tcv
                            ,digits = 2
                            , nsmall = 2
                            , big.mark = '.'
                            , decimal.mark = ','
                            , format = 'f' )

REP$cap_2021 <- format( tasa_cob[ year == 2021 ]$cap
                            ,digits = 2
                            , nsmall = 2
                            , big.mark = '.'
                            , decimal.mark = ','
                            , format = 'f' )

REP$cap_2060 <- format( tasa_cob[ year == 2060 ]$cap
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$pcjact_2020 <- format( 100 * pctjact_2020
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$pcjinact_2020 <- format( 100 * pctjinac_2020
                           ,digits = 2
                           , nsmall = 2
                           , big.mark = '.'
                           , decimal.mark = ','
                           , format = 'f' )

REP$pcjact_2060 <- format( 100 * pctjact_2060
                           ,digits = 2
                           , nsmall = 2
                           , big.mark = '.'
                           , decimal.mark = ','
                           , format = 'f' )

REP$pcjinact_2060 <- format( 100 * pctjinac_2060
                             ,digits = 2
                             , nsmall = 2
                             , big.mark = '.'
                             , decimal.mark = ','
                             , format = 'f' )

REP$FL_2020 <- format( tasa_cob[year=='2021']$fuerza_la_f_m
                             ,digits = 2
                             , nsmall = 2
                             , big.mark = '.'
                             , decimal.mark = ','
                             , format = 'f' )

REP$AFI_2020 <- format( tasa_cob[year=='2021']$activos_f_m
                       ,digits = 2
                       , nsmall = 2
                       , big.mark = '.'
                       , decimal.mark = ','
                       , format = 'f' )

REP$FL_AFI_2020 <- format( 100 * ( tasa_cob[year=='2021']$activos_f_m / tasa_cob[year=='2021']$fuerza_la_f_m  )
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$FL_2060 <- format( tasa_cob[year=='2060']$fuerza_la_f_m
                       ,digits = 2
                       , nsmall = 2
                       , big.mark = '.'
                       , decimal.mark = ','
                       , format = 'f' )

REP$AFI_2060 <- format( tasa_cob[year=='2060']$activos_f_m
                        ,digits = 2
                        , nsmall = 2
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

REP$FL_AFI_2060 <- format( 100 * ( tasa_cob[year=='2060']$activos_f_m / tasa_cob[year=='2060']$fuerza_la_f_m  )
                           ,digits = 2
                           , nsmall = 2
                           , big.mark = '.'
                           , decimal.mark = ','
                           , format = 'f' )

REP$jv_f <- format( tasa_cob[ year=='2021']$jv_female
                       ,digits = 0
                       , nsmall = 0
                       , big.mark = '.'
                       , decimal.mark = ','
                       , format = 'f' )

REP$jv_m <- format( tasa_cob[ year=='2021']$jv_male
                    ,digits = 0
                    , nsmall = 0
                    , big.mark = '.'
                    , decimal.mark = ','
                    , format = 'f' )

REP$jv_f_2060 <- format( tasa_cob[ year=='2060']$jv_female
                    ,digits = 0
                    , nsmall = 0
                    , big.mark = '.'
                    , decimal.mark = ','
                    , format = 'f' )

REP$jv_m_2060 <- format( tasa_cob[ year=='2060']$jv_male
                    ,digits = 0
                    , nsmall = 0
                    , big.mark = '.'
                    , decimal.mark = ','
                    , format = 'f' )


REP$dis_f_2010 <- format( pen_dis[ anio==2010]$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_f_2020 <- format( pen_dis[ anio==2020]$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_f_2060 <- format( pen_dis[ anio==2060]$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_m_2010 <- format( pen_dis[ anio==2010]$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_m_2020 <- format( pen_dis[ anio==2020]$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$dis_m_2060 <- format( pen_dis[ anio==2060]$hombres
                         ,digits = 0
                         , nsmall = 0
                         , big.mark = '.'
                         , decimal.mark = ','
                         , format = 'f' )


REP$viu_f_2012 <- format( pen_viu[ anio=='2012']$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_f_2020 <- format( pen_viu[ anio=='2020']$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_f_2060 <- format( pen_viu[ anio=='2060']$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_m_2012 <- format( pen_viu[ anio=='2012']$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_m_2020 <- format( pen_viu[ anio=='2020']$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$viu_m_2060 <- format( pen_viu[ anio=='2060']$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_f_2012 <- format( pen_orf[ anio==2012]$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_m_2012 <- format( pen_orf[ anio==2012]$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_m_2020 <- format( pen_orf[ anio==2020]$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_f_2020 <- format( pen_orf[ anio==2020]$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_m_2060 <- format( pen_orf[ anio==2060]$hombres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

REP$orf_f_2060 <- format( pen_orf[ anio==2060]$mujeres
                          ,digits = 0
                          , nsmall = 0
                          , big.mark = '.'
                          , decimal.mark = ','
                          , format = 'f' )

#------------------------------------------------------------------------------.
# Escenario 1 ----
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_analisis_ratios_', escenario, '.RData' ) )

# Tasas Mauricio#######
# Afiliados
REP$afi_otros_nocat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_8,
                               digits = 2, nsmall = 2, big.mark = '.', 
                               decimal.mark = ',', format = 'f' )
# Jubilados
REP$jubilados_esc_1       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_t,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
REP$jubilados_nocat_esc_1       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
REP$jubilados_cat_esc_1       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Hijos
REP$hijos_esc_1           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )

REP$hijos_nocat_esc_1           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                                           digits = 2, nsmall = 2, big.mark = '.', 
                                           decimal.mark = ',', format = 'f' )
REP$hijos_cat_esc_1           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                         digits = 1, nsmall = 1, big.mark = '.', 
                                         decimal.mark = ',', format = 'f' )
# Afiliados Catastroficas
REP$afi_otros_cat_esc_1   <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_7_8_cat,
                           digits = 2, nsmall = 2, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )
# Total
REP$total_esc_1           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )
# Total Legal
REP$total_legal_esc_1     <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total_legal,
                           digits = 2, nsmall = 2, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )
# Fin Tasas Mauricio##

# tasa actuarial
REP$tasa_act <- format( 100 * esc$i_a,
                        digits = 2, nsmall = 2, big.mark = '.', 
                        decimal.mark = ',', format = 'f' )

# balance actuarial en el último año del horizonte de estudio
REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )
# Deficit o superavit
REP$defi_super_esc_1 <- if(balance_anual[ t == parametros$horizonte ]$V > 0 ){'superávit'} else{'déficit'}

# último año con reserva positiva
REP$duracion_esc_1 <- max( 1, which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1

# porcentaje de cumplimineto de la contribución del estado
REP$apo_est_esc_1 <- format( 100 * esc$aporte_estado,
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

# prima media nivelada en el horizonte de estudio
REP$prima_niv_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est,
                               digits = 2, nsmall = 2, big.mark = '.', 
                               decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de menores de 18 años
REP$dep_18_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de extension de conyuges
REP$dep_cony_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima nivelada para jubilados jv y in
REP$prima_niv_jub_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para enfermedades catastróficas
REP$prima_niv_cat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_cat,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para Viudedad
REP$viudedad_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Pensionistas
REP$jub_cat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Viudedad
REP$viud_cat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6_cat,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Activos
REP$activos_cat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_cat,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Menores de 18 años
REP$menores_cat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                 digits = 1, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Extencion conyuges
REP$exten_cat_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_cat ,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima suficiente activos
REP$suf_acticos_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_suf,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima suficiente menores 18 años
REP$suf_dep_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima suficiente estension conyuges
REP$suf_extens_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_suf,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

#------------------------------------------------------------------------------.
# Escenario 2 ----
escenario <- 'escenario_2'
load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_analisis_ratios_', escenario, '.RData' ) )

# Tasas Mauricio#######
# Afiliados
REP$afi_otros_nocat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_8,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Jubilados
REP$jubilados_esc_2       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_t,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )

REP$jubilados_nocat_esc_2       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                           digits = 2, nsmall = 2, big.mark = '.', 
                                           decimal.mark = ',', format = 'f' )
REP$jubilados_cat_esc_2       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                                         digits = 2, nsmall = 2, big.mark = '.', 
                                         decimal.mark = ',', format = 'f' )

# Hijos
REP$hijos_esc_2           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )

REP$hijos_nocat_esc_2           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                                           digits = 2, nsmall = 2, big.mark = '.', 
                                           decimal.mark = ',', format = 'f' )
REP$hijos_cat_esc_2           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                         digits = 1, nsmall = 1, big.mark = '.', 
                                         decimal.mark = ',', format = 'f' )

# Afiliados Catastroficas
REP$afi_otros_cat_esc_2   <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_7_8_cat,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Total
REP$total_esc_2           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Total Legal
REP$total_legal_esc_2     <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total_legal,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Fin Tasas Mauricio##

# balance actuarial en el último año del horizonte de estudio
REP$bal_act_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Deficit o superavit
REP$defi_super_esc_2 <- if(balance_anual[ t == parametros$horizonte ]$V > 0 ){'superávit'} else{'déficit'}

# último año con reserva positiva
REP$duracion_esc_2 <- max( 1, which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1

# porcentaje de cumplimineto de la contribución del estado
REP$apo_est_esc_2 <- format( 100 * esc$aporte_estado,
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

# prima media nivelada en el horizonte de estudio
REP$prima_niv_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est,
                               digits = 2, nsmall = 2, big.mark = '.', 
                               decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de menores de 18 años
REP$dep_18_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de extension de conyuges
REP$dep_cony_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima nivelada para jubilados jv y in
REP$prima_niv_jub_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para enfermedades catastróficas
REP$prima_niv_cat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_cat,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para Viudedad
REP$viudedad_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Pensionistas
REP$jub_cat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Viudedad
REP$viud_cat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6_cat,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Activos
REP$activos_cat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_cat,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Menores de 18 años
REP$menores_cat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                 digits = 1, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Extencion conyuges
REP$exten_cat_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_cat ,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' )

# Prima suficiente activos
REP$suf_acticos_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_suf,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima suficiente menores 18 años
REP$suf_dep_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima suficiente estension conyuges
REP$suf_extens_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_suf,
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

# Escenario 3 ----
escenario <- 'escenario_3'
load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_analisis_ratios_', escenario, '.RData' ) )

# Tasas Mauricio#######
# Afiliados
REP$afi_otros_nocat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_8,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Jubilados
REP$jubilados_esc_3       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_t,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )

REP$jubilados_nocat_esc_3       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                           digits = 2, nsmall = 2, big.mark = '.', 
                                           decimal.mark = ',', format = 'f' )
REP$jubilados_cat_esc_3       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                                         digits = 2, nsmall = 2, big.mark = '.', 
                                         decimal.mark = ',', format = 'f' )
# Hijos
REP$hijos_esc_3           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )

REP$hijos_nocat_esc_3           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
REP$hijos_cat_esc_3           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                     digits = 1, nsmall = 1, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )


# Afiliados Catastroficas
REP$afi_otros_cat_esc_3   <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_7_8_cat,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Total
REP$total_esc_3           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Total Legal
REP$total_legal_esc_3     <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total_legal,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Fin Tasas Mauricio##

REP$vo_esc_3 <- format( 489002157.62#2403773950
                        ,digits = 0
                        , nsmall = 0
                        , big.mark = '.'
                        , decimal.mark = ','
                        , format = 'f' )

# balance actuarial en el último año del horizonte de estudio
REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 0, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Deficit o superavit
REP$defi_super_esc_3 <- if(balance_anual[ t == parametros$horizonte ]$V > 0 ){'superávit'} else{'déficit'}

# último año con reserva positiva
REP$duracion_esc_3 <- max( 1, which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1

# porcentaje de cumplimineto de la contribución del estado
REP$apo_est_esc_3 <- format( 100 * esc$aporte_estado,
                             digits = 2, nsmall = 0, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

# prima media nivelada en el horizonte de estudio
REP$prima_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est,
                               digits = 2, nsmall = 2, big.mark = '.', 
                               decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de menores de 18 años
REP$dep_18_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de extension de conyuges
REP$dep_cony_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima nivelada para jubilados jv y in
REP$prima_niv_jub_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para enfermedades catastróficas
REP$prima_niv_cat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_cat,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para Viudedad
REP$viudedad_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Pensionistas
REP$jub_cat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Viudedad
REP$viud_cat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6_cat,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Activos
REP$activos_cat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_cat,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Menores de 18 años
REP$menores_cat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                 digits = 1, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Extencion conyuges
REP$exten_cat_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_cat ,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' )

# Prima suficiente activos
REP$suf_acticos_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_suf,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima suficiente menores 18 años
REP$suf_dep_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima suficiente estension conyuges
REP$suf_extens_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_suf,
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )
#------------------------------------------------------------------------------.
# Escenario 4 ----
escenario <- 'escenario_4'
load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SAL_analisis_ratios_', escenario, '.RData' ) )

# Tasas Mauricio#######
# Afiliados
REP$afi_otros_nocat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_8,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Jubilados
REP$jubilados_esc_4       <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_t,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Hijos
REP$hijos_esc_4           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )

REP$hijos_nocat_esc_4           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                                           digits = 2, nsmall = 2, big.mark = '.', 
                                           decimal.mark = ',', format = 'f' )
REP$hijos_cat_esc_4           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                         digits = 2, nsmall = 2, big.mark = '.', 
                                         decimal.mark = ',', format = 'f' )
# Afiliados Catastroficas
REP$afi_otros_cat_esc_4   <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_2_6_7_8_cat,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Total
REP$total_esc_4           <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Total Legal
REP$total_legal_esc_4     <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_total_legal,
                                     digits = 2, nsmall = 2, big.mark = '.', 
                                     decimal.mark = ',', format = 'f' )
# Fin Tasas Mauricio##

REP$vo_esc_4 <- format( 489002157.62#2403773950
                             ,digits = 0
                             , nsmall = 0
                             , big.mark = '.'
                             , decimal.mark = ','
                             , format = 'f' )

# balance actuarial en el último año del horizonte de estudio
REP$bal_act_esc_4 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 0, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Deficit o superavit
REP$defi_super_esc_4 <- if(balance_anual[ t == parametros$horizonte ]$V > 0 ){'superávit'} else{'déficit'}

# último año con reserva positiva
REP$duracion_esc_4 <- max( 1, which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1

# porcentaje de cumplimineto de la contribución del estado
REP$apo_est_esc_4 <- format( 100 * esc$aporte_estado,
                             digits = 2, nsmall = 0, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

# prima media nivelada en el horizonte de estudio
REP$prima_niv_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est,
                               digits = 2, nsmall = 2, big.mark = '.', 
                               decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de menores de 18 años
REP$dep_18_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

# prima nivelada para financiar beneficios de extension de conyuges
REP$dep_cony_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima nivelada para jubilados jv y in
REP$prima_niv_jub_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para enfermedades catastróficas
REP$prima_niv_cat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_cat,
                                   digits = 2, nsmall = 2, big.mark = '.', 
                                   decimal.mark = ',', format = 'f' )

# Prima para Viudedad
REP$viudedad_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Pensionistas
REP$jub_cat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_3_4_cat,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Viudedad
REP$viud_cat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_6_cat,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Activos
REP$activos_cat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_cat,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Menores de 18 años
REP$menores_cat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_cat,
                                 digits = 1, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima para catastroficas de Extencion conyuges
REP$exten_cat_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_cat ,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' )

# Prima suficiente activos
REP$suf_acticos_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_suf,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# Prima suficiente menores 18 años
REP$suf_dep_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_7_suf,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

# Prima suficiente estension conyuges
REP$suf_extens_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_8_suf,
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

# write.xlsx( primas, paste0('C:/Users/jendry.toapanta/Downloads/','Resumen_Primas', '.xlsx') )
# 
