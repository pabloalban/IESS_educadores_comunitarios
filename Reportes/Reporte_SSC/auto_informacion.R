message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# Capítulo Demográfico
load( file = paste0( parametros$RData, 'IVM/', 'IESS_IVM_analisis_demografico.RData' ) ) 

REP$prom_anual_sgo <- format( round( mean(pob_afi_ini$Tasa, na.rm=T) * 100 , 2),
                              digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_sgo <- format( pob_afi_ini[ anio==2020]$Afiliados, digits = 0,
                       big.mark = '.', decimal.mark = ',', format = 'f' )

# Capítulo Demográfico del SSC----------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) ) 
REP$afi_ssc <- format( afi_hist_ssc[ Anio==2020]$Afiliados_activos, digits = 0,
                       big.mark = '.', decimal.mark = ',', format = 'f' )

REP$prom_ssc <- format( (afi_hist_ssc[ Anio==2020]$Afiliados_activos/afi_hist_ssc[ Anio==2011]$Afiliados_activos-1)*100,
                              digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_act_m_ssc <- format( prop_jef_act_inac[Descp=='Activos Masculino']$Por*100, digits = 2, nsmall=2,
                       big.mark = '.', decimal.mark = ',', format = 'f' )
REP$afi_act_f_ssc <- format( prop_jef_act_inac[Descp=='Activos Femenino']$Por*100, digits = 2, nsmall=2,
                             big.mark = '.', decimal.mark = ',', format = 'f' )
REP$afi_inact_m_ssc <- format( prop_jef_act_inac[Descp=='Inactivo Masculino']$Por*100, digits = 2, nsmall=2,
                             big.mark = '.', decimal.mark = ',', format = 'f' )
REP$afi_inact_f_ssc <- format( prop_jef_act_inac[Descp=='Inactivo Femenino']$Por*100, digits = 2, nsmall=2,
                             big.mark = '.', decimal.mark = ',', format = 'f' )
REP$afi_act_inact_ssc <- format( prop_jef_act_inac[, sum(Total, na.rm=T)], digits = 0, nsmall=0,
                               big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_prom_x_f_ssc <- format( edad_prom_pen[ tipo=='JEFES']$female, digits = 2, nsmall=2,
                                 big.mark = '.', decimal.mark = ',', format = 'f' )
REP$afi_prom_x_m_ssc <- format( edad_prom_pen[ tipo=='JEFES']$male, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )

REP$afi_por_m_ssc <- format( afi_hist_ssc[ Anio==2020]$Male/afi_hist_ssc[ Anio==2020]$Afiliados_activos *100 , digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )
REP$afi_por_f_ssc <- format( afi_hist_ssc[ Anio==2020]$Female/afi_hist_ssc[ Anio==2020]$Afiliados_activos *100 , digits = 2, nsmall=2,
                             big.mark = '.', decimal.mark = ',', format = 'f' )

REP$masa_ssc <- format( masa_ssc[ anio==2020]$masa_anual , digits = 2, nsmall=2,
                             big.mark = '.', decimal.mark = ',', format = 'f' )
REP$prom_masa_ssc <- format( (masa_ssc[ anio==2020]$masa_anual/masa_ssc[ anio==2009]$masa_anual-1)*100,
                        digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$por_masa_ssc <- format( ( masa_ssc[ anio==2020]$por )*100,
                             digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$vej_m_ssc <- jub_vejez_ssc[tipo=='Hombres' & Anio==2020]$Jubilados_vejez
REP$vej_f_ssc <- jub_vejez_ssc[tipo=='Mujeres' & Anio==2020]$Jubilados_vejez
REP$ben_vej_m_ssc <- jub_vejez_ssc[tipo=='Hombres' & Anio==2020]$Beneficio_pagado_anual
REP$ben_vej_f_ssc <- jub_vejez_ssc[tipo=='Mujeres' & Anio==2020]$Beneficio_pagado_anual


REP$inv_f_ssc <- jub_invalidez_ssc[tipo=='Mujeres' & Anio==2020]$Jubilados_invalidez
REP$inv_m_ssc <- jub_invalidez_ssc[tipo=='Hombres' & Anio==2020]$Jubilados_invalidez
REP$ben_inv_f_ssc <- jub_invalidez_ssc[tipo=='Mujeres' & Anio==2020]$Beneficio_pagado_anual
REP$ben_inv_m_ssc <- jub_invalidez_ssc[tipo=='Hombres' & Anio==2020]$Beneficio_pagado_anual

REP$pens_total_ssc <- REP$vej_m_ssc + REP$vej_f_ssc + REP$inv_f_ssc + REP$inv_m_ssc
REP$ch_pens_total_ssc <- format( REP$vej_m_ssc + REP$vej_f_ssc + REP$inv_f_ssc + REP$inv_m_ssc,
                              digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f')
REP$bens_total_ssc <- REP$ben_vej_m_ssc + REP$ben_vej_f_ssc + REP$ben_inv_f_ssc + REP$ben_inv_m_ssc
REP$ch_bens_total_ssc <- format( REP$ben_vej_m_ssc + REP$ben_vej_f_ssc + REP$ben_inv_f_ssc + REP$ben_inv_m_ssc,
                                 digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f')

REP$p_vej_ssc <- format(( REP$vej_m_ssc + REP$vej_f_ssc )/REP$pens_total_ssc * 100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_inv_ssc <- format(( REP$inv_f_ssc + REP$inv_m_ssc)/REP$pens_total_ssc * 100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_vej_m_ssc <- format(  jub_vejez_ssc[tipo=='Hombres' & Anio==2020]$Jubilados_vejez/REP$pens_total_ssc *100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_vej_f_ssc <- format(  jub_vejez_ssc[tipo=='Mujeres' & Anio==2020]$Jubilados_vejez/REP$pens_total_ssc * 100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_inv_f_ssc <- format(  jub_invalidez_ssc[tipo=='Mujeres' & Anio==2020]$Jubilados_invalidez/REP$pens_total_ssc * 100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_inv_m_ssc <- format(  jub_invalidez_ssc[tipo=='Hombres' & Anio==2020]$Jubilados_invalidez/REP$pens_total_ssc * 100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_cre_vej <- format( jub_vejez_ssc[tipo=='Total' & Anio==2020]$Jubilados_vejez/
                          jub_vejez_ssc[tipo=='Total' & Anio==2012]$Jubilados_vejez,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$prom_anual_vej <- format( round( mean(jub_vejez_ssc[tipo=='Total']$Tasa_crecimiento_pob, na.rm=T) * 100 , 2),
                              digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_prom_vej <- format( jub_vejez_ssc[tipo=='Total' & Anio==2018]$por_cre * 100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_ben_vej <- format( jub_vejez_ssc[tipo=='Total' & Anio==2018]$Tasa_crecimiento_ben * 100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_vej_f_2020_ssc <- format( REP$vej_f_ssc/(REP$vej_m_ssc + REP$vej_f_ssc) * 100,
                        digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_vej_m_2020_ssc <- format( REP$vej_m_ssc/(REP$vej_m_ssc + REP$vej_f_ssc) * 100,
                                digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$vej_prom_x_f_ssc <- format( edad_prom_pen[ tipo=='VEJEZ']$female, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )
REP$vej_prom_x_m_ssc <- format( edad_prom_pen[ tipo=='VEJEZ']$male, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_prom_inv <- format( jub_invalidez_ssc[tipo=='Total' & Anio==2018]$por_cre * 100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_ben_inv <- format( jub_invalidez_ssc[tipo=='Total' & Anio==2018]$Tasa_crecimiento_ben * 100,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_inv_f_2020_ssc <- format( REP$inv_f_ssc/(REP$inv_m_ssc + REP$inv_f_ssc) * 100,
                                digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_inv_m_2020_ssc <- format( REP$inv_m_ssc/(REP$inv_m_ssc + REP$inv_f_ssc) * 100,
                                digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inv_prom_x_f_ssc <- format( edad_prom_pen[ tipo=='INVALIDEZ']$female, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inv_prom_x_m_ssc <- format( edad_prom_pen[ tipo=='INVALIDEZ']$male, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dep_prom_x_f_ssc <- format( edad_prom_pen[ tipo=='DEPENDIENTES']$female, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )
REP$dep_prom_x_m_ssc <- format( edad_prom_pen[ tipo=='DEPENDIENTES']$male, digits = 2, nsmall=2,
                                big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_ben_vej_ssc <- format(( REP$ben_vej_m_ssc + REP$ben_vej_f_ssc )/REP$bens_total_ssc * 100,
                        digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_ben_inv_ssc <- format(( REP$ben_inv_f_ssc + REP$ben_inv_m_ssc)/REP$bens_total_ssc * 100,
                        digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$p_ben_vej_m_ssc <- format(  jub_vejez_ssc[tipo=='Hombres' & Anio==2020]$Beneficio_pagado_anual/REP$bens_total_ssc *100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_ben_vej_f_ssc <- format(  jub_vejez_ssc[tipo=='Mujeres' & Anio==2020]$Beneficio_pagado_anual/REP$bens_total_ssc * 100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_ben_inv_f_ssc <- format(  jub_invalidez_ssc[tipo=='Mujeres' & Anio==2020]$Beneficio_pagado_anual/REP$bens_total_ssc * 100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$p_ben_inv_m_ssc <- format(  jub_invalidez_ssc[tipo=='Hombres' & Anio==2020]$Beneficio_pagado_anual/REP$bens_total_ssc * 100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$dep_2020_ssc <- format(  dependientes_ssc[tipo=='Total' & Anio==2020]$Dependientes,
                                digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$dep_2015_ssc <- format(  dependientes_ssc[tipo=='Total' & Anio==2015]$Dependientes,
                             digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$rel_dep_ssc <- format(  (dependientes_ssc[tipo=='Total' & Anio==2020]$Dependientes/dependientes_ssc[tipo=='Total' & Anio==2015]$Dependientes)*100 ,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$act_m_g1 <- format(  past_jef_male_tipo_riesgo[ riesgo=='Requisitos no cumplidos']$por*100 ,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_m_g2 <- format(  past_jef_male_tipo_riesgo[ riesgo=='Requisitos cumplidos para invalidez y muerte']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_m_g3 <- format(  past_jef_male_tipo_riesgo[ riesgo=='Cumple requisitos para vejez en 5 años']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_m_g4 <- format(  past_jef_male_tipo_riesgo[ riesgo=='Cumple requisitos para vejez en 1 año o menos']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_f_g1 <- format(  past_jef_female_tipo_riesgo[ riesgo=='Requisitos no cumplidos']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_f_g2 <- format(  past_jef_female_tipo_riesgo[ riesgo=='Requisitos cumplidos para invalidez y muerte']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_f_g3 <- format(  past_jef_female_tipo_riesgo[ riesgo=='Cumple requisitos para vejez en 5 años']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$act_f_g4 <- format(  past_jef_female_tipo_riesgo[ riesgo=='Cumple requisitos para vejez en 1 año o menos']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inact_m_g1 <- format(  past_jef_male_tipo_riesgo_inac[ riesgo=='Requisitos no cumplidos']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_m_g2 <- format(  past_jef_male_tipo_riesgo_inac[ riesgo=='Requisitos cumplidos para invalidez y muerte']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_m_g3 <- format(  past_jef_male_tipo_riesgo_inac[ riesgo=='Cumple requisitos para vejez en 5 años']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_m_g4 <- format(  past_jef_male_tipo_riesgo_inac[ riesgo=='Cumple requisitos para vejez en 1 año o menos']$por*100 ,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_f_g1 <- format(  past_jef_female_tipo_riesgo_inac[ riesgo=='Requisitos no cumplidos']$por*100 ,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_f_g2 <- format(  past_jef_female_tipo_riesgo_inac[ riesgo=='Requisitos cumplidos para invalidez y muerte']$por*100 ,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_f_g3 <- format(  past_jef_female_tipo_riesgo_inac[ riesgo=='Cumple requisitos para vejez en 5 años']$por*100 ,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$inact_f_g4 <- format(  past_jef_female_tipo_riesgo_inac[ riesgo=='Cumple requisitos para vejez en 1 año o menos']$por*100 ,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Modelo Actuarial ---------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_indicadores.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) )

REP$tc <- format( geometric.mean( indicadores[ t>=parametros$anio_ini & t<=parametros$anio_fin]$tc ) ,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

aux <- copy( acum_dem )
aux <- aux[ ,  list( l2 = sum( l2, na.rm=T)), by = t ]
aux[ , s_l2:= shift( l2, 1, "lag", fill = NA ) ]
aux[ , var := l2/s_l2 -1 ]

REP$cre_ssc <- format( geometric.mean( aux[ t>=parametros$anio_ini & t<=parametros$anio_fin]$var ) * 100 ,
                  digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$cpti <- format( indicadores[ t==2020 ]$cpt ,
                   digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$cptf <- format( indicadores[ t==2040 ]$cpt ,
                   digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_act_inac <- format( (acum_dem[ t==2020, list(l2 = sum(l2, na.rm=T)), by=t]$l2/
               ( acum_dem[ t==2020, list(l2 = sum(l2, na.rm=T)), by=t]$l2 + 
                 acum_dem[ t==2020, list(l2_inac = sum(l2_inac, na.rm=T)), by=t]$l2_inac))*100,
               digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_inac_act <- format( (acum_dem[ t==2020, list(l2_inac = sum(l2_inac, na.rm=T)), by=t]$l2_inac/
                               ( acum_dem[ t==2020, list(l2 = sum(l2, na.rm=T)), by=t]$l2 + 
                                 acum_dem[ t==2020, list(l2_inac = sum(l2_inac, na.rm=T)), by=t]$l2_inac))*100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_act_inac_f <- format( (acum_dem[ t==2040, list(l2 = sum(l2, na.rm=T)), by=t]$l2/
                               ( acum_dem[ t==2040, list(l2 = sum(l2, na.rm=T)), by=t]$l2 + 
                                   acum_dem[ t==2040, list(l2_inac = sum(l2_inac, na.rm=T)), by=t]$l2_inac))*100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$por_inac_act_f <- format( (acum_dem[ t==2040, list(l2_inac = sum(l2_inac, na.rm=T)), by=t]$l2_inac/
                               ( acum_dem[ t==2040, list(l2 = sum(l2, na.rm=T)), by=t]$l2 + 
                                   acum_dem[ t==2040, list(l2_inac = sum(l2_inac, na.rm=T)), by=t]$l2_inac))*100,
                            digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$lfe_i <- format( acum_dem[ t==2020, list( lfe=sum(l1, na.rm=T)), by=list(t)]$lfe ,
                     digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$act_i <- format( acum_dem[ t==2020, list( l2=sum(l2, na.rm=T)), by=list(t)]$l2 ,
                     digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$lfe_f <- format( acum_dem[ t==2040, list( lfe=sum(l1, na.rm=T)), by=list(t)]$lfe ,
                     digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$act_f <- format( acum_dem[ t==2040, list( l2=sum(l2, na.rm=T)), by=list(t)]$l2 ,
                     digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_vej_f_i <- format( acum_dem[ t==2020 & sexo=='Female']$l3,
                     digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_vej_f_f <- format( acum_dem[ t==2040 & sexo=='Female']$l3,
                         digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_vej_m_i <- format( acum_dem[ t==2020 & sexo=='Male']$l3,
                           digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_vej_m_f <- format( acum_dem[ t==2040 & sexo=='Male']$l3,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_inv_f_i <- format( acum_dem[ t==2020 & sexo=='Female']$l4,
                           digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_inv_f_f <- format( acum_dem[ t==2040 & sexo=='Female']$l4,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_inv_m_i <- format( acum_dem[ t==2020 & sexo=='Male']$l4,
                           digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_inv_m_f <- format( acum_dem[ t==2040 & sexo=='Male']$l4,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_viu_f_f <- format( acum_dem[ t==2040 & sexo=='Female']$l9,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_viu_m_f <- format( acum_dem[ t==2040 & sexo=='Male']$l9,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_orf_f_f <- format( acum_dem[ t==2040 & sexo=='Female']$l8,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )
REP$pen_orf_m_f <- format( acum_dem[ t==2040 & sexo=='Male']$l8,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_com_vej_i <- format(  (new_acum_dem[ t==2020, list(l23=sum(l23, na.rm=T)), by=t ]$l23/
                           ( new_acum_dem[ t==2020, list(l23=sum(l23, na.rm=T)), by=t ]$l23 + 
                             new_acum_dem[ t==2020, list(l24=sum(l24, na.rm=T)), by=t ]$l24) )*100,
                             digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$pen_com_vej_f <- format(  (new_acum_dem[ t==2040, list(l23=sum(l23, na.rm=T)), by=t ]$l23/
                                 ( new_acum_dem[ t==2040, list(l23=sum(l23, na.rm=T)), by=t ]$l23 + 
                                     new_acum_dem[ t==2040, list(l24=sum(l24, na.rm=T)), by=t ]$l24 ) )*100,
                              digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

# Escenario 1 --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_SSC_analisis_ratios_', esc$nombre, '.RData' ) )
# 
REP$opcion_esc_1 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")
 
REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V, 
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
REP$bal_act_esc_1_num <- balance_anual[ t == parametros$horizonte ]$V


REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )
 
REP$duracion_esc_1 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio -1
 
# REP$sup_apo_esc_1 <- min( balance_anual[ A_est >= A & t > 0 ]$t ) + parametros$anio_ini

REP$cap_ini <- format( esc$V0,
                       digits = 2, nsmall = 2, big.mark = '.',
                       decimal.mark = ',', format = 'f' )

# REP$dep_tas_ini_esc_1 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_1 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_1 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_1 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
#
# REP$factor_aporte_familias_esc_1 <- format( esc$factor,
#                                             digits = 2, nsmall = 2, big.mark = '.',
#                                             decimal.mark = ',', format = 'f' )

# REP$aporte_familias_esc_1 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)],
#                                      digits = 2, nsmall = 2, big.mark = '.',
#                                      decimal.mark = ',', format = 'f' )

REP$pri_med_niv_est_otr_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
                                            digits = 2, nsmall = 2, big.mark = '.',
                                            decimal.mark = ',', format = 'f' )

REP$pri_med_niv_sgo_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
                                             digits = 2, nsmall = 2, big.mark = '.',
                                             decimal.mark = ',', format = 'f' )

# REP$pri_med_niv_sgo_apo_est_pen_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo_apo_est_pen,
#                                                  digits = 2, nsmall = 2, big.mark = '.',
#                                                  decimal.mark = ',', format = 'f' )
#
REP$porce_masa_dep_1 <- format(  esc$hip_esc$por_mas_rel_dep[1], digits = 4, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' )

# REP$apo_est_esc_1<-format( 100 * esc$aporte_estado,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_esc_1<-format( 100 * esc$aporte_estado / 0.4,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$apo_est_dep_esc_1<-format( 100 * esc$aporte_estado_dep,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_dep_esc_1<-format( 100 * esc$aporte_estado_dep/0.003,
#                                digits = 2, nsmall = 2, big.mark = '.',
#                                decimal.mark = ',', format = 'f' )
#
# REP$valor_est_fijo_esc_1 <- format( esc$aporte_estado_fijo,
#                                digits = 2, nsmall = 2, big.mark = '.',
#                                decimal.mark = ',', format = 'f' )
#
# REP$porce_valor_est_fijo_esc_1 <- format( 100 * esc$por_aporte_estado_fijo,
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#
# REP$tasa_act_esc_1<-format( 100 * esc$i_a,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )

# Escenario 2 --------------------------------------------------------------------------------------
escenario <- 'escenario_2'
load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_SSC_analisis_ratios_', esc$nombre, '.RData' ) )
# 
REP$opcion_esc_2 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")

REP$bal_act_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V, 
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
 
REP$bal_cap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )
 
REP$duracion_esc_2 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio -1

# REP$dep_tas_ini_esc_2 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_2 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_2 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_2 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
#
# REP$factor_aporte_familias_esc_2 <- format( esc$factor,
#                                       digits = 2, nsmall = 2, big.mark = '.',
#                                       decimal.mark = ',', format = 'f' )
#
# REP$aporte_familias_esc_2 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)],
#                                      digits = 2, nsmall = 2, big.mark = '.',
#                                      decimal.mark = ',', format = 'f' )

REP$pri_med_niv_est_otr_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' )

REP$pri_med_niv_sgo_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
                                     digits = 2, nsmall = 2, big.mark = '.',
                                     decimal.mark = ',', format = 'f' )

# REP$apo_est_esc_2 <- format( 100 * esc$aporte_estado,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_esc_2 <- format( 100 * esc$aporte_estado/0.4,
#                                   digits = 2, nsmall = 2, big.mark = '.',
#                                   decimal.mark = ',', format = 'f' )
#
# REP$apo_est_dep_esc_2 <- format( 100 * esc$aporte_estado_dep,
#                                digits = 2, nsmall = 2, big.mark = '.',
#                                decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_dep_esc_2 <- format( 100 * esc$aporte_estado_dep/0.003,
#                                       digits = 2, nsmall = 2, big.mark = '.',
#                                       decimal.mark = ',', format = 'f' )
#
# REP$valor_est_fijo_esc_2 <- format( esc$aporte_estado_fijo,
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#
# REP$porce_valor_est_fijo_esc_2 <- format( 100 * esc$por_aporte_estado_fijo,
#                                           digits = 2, nsmall = 2, big.mark = '.',
#                                           decimal.mark = ',', format = 'f' )
#
# REP$tasa_act_esc_2 <- format( 100 * esc$i_a,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )

# Escenario 3 --------------------------------------------------------------------------------------
escenario <- 'escenario_3'
load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_SSC_analisis_ratios_', esc$nombre, '.RData' ) )
 
REP$opcion_esc_3 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")

REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$duracion_esc_3 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1

# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# # REP$factor_aporte_familias_esc_3 <- format( esc$factor, 
# #                                             digits = 2, nsmall = 2, big.mark = '.', 
# #                                             decimal.mark = ',', format = 'f' )
# # 
# # REP$aporte_familias_esc_3 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)], 
# #                                             digits = 2, nsmall = 2, big.mark = '.', 
# #                                             decimal.mark = ',', format = 'f' )
# 
REP$pri_med_niv_est_otr_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' )

REP$pri_med_niv_sgo_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
                                     digits = 2, nsmall = 2, big.mark = '.',
                                     decimal.mark = ',', format = 'f' )
# 
# # REP$apo_est_esc_3 <- format( 100 * esc$aporte_estado,
# #                            digits = 2, nsmall = 2, big.mark = '.', 
# #                            decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_esc_3 <- format( 100 * esc$aporte_estado/0.4,
# #                                   digits = 2, nsmall = 2, big.mark = '.', 
# #                                   decimal.mark = ',', format = 'f' )
# # 
# # REP$apo_est_dep_esc_3 <- format( 100 * esc$aporte_estado_dep,
# #                                digits = 2, nsmall = 2, big.mark = '.', 
# #                                decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_dep_esc_3 <- format( 100 * esc$aporte_estado_dep/0.003,
# #                                       digits = 2, nsmall = 2, big.mark = '.', 
# #                                       decimal.mark = ',', format = 'f' )
# # 
# # REP$valor_est_fijo_esc_3 <- format( esc$aporte_estado_fijo,
# #                                     digits = 2, nsmall = 2, big.mark = '.', 
# #                                     decimal.mark = ',', format = 'f' )
# # 
# # REP$porce_valor_est_fijo_esc_3 <- format( 100 * esc$por_aporte_estado_fijo,
# #                                           digits = 2, nsmall = 2, big.mark = '.', 
# #                                           decimal.mark = ',', format = 'f' )
# # REP$tasa_act_esc_3 <- format( 100 * esc$i_a,
# #                             digits = 2, nsmall = 2, big.mark = '.', 
# #                             decimal.mark = ',', format = 'f' )

# Escenario Alternativo Primero --------------------------------------------------------------------
escenario <- 'escenario_4'
load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_SSC_analisis_ratios_', esc$nombre, '.RData' ) )
# 
REP$opcion_esc_4 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")

REP$bal_act_esc_4 <- format( balance_anual[ t == parametros$horizonte ]$V, 
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )
REP$bal_act_esc_4_num <- balance_anual[ t == parametros$horizonte ]$V


REP$bal_cap_esc_4 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$duracion_esc_4 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio -1

# REP$sup_apo_esc_1 <- min( balance_anual[ A_est >= A & t > 0 ]$t ) + parametros$anio_ini

REP$cap_ini_biess <- format( esc$V0,
                       digits = 2, nsmall = 2, big.mark = '.',
                       decimal.mark = ',', format = 'f' )

# REP$dep_tas_ini_esc_1 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_1 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_1 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_1 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
#
# REP$factor_aporte_familias_esc_1 <- format( esc$factor,
#                                             digits = 2, nsmall = 2, big.mark = '.',
#                                             decimal.mark = ',', format = 'f' )

# REP$aporte_familias_esc_1 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)],
#                                      digits = 2, nsmall = 2, big.mark = '.',
#                                      decimal.mark = ',', format = 'f' )

REP$pri_med_niv_est_otr_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' )

REP$pri_med_niv_sgo_esc_4 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
                                     digits = 2, nsmall = 2, big.mark = '.',
                                     decimal.mark = ',', format = 'f' )

# REP$pri_med_niv_sgo_apo_est_pen_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo_apo_est_pen,
#                                                  digits = 2, nsmall = 2, big.mark = '.',
#                                                  decimal.mark = ',', format = 'f' )
#
REP$porce_masa_dep_4 <- format(  esc$hip_esc$por_mas_rel_dep[1], digits = 4, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# REP$apo_est_esc_1<-format( 100 * esc$aporte_estado,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_esc_1<-format( 100 * esc$aporte_estado / 0.4,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$apo_est_dep_esc_1<-format( 100 * esc$aporte_estado_dep,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_dep_esc_1<-format( 100 * esc$aporte_estado_dep/0.003,
#                                digits = 2, nsmall = 2, big.mark = '.',
#                                decimal.mark = ',', format = 'f' )
#
# REP$valor_est_fijo_esc_1 <- format( esc$aporte_estado_fijo,
#                                digits = 2, nsmall = 2, big.mark = '.',
#                                decimal.mark = ',', format = 'f' )
#
# REP$porce_valor_est_fijo_esc_1 <- format( 100 * esc$por_aporte_estado_fijo,
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#
# REP$tasa_act_esc_1<-format( 100 * esc$i_a,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )


# Escenario Alternativo Segundo --------------------------------------------------------------------
escenario <- 'escenario_5'
load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_SSC_analisis_ratios_', esc$nombre, '.RData' ) )
# 
REP$opcion_esc_5 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")

REP$bal_act_esc_5 <- format( balance_anual[ t == parametros$horizonte ]$V, 
                             digits = 2, nsmall = 2, big.mark = '.', 
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_5 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$duracion_esc_5 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio -1

# REP$dep_tas_ini_esc_2 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_2 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_2 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_2 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.',
#                                  decimal.mark = ',', format = 'f' )
#
# REP$factor_aporte_familias_esc_2 <- format( esc$factor,
#                                       digits = 2, nsmall = 2, big.mark = '.',
#                                       decimal.mark = ',', format = 'f' )
#
# REP$aporte_familias_esc_2 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)],
#                                      digits = 2, nsmall = 2, big.mark = '.',
#                                      decimal.mark = ',', format = 'f' )

REP$pri_med_niv_est_otr_esc_5 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
                                         digits = 2, nsmall = 2, big.mark = '.',
                                         decimal.mark = ',', format = 'f' )

REP$pri_med_niv_sgo_esc_5 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
                                     digits = 2, nsmall = 2, big.mark = '.',
                                     decimal.mark = ',', format = 'f' )

# REP$apo_est_esc_2 <- format( 100 * esc$aporte_estado,
#                            digits = 2, nsmall = 2, big.mark = '.',
#                            decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_esc_2 <- format( 100 * esc$aporte_estado/0.4,
#                                   digits = 2, nsmall = 2, big.mark = '.',
#                                   decimal.mark = ',', format = 'f' )
#
# REP$apo_est_dep_esc_2 <- format( 100 * esc$aporte_estado_dep,
#                                digits = 2, nsmall = 2, big.mark = '.',
#                                decimal.mark = ',', format = 'f' )
#
# REP$porcen_apo_est_dep_esc_2 <- format( 100 * esc$aporte_estado_dep/0.003,
#                                       digits = 2, nsmall = 2, big.mark = '.',
#                                       decimal.mark = ',', format = 'f' )
#
# REP$valor_est_fijo_esc_2 <- format( esc$aporte_estado_fijo,
#                                     digits = 2, nsmall = 2, big.mark = '.',
#                                     decimal.mark = ',', format = 'f' )
#
# REP$porce_valor_est_fijo_esc_2 <- format( 100 * esc$por_aporte_estado_fijo,
#                                           digits = 2, nsmall = 2, big.mark = '.',
#                                           decimal.mark = ',', format = 'f' )
#
# REP$tasa_act_esc_2 <- format( 100 * esc$i_a,
#                             digits = 2, nsmall = 2, big.mark = '.',
#                             decimal.mark = ',', format = 'f' )


# Análisis de sensibilidad Escenario 1--------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad.RData' ) ) 

aux <- copy( sensi )
aux$ultimo <- as.numeric(aux$ultimo)

REP$min_ultimo <- format( as.character(aux[ which( aux$balance==min(aux$balance))]$ultimo),
                          digits = 0, nsmall = 0, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$tasa_ultimo <- format( aux[ which( aux$balance==min(aux$balance))]$tasa,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad_medica.RData' ) ) 

aux <- copy( sensi )

REP$infl_med <- format( aux[ which( aux$escenarios_part==max(aux$escenarios_part))]$escenarios_part * 100,
                          digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$balance_med <- format( aux[ which( aux$escenarios_part==max(aux$escenarios_part))]$balance,
                           digits = 2, nsmall = 2, big.mark = '.', decimal.mark = ',', format = 'f' )

REP$inf_med_opc <- ifelse( REP$bal_act_esc_1_num < aux[ which( aux$escenarios_part==max(aux$escenarios_part))]$balance,
                           'menor', 'mayor')
