# Preparación
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

 # Gráficos genéricos
#  source( 'R/402_graf_tasas_decrementos.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/403_graf_tasa_entrada.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/404_graf_macro_fin.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/405_graf_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/407_graf_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/410_graf_poblacion_piramide.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/414_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/TNRH/305_agregado_financiero_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/TNRH/306_calculo_prima_tnrh.R', encoding = 'UTF-8', echo = FALSE )

# 
#  # Gráficos específicos SAL
#  # source( 'R/ivm/401_graf_estados_financieros_ivm.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/400_graf_gastos_administrativos_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/401_graf_riesgos_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/402_graf_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/403_graf_balance_actuarial_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/408_graf_inversiones_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/409_graf_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )

source( 'R/TNRH/404_graf_analisis_demografico_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/TNRH/407_graf_balance_actuarial_tnrh.R', encoding = 'UTF-8', echo = FALSE )
#  # Tablas genéricas
#  # source( 'R/500_tab_decrementos.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/500_tab_decrementos.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/501_tab_macro_fin.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
# 
#  # Tablas específicas 
#  # source( 'R/ivm/500_tab_estados_financieros_ivm.R', encoding = 'UTF-8', echo = FALSE )
#  # source( 'R/ivm/501_tab_afiliados_jubilados_inicial_ivm.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/500_tab_macro_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/501_tab_gastos_administrativos_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/502_tab_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/503_tab_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/504_tab_escenarios_sal.R', encoding = 'UTF-8', echo = FALSE )

source( 'R/TNRH/504_tab_analisis_demografico_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/TNRH/509_tab_balance_actuarial_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/TNRH/510_tab_balance_corriente_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/TNRH/511_tab_agregado_demografico_tnrh.R', encoding = 'UTF-8', echo = FALSE )
# #
#  source( 'R/sal/503_tab_inversiones_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/504_tab_bal_act_resul_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/505_tab_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/506_tab_anexos_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/507_tab_estadistica_sal.R', encoding = 'UTF-8', echo = FALSE )
#  source( 'R/sal/508_tab_primas_sal.R', encoding = 'UTF-8', echo = FALSE )
# 
# # Reporte LaTeX
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel
