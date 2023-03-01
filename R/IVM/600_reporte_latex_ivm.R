# Preparación
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )




# # Gráficos genéricos
source( 'R/402_graf_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/403_graf_tasa_entrada.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/405_graf_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/407_graf_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/410_graf_poblacion_piramide.R', encoding = 'UTF-8', echo = FALSE )

source( 'R/IVM/303_agregado_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/304_calculo_prima_ivm.R', encoding = 'UTF-8', echo = FALSE )

source( 'R/IVM/305_agregado_financiero_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/306_calculo_prima_tnrh.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos IVM
source( 'R/IVM/400_graf_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/401_graf_analisis_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/402_graf_analisis_macroeconomico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/403_graf_inversiones_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/404_graf_analisis_demografico_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/405_graf_salidas_demograficos.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/406_graf_balance_actuarial_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/407_graf_balance_actuarial_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/408_graf_analisis_sensibilidad_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas
# source( 'R/500_tab_decrementos.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas IVM
source( 'R/IVM/500_tab_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/501_tab_analisis_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/502_tab_analisis_macroeconomico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/503_tab_inversiones_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/504_tab_analisis_demografico_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/505_tab_salidas_demograficas_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/505_tab_balance_actuarial_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/506_tab_balance_corriente_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/507_tab_agregado_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/509_tab_balance_actuarial_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/510_tab_balance_corriente_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/511_tab_agregado_demografico_tnrh.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/508_tab_causas_desfinanciamientos_ivm.R', encoding = 'UTF-8', echo = FALSE )


source( 'R/IVM/107_lectura_agregado_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/IVM/507_tab_agregado_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/ivm/504_tab_escenarios_ivm.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/ivm/505_tab_primas_ivm.R', encoding = 'UTF-8', echo = FALSE )


# Reporte LaTeX
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel
# source( 'R/600_reporte_poblacion.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/ivm/601_reporte_balance_ivm.R', encoding = 'UTF-8', echo = FALSE )
