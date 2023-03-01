# Preparación reporte ------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos
source( 'R/402_graf_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/403_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/405_graf_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/414_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas
source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos SSC
source( 'R/SSC/400_graf_analisis_demografico_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/401_graf_analisis_financiero_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/402_graf_inversiones_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/404_graf_analisis_contexto_economico_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/405_graf_proyecciones_ssc.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/ssc/408_graf_inversiones_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/406_graf_balance_actuarial_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/407_graf_analisis_sensibilidad_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/410_graf_biometricas_estaticas_ssc.R', encoding = 'UTF-8', echo = FALSE )

source( parametros$reporte_script_latex, encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/412_conversion_estados_financieros_excel_to_pdf.R', encoding = 'UTF-8', echo = FALSE )

source( 'R/SSC/411_graf_convertir_pdf_png_ssc.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas SSC
# source( 'R/ssc/500_tab_introduccion_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/500_tab_analisis_demografico_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/501_tab_analisis_financiero_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/502_tab_analisis_contexto_economico_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/506_tab_proyecciones_ssc.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/ssc/504_tab_decrementos_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ssc/505_tab_inversiones_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/507_tab_escenarios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/508_tab_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/511_tab_resumen_resultados_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/512_tab_causas_desfinanciamiento_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/513_tab_analisis_sensibilidad_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/515_tab_biometricas_estaticas_ssc.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------
source( 'R/SSC/601_reporte_poblacion_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/602_reporte_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )