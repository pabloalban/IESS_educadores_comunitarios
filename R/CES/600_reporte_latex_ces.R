# Preparación---------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

# # Gráficos genéricos------------------------------------------------------------------------------
source( 'R/402_graf_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/410_graf_poblacion_piramide.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
#source( 'R/500_tab_decrementos.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos Cesantía---------------------------------------------------------------------
source( 'R/ces/400_graf_analisis_contexto_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/401_graf_analisis_demografico_ces.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/ces/402_graf_inversiones_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/403_graf_analisis_financiero_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/405_graf_alisado_den_aport_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/406_graf_comp_primas_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/407_graf_balance_actuarial_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/408_graf_proyeccion_poblacion_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/409_graf_factores_riesgos_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/410_graf_tasas_decrementos_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/411_graf_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas Cesantía-----------------------------------------------------------------------
source( 'R/ces/500_tab_analisis_contexto_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/501_tab_analisis_demografico_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/502_tab_analisis_contable_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/503_tab_inversiones_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/505_tab_densidad_aportacion_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/506_tab_causas_desfinanciamiento_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/507_tab_balance_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/508_tab_escenarios_ces.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/ces/509_tab_proyeccion_poblacion_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/510_tab_factores_riesgos_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/511_tab_resumen_resultados_ces.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/ces/512_tab_decrementos_ces.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX-------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel------------------------------------------------------------------------------------
#source( 'R/ces/601_reporte_balance_ces.R', encoding = 'UTF-8', echo = FALSE )