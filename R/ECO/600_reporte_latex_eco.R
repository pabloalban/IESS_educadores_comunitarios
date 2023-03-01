# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

# # Gráficos genéricos
source( 'R/402_graf_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de RTR ----------------------------------------------------------------------
source( 'R/eco/400_graf_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/401_graf_situacion_actual_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/401_graf_analisis_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/403_graf_inversiones_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de RTR ------------------------------------------------------------------------
source( 'R/eco/500_tab_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/501_tab_analisis_financiero_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/501_tab_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/502_tab_situacion_actual_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/503_tab_causas_desfinanciamientos_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/503_tab_inversiones_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/506_tab_escenarios_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/eco/508_tab_causas_desfinanciamientos_ivm.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------
# source( 'R/600_reporte_poblacion.R', encoding = 'UTF-8', echo = FALSE )
