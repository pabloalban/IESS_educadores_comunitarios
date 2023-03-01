# Preparación---------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Lectura--------------------------------------------------------------------------------------------
#source( 'R/sal/100_lectura_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/sal/101_lectura_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
#source( 'R/sal/106_lectura_salidas_cap4_1_6.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos--------------------------------------------------------------------------------
source( 'R/402_graf_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/403_graf_tasa_familia_tipo.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos SAL--------------------------------------------------------------------------
source( 'R/sal/400_graf_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/402_graf_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/403_graf_balance_actuarial_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/404_graf_inversiones_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/sal/405_graf_salidas_demograficos.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SAL/409_graf_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )


# Tablas genéricas----------------------------------------------------------------------------------
source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas SAL----------------------------------------------------------------------------
source('R/SAL/500_tab_analisis_demografico_ivm.R', encoding = 'UTF-8', echo = FALSE )
source('R/SAL/501_tab_analisis_financiero_sal.R', encoding = 'UTF-8', echo = FALSE )
source('R/SAL/502_tab_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
source('R/SAL/503_tab_estimacion_sal.R', encoding = 'UTF-8', echo = FALSE )
source('R/SAL/504_tab_escenarios_sal.R', encoding = 'UTF-8', echo = FALSE )
source('R/SAL/507_tab_estadistica_sal.R', encoding = 'UTF-8', echo = FALSE )
source('R/SAL/508_tab_inversiones_sal.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX-------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

