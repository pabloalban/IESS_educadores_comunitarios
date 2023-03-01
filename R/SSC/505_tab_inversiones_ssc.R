message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <- paste0( parametros$RData_seg, 'IESS_SSC_inversiones.RData' )
load( file = file_inversiones )

#Función de tildes a latex--------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )


# Tabla de la evolución del portafolio -------------------------------------------------------------
aux <- recurs_adm_biess
aux$ano <- as.character(aux$ano)
aux$rendimiento_neto <- aux$rendimiento_neto*100
aux$rendimiento_ponderado <- aux$rendimiento_ponderado*100
aux$rendimiento_neto_real <- aux$rendimiento_neto_real*100

aux_xtab <- xtable( aux, digits = c(0,0,rep(2,6),0) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#Tabla Resumen situacion actual de las inversiones--------------------------------------------------
aux <- inver_corte
aux$rendimiento_promedio <- aux$rendimiento_promedio*100
aux$rendimiento_promedio_real <- aux$rendimiento_promedio_real*100 
aux <- as.data.table( aux )
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2))
aux_xtab <- tildes_a_latex(aux_xtab)

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_corte', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))



# Tabla Rendimientos con ingresos y gastos -----------------------------------------------------
aux <- rendimientos_netos
aux$rendimiento_bruto <- aux$rendimiento_bruto*100
aux$rendimiento_neto <- aux$rendimiento_neto*100
aux$corte_a <- format(aux$corte_a, "%b/%Y")
aux$corte_a <- as.character(aux$corte_a)
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rend_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla del detalle de los ingresos que producieron las inversiones --------------------------------
aux <- ingresos
aux[3,1]<-"De deuda renta fija sector p\\\'{u}blico"
aux[4,1]<-"De inversiones privativas pr\\\'{e}stamos quirografarios"
aux[5,1]<-"En valuaci\\\'{o}n inversiones"
aux_xtab <- xtable( aux, digits = c(0,rep(2,10)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_ingre_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla del detalle de los gastos que producieron las inversiones ----------------------------------
aux <- gastos_opera
aux[1,1]<-"Comisi\\\'{o}n bolsa de valores"
aux[3,1]<-"En valuaci\\\'{o}n de inversiones"
#aux[5,1]<-"Provisiones antic\\\'{i}clicas y gen\\\'{e}ricas"
aux[6,1]<-"Gasto liquidaci\\\'{o}n fideicomisos"
aux[9,1]<-"Gastos provisi\\\'{o}n mora patronal pr\\\'{e}stamos"
aux[10,1]<-"Provisi\\\'{o}n para valuaci\\\'{o}n inversiones privativas quirografarias"
aux[11,1]<-"Provisi\\\'{o}n para valuaci\\\'{o}n inversiones de capital "
aux[12,1]<-"Provisiones antic\\\'{i}clicas y gen\\\'{e}ricas"  
aux_xtab <- xtable( aux, digits = c(0,rep(2,10)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_gastos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       compress = FALSE,
       fileEncoding="UTF-8",
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla evolución Inversiones en Créditos --------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Créditos') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_creditos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Bonos del Estado') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla detalle Inversiones en Bonos del Estado Ecuatoriano al corte--------------------------------
aux <- detalle_bonos
#aux <- na.omit(aux)
#aux <- select(aux,-Detalle)
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_bonos_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla Recepción de Bonos del Estado por el 40% del pago de las pensiones---------------------------
aux <- detalle_bonos_40 %>%
        mutate(tasa=tasa*100,
               fecha_colocacion=as.character(fecha_colocacion),
               vencimiento=as.character(vencimiento),
               pago_del_periodo=as.character(format(detalle_bonos_40$pago_del_periodo,"%Y-%B")))

aux_xtab <- xtable( aux, digits = c(0,0,0,0,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_repbonos40_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla Recuperación de bonos del estado-------------------------------------------------------------
aux <- recuperacion_bonos %>%
        mutate(fecha_cupon = as.character( fecha_cupon ),
               fecha_vcmto = as.character( fecha_vcmto ) )

aux_xtab <- xtable( aux, digits = c(0,rep(0,4),2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_recup_bonos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(nrow(aux)-1, nrow(aux) ),
       sanitize.text.function = identity )

#Tabla Resumen de recuperación de bonos del estado-------------------------------------------------------------
aux_1 <- recuperacion_bonos[-nrow(recuperacion_bonos),] %>%
        mutate(fecha_cupon = as.character( fecha_cupon ),
               fecha_vcmto = as.character( fecha_vcmto ) ) %>%
        mutate( anio_recup = year( fecha_cupon ),
                anio_vcmto = year( fecha_vcmto ) ) %>%
        group_by( anio_recup ) %>%
        mutate( suma_interes = sum(interes, na.rm = TRUE)) %>%
        ungroup() %>%
                distinct( anio_recup, .keep_all = TRUE) %>%
                select( anio:=anio_recup, suma_interes)

aux_2 <- recuperacion_bonos[-nrow(recuperacion_bonos),] %>%
        mutate(fecha_cupon = as.character( fecha_cupon ),
               fecha_vcmto = as.character( fecha_vcmto ) ) %>%
        mutate( anio_recup = year( fecha_cupon ),
                anio_vcmto = year( fecha_vcmto ) ) %>%
        group_by( anio_vcmto ) %>%
        mutate( suma_cap = sum(capital, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct( anio_vcmto, .keep_all = TRUE) %>%
        select( anio:=anio_vcmto, suma_cap)

aux <- full_join( aux_2, aux_1, by ='anio') %>%
        mutate( suma_cap = ifelse( is.na(suma_cap), 0, suma_cap)) %>%
        mutate( total = suma_interes + suma_cap) %>%
        mutate( anio = as.character( anio ) ) %>%
        arrange( anio ) 


aux <- aux  %>%
        add_row( anio = 'Total',
                 suma_cap = sum( aux$suma_cap, na.rm = TRUE ),
                 suma_interes = sum( aux$suma_interes, na.rm = TRUE ),
                 total = sum(aux$total, na.rm = TRUE))

aux_xtab <- xtable( aux, digits = c(0, 0, rep(2,3)))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_resumen_recup_bonos_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1, nrow(aux) ),
       sanitize.text.function = identity )


#Tabla Evolución Inversiones en Obligaciones--------------------------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Obligaciones') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_obligaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla detalle de las inversiones en obligaciones---------------------------------------------------
aux <- detalle_obligaciones
aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0 ))
aux_xtab <- tildes_a_latex( aux_xtab )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_oblig_detalle_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla Evolución Inversiones en Titularizaciones----------------------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Titularizaciones') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_titularizaciones_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla Evolución Inversiones en Fideicomisos y Negocios Fiduciarios----------------------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Fideicomisos y Negocios Fiduciarios') %>%
        #na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento,-plazo_prom_pond)


aux_xtab <- xtable( aux, digits = c(0,0,2,2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_fidecomisos_hist_inv', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
