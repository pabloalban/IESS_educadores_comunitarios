message( '\tLectura del portafolio de inversiones' )

# Carga de datos -----------------------------------------------------------------------------------
file_inversiones <- paste0( parametros$RData_seg, 'IESS_DES_inversiones.RData' )
load( file = file_inversiones )

message( '\tGenerando tablas de las inversiones' )
# Tabla de la evolución del protafolio DES ---------------------------------------------------------
aux <- inv_des
aux$rendimiento_promedio<-aux$rendimiento_promedio*100
aux <- as.data.table( aux )
aux<-aux[ , print_names := c( 'Certificados de Tesorer\\\'{i}a', 'Caja', 
                              'Total fondos administrados') ]
aux<-aux[ ,list(print_names,valor_nominal,rendimiento_promedio,plazo_remanente)]
aux_xtab <- xtable( aux, digits = c(0,0,rep(2,2),0) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))
# Tabla de la evolución del protafolio CES ---------------------------------------------------------
aux <- inv_total_ces
aux<-select(aux,-rdto_prom_real,-inflacion)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rendimiento_neto<-aux$rendimiento_neto*100
aux$rendimiento_neto_real<-aux$rendimiento_neto_real*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,0,rep(2,6),0) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_total_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla Rendimientos DES con ingresos y gastos -----------------------------------------------------
aux <- inv_rendimiento_ces
aux$rendimiento_bruto<-aux$rendimiento_bruto*100
aux$rendimiento_neto<-aux$rendimiento_neto*100
aux$corte<- as.Date(aux$corte,"%d/%m/%Y")
aux$corte<-format(aux$corte, "%b/%Y")
aux$corte<-as.character(aux$corte)
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_rend_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla del detalle de los ingresos que producieron las inversiones --------------------------------
aux <- inv_ingresos_ces
aux$ingresos <- c("De capital renta variable sector privado",
                  "De deuda renta fija sector privado",
                  "De deuda renta fija Sector P\\\'{u}blico",
                  "De Inversiones privativas pr\\\'{e}stamos hipotecarios",
                  "De Inversiones privativas pr\\\'{e}stamos quirografarios",
                  "En valuaci\\\'{o}n de inversiones",
                  "En venta de inversiones",
                  "Ingresos intereses interfondos",
                  "Intereses y comisiones ejercicios anteriores",
                  "Total")
aux_xtab <- xtable( aux, digits = c(0,rep(2,8)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_ingre_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       compress = FALSE,
       fileEncoding="UTF-8",
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

# Tabla del detalle de los gastos que producieron las inversiones ----------------------------------
aux <- inv_gastos_ces
aux$gastos_operativos <- c( "Comisi\\\'{o}n bolsa de valores",
                            "En valuaci\\\'{o}n de inversiones",
                            "En venta de inversiones",
                            "Provisi\\\'{o}n para valuaci\\\'{o}n inv. priv. hipotecarias",
                            "Provisiones antic\\\'{i}clicas y gen\\\'{e}ricas",
                            "Gasto liquidaci\\\'{o}n fideicomisos",
                            "Gastos de cobranza en inversiones privativas",
                            "Gastos liquidaci\\\'{o}n fideicomisos",
                            "Reverso de intereses",
                            "Gastos interfondos",
                            "Gastos varios",
                            "Gastos provisi\\\'{o}n mora patronal prest.",
                            "Provisi\\\'{o}n para valuaci\\\'{o}n inv. priv. quirografarias",
                            "Provisi\\\'{o}n para valuaci\\\'{o}n inversiones de capital",
                            "Seguro de fraude",
                            "Servicios bancarios",
                            "Servicios custodia de valores",
                            "Total general")
aux_xtab <- xtable( aux, digits = c(0,rep(2,8)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_gastos_ces', '.tex' ),
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

# Tabla inversiones en créditos --------------------------------------------------------------------
aux <- inv_creditos_ces %>% select(-inflacion)
#aux<-na.omit(aux)
aux$rendimiento_promedio_ponderado<-aux$rendimiento_promedio_ponderado*100
aux$rndt_prom_real<-aux$rndt_prom_real*100
aux$ano<-as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c(0,rep(2,7)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_creditos_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla evolución Inversiones en Bonos del Estado Ecuatoriano --------------------------------------
inv_instrumento_ces$ano<-as.character(inv_instrumento_ces$ano)
aux <- inv_instrumento_ces %>% 
        filter(sectores_y_seguros=='Bonos del Estado') %>% 
        select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_bonos_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla detalle al corte bonos del Estado Ecuatoriano ----------------------------------------------
aux <- inv_renta_fija_publico_ces %>% 
        filter(titulo=='Bonos') %>% select(-observ,-portafolio,-titulo)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_det_bonos_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla Inversiones en certificados de inversión de la CFN------------------------------------------
aux <- inv_instrumento_ces %>% 
        filter(sectores_y_seguros=='Certificados de Inversión CFN') %>% 
        select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_cert_CFN_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
#Tabla Evolución Inversiones en Obligaciones--------------------------------------------------------
aux <- inv_instrumento_ces %>% 
        filter(sectores_y_seguros=='Obligaciones') %>% 
        select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_obligaciones_inv_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#Tabla detalle al corte Obligaciones----------------------------------------------------------------
aux <- inv_renta_fija_privado_ces %>% 
        filter(portafolio=='S. CESANTIAS', titulo=='Obligaciones') %>% 
        select(emisor,
               valor_nominal_de_compra,
               saldo_valor_nominal,
               tasa_cupon,
               plazo_remanente,
               amortizacion_de_capital)
aux$emisor<-gsub(" ","",aux$emisor)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_det_obligaciones_inv_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla Evolución Inversiones en Titularizaciones----------------------------------------------------
aux <- inv_instrumento_ces %>% 
        filter(sectores_y_seguros=='Titularizaciones') %>% 
        select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_titularizaciones_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#Tabla detalle al corte Titularizaciones------------------------------------------------------------
aux <- inv_renta_fija_privado_ces %>% 
        filter(portafolio=='S. CESANTIAS', titulo=='Titularizaciones') %>% 
        select(emisor,
               valor_nominal_de_compra,
               saldo_valor_nominal,
               tasa_cupon,
               plazo_remanente,
               amortizacion_de_capital)
aux$emisor<-gsub(" ","",aux$emisor)
aux$emisor<-gsub("TIT.HIP.","",aux$emisor)

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2,2,0 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_det_titularizaciones_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
#Tabla Evolución de inversiones en Fidecomisos------------------------------------------------------
aux <- inv_instrumento_ces %>% 
        filter(sectores_y_seguros=='Fideicomisos y Negocios Fiduciarios') %>% 
        select(-partic,-inflacion,-fondo, -sectores_y_seguros,-plazo_prom_pond)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_fidecomisos_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )



#Tabla Evolución inversiones en Renta variable------------------------------------------------------
aux <- inv_instrumento_ces %>% 
        filter(sectores_y_seguros=='Renta Variable') %>% 
        select(-partic,-inflacion,-fondo, -sectores_y_seguros,-plazo_prom_pond)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100

aux_xtab <- xtable( aux, digits = c(0,0,2,2,2 ))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_renta_variable_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


#Tabla detalle corte  renta variable----------------------------------------------------------------
aux <- inv_renta_variable_ces %>%
        select(-fecha_corte,-no_inv,-nombre)
aux$fecha_compra<-as.character(aux$fecha_compra)
aux_xtab <- xtable( aux, digits = c(0,0,0,2,2,4,0,4,4,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_det_renta_variable_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Tabla Resumen situacion actual de las inversiones--------------------------------------------------
aux <- inv_instrumento_ces %>% 
        filter(ano=='2018' &
                       sectores_y_seguros !='Caja' &
                       sectores_y_seguros !='Privado ' &
                       sectores_y_seguros !='Público') %>% 
        select(-inflacion,-ano,-fondo,-partic)
aux$rdto_prom_pond<-aux$rdto_prom_pond*100
aux$rdt_pond_real<-aux$rdt_pond_real*100 
aux <- as.data.table( aux )
aux<-aux[ , print_names := c( 'Bonos del Estado',
                              'Obligaciones',
                              'Titularizaciones',
                              'Fideicomisos y Negocios Fiduciarios',
                              'Renta Variable',
                              'Pr\\\'{e}stamos',
                              'Total inversiones') ]
aux[NROW(aux),'valor_nominal']<- 0
aux[NROW(aux),'valor_nominal']<- sum(aux$valor_nominal)
aux<-aux[ ,list(print_names,valor_nominal,rdto_prom_pond,rdt_pond_real,plazo_prom_pond)]
aux_xtab <- xtable( aux, digits = c(0,2,2,2,2,2))

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inv_corte_ces', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtab)-1),
                         command = c(paste("\\hline \n"))))

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
