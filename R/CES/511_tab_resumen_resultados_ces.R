message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando la tabla de resumen de resultados de la valuación' )

#parametrización de los escenarios------------------------------------------------------------------
nom_esc <- c( 'Base')
escenario <- paste0( 'escenario_', 1:length( nom_esc ) )

#Creación data.frame--------------------------------------------------------------------------------
result<-NULL  
result_list <- vector(mode = "list", length = length( escenario ))
for( i in 1:length( escenario ) ){
  load( paste0( parametros$RData_seg, 'IESS_CES_configuracion_', escenario[i], '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_CES_balances_',escenario[i],'.RData' ) )
  
  aux <- balance_anual[ t == max(t), 
                        list( 
                          i_a = formatC( esc$hip_esc$i_a[2]*100, decimal.mark = ",", format='f', digits = 5),
                          i_r = formatC( esc$hip_esc$i_r[2]*100, decimal.mark = ",", format='f', digits = 5),
                          i_q = formatC( esc$hip_esc$i_q[2]*100, decimal.mark = ",", big.mark = ".", format='f', digits = 5 ),
                          V0 = formatC( V0, decimal.mark = ",", big.mark = ".", format='f', digits = 2 ), 
                          C0 = formatC( C0 , decimal.mark = ",", big.mark = ".", format='f', digits = 2 ), 
                          A2_per_vap = formatC( A2_per_vap, decimal.mark = ",", big.mark = ".", format='f', digits = 2 ), 
                          A2_pat_vap = formatC( A2_pat_vap, decimal.mark = ",", big.mark = ".", format='f', digits = 2 ), 
                          activo = formatC( A2_per_vap + A2_pat_vap + C0 + V0, decimal.mark = ",", big.mark = ".", format='f', digits = 2 ), 
                          B_vap = formatC( round( B_vap, 2 ),decimal.mark = ",",big.mark = ".", format='f',digits = 2), 
                          G_vap = formatC( round( G_vap, 2 ),decimal.mark = ",",big.mark = ".", format='f',digits = 2), 
                          C_vap = formatC( round( G_vap, 2 ),decimal.mark = ",",big.mark = ".", format='f',digits = 2), 
                          pasivo = formatC( B_vap + G_vap + C_vap, decimal.mark = ",", big.mark = ".", format='f',digits = 2),
                          V = formatC( V, decimal.mark = ",", big.mark = ".", format='f', digits = 2)
                               ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( 
    i_a='Tasa actuarial (\\%)',
    i_r='Tasa de crecimiento salarial (\\%)',
    i_q='Tasa pasiva referencial (\\%)',
    V0 = 'Patrimonio inicial (USD)',
    C0 = 'Saldo inicial cuentas individuales (USD)',
    A2_per_vap = 'Aporte personal (USD)', 
    A2_pat_vap = 'Aporte patronal (USD)', 
    activo = 'Activo actuarial (USD)',
    B_vap = 'Beneficios totales (USD)', 
    G_vap = 'Gastos administrativos (USD)',
    C_vap = 'Saldo al 2060 de las cuentas individuales (USD)',
    pasivo = 'Pasivo actuarial (USD)',
    V = 'Super\\\'{a}vit actuarial (USD)')
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- as_tibble(merge( aux2, aux1, by = 'variable', all.x = TRUE ))
  setnames(aux, c('item', 'descripcion', 'valor'))
  aux<-select(aux,-item)
  result_list[[i]]<-aux
  if ( i=='1' ) {
    result=aux 
  } else {
    result<-left_join(result,aux,by='descripcion')}
  rm( balance, balance_anual )
}

#Guardar en latex-----------------------------------------------------------------------------------
xtb_aux <- xtable(result)
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_resultados.tex' ),
       type = 'latex', 
       include.colnames = FALSE, 
       include.rownames = FALSE, 
       #format.args = list( decimal.mark = ',', big.mark = '.' ), 
       #hline.after = c(1, 2 , 3, 8, 9, 10, 11),
       only.contents = TRUE,
       sanitize.text.function = identity,
       hline.after = NULL)
#Borrando los dataframes----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()