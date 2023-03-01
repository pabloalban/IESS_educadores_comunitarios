message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de primas por escenario' )

escenario <- paste0( 'escenario_', 1:1)
nom_esc <- c( 'Escenario 1')

pri <- NULL
for( i in 1:length( escenario ) ){
  load( paste0( parametros$RData_seg, 'IESS_DES_primas_', escenario[i], '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_DES_configuracion_', escenario[i], '.RData' ) )
  
  pri <- rbind( pri, 
                prima[ t == parametros$horizonte, 
                       list( nombre = nom_esc[ i ],
                             tasa_actuarial = paste0(formatC(esc$i_a*100,decimal.mark = ",",digits = 5),"%"),
                             aporte = paste0(formatC(as.numeric(esc$apo_act[1,2])*100,decimal.mark = ",", format='f',digits = 2),"%"),
                             pagos_extemporaneos=ifelse(i==1,'No',ifelse(i==2,'Si','No')),
                             tabla= ifelse(i==1,'Tabla 9.1',ifelse(i==2,'Tabla 9.2','Tabla 9.1')),
                             prima_med_niv = paste0(formatC(pri_med_niv_apo*100,decimal.mark = ",",digits = 5),"%")) ] )
}

xtb_pri <- xtable( pri, digits = c( 0, 0, 4, 4, 4, 4, 4 ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_tab_primas.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL)

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
