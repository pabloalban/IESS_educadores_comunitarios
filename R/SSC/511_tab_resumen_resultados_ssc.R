message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de resumen del balance y primas por escenario' )

escenario <- paste0( 'escenario_', 1:5 )
nom_esc <- c( 'Base', 'Pesimista', 'Legal', 'Alt. Primero', 'Alt. Segundo')

resu <- NULL
raiz <- NULL
for( i in 1:length( escenario ) ){ # i<-1
  load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', escenario[i], '.RData' ) )
  
  resu <- rbind( resu, 
                 balance_anual[ t == parametros$horizonte, 
                                list( nombre = nom_esc[ i ],
                                      balance = V,
                                      ultimo = ifelse( (max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1)!=parametros$anio_fin,
                                                        max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1, 
                                                        paste0("superior a ", (max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1))  ) ) ] )
  
  aux <- balance_anual[ , list( t, V_cap )]
  fit3 <- lm ( V_cap ~ poly( t, 3, raw = T), data = aux )
  # summary( fit3)
  est_3 <- function( x ){
    fit3$coefficients[[1]] + fit3$coefficients[[2]]*x + fit3$coefficients[[3]]*x^2 + fit3$coefficients[[4]]*x^3
  }
  # curve( est_3(x), 0, 20 )
  # if (i==1){
  #   est_3<- function(x){
  #     ( -2.149e+04)*x^4+(1.515e+05  )*x^3 +(-1.481e+07  )*x^2 +(-4.771e+07)*x+ (1.792e+09)
  #   }
  # }
  # if (i==2){
  #   est_3 <- function(x){
  #     (-4.149e+07 )*x^2 -(3.906e+07)*x+ (1.461e+09 )
  #   }
  # }
  val_root <- ifelse( resu$balance[i] < 0 & balance_anual$V0[1] > 0, 
                      format( uniroot(est_3, c( 0, parametros$horizonte ))$root,
                              digits = 2, nsmall = 2, decimal.mark = "," ),
                      paste0( "mayor a ", parametros$horizonte ) )
  raiz <- rbind( raiz, data.frame( nombre = nom_esc[ i ], root = val_root, ind = i) )
}

pri <- NULL
for( i in 1:length( escenario ) ){ # i<-1
  load( paste0( parametros$RData_seg, 'IESS_SSC_primas_', escenario[i], '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario[i], '.RData' ) )
  
  pri <- rbind( pri, 
                prima[ t == parametros$horizonte, 
                       list( nombre = nom_esc[ i ],
                             tasa = 100 * esc$hip_esc$i_a[1],
                             tasa_ssc = 100 * ( esc$hip_esc$apo_ind[1] + esc$hip_esc$apo_inv[1] ),
                             tasa_sgo = 100 * esc$hip_esc$apo_sgo[1],
                             tasa_est_afi = 100 * esc$hip_esc$apo_est_rel_dep[1],
                             tasa_est_pen = 100 * esc$hip_esc$apo_est[1],
                             prima_ssc = 100 * pri_med_niv_reg
                             #prima_sgo = 100 * pri_med_niv_sgo
                       ) ] )
  
}
pri[ prima_ssc < 0, prima_ssc := tasa_ssc ]

resu_pri <- as.data.table( merge( pri, resu, by = c('nombre'), all.x=T ) )
resu_pri[, ultimo := as.character( ultimo ) ]
resu_pri <- as.data.table( merge( resu_pri, raiz, by = c('nombre'), all.x = T ) )
setorder( resu_pri, ind)
resu_pri <- resu_pri[ , -c('ind')]

xtb_pri <- xtable( resu_pri, digits = c( 0, 0, rep( 2, 8 ), 2 ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_bal_prima_suficiente_re.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()