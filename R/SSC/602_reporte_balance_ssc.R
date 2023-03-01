message( paste( rep('-', 100 ), collapse = '' ) )

require( XLConnect, quietly = TRUE )

source( 'R/SSC/510_descripcion_var_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

#escenario <- 'escenario_1'
#load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', escenario, '.RData' ) )

escenarios <- paste0( 'escenario_', 1:5 )

for ( escenario in escenarios ) { # escenario <- escenarios[1]
  
  # Reporte balance corriente ------------------------------------------------------------------------
  message( '\tGenerando balance corriente_', escenario )
  
  load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', escenario, '.RData' ) )
  
  rep_balance <- balance_anual[ , list( anio = t + parametros$anio, t, 
                                        M, MD, MS, A2, A_sgo, A_afi,
                                        A_est_pen, A_est_rel_dep, A_est_cat, A_est_pen_sal, A_est_fij, A_est,
                                        A_issfa, A_isspol, A_seg_pri, A_otr, A, Int = interes, AI=AA,
                                        B3, B4, B8, B9, B_pen, B_aux, B_pen_sal, B_cot_dep_sal, B_sal,
                                        B_pen_sal_cat, B_cot_dep_sal_cat, B_sal_cat, B, G, Exc_G, V_cor, V_cap ) ]
  setorder( rep_balance, t )
  
  rep_nom <- paste0( parametros$resultado_seguro, 'IESS_SSC_balances_', escenario, '.xlsx' )
  if ( file.exists( rep_nom ) ) {
    file.remove( rep_nom )
  }
  rep_file <- loadWorkbook( rep_nom, create = TRUE )
  
  num_style<-createCellStyle( rep_file )
  head_style<-createCellStyle( rep_file )
  date_style<-createCellStyle( rep_file )
  
  setDataFormat( num_style, format = '#,##0.00' )
  setDataFormat( date_style, format = 'yyyy-m-d' )
  setWrapText( head_style, wrap = TRUE )
  setFillPattern( head_style, fill = XLC$FILL.SOLID_FOREGROUND )
  setFillForegroundColor( head_style, color = XLC$COLOR.LIGHT_GREEN )
  
  # Diccionario variables ----------------------------------------------------------------------------
  sheet_name <- 'diccionario'
  createSheet( rep_file, name = sheet_name )
  
  setColumnWidth( rep_file, sheet = sheet_name, column = 1, width = 4500 )
  setColumnWidth( rep_file, sheet = sheet_name, column = 2, width = 6000 )
  writeWorksheet( rep_file, diccionario_tot, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )
  writeWorksheet( rep_file, 'VAP = valor actuarial presente', sheet = sheet_name, 
                  startRow = nrow(diccionario_tot)+3, startCol = 1, header = FALSE )
  
  setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( diccionario_tot ) ), 
                col = 1:ncol( diccionario_tot ), cellstyle = head_style )
  
  # -------------------------------------------------------------------------------------------------
  sheet_name <- 'balance_corriente'
  createSheet( rep_file, name = sheet_name )
  
  setColumnWidth( rep_file, sheet = sheet_name, column = 1:2, width = 1500 )
  setColumnWidth( rep_file, sheet = sheet_name, column = 3:ncol( rep_balance ), width = 5000 )
  writeWorksheet( rep_file, rep_balance, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )
  
  setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( rep_balance ) ), 
                col = 1:ncol( rep_balance ), cellstyle = head_style )
  
  setCellStyle( rep_file, sheet = sheet_name, 
                row = rep( 2:( nrow( rep_balance ) + 1 ), ncol( rep_balance ) - 2 ), 
                col = rep( 3:ncol( rep_balance ), each = nrow( rep_balance ) ), 
                cellstyle = num_style )
  
  # Reporte balance corriente ------------------------------------------------------------------------
  message( '\tGenerando balance actuarial_', escenario )
  rep_balance <- balance_anual[ , list( anio = t + parametros$anio_ini, t, 
                                        M_vap, MD_vap, MS_vap, A2_vap, A_sgo_vap, A_afi_vap,
                                        A_est_pen_vap, A_est_rel_dep_vap, A_est_cat_vap, A_est_pen_sal_vap, A_est_fij_vap, A_est_vap,
                                        A_issfa_vap, A_isspol_vap, A_seg_pri_vap, A_otr_vap, A_vap, Int_vap, AI_vap,
                                        B3_vap, B4_vap, B8_vap, B9_vap, B_pen_vap, B_aux_vap, B_pen_sal_vap, B_cot_dep_sal_vap, B_sal_vap,
                                        B_pen_sal_cat_vap, B_cot_dep_sal_cat_vap, B_sal_cat_vap, B_vap, G_vap, Exc_G_vap, V0,
                                        V ) ]
  setorder( rep_balance, t )
  
  sheet_name <- 'balance_actuarial'
  createSheet( rep_file, name = sheet_name )
  
  setColumnWidth( rep_file, sheet = sheet_name, column = 1:2, width = 1500 )
  setColumnWidth( rep_file, sheet = sheet_name, column = 3:ncol( rep_balance ), width = 6000 )
  writeWorksheet( rep_file, rep_balance, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )
  
  setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( rep_balance ) ), 
                col = 1:ncol( rep_balance ), cellstyle = head_style )
  
  setCellStyle( rep_file, sheet = sheet_name, 
                row = rep( 2:( nrow( rep_balance ) + 1 ), ncol( rep_balance ) - 2 ), 
                col = rep( 3:ncol( rep_balance ), each = nrow( rep_balance ) ), 
                cellstyle = num_style )
  
  message( '\tEscribiendo resultados_', escenario )
  saveWorkbook( rep_file )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
