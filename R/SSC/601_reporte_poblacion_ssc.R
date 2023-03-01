message( paste( rep('-', 100 ), collapse = '' ) )

require( XLConnect, quietly = TRUE )

source( 'R/SSC/509_descripcion_var_poblacion_ssc.R', encoding = 'UTF-8', echo = FALSE )

load( paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) )

# Reporte población proyectada ---------------------------------------------------------------------
message( '\tGenerando reporte de población proyectada' )

proy[ is.na( proy )] <- 0

rep_pob_proy <- proy[ , list( anio = t + parametros$anio, sexo, t, x, 
                                  l2, l2_inac, l3, l4, l6, l7, l8, l9,
                                  l12,
                                  l23, l24, l25,
                                  l35,
                                  l45,
                                  l65, l75,
                                  l85, l95 ) ]

rep_pob_proy[ sexo=='Female', sexo := 'Femenino']
rep_pob_proy[ sexo=='Male', sexo := 'Masculino']
setorder( rep_pob_proy, t, sexo, x )

rep_nom <- paste0( parametros$resultado_seguro, 'IESS_SSC_poblacion_evolucion.xlsx' )
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

setColumnWidth( rep_file, sheet = sheet_name, column = 1, width = 2500 )
setColumnWidth( rep_file, sheet = sheet_name, column = 2, width = 6000 )
writeWorksheet( rep_file, diccionario_tot, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )
# writeWorksheet( rep_file, 'VAP = valor actuarial presente', sheet = sheet_name, 
#                 startRow = nrow(diccionario_tot)+3, startCol = 1, header = FALSE )

setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( diccionario_tot ) ), 
              col = 1:ncol( diccionario_tot ), cellstyle = head_style )

# -------------------------------------------------------------------------------------------------
sheet_name <- 'poblacion_proyectada_edad_sexo'
createSheet( rep_file, name = sheet_name )

setColumnWidth( rep_file, sheet = sheet_name, column = 1:4, width = 1500 )
setColumnWidth( rep_file, sheet = sheet_name, column = 5:ncol( rep_pob_proy ), width = 4000 )
# setRowHeight( rep_file, sheet = sheet_name, row = 1, height = 20 )
writeWorksheet( rep_file, rep_pob_proy, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )

setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( rep_pob_proy ) ),
              col = 1:ncol( rep_pob_proy ), cellstyle = head_style )

setCellStyle( rep_file, sheet = sheet_name,
              row = rep( 2:( nrow( rep_pob_proy ) + 1 ), ncol( rep_pob_proy ) - 4 ),
              col = rep( 5:ncol( rep_pob_proy ), each = nrow( rep_pob_proy ) ),
              cellstyle = num_style )

# -------------------------------------------------------------------------------------------------
sheet_name <- 'total_poblacion_proyectada'
createSheet( rep_file, name = sheet_name )

rep_pob_proy_total <- rep_pob_proy[, list(  
                                            l2 = sum (l2), l2_inac = sum(l2_inac), l3 = sum(l3),
                                            l4 = sum(l4), 
                                            l6 = sum(l6), l7 = sum(l7),
                                            l8 = sum(l8), l9 = sum(l9),
                                            l12 = sum(l12),
                                            l23 = sum(l23), l24 = sum(l24), l25 = sum(l25),
                                            l35 = sum(l35),
                                            l45 = sum(l45),
                                            l65 = sum(l65),
                                            l75 = sum(l75),
                                            l85 = sum(l85), 
                                            l95 = sum(l95)
),
list( t )]

setColumnWidth( rep_file, sheet = sheet_name, column = 1, width = 1500 )
setColumnWidth( rep_file, sheet = sheet_name, column = 2:ncol( rep_pob_proy_total ), width = 4000 )
# setRowHeight( rep_file, sheet = sheet_name, row = 1, height = 20 )
writeWorksheet( rep_file, rep_pob_proy_total, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )

setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( rep_pob_proy_total ) ),
              col = 1:ncol( rep_pob_proy_total ), cellstyle = head_style )

setCellStyle( rep_file, sheet = sheet_name,
              row = rep( 2:( nrow( rep_pob_proy_total ) + 1 ), ncol( rep_pob_proy_total ) - 2 ),
              col = rep( 2:ncol( rep_pob_proy_total ), each = nrow( rep_pob_proy_total ) ),
              cellstyle = num_style )

message( '\tEscribiendo resultados' )
saveWorkbook( rep_file )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

# message( paste( rep('-', 100 ), collapse = '' ) )
# 
# require( XLConnect, quietly = TRUE )
# 
# load( paste0( parametros$RData_seg, 'IESS_SSC_proyeccion_poblacion.RData' ) )
# 
# # Reporte población proyectada ---------------------------------------------------------------------
# message( '\tGenerando reporte de población proyectada' )
# rep_pob_proy <- pob_proy[ , list( anio = t + parametros$anio_ini, sexo, t, x, 
#                                   l1, l2, l3, l4, l5, l6, l2_cot, l2_ces ) ]
# setorder( rep_pob_proy, t, sexo, x )
# 
# rep_nom <- paste0( parametros$resultado_seguro, 'IESS_SSC_poblacion_evolucion.xlsx' )
# if ( file.exists( rep_nom ) ) {
#   file.remove( rep_nom )
# }
# rep_file <- loadWorkbook( rep_nom, create = TRUE )
# 
# num_style<-createCellStyle( rep_file )
# head_style<-createCellStyle( rep_file )
# date_style<-createCellStyle( rep_file )
# 
# setDataFormat( num_style, format = '#,##0.00' )
# setDataFormat( date_style, format = 'yyyy-m-d' )
# setWrapText( head_style, wrap = TRUE )
# setFillPattern( head_style, fill = XLC$FILL.SOLID_FOREGROUND )
# setFillForegroundColor( head_style, color = XLC$COLOR.LIGHT_GREEN )
# 
# sheet_name <- 'poblacion_proyectada'
# createSheet( rep_file, name = sheet_name )
# 
# setColumnWidth( rep_file, sheet = sheet_name, column = 1:4, width = 1500 )
# setColumnWidth( rep_file, sheet = sheet_name, column = 5:ncol( rep_pob_proy ), width = 4000 )
# # setRowHeight( rep_file, sheet = sheet_name, row = 1, height = 20 )
# writeWorksheet( rep_file, rep_pob_proy, sheet = sheet_name, startRow = 1, startCol = 1, header = TRUE )
# 
# setCellStyle( rep_file, sheet = sheet_name, row = rep( 1, ncol( rep_pob_proy ) ), 
#               col = 1:ncol( rep_pob_proy ), cellstyle = head_style )
# 
# setCellStyle( rep_file, sheet = sheet_name, 
#               row = rep( 2:( nrow( rep_pob_proy ) + 1 ), ncol( rep_pob_proy ) - 4 ), 
#               col = rep( 5:ncol( rep_pob_proy ), each = nrow( rep_pob_proy ) ), 
#               cellstyle = num_style )
# 
# saveWorkbook( rep_file )
# 
# 
# message( paste( rep('-', 100 ), collapse = '' ) )
# rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
# gc()