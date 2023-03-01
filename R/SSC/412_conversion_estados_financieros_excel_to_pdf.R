message( paste( rep('-', 100 ), collapse = '' ) )

#Ejecutando script de conversión de estados financieros de excel a pdf -----------------------------
message( '\tEjecutando script de conversión de estados financieros de excel a pdf' )

reticulate::source_python( paste0( parametros$work_dir,"R/SSC/conversion_estados_financieros_excel_to_pdf.py"))

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
