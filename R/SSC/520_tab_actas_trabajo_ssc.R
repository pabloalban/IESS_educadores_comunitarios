message( paste( rep('-', 100 ), collapse = '' ) )

source( 'R/502_tab_plantilla.R', encoding = 'UTF-8', echo = FALSE )

tildes_a_latex <- function(xtb_aux) {
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("%", "\\%", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("á", "\\\'{a}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("é", "\\\'{e}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("í", "\\\'{i}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("ó", "\\\'{o}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("ú", "\\\'{u}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("ñ", "$\\tilde{\\text{n}}$", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Á", "\\\'{A}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("É", "\\\'{E}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Í", "\\\'{I}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ó", "\\\'{O}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ú", "\\\'{U}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ñ", "$\\tilde{\\text{N}}$", x, fixed = TRUE) else x }))
  return(xtb_aux)
}

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_inputs_actas_ssc.RData' ) ) 

# Creación de actas --------------------------------------------------------------------------------
#Selección del acta a diseñar
i <- 1

num <- paste0('\\parbox{2cm}{\\centering\\vspace{-1.2cm}\\includegraphics[width=0.055\\textwidth]{graficos/logo_iess_azul.png}}
& \\multicolumn{3}{c!{\\color{white}\\vrule}}{\\bfseries\\parbox{10cm}{\\centering \\vspace{1mm} ACTA DE TRABAJO \\\\ Nro. IESS-DAIE-2021-0',i,'\\vspace{1mm} }}')

num <- xtable( data.frame( num ), digits = c( 0, 0 ) )
print( num,
       file = paste0( parametros$resultado_tablas, 'iess_acta_num', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )



message( '\tCreación de actas' )
tem <- copy( tema )

tem <-  data.frame( paste0( '&\\multicolumn{3}{c!{\\color{white}\\vrule}}{\\parbox{14cm}{\\vspace{1mm}',
                tildes_a_latex( tem ) ,'\\vspace{1mm}}}') )

aux_xtable <- xtable( tem, digits = c( 0, 0 ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_tema', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 1
fec <- copy( fecha )
lug <- copy( lugar )
fec <- tildes_a_latex( format( fec, '%d/%m/%Y') )

line <- data.frame( paste0( '\\textbf{Fecha:} & ', fec, '&', '\\textbf{Lugar:} & ', lug  ) )
aux_xtable <- xtable( line, digits = c( 0, 0 ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_linea1', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 2
ini <- copy( hini )
fin <- copy( hfin )
line <- data.frame( paste0( '\\textbf{Hora de Inicio:} & ', ini, '&', '\\textbf{Hora de Finalizaci\\\'{o}n:} & '
                            , fin  ) )
aux_xtable <- xtable( line, digits = c( 0, 0 ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_linea2', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 3 -------------------------------------------------------------------------------------------
pa <- copy( parti )
pa[ , n:=row.names(pa)]
pa <- data.frame( pa[ !is.na(Nombres)] )
pa <- data.table( tildes_a_latex( pa ) )
pa <- pa[ , list( n, Nombres, Dependencia, Cargo )]

aux_xtable <- xtable( pa, digits = c( 0, 0, 0, 0, 0 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_participantes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 4 -------------------------------------------------------------------------------------------
ag <- copy( agenda )
ag <- ag[ !is.na(`Agenda`)] 
ag[ , n:=row.names( ag )]
ag <- data.table( tildes_a_latex( ag ) )
ag <- data.frame( ag[ , list( n, Agenda )] )
aux <- matrix( ncol = 1, nrow = dim(ag)[1] )
for( i in 1:dim(ag)[1]){ # i <- 1
  aux[ i, 1 ] <- paste0( '& \\multicolumn{3}{c!{\\color{white}\\vrule}}{\\parbox{14cm}{\\vspace{1mm}', ag[ i,2 ],
                         '\\vspace{1mm}}}'  )
}
aux <- data.frame( aux )
aux_xtable <- xtable( aux, digits = c( 0, 0 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_agenda', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 5 -------------------------------------------------------------------------------------------
res <- copy( resumen )
res <- res[ !is.na(`Resumen`)] 
res[ , n:=row.names( res )]
res <- data.table( tildes_a_latex( res ) )
res <- data.frame( res[ , list( n, Resumen )] )
aux <- matrix( ncol = 1, nrow = dim(res)[1] )
for( i in 1:dim(res)[1]){ # i <- 1
  aux[ i, 1 ] <- paste0( i, '& \\multicolumn{3}{c!{\\color{white}\\vrule}}{\\parbox{14cm}{\\vspace{1mm}', res[ i,2 ],
                         '\\vspace{1mm}}}'  )
}
aux <- data.frame( aux )
aux_xtable <- xtable( aux, digits = c( 0, 0 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_resumen', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 6 -------------------------------------------------------------------------------------------
con <- copy( compromisos )
con <- data.frame( con[ !is.na(Compromisos)] )
if( dim(con)[1]==0 ){
  con <- data.table( matrix( "", ncol=4, nrow=1) )
  con[ , n:=row.names(con)]
  con <- con[ , list( n, V2, V3, V4)]
}else{
con[ , n:=row.names(con)]
con <- data.table( tildes_a_latex( con ) )
con <- con[ , list( n, Compromisos, Responsables, 
                    ifelse( is.na(Fecha_Tentativa), "", format( Fecha_Tentativa, '%d/%m/%Y') ) )]
}

aux_xtable <- xtable( con, digits = c( 0, 0, 0, 0, 0 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_compromisos', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Linea 7 -------------------------------------------------------------------------------------------
pa <- copy( parti )
pa[ , n:=row.names(pa)]
pa <- data.frame( pa[ !is.na(Nombres)] )
pa <- data.table( tildes_a_latex( pa ) )
pa <- pa[ , list( n, Nombres, Correo )]
pa[ , firma:=paste0("\\parbox{3cm}{\\vspace{0.5cm} \\vspace{0.5cm}}")]

aux_xtable <- xtable( pa, digits = c( 0, 0, 0, 0, 0 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_acta_firma', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = seq(1,dim(pa)[1], 1),
       sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

