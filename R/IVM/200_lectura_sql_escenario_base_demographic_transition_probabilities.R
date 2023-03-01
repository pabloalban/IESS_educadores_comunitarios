message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tEjecutando configuraciones previas a los servidores' )

driver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", 
               paste0( parametros$Driver,'ojdbc6.jar' ) )

conexion <- dbConnect( driver,"jdbc:oracle:thin:@192.168.96.75:1521:orcl",
                       "boletin","actuariess" )

#Pensionistas ----------------------------------------------------------------
message( '\tConsulta SQL de apensionstas de RT' )
pen_rt <- dbGetQuery( conexion,
                      statement = read_file( paste0(parametros$SQL_seg,
                                                    'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                                                    'base_matriz_q_sgo_fem.sql')) )

pen_rt <- as.data.table( pen_rt )

#Para archivos en formato excel
file <- paste0( parametros$SQL_seg,
                'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                'base_matriz_q_sgo_fem.xlsx')

wb <- loadWorkbook( file  )
writeData( wb, 'datos', pen_rt, colNames = FALSE,
           startCol = 1,
           startRow = 2,) 

saveWorkbook(wb, file, overwrite = T)

# Para archivos en formato csv
write.table( pen_rt, 
            file=paste0(parametros$SQL_seg,
                       'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                       "base_matriz_q_sgo_fem.csv"),
            quote=FALSE, sep=",", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)


lista <- c('pen_rt')

save( list=lista, file = paste0( parametros$RSQL_seg,
                                 'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                                 'IESS_IVM_base_demographic_transition_probabilities.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
