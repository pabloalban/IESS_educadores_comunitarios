message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo aporte solidario al SSC' )

# Carga de datos
file <- paste0( parametros$Data_seg, 'IESS_SSC_analisis_contexto_economico.xlsx' )

# Aporte Solidario SGO -----------------------------------------------------------------------------
message( '\tLeyendo aporte solidario del SGO' )
col_nom <- c( 'anio', 'aporte', 'incremento', 'tasa')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

aporte_sgo <-read_excel( file,
                           sheet="Aporte_Solidario_SGO",
                           col_names=TRUE,
                           guess_max = 24000 )
aporte_sgo <- as.data.table( aporte_sgo)
setnames(aporte_sgo, col_nom )

# Aporte Jefes de Familia --------------------------------------------------------------------------
message( '\tLeyendo aporte Jefes de Familia ' )
col_nom <- c( 'anio', 'aporte', 'incremento', 'tasa')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

aporte_jefes <-read_excel( file,
                         sheet="Aporte_Jefes",
                         col_names=TRUE,
                         guess_max = 24000 )
aporte_jefes <- as.data.table( aporte_jefes )
setnames(aporte_jefes, col_nom )

# Aporte Solidario ISSFA-ISSPOL --------------------------------------------------------------------
message( '\tLeyendo Aporte Solidario ISSFA-ISSPOL' )
col_nom <- c( 'anio', 'issfa', 'isspol')
col_tip <- c( 'character', 'numeric', 'numeric')

aporte_issfa_isspol <-read_excel( file,
                           sheet="Aporte_ISSFA_ISSPOL",
                           col_names=TRUE,
                           guess_max = 24000 )
aporte_issfa_isspol <- as.data.table( aporte_issfa_isspol )
setnames(aporte_issfa_isspol, col_nom )

# Aporte Solidario Seguros Privados ----------------------------------------------------------------
message( '\tLeyendo Aporte Seguros Privados' )
col_nom <- c( 'anio', 'aporte', 'incremento', 'tasa')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

aporte_sp <-read_excel( file,
                           sheet="Aporte_Seguros_Privados",
                           col_names=TRUE,
                           guess_max = 24000 )
aporte_sp <- as.data.table( aporte_sp )
setnames(aporte_sp, col_nom )

# Aporte Solidario 0,3% del Estado------------------------------------------------------------------
message( '\tLeyendo Aporte Solidario 0,3% del Estado' )
col_nom <- c( 'anio', 'aporte', 'incremento', 'tasa')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric')

aporte_est <-read_excel( file,
                        sheet="Aporte_Estado_03",
                        col_names=TRUE,
                        guess_max = 24000 )
aporte_est <- as.data.table( aporte_est )
setnames(aporte_est, col_nom )

# Crecimiento de las pensiones del SSC--------------------------------------------------------------
message( '\tLeyendo Crecimiento de las pensiones del SSC' )
col_nom <- c( 'anio', 'sbu', 'pension', 'incremento','tasa')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')

cre_pen <-read_excel( file,
                         sheet="Evolucion_crecimiento_pensiones",
                         col_names=TRUE,
                         guess_max = 24000 )
cre_pen <- as.data.table( cre_pen )
setnames( cre_pen, col_nom )

# Variacion de las tasas --------------------------------------------------------------------------
message( '\tLeyendo variación de las tasas' )
col_nom <- c( 'anio', 'sgo', 'jefes', 'sp', 'estado')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric')

tasas_aporte <-read_excel( file,
                      sheet="Tasas_aporte",
                      col_names=TRUE,
                      guess_max = 24000 )
tasas_aporte <- as.data.table( tasas_aporte )
setnames( tasas_aporte, col_nom )

# Leyendo proyección Masa Salarial ISSFA -------------------------------------------------------------------
message( '\tLeyendo proyección Masa Salarial ISSFA' )
col_nom <- c( 'anio', 'RT', 'NS', 'Total')

issfa_proy <-read_excel( file,
                           sheet="Proy_ISSFA",
                           col_names=TRUE,
                           guess_max = 24000, range = 'A1:D51' )
issfa_proy <- as.data.table( issfa_proy )
setnames( issfa_proy, col_nom )

lista <- c( 'aporte_sgo', 'aporte_jefes', 'aporte_issfa_isspol', 'aporte_sp', 'aporte_est', 
            'cre_pen', 'tasas_aporte', 'issfa_proy')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_contexto_economico.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
