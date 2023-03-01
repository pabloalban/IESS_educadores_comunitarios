message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga de los cesantes con su número de aportaciones y meses consecutivos cesante------------------
message( '\tCargando número de cesantes con al menos 24 aportaciones (6 últimas consecutivas) y 
         2 meses consecutivos cesantes' )
load( paste0(  parametros$RData_seg, 'IESS_DES_afiliados_cumplen_requisitos.RData'  ) )

# Número número de cesantes con al menos 24 aportaciones y 2 meses consecutivos cesantes------------
# Por rango de edad, sexo y año---------------------------------------------------------------------
tabla_des<-tabla_des %>%
          mutate(etiqueta_superior=paste0(anio,"-",sexo)) %>%
          arrange(sexo,anio) %>%
          select(rango,etiqueta_superior,cesantes)

aux <- spread(tabla_des, etiqueta_superior,cesantes)

aux['2016-T']<-aux$`2016-F`+aux$`2016-M`
aux['2017-T']<-aux$`2017-F`+aux$`2017-M`
aux['2018-T']<-aux$`2018-F`+aux$`2018-M`

aux <- aux %>%
      select(rango,`2016-M`,`2016-F`,`2016-T`,`2017-M`,`2017-F`,`2017-T`, `2018-M`,`2018-F`,`2018-T`)
aux$rango<-as.character(aux$rango)

aux <- aux %>% bind_rows( summarise_all( ., funs(if(is.numeric(.)) sum(.) else "Total") ) )
aux_xtab <- xtable( aux, digits = c(rep(0,11)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cesante_con_requisitos_des', '.tex' ),
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