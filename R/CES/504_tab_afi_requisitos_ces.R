message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga de los cesantes con su número de aportaciones y meses consecutivos cesante------------------
message( '\tCargando número de cesantes con al menos 24 aportaciones  y 
         2 meses consecutivos cesantes' )
load( paste0(  parametros$RData_seg, 'IESS_CES_afiliados_cumplen_requisitos.RData'  ) )

# Número número de cesantes con al menos 24 aportaciones y 2 meses consecutivos cesantes------------
# Por rango de edad, sexo y año---------------------------------------------------------------------

#Generar rangos de edad--------------------------------------------------------
cortes<-c(1,seq(20,80,5),120)
etiquetas<-c(paste0("(",c(18,seq(20,80,5)),"-",c(seq(20,80,5),118),"]"))
tabla_ces <-  tabla_ces %>%
  mutate(rango=cut(edad, breaks = cortes,
                   labels = etiquetas,
                   #include.lowest = TRUE,
                   right = TRUE)) %>%
  group_by(rango,sexo) %>%
  mutate(cesantes=sum(cesantes,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(etiqueta_superior=paste0(anio,"-",sexo)) %>%
  arrange(sexo,anio) %>%
  select(rango,etiqueta_superior,cesantes)

aux <- spread(tabla_ces, etiqueta_superior,cesantes)
aux['2012-T']<-aux$`2012-F`+aux$`2012-M`
aux['2013-T']<-aux$`2013-F`+aux$`2013-M`
aux['2014-T']<-aux$`2014-F`+aux$`2014-M`
aux['2015-T']<-aux$`2015-F`+aux$`2015-M`
aux['2016-T']<-aux$`2016-F`+aux$`2016-M`
aux['2017-T']<-aux$`2017-F`+aux$`2017-M`
aux['2018-T']<-aux$`2018-F`+aux$`2018-M`

aux <- aux %>%
  select(rango,
         `2012-M`,`2012-F`,`2012-T`,
         `2013-M`,`2013-F`,`2013-T`,
         `2014-M`,`2014-F`,`2014-T`,
         `2015-M`,`2015-F`,`2015-T`,
         `2016-M`,`2016-F`,`2016-T`,
         `2017-M`,`2017-F`,`2017-T`,
         `2018-M`,`2018-F`,`2018-T`)
aux$rango<-as.character(aux$rango)

aux <- aux %>% bind_rows( summarise_all( ., funs(if(is.numeric(.)) sum(.) else "Total") ) )
aux_xtab <- xtable( aux, digits = c(rep(0,11)) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_cesante_con_requisitos_ces', '.tex' ),
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