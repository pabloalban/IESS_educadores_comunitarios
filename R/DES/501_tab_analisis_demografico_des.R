message( paste( rep('-', 100 ), collapse = '' ) )

message( '\t Tablas del Análisis demográfico de la población cubierta' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_DES_analisis_demografico.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_historicos.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_masa_salarial_historico.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_DES_pagos_edad_sex.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_DES_CES_afi_tiempo_aportacion.RData' ) ) 

#Evolución de cotizantes a desempleo y cesantía-----------------------------------------------------
aux<-as.data.table(evo_anual_cotizantes_ces_des)
aux[ , tasa_crecimiento:=100*tasa_crecimiento]
aux$aniper<-as.character(aux$aniper)
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 2) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_cotizantes_ces_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Evolución de la masa salarial de los cotizantes a desempleo y cesantía-----------------------------------------------------
aux<-as.data.table(evo_masa_sal_ces_des)
aux[ , tasa_crecimiento:=100*tasa_crecimiento]
aux$aniper<-as.character(aux$aniper)
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial_ces_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

# Tabla salario promedio por aportaciones-----------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_afi_tiempo_aportacion.RData' ) )
aux <- copy( afi_tiempo_aportacion )

aux_xtable <- xtable( aux )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

message( '\t Tablas del Análisis demográfico de la población beneficiaria' )
#Número de beneficiarios de desempleo---------------------------------------------------------------
aux1<-siniestros_desempleo %>% 
      mutate(anio=year(sd_fecha_ord)) %>% 
      filter(sd_numero_pagos=='1') %>%  #Solo se contabiliza el primer pago
      distinct(anio,sd_tipo_pago,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_tipo_pago) %>%
      mutate(suma=n()) %>%
      arrange(anio) %>%
      select(anio,sd_tipo_pago,suma)%>%
      distinct(anio,sd_tipo_pago, .keep_all = TRUE) %>%      
      reshape2::dcast(.,anio~sd_tipo_pago,value.var = "suma",sum)
colnames(aux1)<-c("anio","benf_f","benf_v")

aux2<-siniestros_desempleo %>% 
      mutate(anio=year(sd_fecha_ord)) %>% 
      group_by(anio,sd_tipo_pago) %>%
      mutate(suma=sum(sd_valor_pagado)) %>%
      ungroup() %>% 
      #filter(anio<2019) %>%
      distinct(anio,sd_tipo_pago, .keep_all = TRUE) %>%
      arrange(anio) %>%
      select(anio,sd_tipo_pago,suma)%>%
      reshape2::dcast(.,anio~sd_tipo_pago,value.var = "suma") %>%
      mutate(pag_tot = rowSums(.[2:3]))
colnames(aux2)<-c("anio","pag_f","pag_v","pag_tot")

aux<-left_join(aux1,aux2,by="anio")
aux$anio<-as.character(aux$anio)
aux <- rbind(aux, c("Total", colSums(aux[,2:6])))
aux[2:6] <- lapply(aux[2:6], function(x) as.numeric(x))

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 2, 2, 2) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_tipo_pago_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux)-1,
       sanitize.text.function = identity
       #add.to.row = list(pos = list(nrow(aux_xtable)-1), command = c(paste("\\hline \n")))
       )

# Parte Fija Número y montos por pagos de desempleo---------------------------------------------------------------
aux1<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='F') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","benf_p1","benf_p2","benf_p3","benf_p4","benf_p5")) %>%
      ungroup()


aux2<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='F') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      #distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=sum(sd_valor_pagado,na.rm = TRUE)) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","pag_p1","pag_p2","pag_p3","pag_p4","pag_p5")) %>%
      ungroup()

aux<-left_join(aux1,aux2,by="anio")
aux$anio<-as.character(aux$anio)
aux <- rbind(aux, c("Total", colSums(aux[,2:ncol(aux)])))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_pf_numero_pago_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1, nrow(aux) ),
       sanitize.text.function = identity
       #add.to.row = list(pos = list(nrow(aux_xtable)-1), command = c(paste("\\hline \n")))
       )

# Parte variable: Número y montos por pagos de desempleo---------------------------------------------------------------
aux1<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='V') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","benf_p1","benf_p2","benf_p3","benf_p4","benf_p5")) %>%
      ungroup()


aux2<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='V') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      #distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=sum(sd_valor_pagado,na.rm = TRUE)) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","pag_p1","pag_p2","pag_p3","pag_p4","pag_p5")) %>%
      ungroup()

aux<-left_join(aux1,aux2,by="anio")
aux$anio<-as.character(aux$anio)
aux <- rbind(aux, c("Total", colSums(aux[,2:ncol(aux)])))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_pv_numero_pago_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1, nrow(aux) ),
       sanitize.text.function = identity
       #add.to.row = list(pos = list(nrow(aux_xtable)-1), command = c(paste("\\hline \n")))
       )

# Monto promedio (parte fija) del beneficio por pagos de desempleo----------------------------------
#Parte fija-----------------------------------------------------------------------------------------
aux1<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='F') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","benf_p1","benf_p2","benf_p3","benf_p4","benf_p5")) %>%
      ungroup()


aux2<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='F') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      #distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=sum(sd_valor_pagado,na.rm = TRUE)) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","pag_p1","pag_p2","pag_p3","pag_p4","pag_p5")) %>%
      ungroup()

auxf<- left_join(aux1,aux2,by="anio") %>%
      mutate(pro_p1=pag_p1/benf_p1,
             pro_p2=pag_p2/benf_p2,
             pro_p3=pag_p3/benf_p3,
             pro_p4=pag_p4/benf_p4,
             pro_p5=pag_p5/benf_p5) %>%
      select(anio,pro_p1,pro_p2,pro_p3,pro_p4,pro_p5)

#Parte variable-------------------------------------------------------------------------------------
aux1<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='V') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","benf_p1","benf_p2","benf_p3","benf_p4","benf_p5")) %>%
      ungroup()


aux2<-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='V') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      #distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(suma=sum(sd_valor_pagado,na.rm = TRUE)) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,suma)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE) %>%
      reshape2::dcast(.,anio~sd_numero_pagos,value.var = "suma") %>%
      magrittr::set_names(c("anio","pag_p1","pag_p2","pag_p3","pag_p4","pag_p5")) %>%
      ungroup()

auxv<-left_join(aux1,aux2,by="anio") %>%
     mutate(pro_p1=pag_p1/benf_p1,
            pro_p2=pag_p2/benf_p2,
            pro_p3=pag_p3/benf_p3,
            pro_p4=pag_p4/benf_p4,
            pro_p5=pag_p5/benf_p5) %>%
     select(anio,pro_p1,pro_p2,pro_p3,pro_p4,pro_p5)

aux<- left_join(auxf,auxv,by='anio')
aux$anio<-as.character(aux$anio)
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_prom_pago_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Valores pagados de la parte fija por tipo. edad y sexo--------------------------------------------------------------
aux<-siniestros_desempleo %>% 
   filter(sd_tipo_pago=='F') %>% 
   mutate(anio=year(sd_fecha_ord)) #%>% filter(anio<2019)

aux["rango"]<-"NA"
aux[which(aux$edad<='20'),]$rango<-"19-20"
aux[which(aux$edad >'20' & aux$edad<='30'),]$rango<-"21-30"
aux[which(aux$edad >'30' & aux$edad<='40'),]$rango<-"31-40"
aux[which(aux$edad >'40' & aux$edad<='50'),]$rango<-"41-50"
aux[which(aux$edad >'50' & aux$edad<='60'),]$rango<-"51-60"
aux[which(aux$edad >'60' & aux$edad<='70'),]$rango<-"61-70"
aux[which(aux$edad >'70' & aux$edad<='80'),]$rango<-"71-80"
aux[which(aux$edad >'80' & aux$edad<='90'),]$rango<-"81-90"
aux[which(aux$edad >'90'),]$rango<-"mayor a 90"


aux<- aux  %>% 
   group_by(anio,sd_genero,rango) %>%
   mutate(suma=sum(sd_valor_pagado,na.rm = TRUE)) %>%
   arrange(rango) %>%
   select(anio,rango,sd_genero,suma)%>%
   distinct(anio,rango,sd_genero,.keep_all = TRUE) %>%
   reshape2::dcast(.,rango~anio+sd_genero,value.var = "suma",sum) 

aux["t_2016"]<-aux$`2016_F`+aux$`2016_M`
aux["t_2017"]<-aux$`2017_F`+aux$`2017_M`
aux["t_2018"]<-aux$`2018_F`+aux$`2018_M`
aux["t_2019"]<-aux$`2019_F`+aux$`2019_M`
aux["t_2020"]<-aux$`2020_F`+aux$`2020_M`

aux <-aux %>%
   select(rango,
          '2016_M','2016_F',t_2016,
          '2017_M','2017_F',t_2017,
          '2018_M','2018_F',t_2018,
          '2019_M','2019_F',t_2019,
          '2020_M','2020_F',t_2020
   ) %>%
   ungroup()

aux <- rbind(aux, c("Total", colSums(aux[,2:ncol(aux)],na.rm = TRUE)))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux_xtable <- xtable( aux, digits = c( 0,0,rep(2,15)) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_tot_pag_pf_rango_edad_sexo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1, nrow(aux) ),
       sanitize.text.function = identity
       # add.to.row = list(pos = list(nrow(aux_xtable)-1),
       #                   command = c(paste("\\hline \n")))
)


#Valores pagados de la parte variable por tipo. edad y sexo--------------------------------------------------------------
aux<-siniestros_desempleo %>% 
     filter(sd_tipo_pago=='V') %>% 
     mutate(anio=year(sd_fecha_ord)) #%>% filter(anio<2019)

aux["rango"]<-"NA"
aux[which(aux$edad<='20'),]$rango<-"19-20"
aux[which(aux$edad >'20' & aux$edad<='30'),]$rango<-"21-30"
aux[which(aux$edad >'30' & aux$edad<='40'),]$rango<-"31-40"
aux[which(aux$edad >'40' & aux$edad<='50'),]$rango<-"41-50"
aux[which(aux$edad >'50' & aux$edad<='60'),]$rango<-"51-60"
aux[which(aux$edad >'60' & aux$edad<='70'),]$rango<-"61-70"
aux[which(aux$edad >'70' & aux$edad<='80'),]$rango<-"71-80"
aux[which(aux$edad >'80' & aux$edad<='90'),]$rango<-"81-90"
aux[which(aux$edad >'90'),]$rango<-"mayor a 90"


aux<- aux  %>% 
      group_by(anio,sd_genero,rango) %>%
      mutate(suma=sum(sd_valor_pagado,na.rm = TRUE)) %>%
      arrange(rango) %>%
      select(anio,rango,sd_genero,suma)%>%
      distinct(anio,rango,sd_genero,.keep_all = TRUE) %>%
      reshape2::dcast(.,rango~anio+sd_genero,value.var = "suma",sum) 

aux["t_2016"]<-aux$`2016_F`+aux$`2016_M`
aux["t_2017"]<-aux$`2017_F`+aux$`2017_M`
aux["t_2018"]<-aux$`2018_F`+aux$`2018_M`
aux["t_2019"]<-aux$`2019_F`+aux$`2019_M`
aux["t_2020"]<-aux$`2020_F`+aux$`2020_M`

aux <-aux %>%
      select(rango,
             '2016_M','2016_F',t_2016,
             '2017_M','2017_F',t_2017,
             '2018_M','2018_F',t_2018,
             '2019_M','2019_F',t_2019,
             '2020_M','2020_F',t_2020
             ) %>%
      ungroup()

aux <- rbind(aux, c("Total", colSums(aux[,2:ncol(aux)],na.rm = TRUE)))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux_xtable <- xtable( aux, digits = c( 0,0,rep(2,15)) )

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_tot_pag_pv_rango_edad_sexo_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux)-1, nrow(aux) ),
       sanitize.text.function = identity
       # add.to.row = list(pos = list(nrow(aux_xtable)-1),
       #                   command = c(paste("\\hline \n")))
       )

# Tabla salarios y afiliadps a desempleo por edad y aportaciones---------------------------------------
message( '\tTabla salarios y afiliados de desempleo por aportaciones y edad' )
aux <- tabla
aux_xtable <- xtable( aux, digits = c( rep(0,14) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_des', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c(34,36),
       sanitize.text.function = identity )



###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()