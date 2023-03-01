message(paste(rep("-", 100), collapse = ""))

# Carga de los cesantes con su número de aportaciones y meses consecutivos cesante -----------------
message("\tCargando cesantes mensuales")
#load(paste0('D://Datos (D)//Bases//', "IESS_act_15_16.RData"))
#load(paste0('D://Datos (D)//Bases//', "IESS_act_19_20.RData"))
load(paste0('D://Datos (D)//Bases//', "IESS_inventario_afi.RData"))
#load(paste0('D://Datos (D)//Bases//', "IESS_act_dic_ene.RData"))
load(paste0('D://Datos (D)//Bases//', "IESS_avisos_salida.RData"))


inv_afi <- inv_afi %>% mutate( edad = as.integer(edad))


library(tictoc)
library(rlang)
require(utils)

tic.clearlog()

#Función de cesantes del sector público y privado---------------------------------------------------
cesantes <- function(data,x)
{
  aux <- eval(dplyr::filter(data,anio == (rlang::parse_expr(x)) ) ) 
  
  act <- aux %>%
    dplyr::select( cedula ) %>%
    distinct( cedula, .keep_all = TRUE)  %>% 
    left_join( . , inv_afi, by = 'cedula') 
  
  pla <- aux %>%
    mutate(act = 1)
  
  ces <- expand.grid(anio = as.integer(x),
                        mes = c(1,2,3,4,5,6,7,8,9,10,11,12),
                        cedula = act$cedula) %>%
    lazy_dt() %>%
    left_join( ., act, by = 'cedula') %>%
    left_join( ., pla, by = c('cedula'='cedula', 'anio'='anio', 'mes' = 'mes') ) %>%
    mutate( act = ifelse( is.na(act), 0, act ) ) %>%
    mutate( ces = lag(act) - act ) %>%
    filter( ces == '1', mes != c(1) )%>%
    as_tibble()
  
  ces <- ces %>% select( -edad, -sexo ) %>%
    left_join(., avisos_salida, by = c('cedula'= 'cedula', 'anio'='anio', 'mes'='mes') ) %>%
    filter( !is.na( fecininov )) %>%
    distinct(cedula, anio, mes,.keep_all = TRUE)
  return(ces)
  
  return(ces)
}


#Función de cesantes enero y diciembre--------------------------------------------------------------
cesantes_ene <- function(data,avisos_salida,x,y)
{
  aux <- eval(dplyr::filter(data,anio %in% c( rlang::parse_expr(x), 
                                             rlang::parse_expr(y) ) ) )
                             
  aux <- eval(dplyr::filter(aux, (anio == rlang::parse_expr(x) & mes == '12') |
                            (anio == rlang::parse_expr(y) & mes == '1' ) ) )
 
  act <- aux %>%
    dplyr::select( cedula ) %>%
    distinct( cedula, .keep_all = TRUE)  %>% 
    left_join( . , inv_afi, by = 'cedula') 
  
  pla <- aux %>%
    mutate(act = 1)
  
  ces <- expand.grid(anio = c(as.integer(x), as.integer(y)),
                     mes = c(12,1),
                     cedula = act$cedula)
    
  ces <- eval(dplyr::filter(ces, (anio == rlang::parse_expr(x) & mes == '12') |
                              (anio == rlang::parse_expr(y) & mes == '1' ) ) )  
    
  ces <- ces %>%  
    lazy_dt() %>%
    left_join( ., act, by = 'cedula') %>%
    left_join( ., pla, by = c('cedula'='cedula', 'anio'='anio', 'mes' = 'mes') ) %>%
    mutate( act = ifelse( is.na(act), 0, act ) ) %>%
    group_by( cedula ) %>%
    mutate( ces = lag(act) - act ) %>%
    ungroup() %>%
    filter( ces == '1' )%>%
    as_tibble()
  
  ces <- ces %>% select( -edad, -sexo ) %>%
    left_join(., avisos_salida, by = c('cedula'= 'cedula', 'anio'='anio', 'mes'='mes') ) %>%
    filter( !is.na( fecininov )) %>%
    distinct(cedula, anio, mes,.keep_all = TRUE)
  return(ces)
}


#Tabla de contigencia-------------------------------------------------------------------------------
tab_cont <- function(data, tipo_salida)
{
  tab <- data %>%
    group_by( anio, mes, edad, genero, sector, cod_causal ) %>%
    mutate( n = n() ) %>%
    ungroup() %>%
    distinct(anio, mes, edad, genero, sector, cod_causal,.keep_all = TRUE ) %>%
    select(anio, mes, edad, genero, sector, cod_causal, n) %>%
    left_join( ., tipo_salida, by = 'cod_causal') %>%
    select( -cod_causal )
  
  return(tab)
}

#Cesantes del 2015----------------------------------------------------------------------------------
load(paste0('D://Datos (D)//Bases//', "IESS_act_15_16.RData"))
tic()
aux <- cesantes(act_15_16, '2015')
c_15 <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)


tic()
aux <- cesantes(act_15_16, '2016')
c_16 <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)

rm(list = ls()[(ls() %in% c("act_15_16"))])


load(paste0('D://Datos (D)//Bases//', "IESS_act_17_18.RData"))
tic()
aux <- cesantes(act_17_18, '2017')
c_17 <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)


tic()
aux <- cesantes(act_17_18, '2018')
c_18 <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)

rm(list = ls()[(ls() %in% c("act_17_18"))])


load(paste0('D://Datos (D)//Bases//', "IESS_act_19_20.RData"))
tic()
aux <- cesantes(act_19_20, '2019')
c_19 <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)


tic()
aux <- cesantes(act_19_20, '2020')
c_20 <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)

rm(list = ls()[(ls() %in% c("act_19_20"))])


load(paste0('D://Datos (D)//Bases//', "IESS_act_dic_ene.RData"))

tic()
aux <- cesantes_ene(act_dic_ene,avisos_salida, '2015', '2016')
c_16_ene <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)

tic()
aux <- cesantes_ene(act_dic_ene,avisos_salida, '2016', '2017')
c_17_ene <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)


tic()
aux <- cesantes_ene(act_dic_ene,avisos_salida, '2017', '2018')
c_18_ene <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)

tic()
aux <- cesantes_ene(act_dic_ene,avisos_salida, '2018', '2019')
c_19_ene <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)


tic()
aux <- cesantes_ene(act_dic_ene,avisos_salida, '2019', '2020')
c_20_ene <- tab_cont(aux, tipo_salida)
rm(list = ls()[(ls() %in% c("aux"))])
toc(log = TRUE, quiet = TRUE)
tic.log(format = TRUE)

#Consolidar bases-----------------------------------------------------------------------------------

base <- rbind( c_15,
               c_16_ene,
               c_16,
               c_17_ene,
               c_17,
               c_18_ene,
               c_18,
               c_19_ene,
               c_19,
               c_20_ene,
               c_20)

#Exportar a excel-----------------------------------------------------------------------------------

message( '\tGenerando fechas de cese de beneficiarios de liquidaciones' )

write.xlsx(base,
           file = paste0( 'D://Datos (D)//Bases//', 'cesantes_sector_aviso_salida.xlsx' ),
           sheetName = "cesantes", 
           col.names = TRUE,
           row.names = FALSE, 
           append = TRUE)


# Guardando en un Rdata ----------------------------------------------------------------------------
message("\tGuardando número de cesantes mensuales")

save(ces_15,
     file = paste0( 'D://Datos (D)//Bases//', "cesantes_planillas.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
