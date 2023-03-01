message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población afiliada activa inicial del IESS' )

#file
file <- paste0( parametros$Data_seg, 'IESS_IVM_analisis_demografico.xlsx' )


# Poblacion afiliada IVM --------------------------------------------------
message( '\tLeyendo distribucion población afiliada IVM del IESS' )
col_nom <- c( 'anio', 'Afiliados_m', 'Afiliados_f', 'Afiliados', 'Tasa')
col_tip <- c( 'text', 'numeric', 'numeric' , 'numeric', 'numeric')

pob_afi_ini<-read_excel(file,sheet="NumAfi (2)"
                        ,col_names=TRUE,guess_max = 24000, range = "O1:S17")
pob_afi_ini <- as.data.table( pob_afi_ini )[1:16,]
setnames( pob_afi_ini, col_nom )



# Población afiliada por edad y sexo al IVM -------------------------------
message( '\tLeyendo población afiliada y pensionistas por sexo y edad activa inicial IVM del IESS' )
col_nom <- c( 'sexo', 'edad', 'n' )
col_tip <- c( 'text', 'numeric', 'numeric', 'text'  )

pob_afi_edad_sexo_ini<-read_excel(file,sheet="NumAfiSexoEdad"
                                  ,col_names=TRUE,guess_max = 24000)
pob_afi_edad_sexo_ini <- as.data.table( pob_afi_edad_sexo_ini )[-182,]
setnames( pob_afi_edad_sexo_ini, col_nom )

# Masa Salarial -----------------------------------------------------------
message( '\tLeyendo masa salarial inicial de IVM del IESS' )

col_nom <- c( 'anio',  'Masa_Anual_Mas', 'Masa_Anual_Fem','Masa_Anual', 'Masa_Mensual_Mas', 'Masa_Mensual_Fem','Masa_Mensual', 'Crecimiento_anual','Tasa')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

masa_salarial_ini<-read_excel(file,sheet="Masa_Salarial"
                              ,col_names=TRUE,guess_max = 24000, range = "H1:P17")
masa_salarial_ini <- as.data.table( masa_salarial_ini )
setnames( masa_salarial_ini, col_nom )
#masa_salarial_ini$Crecimiento_anual=as.numeric(masa_salarial_ini$Crecimiento_anual)
#masa_salarial_ini$Tasa=as.numeric(masa_salarial_ini$Tasa)

# Masa Salarial por monto y sexo -------------------------------------------
message( '\tLeyendo masa salarial por sexo y monto del IVM del IESS' )
col_nom <- c( 'sexo', 'monto', 'n')
col_tip <- c( 'text', 'numeric', 'numeric', 'text' )
#file <- paste0( parametros$Data, 'IESS_poblacion_inicial_edad_sexo.xlsx' )

masa_sal_monto <-read_excel(file,sheet="Salario_Sexo"
                                     ,col_names=TRUE,guess_max = 24000)
masa_sal_monto <- as.data.table( masa_sal_monto )
setnames( masa_sal_monto, col_nom )

#--------------------Tiempo de aportación
message( '\tLeyendo tiempo de aportación de afiliados IVM del IESS' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')
  # col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
  #               'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

afi_tiempo_aportacion <-read_excel(file,sheet="MatrizEdadImpoInfor_text",
                                   col_names=TRUE,guess_max = 24000)

afi_tiempo_aportacion <- as.data.table( afi_tiempo_aportacion )[2:37,]
setnames( afi_tiempo_aportacion, col_nom )



#--------------------Tiempo de aportación Hombres
message( '\tLeyendo tiempo de aportación de afiliados IVM del IESS' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')
 # col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
 #               'numeric', 'numeric', 'numeric', 'numeric', 'numeric')

afi_tiempo_aportacion_h <-read_excel(file,sheet="MatrizEdadImpoInforH_text",
                                   col_names=TRUE,guess_max = 24000)
afi_tiempo_aportacion_h <- as.data.table( afi_tiempo_aportacion_h)[2:37,]
setnames( afi_tiempo_aportacion_h, col_nom )

#--------------------Tiempo de aportación Mujeres
message( '\tLeyendo tiempo de aportación de afiliados IVM del IESS' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')
 # col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
 #               'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

afi_tiempo_aportacion_m <-read_excel(file,sheet="MatrizEdadImpoInforM_text",
                                     col_names=TRUE,guess_max = 24000)
afi_tiempo_aportacion_m <- as.data.table( afi_tiempo_aportacion_m)[2:37,]
setnames( afi_tiempo_aportacion_m, col_nom )


#------------------- PRESTACIONES DE IVM ---------------------------------------------------------
#----------Jubilacion por vejez
message( '\tLeyendo crecimiento de la poblacion jubilada por vejez' )
col_nom <- c( 'anio', 'jub_vjz','creci', 'benef',  'creci_porcen', 'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

jub_vejez <-read_excel(file,sheet="Jub_Vejez"
                       ,col_names=TRUE,guess_max = 24000 , range = "A1:G10")
jub_vejez <- as.data.table( jub_vejez )[-10,]
setnames( jub_vejez, col_nom )
#----------Jubilacion por vejez masculino
message( '\tLeyendo crecimiento de la poblacion jubilada por vejez' )
col_nom <- c( 'anio', 'jub_vjz','creci', 'benef',  'creci_porcen', 'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

jub_vejez_h <-read_excel(file,sheet="Jub_Vejez"
                       ,col_names=TRUE,guess_max = 24000 , range = "A15:G24")
jub_vejez_h <- as.data.table( jub_vejez_h )[-10,]
setnames( jub_vejez_h, col_nom )
#----------Jubilacion por vejez femenino
message( '\tLeyendo crecimiento de la poblacion jubilada por vejez' )
col_nom <- c( 'anio', 'jub_vjz','creci', 'benef',  'creci_porcen', 'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

jub_vejez_m <-read_excel(file,sheet="Jub_Vejez"
                       ,col_names=TRUE,guess_max = 24000 , range = "A29:G38")
jub_vejez_m <- as.data.table( jub_vejez_m )[-10,]
setnames( jub_vejez_m, col_nom )



#----------prestacion por vejez por edad y sexo
message( '\tLeyendo distribucion de los jubilados por vejez, por genero y edad' )
col_nom <- c( 'sexo','edad', 'n')
col_tip <- c( 'text', 'numeric', 'numeric')

jub_vjz_edad_sexo <-read.xlsx(file,sheet="JubVej_Edad_Sexo",cols=c(1,2,3))
jub_vjz_edad_sexo <- as.data.table( jub_vjz_edad_sexo )[-126,]
setnames( jub_vjz_edad_sexo, col_nom )

#----------Montos prestacion por vejez por monto y sexo
message( '\tLeyendo montos prestacion por vejez por monto y sexo de IVM' )
col_nom <- c( 'sexo','monto','n')
col_tip <- c( 'text','numeric', 'numeric')

montos_vjz_sexo <-read_excel(file,sheet="PensionV_Sexo"
                                   ,col_names=TRUE,guess_max = 24000)
montos_vjz_sexo <- as.data.table( montos_vjz_sexo )
setnames( montos_vjz_sexo, col_nom )


#----------Jubilacion por invalidez
message( '\tLeyendo crecimiento de la poblacion jubilada por invalidez' )
col_nom <- c( 'anio', 'jub_invlz','creci', 'benef',  'creci_porcen', 'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric', 'numeric' )

jub_inv <-read_excel(file,sheet="Jub_Invalidez"
                       ,col_names=TRUE,guess_max = 24000 , range = "A1:G10")
jub_inv <- as.data.table( jub_inv )
setnames( jub_inv, col_nom )

#----------Jubilacion por invalidez hombre
message( '\tLeyendo crecimiento de la poblacion jubilada por invalidez' )
col_nom <- c( 'anio', 'jub_invlz','creci', 'benef',  'creci_porcen', 'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric', 'numeric' )

jub_inv_h<-read_excel(file,sheet="Jub_Invalidez"
                     ,col_names=TRUE,guess_max = 24000 , range = "A14:G23")
jub_inv_h <- as.data.table( jub_inv_h )
setnames( jub_inv_h, col_nom )

#----------Jubilacion por invalidez mujer
message( '\tLeyendo crecimiento de la poblacion jubilada por invalidez' )
col_nom <- c( 'anio', 'jub_invlz','creci', 'benef',  'creci_porcen', 'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric', 'numeric' )

jub_inv_m <-read_excel(file,sheet="Jub_Invalidez"
                     ,col_names=TRUE,guess_max = 24000, range = "A27:G36")
jub_inv_m <- as.data.table( jub_inv_m)
setnames( jub_inv_m, col_nom )





#----------prestacion por invalidez por edad y sexo
message( '\tLeyendo distribucion de los jubilados por invalidez, por genero y edad' )
col_nom <- c( 'sexo','edad', 'n')
col_tip <- c( 'text', 'numeric', 'numeric')

jub_inv_edad_sexo <-read.xlsx(file,sheet="JubInv_Edad_Sexo",cols=c(1,2,3))
jub_inv_edad_sexo <- as.data.table( jub_inv_edad_sexo )[-158,]
setnames( jub_inv_edad_sexo, col_nom )

#----------Montos prestacion por invalidez por monto y sexo
message( '\tLeyendo montos prestacion por invalidez por monto y sexo de IVM' )
col_nom <- c( 'sexo','monto','n')
col_tip <- c( 'text','numeric', 'numeric')

montos_inv_sexo <-read_excel(file,sheet="PensionI_Sexo"
                                    ,col_names=TRUE,guess_max = 24000)
montos_inv_sexo <- as.data.table( montos_inv_sexo )
setnames( montos_inv_sexo, col_nom )


# Jubilacion especial de vejez --------------------------------------------

message( '\tLeyendo crecimiento de la poblacion jubilada especial de vejez' )
col_nom <- c( 'anio', 'jub_dsc','creci', 'benef',  'creci_porcen' ,  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

jub_vjz_especial <-read_excel(file,sheet="Jub_Discapacidad"
                       ,col_names=TRUE,guess_max = 24000, range = "A1:G8")
jub_vjz_especial <- as.data.table( jub_vjz_especial )
setnames( jub_vjz_especial, col_nom )
# Jubilacion especial de vejez hombre --------------------------------------------

message( '\tLeyendo crecimiento de la poblacion jubilada especial de vejez' )
col_nom <- c( 'anio', 'jub_dsc','creci', 'benef',  'creci_porcen' ,  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

jub_vjz_especial_h <-read_excel(file,sheet="Jub_Discapacidad"
                              ,col_names=TRUE,guess_max = 24000, range = "A12:G19")
jub_vjz_especial_h <- as.data.table( jub_vjz_especial_h )
setnames( jub_vjz_especial_h, col_nom )
# Jubilacion especial de vejez mujer --------------------------------------------

message( '\tLeyendo crecimiento de la poblacion jubilada especial de vejez' )
col_nom <- c( 'anio', 'jub_dsc','creci', 'benef',  'creci_porcen' ,  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

jub_vjz_especial_m <-read_excel(file,sheet="Jub_Discapacidad"
                              ,col_names=TRUE,guess_max = 24000, range = "A23:G30")
jub_vjz_especial_m <- as.data.table( jub_vjz_especial_m )
setnames( jub_vjz_especial_m, col_nom )




#----------prestacion especial por vejez por edad y sexo
message( '\tLeyendo distribucion de los jubilados especial por vejez, por genero y edad' )
col_nom <- c( 'sexo','edad', 'n')
col_tip <- c( 'text', 'numeric', 'numeric')

jub_vjz_especial_edad_sexo <-read.xlsx(file,sheet="JubDis_Edad_Sexo",cols=c(1,2,3))
jub_vjz_especial_edad_sexo <- as.data.table( jub_vjz_especial_edad_sexo )[-64,]
setnames( jub_vjz_especial_edad_sexo, col_nom )

#----------Montos prestacion especial por vejez  por monto y sexo
message( '\tLeyendo montos prestacion especial por vejez por monto y sexo de IVM' )
col_nom <- c( 'sexo','monto','n')
col_tip <- c( 'text','numeric', 'numeric')

montos_vjz_esp <-read_excel(file,sheet="PensionDisSexo"
                                        ,col_names=TRUE,guess_max = 24000)
montos_vjz_esp <- as.data.table( montos_vjz_esp )
setnames( montos_vjz_esp, col_nom )


# Pensionistas viudedad ---------------------------------------------------

message( '\tLeyendo crecimiento de los pensionistas por viudedad' )
col_nom <- c( 'anio', 'pension','creci', 'benef',  'creci_porcen',  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric')

pen_viud <-read_excel(file,sheet="Pen_Viudedad"
                       ,col_names=TRUE,guess_max = 24000, range = "A1:G10")
pen_viud <- as.data.table( pen_viud )[-10,]
setnames( pen_viud, col_nom )
# Pensionistas viudedad hombre ---------------------------------------------------

message( '\tLeyendo crecimiento de los pensionistas por viudedad' )
col_nom <- c( 'anio', 'pension','creci', 'benef',  'creci_porcen',  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric')

pen_viud_h <-read_excel(file,sheet="Pen_Viudedad"
                      ,col_names=TRUE,guess_max = 24000, range = "A14:G23")
pen_viud_h <- as.data.table( pen_viud_h )[-10,]
setnames( pen_viud_h, col_nom )
# Pensionistas viudedad mujer ---------------------------------------------------

message( '\tLeyendo crecimiento de los pensionistas por viudedad' )
col_nom <- c( 'anio', 'pension','creci', 'benef',  'creci_porcen',  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric')

pen_viud_m <-read_excel(file,sheet="Pen_Viudedad"
                      ,col_names=TRUE,guess_max = 24000, range = "A27:G36")
pen_viud_m <- as.data.table( pen_viud_m )[-10,]
setnames( pen_viud_m, col_nom )


#----------pensionistas de viudedad por edad y sexo
message( '\tLeyendo distribucion de pensionistas de viudedad por genero y edad' )
col_nom <- c( 'sexo','edad', 'n')
col_tip <- c( 'text', 'numeric', 'numeric')

pen_viu_edad_sexo <-read.xlsx(file,sheet="PenViu_Edad_Sexo",cols=c(1,2,3))
pen_viu_edad_sexo <- as.data.table( pen_viu_edad_sexo )[-172,]
setnames( pen_viu_edad_sexo, col_nom )

#----------Montos pensiones por viudedad por monto y sexo
message( '\tLeyendo montos pension por viudedad por monto y sexo de IVM' )
col_nom <- c( 'sexo','monto','n')
col_tip <- c( 'text','numeric', 'numeric')

montos_pen_viu <-read_excel(file,sheet="PensionVi_Sexo"
                                        ,col_names=TRUE,guess_max = 24000)
montos_pen_viu <- as.data.table( montos_pen_viu )
setnames( montos_pen_viu, col_nom )


# Pensionistas de orfandad ------------------------------------------------

message( '\tLeyendo crecimiento de los pensionistas por orfandad' )
col_nom <- c( 'anio', 'pension','creci', 'benef',  'creci_porcen',  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric')

pen_orf <-read_excel(file,sheet="Pen_Orfandad"
                      ,col_names=TRUE,guess_max = 24000, range = "A1:G10")
pen_orf <- as.data.table( pen_orf )[-10,]
setnames( pen_orf, col_nom )
# Pensionistas de orfandad hombre ------------------------------------------------

message( '\tLeyendo crecimiento de los pensionistas por orfandad' )
col_nom <- c( 'anio', 'pension','creci', 'benef',  'creci_porcen',  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric')

pen_orf_h <-read_excel(file,sheet="Pen_Orfandad"
                     ,col_names=TRUE,guess_max = 24000, range = "A14:G23")
pen_orf_h <- as.data.table( pen_orf_h )[-10,]
setnames( pen_orf_h, col_nom )
# Pensionistas de orfandad mujer ------------------------------------------------

message( '\tLeyendo crecimiento de los pensionistas por orfandad' )
col_nom <- c( 'anio', 'pension','creci', 'benef',  'creci_porcen',  'Salario Promedio', 'tasa_variacion')
col_tip <- c( 'text', 'numeric', 'numeric', 'numeric', 'numeric' , 'numeric')

pen_orf_m <-read_excel(file,sheet="Pen_Orfandad"
                     ,col_names=TRUE,guess_max = 24000, range = "A27:G36")
pen_orf_m <- as.data.table( pen_orf_m )[-10,]
setnames( pen_orf_m, col_nom )



#----------pensionistas de orfandad por edad y sexo
message( '\tLeyendo distribucion de pensionistas por orfandad por genero y edad' )
col_nom <- c( 'sexo','edad', 'n')
col_tip <- c( 'text', 'numeric', 'numeric')

pen_orf_edad_sexo <-read.xlsx(file,sheet="PenOrf_Edad_Sexo",cols=c(1,2,3))
pen_orf_edad_sexo <- as.data.table( pen_orf_edad_sexo )[-204,]
setnames( pen_orf_edad_sexo, col_nom )

#----------Montos pensiones por orfandad por monto y sexo
message( '\tLeyendo montos pension por orfandad por monto y sexo de IVM' )
col_nom <- c( 'sexo','monto','n')
col_tip <- c( 'text','numeric', 'numeric')

montos_pen_orf <-read.xlsx(file,sheet="PensionO_Sexo"
                            ,cols=c(1,2,3))
montos_pen_orf <- as.data.table( montos_pen_orf )
setnames( montos_pen_orf, col_nom )



# Guardando ---------------------------------------------------------------

lista <- c('pob_afi_ini', 'pob_afi_edad_sexo_ini','masa_salarial_ini','masa_sal_monto','afi_tiempo_aportacion','afi_tiempo_aportacion_h', 'afi_tiempo_aportacion_m',
           'jub_vejez', 'jub_vejez_h', 'jub_vejez_m','jub_vjz_edad_sexo','montos_vjz_sexo','jub_inv','jub_inv_h','jub_inv_m','jub_inv_edad_sexo','montos_inv_sexo',
           'jub_vjz_especial','jub_vjz_especial_h','jub_vjz_especial_m','jub_vjz_especial_edad_sexo','montos_vjz_esp','pen_viud','pen_viud_h','pen_viud_m','pen_viu_edad_sexo',
           'montos_pen_viu','pen_orf','pen_orf_h','pen_orf_m','pen_orf_edad_sexo','montos_pen_orf' )

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_demografico.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
