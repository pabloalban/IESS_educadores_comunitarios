message( paste( rep('-', 100 ), collapse = '' ) )

diccionario_default <- 
  data.table( item = c('anio', 'sexo', 
                       't', 'x', 'F', 'M'), descripcion = c('Año', 'Sexo de la Persona', 
                                                            'Año del Horizonte de Estudio', 'Edad Simple',
                                                            'Femenino', 'Masculino') )

diccionario_poblacion <- 
  data.table( item = c('l2', 'l2_inac', 'l3',
                       'l4', 'l6', 'l7', 'l8', 'l9')
              , descripcion = c( 'Afiliado Activo SSC', 
                                 'Afiliado Inactivo SSC', 'Pensionista por Vejez',
                                 'Pensionista por Invalidez',
                                 'Dependiente Hijo', 'Dependiente Conyuge','Montepio por Orfandad',
                                 'Montepio por Viudedad') )

diccionario_transicion_poblacion <- 
  data.table( item = c('l12', 
                       'l23', 'l24', 'l25',
                       'l35', 
                       'l45',
                       'l65', 'l75',
                       'l85', 'l95')
              , descripcion = c('Fuerza Laboral Empleada Rural a Cotizante', 
                                'Afiliado Cotizante a Pensionistas por Vejez', 
                                'Afiliado Cotizante a Pensionistas por Invalidez', 
                                'Afiliado Cotizante a Muerto',
                                'Pensionista de Vejez a Muerto',
                                'Pensionista de Invalidez a Muerto',
                                'Dependientes Hijo a Muerto', 'Dependiente Conyuge a Muerto',
                                'Montepio por Orfandad a Muerto', 'Montepio por Viudedad a Muerto') )

diccionario_tot <- do.call('rbind', list(diccionario_default, diccionario_poblacion,diccionario_transicion_poblacion ))

