message( paste( rep('-', 100 ), collapse = '' ) )

load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) ) 
rm( list = ls()[ !( ls() %in% c( 'parametros', 'dependientes_ssc' ) ) ] )

message( '\tLeyendo outputs modelo ILO/PENSIONS para el SSC' )
dir_lec_int <- paste0( parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/')
setwd( dir_lec_int )
arch <- list.files()

# Labor force by sex (s,t). -------------------------------------------------------------------
message( '\t\tLabour force (s,t)' )
file_wr <- paste0( dir_lec_int, 
                  arch[match( TRUE, c(str_detect( arch, ",LF,")==T & str_detect( arch, "Male")==T &
                                        str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'lf')
lf_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(lf_male, col_nom)
lf_male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",LF,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'lf')
lf_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(lf_female, col_nom)
lf_female[ , sexo:='Female' ]

plf <- rbind(lf_male, lf_female)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HWEP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'lf')
hlf_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(hlf_male, col_nom)
hlf_male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HWEP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'lf')
hlf_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(hlf_female, col_nom)
hlf_female[ , sexo:='Female' ]

hlf <- rbind( hlf_male, hlf_female)

lf <- rbind( hlf, plf )

pob_hist_dem <- merge( data.table(expand.grid( t = 2012:2040, sexo=c('Male', 'Female'))),
                   lf, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Employed labour force (s,t). ---------------------------------------------------------------------
message( '\t\tEmployed labour force (s,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HLF,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l1')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HLF,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l1')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

emp_hlf <- rbind( male, female)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",RPT_MDAT_B,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l1')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",RPT_MDAT_B,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l1')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

emp_plf <- rbind( male, female)

emp_lf <- rbind( emp_hlf, emp_plf )

pob_hist_dem <- merge( pob_hist_dem, emp_lf, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Jefes de Familia. ---------------------------------------------------------------------
message( '\t\tJefes de Familia.' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HAC,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l2')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HAC,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l2')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hjef <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hjef, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Pensionistas de Vejez. ---------------------------------------------------------------------------
message( '\t\tPensionistas de Vejez.' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HOAP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l3')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HOAP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l3')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hvej <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hvej, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Pensionistas de Invalidez. -----------------------------------------------------------------------
message( '\t\tPensionistas de Invalidez..' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HDISP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l4')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HDISP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l4')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hinv <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hinv, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Pensionistas de Viudedad -----------------------------------------------------------------------
message( '\t\tPensionistas de Viudedad.' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HWP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l9')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HWP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l9')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hviu <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hviu, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Pensionistas de Orfandad -----------------------------------------------------------------------
message( '\t\tPensionistas de Orfandad' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HORP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l8')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HORP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'l8')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

horf <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, horf, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

#Dependientes hijos --------------------------------------------------------------------------------
aux <- copy( dependientes_ssc )
aux_f <- aux[ tipo=='Femenino', list( t = Anio, sexo='Female', l6 = hijos, l7 = conyuge )]
aux_m <- aux[ tipo=='Masculino', list( t = Anio, sexo='Male',l6 = hijos, l7 = conyuge )]

aux_dep <- rbind( aux_f, aux_m)

pob_hist_dem <- merge( pob_hist_dem, aux_dep, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Gasto en Vejez -----------------------------------------------------------------------------------
message( '\t\tGasto en Vejez' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HOAEXP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B3')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HOAEXP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B3')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hgvej <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hgvej, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Gasto en invalidez -----------------------------------------------------------------------------------
message( '\t\tGasto en invalidez' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HDISEXP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B4')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HDISEXP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B4')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hginv <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hginv, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Gasto en viudez -----------------------------------------------------------------------------------
message( '\t\tGasto en iviudez' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HWIEXP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B9')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HWIEXP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B9')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hgviu <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hgviu, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

# Gasto en orfandad --------------------------------------------------------------------------------
message( '\t\tGasto en orfandad' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HOREXP,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B8')
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(male, col_nom)
male[ , sexo:='Male' ]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",HOREXP,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('t', 'B8')
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(female, col_nom)
female[ , sexo:='Female' ]

hgorf <- rbind( male, female)

pob_hist_dem <- merge( pob_hist_dem, hgorf, by=c('t', 'sexo'), all.x=T)
setorder( pob_hist_dem, sexo, t)

#Proyecciones
# [actgx] Total number of active contributors by sex, population group and age (s,g,x,t).-----------
message( '\t\t[actgx] Total number of active contributors by sex, population group and age (s,g,x,t)' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "actgx")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq(parametros$anio, parametros$anio_fin )) )
jef_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(jef_male, col_nom)
jef_male[ , sexo:='Male' ]
jef_male <- data.table( melt( jef_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l2' ))

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "actgx")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq(parametros$anio, parametros$anio_fin)) )
jef_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(jef_female, col_nom)
jef_female[ , sexo:='Female' ]
jef_female <- data.table( melt( jef_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l2' ) )

jef <- data.table( rbind( jef_male, jef_female) )
jef[ , t:=as.numeric(as.character(t))]

pob_proy <- data.table( expand.grid( t = seq( parametros$anio_ini, parametros$horizonte+parametros$anio),
                         x = 0:105, sexo=c('Male', 'Female') ) )

pob_proy <- merge( pob_proy, jef , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( pob_proy, t, sexo, x ) 

# [inactx] Inactive contributors by sex, and age (s,x,t).-------------------------------------------
message( '\t\t[inactx] Inactive contributors by sex, and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "inactx")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq(parametros$anio, parametros$anio_fin )) )
jef_inac_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(jef_inac_male, col_nom)
jef_inac_male[ , sexo:='Male' ]
jef_inac_male <- melt( jef_inac_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l2_inac' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "inactx")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq(parametros$anio, parametros$anio_fin)) )
jef_inac_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames(jef_inac_female, col_nom)
jef_inac_female[ , sexo:='Female' ]
jef_inac_female <- melt( jef_inac_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l2_inac' )

jef_inac <- data.table( rbind( jef_inac_male, jef_inac_female) )
jef_inac[ , t:=as.numeric(as.character(t))]

pob_proy <- merge( pob_proy, jef_inac , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( pob_proy, t, sexo, x ) 

# [oldage] Number of old-age pensioners by sex and age (s,x,t).-------------------------------------
message( '\t\t[oldage] Number of old-age pensioners by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",oldage,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
vej_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( vej_male, col_nom)
vej_male[ , sexo:='Male' ]
vej_male <- melt( vej_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l3' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",oldage,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
vej_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( vej_female, col_nom)
vej_female[ , sexo:='Female' ]
vej_female <- melt( vej_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l3' )

vej <- data.table( rbind( vej_male, vej_female) )
vej[ , t:=as.numeric(as.character(t))]

pob_proy <- merge( pob_proy, vej , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( pob_proy, t, sexo, x ) 

# [dis] Disability pensioners by sex and age (s,x,t).-----------------------------------------------
message( '\t\t[dis] Disability pensioners by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",dis,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
inv_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( inv_male, col_nom)
inv_male[ , sexo:='Male' ]
inv_male <- melt( inv_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l4' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",dis,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
inv_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( inv_female, col_nom)
inv_female[ , sexo:='Female' ]
inv_female <- melt( inv_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l4' )

inv <- data.table( rbind( inv_male, inv_female) )
inv[ , t:=as.numeric(as.character(t))]

pob_proy <- merge( pob_proy, inv , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( pob_proy, t, sexo, x ) 

# [wid] Widow(er)s pensioners by sex and age (s,x,t).-----------------------------------------------
message( '\t\t[wid] Widow(er)s pensioners by sex and age (s,x,t)..' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",wid,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
viu_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( viu_male, col_nom)
viu_male[ , sexo:='Male' ]
viu_male <- melt( viu_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l9' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",wid,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
viu_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( viu_female, col_nom)
viu_female[ , sexo:='Female' ]
viu_female <- melt( viu_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l9' )

viu <- data.table( rbind( viu_male, viu_female) )
viu[ , t:=as.numeric(as.character(t))]

pob_proy <- merge( pob_proy, viu , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( pob_proy, t, sexo, x ) 

# [orph] Orphans pensions by sex and age (s,x,t)..-----------------------------------------------
message( '\t\t[orph] Orphans pensions by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",orph,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
orf_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( orf_male, col_nom)
orf_male[ , sexo:='Male' ]
orf_male <- melt( orf_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l8' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",orph,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
orf_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( orf_female, col_nom)
orf_female[ , sexo:='Female' ]
orf_female <- melt( orf_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l8' )

orf <- data.table( rbind( orf_male, orf_female) )
orf[ , t:=as.numeric(as.character(t))]

pob_proy <- merge( pob_proy, orf , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( pob_proy, t, sexo, x ) 

#Proyección de dependientes ------------------------------------------------------------------------
message( '\tIncluyendo dependientes' )
# l6 dependientes hijos
# l7 dependientes cónyuges
load( paste0( parametros$RData_seg, 'INEC_censo_iess_fertilidad_alisado_2010.RData' ) )
aux <- cen_iess_hij_alis[ , list( x, z = y, sexo, sexo_dep, qh = q ) ]
aux[ , sexo := as.character( sexo ) ]
aux[ , sexo_dep := as.character( sexo_dep ) ]
aux[ sexo == 'M', sexo := 'Female' ]
aux[ sexo == 'H', sexo := 'Male' ]
aux[ sexo_dep == 'M', sexo_dep := 'Female' ]
aux[ sexo_dep == 'H', sexo_dep := 'Male' ]

pob_proy_dep <- merge( pob_proy[ , list( sexo, x, t, 
                                         l2 ) ], 
                       aux, 
                       by = c( 'sexo', 'x' ), 
                       all.x = TRUE, allow.cartesian = TRUE )

pob_proy_dep[ is.na( qh ), qh := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( z ) ]

pob_proy_dep <- merge( pob_proy_dep,
                       cen_iess_cony_alis[ , list( x, y, qc = q ) ], by = c( 'x' ),
                       all.x = TRUE, allow.cartesian = TRUE )
gc()
pob_proy_dep[ is.na( qc ), qc := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( y ) ]
pob_proy_dep[ sexo == 'Female', sexo_cony := 'Male' ]
pob_proy_dep[ sexo == 'Male', sexo_cony := 'Female' ]

aux1 <-aux[ , list( y = x, z, sexo_cony = sexo, sexo_dep, qhc = qh ) ]
pob_proy_dep <- merge( pob_proy_dep,
                       aux1,
                       by = c( 'sexo_cony', 'y', 'sexo_dep', 'z' ) )

pob_proy_dep <- pob_proy_dep[ y - z >= 15 ]
pob_proy_dep <- pob_proy_dep[ y - z <= 50 ]
pob_proy_dep <- pob_proy_dep[ x - z >= 15 ]
pob_proy_dep <- pob_proy_dep[ x - z <= 70 ]
gc()

# pob_proy_dep1 <- pob_proy_dep[ y <= 105,
#                                list( l6 = sum( l2 * qh * qhc * ( 1 - 0.5 * qc ), na.rm = T ) ),
#                                by = list( t, sexo = sexo_dep, x = z ) ]

l6_fem <- pob_proy_dep[ y <= 105 & sexo_dep=='Female',
                              list( l6 = sum( l2 * qh * qhc * ( 1 - 0.5 * qc ), na.rm = T ) * 1.04 ),
                              by = list( t, sexo = sexo_dep, x = z ) ]

l6_mas <- pob_proy_dep[ y <= 105 & sexo_dep=='Male',
                              list( l6 = sum( l2 * qh * qhc * ( 1 - 0.5 * qc ), na.rm = T )* 1.04 ),
                              by = list( t, sexo = sexo_dep, x = z ) ]

pob_proy_dep <- rbind( l6_fem, l6_mas )

gc()

pob_proy_cony <- merge( pob_proy[ , list( sexo, x, t, 
                                          l2 ) ], 
                        cen_iess_cony_alis[ , list( x, y, qc = 3 * q ) ], by = c( 'x' ), 
                        all.x = TRUE, allow.cartesian = TRUE )
pob_proy_cony[ sexo == 'Female', sexo_cony := 'Male' ]
pob_proy_cony[ sexo == 'Male', sexo_cony := 'Female' ]

pob_proy_cony[ is.na( qc ), qc := 0 ]
pob_proy_cony <- pob_proy_cony[ !is.na( y ) ]
# pob_proy_cony1 <- pob_proy_cony[ y <= 105, list( l7 = sum( l2 * qc, na.rm = T  ) ), 
#                                 by = list( t, sexo = sexo_cony, x = y ) ]

l7_fem <- pob_proy_cony[ y <= 105 & sexo_cony=='Female', list( l7 = sum( l2 * qc * 2.2, na.rm = T  ) ), 
                                by = list( t, sexo = sexo_cony, x = y ) ]

l7_mas <- pob_proy_cony[ y <= 105 & sexo_cony=='Male', list( l7 = sum( l2 * qc * 0.1, na.rm = T  ) ), 
                                by = list( t, sexo = sexo_cony, x = y ) ]

pob_proy_cony <- rbind(l7_mas, l7_fem)

pob_proy <- merge( pob_proy, pob_proy_dep, by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy <- merge( pob_proy, pob_proy_cony, by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy[ is.na( l6 ), l6 := 0 ]
pob_proy[ is.na( l7 ), l7 := 0 ]
# write.xlsx( dcast( pob_proy[ , l6, by=list(t, sexo, x ) ], sexo+x~t, value.var='l6'  ),  file=paste0( parametros$Data_seg, 'l6.xlsx'))
# pob_proy[, sum(l6), by=list(t, sexo)]
# pob_proy[, sum(l7), by=list(t, sexo)]

# Mortalidad dinámica ------------------------------------------------------------------------------
# Horizonte de proyección
t_horiz <- parametros$horizonte

# Año inicial de proyección
fec_ini <- parametros$anio

# Año final de proyección
fec_fin <- parametros$anio_fin

# Tiempo
t <- 1:t_horiz

# Edades
x_max <- parametros$edad_max
x <- 0:x_max

N <- length( t )
M <- length( x )

#load( paste0( parametros$RData_seg, 'IESS_SSC_tablas_mortalidad_todos_estados.RData' ) )
dir_lec_int <- paste0( 'Y:/IESS_2020/Data/IVM/INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/')
setwd( dir_lec_int )
arch <- list.files()
# Se considera la mortalidad de Ecuador según la ONU
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "base_noafi_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( 0, parametros$horizonte ) ) )
qd_f <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = "\t", dec = ".") )[ - 1, 1:22]
setnames( qd_f, col_nom)
qd_f[ , x:=as.integer(x) ]
setorder( qd_f, x  )
qd_f <- qd_f[ , `0`:=NULL]
qd_f <- as.matrix( data.table( merge( expand.grid(x=0:105), qd_f, by='x', all.x=T ) )[, 2:ncol(qd_f)])
pd_f <- 1 - qd_f

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "base_noafi_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( 0, parametros$horizonte ) ) )
qd_m <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = "\t", dec = ".") )[ - 1, 1:22]
setnames( qd_m, col_nom)
qd_m[ , x:=as.integer(x) ]
setorder( qd_m, x  )
qd_m <- qd_m[ , `0`:=NULL]
qd_m <- as.matrix( data.table( merge( expand.grid(x=0:105), qd_m, by='x', all.x=T ) )[, 2:ncol(qd_m)])
pd_m <- 1 - qd_m

# Evolución de la población de dependientes --------------------------------------------------------
# Hijos
l6_f <- dcast.data.table( data = pob_proy[ sexo == 'Female' ],
                          formula = x ~ t, value.var = 'l6' )
setorder( l6_f, x )
setnames( l6_f, c('x', as.character( seq( 1 ,t_horiz) ) ) )
l6_f[ , x := NULL ]

# at <- data.frame( matrix( 0, 106, 1 ) )
# colnames(at) <- as.character( 0 )
# 
# l6_f <- as.data.table( cbind( at, l6_f ) )
# l6_f <- l6_f[ , c( as.character( seq(0,20) ) ) ]

l6_f <- as.matrix( l6_f )

l6_m <- dcast.data.table( data = pob_proy[ sexo == 'Male' ],
                          formula = x ~ t, value.var = 'l6' )
setorder( l6_m, x )
setnames( l6_m, c('x', as.character( seq( 1 ,t_horiz) ) ) )
l6_m[ , x := NULL ]
# l6_m <- as.data.table( cbind( at, l6_m ) )
# l6_m <- l6_m[ , c( as.character( seq(0,20) ) ) ]

l6_m <- as.matrix( l6_m )

# Cónyuges
l7_f <- dcast.data.table( data = pob_proy[ sexo == 'Female' ],
                          formula = x ~ t, value.var = 'l7' )
setorder( l7_f, x )
setnames( l7_f, c('x', as.character( seq( 1 ,t_horiz) ) ) )
l7_f[ , x := NULL ]
# l7_f <- as.data.table( cbind( at, l7_f ) )
# l7_f <- l7_f[ , c( as.character( seq(0,20) ) ) ]
l7_f <- as.matrix( l7_f )

l7_m <- dcast.data.table( data = pob_proy[ sexo == 'Male' ],
                          formula = x ~ t, value.var = 'l7' )
setorder( l7_m, x )
setnames( l7_m, c('x', as.character( seq( 1 ,t_horiz) ) ) )
l7_m[ , x := NULL ]
# l7_m <- as.data.table( cbind( at, l7_m ) )
# l7_m <- l7_m[ , c( as.character( seq(0,20) ) ) ]
l7_m <- as.matrix( l7_m )

l <- array( 0.0, dim = c( M, N, 2, 2 ) )
lt_5 <- array( 0.0, dim = c( M, N, 2, 2 ) )

# Inicialización
l[ , , 1, 1 ] <- l6_f
l[ , , 2, 1 ] <- l6_m
l[ , , 1, 2 ] <- l7_f
l[ , , 2, 2 ] <- l7_m

for ( n in 1:N ) {
  for ( k in 1:M ) {
    # Estado 6
    # l[ k + 1, n + 1, 1, 1 ] = pd_f[ k + 1, n + 1 ] * l[ k + 1, n + 1, 1, 1 ]
    # l[ k + 1, n + 1, 2, 1 ] = pd_m[ k + 1, n + 1 ] * l[ k + 1, n + 1, 2, 1 ]
    lt_5[ k , n , 1, 1 ] <- ( 1 - pd_f[ k, n ] ) * l[ k, n, 1, 1 ]
    lt_5[ k , n , 2, 1 ] <- ( 1 - pd_m[ k, n ] ) * l[ k, n, 2, 1 ]
    l[ k, n, 1, 1 ] = pd_f[ k, n ] * l[ k, n, 1, 1 ]
    l[ k, n, 2, 1 ] = pd_m[ k, n ] * l[ k, n, 2, 1 ]
    
    # Estado 7
    # l[ k + 1, n + 1, 1, 2 ] = pd_f[ k + 1, n + 1 ] * l[ k + 1, n + 1, 1, 2 ]
    # l[ k + 1, n + 1, 2, 2 ] = pd_m[ k + 1, n + 1 ] * l[ k + 1, n + 1, 2, 2 ]
    lt_5[ k, n, 1, 2 ] <- ( 1 - pd_f[ k, n ] ) * l[ k, n, 1, 2 ]
    lt_5[ k, n, 2, 2 ] <- ( 1 - pd_m[ k, n ] ) * l[ k, n, 2, 2 ]
    l[ k, n, 1, 2 ] = pd_f[ k, n ] * l[ k, n, 1, 2 ]
    l[ k, n, 2, 2 ] = pd_m[ k, n ] * l[ k, n, 2, 2 ]

  }
}

for ( m in 1:2 ) { #m<-1
  l_f <- as.data.table( l[ , , 1, m ] )
  l_f[ , x := x ]
  l_f[ , sexo := 'Female' ]
  
  l_m <- as.data.table( l[ , , 2, m ] )
  l_m[ , x := x ]
  l_m[ , sexo := 'Male' ]
  
  li <- rbind( l_f, l_m )
  li <- melt.data.table( data = li, id.vars = c( 'x', 'sexo' ), 
                         variable.name = 't', value.name = paste0( 'l', 5 + m  ) )
  li[ , t := as.numeric( gsub( 'V', '', t ) ) + fec_ini ]
  
  
  lt_f <- as.data.table( lt_5[ , , 1, m ] )
  lt_f[ , x := x ]
  lt_f[ , sexo := 'Female' ]
  
  lt_m <- as.data.table( lt_5[ , , 2, m ] )
  lt_m[ , x := x ]
  lt_m[ , sexo := 'Male' ]
  
  lti <- rbind( lt_f, lt_m )
  lti <- melt.data.table( data = lti, id.vars = c( 'x', 'sexo' ), 
                          variable.name = 't', value.name = paste0( 'l', 5 + m, '_5'  ) )
  lti[ , t := as.numeric( gsub( 'V', '', t ) ) + fec_ini ]
  
  eval( expr = parse( text = paste0( 'pob_proy[ , l', 5 + m, ' := NULL ]' ) ) )
  
  pob_proy <- merge( pob_proy, li, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
  pob_proy <- merge( pob_proy, lti, by = c( 't', 'sexo', 'x' ), all.x = TRUE )
  
  eval( expr = parse( text = paste0( 'pob_proy[ ', 'is.na( l', 5 + m, ' ), l', 5 + m, ' := 0 ]' ) ) )
  eval( expr = parse( text = paste0( 'pob_proy[ ', 'is.na( l', 5 + m, '_5 ), l', 5 + m, '_5 := 0 ]' ) ) )
}

# pob_proy[, sum(l6, na.rm=T), by=list(t, sexo)]
# aux <- dcast.data.table(pob_proy[ sexo=='Male', list( l6=sum(l6, na.rm=T)), by=list(t, sexo, x)], x+sexo~t, value.var = 'l6' ) 
# setorder( aux, sexo, x )
# aux[, sum(`2022`), by=sexo]
# write.xlsx( aux, file=paste0( parametros$Data_seg, 'l6.xlsx'))

# Proyecciones Financieras -------------------------------------------------------------------------
dir_lec_int <- paste0( parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/')
setwd( dir_lec_int )
arch <- list.files()

# [TOldAge] Total expenditure on old-age pension benefits by sex and age (s,x,t).-------------------
message( '\t\t[TOldAge] Total expenditure on old-age pension benefits by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TOldAge,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_vej_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_vej_male, col_nom)
ben_vej_male[ , sexo:='Male' ]
ben_vej_male <- melt( ben_vej_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B3' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TOldAge,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_vej_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_vej_female, col_nom)
ben_vej_female[ , sexo:='Female' ]
ben_vej_female <- melt( ben_vej_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B3' )

ben_vej <- data.table( rbind( ben_vej_male, ben_vej_female) )
ben_vej[ , t:=as.numeric(as.character(t))]

ben_proy <- data.table( expand.grid( t = seq( parametros$anio_ini, parametros$horizonte+parametros$anio ),
                                     x = 0:105, sexo=c('Male', 'Female') ) )

ben_proy <- merge( ben_proy, ben_vej , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( ben_proy, t, sexo, x ) 

# [TDis] Total expenditure on disability pension benefit by sex and age (s,x,t).--------------------
message( '\t\t[TDis] Total expenditure on disability pension benefit by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TDis,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_inv_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_inv_male, col_nom)
ben_inv_male[ , sexo:='Male' ]
ben_inv_male <- melt( ben_inv_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B4' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TDis,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_inv_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_inv_female, col_nom)
ben_inv_female[ , sexo:='Female' ]
ben_inv_female <- melt( ben_inv_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B4' )

ben_inv <- data.table( rbind( ben_inv_male, ben_inv_female) )
ben_inv[ , t:=as.numeric(as.character(t))]

ben_proy <- merge( ben_proy, ben_inv , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( ben_proy, t, sexo, x ) 

# [TWid] Total expenditure on widows pension benefit by sex and age (s,x,t). -----------------------
message( '\t\t[TWid] Total expenditure on widows pension benefit by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TWid,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_viu_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_viu_male, col_nom)
ben_viu_male[ , sexo:='Male' ]
ben_viu_male <- melt( ben_viu_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B9' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TWid,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_viu_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_viu_female, col_nom)
ben_viu_female[ , sexo:='Female' ]
ben_viu_female <- melt( ben_viu_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B9' )

ben_viu <- data.table( rbind( ben_viu_male, ben_viu_female) )
ben_viu[ , t:=as.numeric(as.character(t))]

ben_proy <- merge( ben_proy, ben_viu , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( ben_proy, t, sexo, x ) 

# [TOrph] Total expenditure on orphans pension benefit by sex and age (s,x,t). ---------------------
message( '\t\t[TOrph] Total expenditure on orphans pension benefit by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TOrph,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_orf_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_orf_male, col_nom)
ben_orf_male[ , sexo:='Male' ]
ben_orf_male <- melt( ben_orf_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B8' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",TOrph,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
ben_orf_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( ben_orf_female, col_nom)
ben_orf_female[ , sexo:='Female' ]
ben_orf_female <- melt( ben_orf_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'B8' )

ben_orf <- data.table( rbind( ben_orf_male, ben_orf_female) )
ben_orf[ , t:=as.numeric(as.character(t))]

ben_proy <- merge( ben_proy, ben_orf , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( ben_proy, t, sexo, x ) 

proy <- merge( pob_proy, ben_proy, by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE  )
setorder( proy, t, sexo, x )

com <- data.table( expand.grid( t=2012:2020, sexo=c('Male', 'Female')) )
com[ , l2_inac:= NA]
pob_hist_dem1 <- merge( com , pob_hist_dem, by=c('t', 'sexo'), all.x=T )
pob_hist_dem1 <- pob_hist_dem1[ , list( t, sexo, lf, l1, l2, l2_inac, l3, l4, l6, l7, l8, l9, B3, B4, B8, B9)]
setorder( pob_hist_dem1, sexo, t )

proy[, lf:=NA]
proy[, l1:=NA]

acum_dem_salud <- proy[ , list(lf = sum(lf,na.rm=T), 
                               l1 = sum(l1,na.rm=T),
                               l2 = sum(l2,na.rm=T), l2_inac=sum(l2_inac,na.rm=T),
                               l3 = sum(l3,na.rm=T), l4=sum(l4,na.rm=T), l6=sum(l6,na.rm=T),
                               l7 = sum(l7,na.rm=T), l8=sum(l8,na.rm=T), l9 = sum(l9,na.rm=T), 
                               B3 = sum(B3,na.rm=T), B4=sum(B4,na.rm=T), 
                               B8 = sum(B8,na.rm=T),
                               B9 = sum(B9,na.rm=T) ), by = list( t, sexo, x ) ]

acum_dem <- data.table( rbind( pob_hist_dem1, proy[ , list(lf = sum(lf,na.rm=T), 
                                                           l1 = sum(l1,na.rm=T),
                                                           l2 = sum(l2,na.rm=T), l2_inac=sum(l2_inac,na.rm=T),
                                                           l3=sum(l3,na.rm=T), l4=sum(l4,na.rm=T), 
                                                           l6=sum(l6,na.rm=T), l7=sum(l7,na.rm=T),l8=sum(l8,na.rm=T),
                                                           l9=sum(l9,na.rm=T), B3=sum(B3,na.rm=T), B4=sum(B4,na.rm=T), 
                                                           B8=sum(B8,na.rm=T),
                                                           B9=sum(B9,na.rm=T) ), by=list(t,sexo)] ) )
setorder( acum_dem, sexo, t)

acum_dem[ t>=2021 & t<=2040 & sexo=='Female', lf:=pob_hist_dem[t>=2021 & t<=2040 & sexo=='Female']$lf] 
acum_dem[ t>=2021 & t<=2040 & sexo=='Male', lf:=pob_hist_dem[t>=2021 & t<=2040 & sexo=='Male']$lf] 
acum_dem[ t>=2021 & t<=2040 & sexo=='Female', l1:=pob_hist_dem[t>=2021 & t<=2040 & sexo=='Female']$l1] 
acum_dem[ t>=2021 & t<=2040 & sexo=='Male', l1:=pob_hist_dem[t>=2021 & t<=2040 & sexo=='Male']$l1] 

load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) )
acum_dem[ t>=2012 & t<=2020 & sexo=='Male', l2_inac:=afi_inac_hist_ssc[Anio>=2012 & Anio<=2020]$Male] 
acum_dem[ t>=2012 & t<=2020 & sexo=='Female', l2_inac:=afi_inac_hist_ssc[Anio>=2012 & Anio<=2020]$Female] 

# Nuevos afiliados y pensionistas ------------------------------------------------------------------
# [entgx] Entries of contributors by sex, population group and age (s,g,x,t). ----------------------
message( '\t\t[entgx] Entries of contributors by sex, population group and age (s,g,x,t)' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",entgx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)
male[ , sexo:='Male' ]
male <- melt( male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l12' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",entgx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)
female[ , sexo:='Female' ]
female <- melt( female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l12' )

jf <- data.table( rbind( male, female) )
jf[ , t:=as.numeric(as.character(t))]

new_pob_proy <- data.table( expand.grid( t = 2021:2040, x=0:105,sexo=c('Male', 'Female') ))

new_pob_proy <- merge( new_pob_proy, 
                       jf , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

# Afiliados muertos---------------------------------------------------------------------------------
message( '\t\t[nretx] Jefes de Familia muertos' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",actgx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)

male <- merge( expand.grid(x=0:105), male, by='x', all.x=T )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",q,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
p_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( p_male, col_nom)

dead_male <- as.data.table( as.matrix(male) * as.matrix(p_male) )
dead_male[ , sexo:='Male' ]
dead_male[ , x:=(x)^0.5]
dead_male <- data.table( melt( dead_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l25' ))
dead_male[ , t:= as.numeric(as.character(t)) ]
setorder( dead_male, x, t)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",actgx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)

female <- merge( expand.grid(x=0:105), female, by='x', all.x=T )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",q,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
p_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( p_female, col_nom)

dead_female <- as.data.table( as.matrix(female) * as.matrix(p_female) )
dead_female[ , sexo:='Female' ]
dead_female[ , x:=(x)^0.5]
dead_female <- data.table( melt( dead_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l25' ))
dead_female[ , t:= as.numeric(as.character(t)) ]
setorder( dead_female, x, t)

dead_jef <- data.table( rbind( dead_male, dead_female) )
dead_jef[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, 
                       dead_jef , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

# [nretx] New old-age beneficiaries by sex and age (s,x,t). ----------------------------------------
message( '\t\t[nretx] New old-age beneficiaries by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",nretx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)
male[ , sexo:='Male' ]
male <- melt( male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l23' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",nretx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)
female[ , sexo:='Female' ]
female <- melt( female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l23' )

vej <- data.table( rbind( male, female) )
vej[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, 
                       vej , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

# Pensionistas muertos de vejez --------------------------- ----------------------------------------
message( '\t\t[nretx] Pensionistas muertos de vejez ' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",oldage,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Ioldage,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
male_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male_base, col_nom)
male_base <- merge( expand.grid(x=0:105), male_base, by='x', all.x=T )

male <- merge( male_base, male, by='x',all.x=T)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",qi,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
p_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( p_male, col_nom)

dead_male <- as.data.table( as.matrix(male) * as.matrix(p_male) )
dead_male[ , sexo:='Male' ]
dead_male[ , x:=(x)^0.5]
dead_male <- data.table( melt( dead_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l35' ))
dead_male[ , t:= as.numeric(as.character(t)) ]
setorder( dead_male, x, t)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",oldage,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Ioldage,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
female_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female_base, col_nom)
female_base <- merge( expand.grid(x=0:105), female_base, by='x', all.x=T )

female <- merge( female_base, female, by='x',all.x=T)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",qi,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
p_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( p_female, col_nom)

dead_female <- as.data.table( as.matrix(female) * as.matrix(p_female) )
dead_female[ , sexo:='Female' ]
dead_female[ , x:=(x)^0.5]
dead_female <- data.table( melt( dead_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l35' ))
dead_female[ , t:= as.numeric(as.character(t)) ]
setorder( dead_female, x, t)

dead_vej <- data.table( rbind( dead_male, dead_female) )
dead_vej[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, 
                       dead_vej , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

# [ndisx] New disability pensioners by sex and age (s,x,t).---------------------------------------------------
message( '\t\t[ndisx] New disability pensioners by sex and age (s,x,t).' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",ndisx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)
male[ , sexo:='Male' ]
male <- melt( male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l24' )


file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",ndisx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)
female[ , sexo:='Female' ]
female <- melt( female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l24' )

new_inv <- data.table( rbind( male, female) )
new_inv[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, new_inv , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x ) 

# Pensionistas muertos de invalidez ----------------------------------------------------------------
message( '\t\t[nretx] Pensionistas muertos de invalidez' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",dis,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Idis,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
male_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male_base, col_nom)
male_base <- merge( expand.grid(x=0:105), male_base, by='x', all.x=T )

male <- merge( male_base, male, by='x',all.x=T)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",qd,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, c(parametros$anio_fin-1) ) ) )
p_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( p_male, col_nom)
p_male <- data.table( merge( expand.grid(x=0:105), p_male, by='x', all.x=T ) )
p_male[ , `2040`:=p_male$`2039` ]

dead_male <- as.data.table( as.matrix(male) * as.matrix(p_male) )
dead_male[ , sexo:='Male' ]
dead_male[ , x:=(x)^0.5]
dead_male <- data.table( melt( dead_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l45' ))
dead_male[ , t:= as.numeric(as.character(t)) ]
setorder( dead_male, x, t)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",dis,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Idis,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
female_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female_base, col_nom)
female_base <- merge( expand.grid(x=0:105), female_base, by='x', all.x=T )

female <- merge( female_base, female, by='x',all.x=T)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",qd,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, c(parametros$anio_fin-1) ) ) )
p_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( p_female, col_nom)
p_female <- data.table( merge( expand.grid(x=0:105), p_female, by='x', all.x=T ) )
p_female[ , `2040`:=p_female$`2039` ]

dead_female <- as.data.table( as.matrix(female) * as.matrix(p_female) )
dead_female[ , sexo:='Female' ]
dead_female[ , x:=(x)^0.5]
dead_female <- data.table( melt( dead_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l45' ))
dead_female[ , t:= as.numeric(as.character(t)) ]
setorder( dead_female, x, t)

dead_inv <- data.table( rbind( dead_male, dead_female) )
dead_inv[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, 
                       dead_inv , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

# [nwidactgx] New widow(er)s caused by the death of an active member by sex, age and population group (s,g,x,t)
message( '\t\t[nwidactgx] New widow(er)s caused by the death of an active member by sex, age and population group (s,g,x,t)' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",nwidactgx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
maleA <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( maleA, col_nom)
maleA <- melt( maleA, id.vars=c( 'x'), variable.name = 't', value.name = 'l9A' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",nwidpenx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
maleP <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( maleP, col_nom)
maleP <- melt( maleP, id.vars=c('x'), variable.name = 't', value.name = 'l9P' )

male <- as.data.table( merge( maleA, maleP, by=c('x', 't'), all.x = T ) )
setorder( male, t, x)
male[ , l9:= l9A + l9P]
male[ , sexo:='Male']
male <- male[ , list( sexo, x, t, l9 )]

# file_wr <- paste0( dir_lec_int, 
#                    arch[match( TRUE, c(str_detect( arch, ",nwidactgx,")==T & str_detect( arch, "Female")==T &
#                                          str_detect( arch, ".csv")==T))] )
# 
# col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
# female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
# setnames( female, col_nom)
# female[ , sexo:='Female' ]
# female <- melt( female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l9' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",nwidactgx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
femaleA <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( femaleA, col_nom)
femaleA <- melt( femaleA, id.vars=c( 'x'), variable.name = 't', value.name = 'l9A' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",nwidpenx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
femaleP <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( femaleP, col_nom)
femaleP <- melt( femaleP, id.vars=c('x'), variable.name = 't', value.name = 'l9P' )

female <- as.data.table( merge( femaleA, femaleP, by=c('x', 't'), all.x = T ) )
setorder( female, t, x)
female[ , l9:= l9A + l9P]
female[ , sexo:='Female']
female <- female[ , list( sexo, x, t, l9 )]

new_viu <- data.table( rbind( male, female) )
new_viu[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, new_viu , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x ) 

# Pensionistas muertos de viudez--------------------------------------------------------------------
message( '\t\tPensionistas muertos de viudez' )
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",wid,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Iwid,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
male_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male_base, col_nom)
male_base <- merge( expand.grid(x=0:105), male_base, by='x', all.x=T )

male <- merge( male_base, male, by='x',all.x=T)

dir_lec_int <- paste0( 'Y:/IESS_2020/Data/IVM/INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/')
setwd( dir_lec_int )
arch <- list.files()

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "base_noafi_")==T & str_detect( arch, "_male_")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio, parametros$anio_fin ) ) )
p_male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = "\t", dec = ".") )[ - 1, 1:22]
setnames( p_male, col_nom)
p_male <- data.table( merge( expand.grid(x=0:105), p_male, by='x', all.x=T ) )

dead_male <- as.data.table( as.matrix(male) * as.matrix(p_male) )
dead_male[ , sexo:='Male' ]
dead_male[ , x:=(x)^0.5]
dead_male <- data.table( melt( dead_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l95' ))
dead_male[ , t:= as.numeric(as.character(t)) ]
setorder( dead_male, x, t)

dir_lec_int <- paste0( parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/')
setwd( dir_lec_int )
arch <- list.files()

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",wid,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Iwid,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
female_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female_base, col_nom)
female_base <- merge( expand.grid(x=0:105), female_base, by='x', all.x=T )

female <- merge( female_base, female, by='x',all.x=T)

dir_lec_int <- paste0( 'Y:/IESS_2020/Data/IVM/INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/')
setwd( dir_lec_int )
arch <- list.files()

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, "base_noafi_")==T & str_detect( arch, "_female_")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio,parametros$anio_fin ) ) )
p_female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = "\t", dec = ".") )[ - 1, 1:22]
setnames( p_female, col_nom)
p_female <- data.table( merge( expand.grid(x=0:105), p_female, by='x', all.x=T ) )

dead_female <- as.data.table( as.matrix(female) * as.matrix(p_female) )
dead_female[ , sexo:='Female' ]
dead_female[ , x:=(x)^0.5]
dead_female <- data.table( melt( dead_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l95' ))
dead_female[ , t:= as.numeric(as.character(t)) ]
setorder( dead_female, x, t)

dead_viu <- data.table( rbind( dead_male, dead_female) )
dead_viu[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, 
                       dead_viu , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

# [norphactsgx] New orphans caused by the death of active members by sex, population group and age (s,g,x,t).
message( '\t\t[norphactsgx] New orphans caused by the death of active members by sex, population group and age (s,g,x,t).' )

dir_lec_int <- paste0( parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out/')
setwd( dir_lec_int )
arch <- list.files()

# file_wr <- paste0( dir_lec_int, 
#                    arch[match( TRUE, c(str_detect( arch, ",norphactsgx,")==T & str_detect( arch, "Male")==T &
#                                          str_detect( arch, ".csv")==T))] )
# col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
# male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
# setnames( male, col_nom)
# male[ , sexo:='Male' ]
# male <- melt( male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l8' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",norphactsgx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
maleA <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( maleA, col_nom)
maleA <- melt( maleA, id.vars=c( 'x'), variable.name = 't', value.name = 'l8A' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",norphpensx,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
maleP <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( maleP, col_nom)
maleP <- melt( maleP, id.vars=c('x'), variable.name = 't', value.name = 'l8P' )

male <- as.data.table( merge( maleA, maleP, by=c('x', 't'), all.x = T ) )
setorder( male, t, x)
male[ , l8:= l8A + l8P]
male[ , sexo:='Male']
male <- male[ , list( sexo, x, t, l8 )]

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",norphactsgx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
femaleA <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( femaleA, col_nom)
femaleA <- melt( femaleA, id.vars=c( 'x'), variable.name = 't', value.name = 'l8A' )

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",norphpensx,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )
col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
femaleP <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( femaleP, col_nom)
femaleP <- melt( femaleP, id.vars=c('x'), variable.name = 't', value.name = 'l8P' )

female <- as.data.table( merge( femaleA, femaleP, by=c('x', 't'), all.x = T ) )
setorder( female, t, x)
female[ , l8:= l8A + l8P]
female[ , sexo:='Female']
female <- female[ , list( sexo, x, t, l8 )]

new_orf <- data.table( rbind( male, female) )
new_orf[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, new_orf , by.x=c('t', 'x', 'sexo'), by.y=c('t', 'x', 'sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x ) 

# Pensionistas muertos de orfandad------------------------------------------------------------------
file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",orph,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
male <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Iorph,")==T & str_detect( arch, "Male")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
male_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( male_base, col_nom)
male_base <- merge( expand.grid(x=0:105), male_base, by='x', all.x=T )

male <- merge( male_base, male, by='x',all.x=T)

dead_male <- as.data.table( as.matrix(male) * as.matrix(p_male) )
dead_male[ , sexo:='Male' ]
dead_male[ , x:=(x)^0.5]
dead_male <- data.table( melt( dead_male, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l85' ))
dead_male[ , t:= as.numeric(as.character(t)) ]
setorder( dead_male, x, t)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",orph,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character( seq( parametros$anio_ini, parametros$anio_fin ) ) )
female <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female, col_nom)

file_wr <- paste0( dir_lec_int, 
                   arch[match( TRUE, c(str_detect( arch, ",Iorph,")==T & str_detect( arch, "Female")==T &
                                         str_detect( arch, ".csv")==T))] )

col_nom <- c('x', as.character(  parametros$anio ) )
female_base <- as.data.table( read.csv( file = file_wr, header = FALSE, sep = ",", dec = ".") )[ -c(1:2), -1]
setnames( female_base, col_nom)
female_base <- merge( expand.grid(x=0:105), female_base, by='x', all.x=T )

female <- merge( female_base, female, by='x',all.x=T)

dead_female <- as.data.table( as.matrix(female) * as.matrix(p_female) )
dead_female[ , sexo:='Female' ]
dead_female[ , x:=(x)^0.5]
dead_female <- data.table( melt( dead_female, id.vars=c('sexo', 'x'), variable.name = 't', value.name = 'l85' ))
dead_female[ , t:= as.numeric(as.character(t)) ]
setorder( dead_female, x, t)

dead_orf <- data.table( rbind( dead_male, dead_female) )
dead_orf[ , t:=as.numeric(as.character(t))]

new_pob_proy <- merge( new_pob_proy, 
                       dead_orf , by.x=c('t', 'x','sexo'), by.y=c('t', 'x','sexo'), all.x=TRUE )
setorder( new_pob_proy, t, sexo, x  ) 

newp <- copy( new_pens )
newp <- dcast.data.table( newp, t + sexo ~ tipo, value.var = "num")
newp[ sexo=='H', sexo:="Male"]
newp[ sexo=='M', sexo:="Female"]
newp[ , l23:=VEJEZ]
newp[ , l24:=INVALIDEZ]
newp[ , l8:=NA]
newp[ , l9:=NA]
newp[ , l15:=NA]
newp[ , l25:=NA]
newp[ , l35:=NA]
newp[ , l45:=NA]
# newp[ , l65:=NA]
# newp[ , l75:=NA]
newp[ , l85:=NA]
newp[ , l95:=NA]
newp[ , l12:=NA]

newp <- newp[ , c('t', 'sexo', 'l12', 'l25','l23', 'l35','l24', 'l45', # 'l65', 'l75',
                  'l8', 'l85','l9', 'l95')]

new_acum_dem <- data.table( rbind( newp, new_pob_proy[ , list( l12=sum(l12,na.rm=T), 
                                                               l25=sum(l25,na.rm=T),
                                                               l23=sum(l23,na.rm=T),
                                                               l35=sum(l35,na.rm=T),
                                                               l24=sum(l24,na.rm=T),
                                                               l45=sum(l45,na.rm=T),
                                                               #l65=sum(pob_proy$l6_5, na.rm=T),
                                                               #l75=sum(pob_proy$l7_5,na.rm=T),
                                                               l8=sum(l8,na.rm=T),
                                                               l85=sum(l85,na.rm=T),
                                                               l9=sum(l9,na.rm=T),
                                                               l95=sum(l95,na.rm=T)
                                                               ),
                                                           by=list(t,sexo)] ) )
setorder( new_acum_dem, sexo, t)

aux_dep <- setorder( pob_proy[ , list( l65 = sum( l6_5, na.rm=T), l75 = sum( l7_5, na.rm=T) ), by=list( t, sexo ) ], sexo, t )
new_acum_dem <- merge( new_acum_dem, aux_dep, by=c('t', 'sexo'), all.x = T )

new_acum_dem <- new_acum_dem[ , list( t, sexo,l12, l25, l23, l35, l24, l45, l65, l75, l8, l85, l9, l95)]

proy <- as.data.table( merge( proy, new_pob_proy[ ,-c('l8', 'l9')], by=c('t','x', 'sexo'), all.x = T ) )
setorder( proy, t, sexo, x)
proy[ , l65:=l6_5]
proy[ , l75:=l7_5]
proy[ , l6_5:=NULL]
proy[ , l7_5:=NULL]

pob_proy[ , t := t - parametros$anio ]
ben_proy[ , t := t - parametros$anio ]
proy[ , t := t - parametros$anio ]

lista <- c('pob_proy', 'ben_proy', 'proy', 'pob_hist_dem', 'acum_dem', 'new_acum_dem', 'acum_dem_salud' )

save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) )

setwd( parametros$work_dir )
###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
