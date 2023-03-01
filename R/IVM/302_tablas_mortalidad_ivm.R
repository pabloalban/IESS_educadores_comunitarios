message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData_seg, 'IESS_IVM_tabla_mortalidad_dinamica.RData' ) )

message( '\tGenerando tablas de mortalidad para cada estado' )

message( '\tMortalidad afiliados activos' )
afi_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, qx, px ) ]
setorder( afi_mor, t, sexo, x )
afi_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
afi_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
afi_mor[ , dx := lx * qx ]
afi_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
afi_mor[ , ex := ex / lx - 0.5 ]


message( '\tMortalidad afiliados inactivos' )
afi_mor_inac <- iess_mort_din[ x >= 15, list( t, sexo, x, qx = q_inac_x, px = p_inac_x ) ]
setorder( afi_mor_inac, t, sexo, x )
afi_mor_inac[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
afi_mor_inac[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
afi_mor_inac[ , dx := lx * qx ]
afi_mor_inac[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
afi_mor_inac[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por vejez' )
pen_vej_mor <- iess_mort_din[ x >= 50, list( t, sexo, x, qx = qvx, px = pvx ) ]
setorder( pen_vej_mor, t, sexo, x )
pen_vej_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_vej_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_vej_mor[ , dx := lx * qx ]
pen_vej_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_vej_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por vejez y afiliados inactivos' )
pen_vej_inac_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, qx = qv_inac_x, px = pv_inac_x ) ]
setorder( pen_vej_inac_mor, t, sexo, x )
pen_vej_inac_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_vej_inac_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_vej_inac_mor[ , dx := lx * qx ]
pen_vej_inac_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_vej_inac_mor[ , ex := ex / lx - 0.5 ]


message( '\tMortalidad pensionistas por invalidez' )
pen_inv_mor <- iess_mort_din[ x >= 20, list( t, sexo, x, qx = qix, px = pix ) ]
setorder( pen_inv_mor, t, sexo, x )
pen_inv_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_inv_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_inv_mor[ , dx := lx * qx ]
pen_inv_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_inv_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por discapacidad' )
pen_dis_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, qx = qdx, px = pdx ) ]
setorder( pen_dis_mor, t, sexo, x )
pen_dis_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_dis_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_dis_mor[ , dx := lx * qx ]
pen_dis_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_dis_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por orfandad' )
pen_orf_mor <- iess_mort_din[ x >= 0, list( t, sexo, x, qx = qox, px = pox ) ]
setorder( pen_orf_mor, t, sexo, x )
pen_orf_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_orf_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_orf_mor[ , dx := lx * qx ]
pen_orf_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_orf_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por viudedad' )
pen_viu_mor <- iess_mort_din[ x >= 14, list( t, sexo, x, qx = qwx, px = pwx ) ]
setorder( pen_viu_mor, t, sexo, x )
pen_viu_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_viu_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_viu_mor[ , dx := lx * qx ]
pen_viu_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_viu_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad no afiliados' )
noafi_mor <- iess_mort_din[ x >= 0, list( t, sexo, x, qx = q_onu_x, px = 1 - q_onu_x ) ]
setorder( noafi_mor, t, sexo, x )
noafi_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
noafi_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
noafi_mor[ , dx := lx * qx ]
noafi_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
noafi_mor[ , ex := ex / lx - 0.5 ]


lista <- c('afi_mor', 'afi_mor_inac',
           'pen_vej_mor', 'pen_vej_inac_mor', 'pen_inv_mor', 'pen_dis_mor',
           'pen_orf_mor', 'pen_viu_mor',
           'noafi_mor')

# Exportación a csv ------------------------------------------------------------
# Probability of death for active members (s,g,x,t)[q]
write.table( dcast.data.table( afi_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_q_sgo_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( afi_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_q_sgo_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

# Probability of Death For a Disability Pensioner (s,x,t)[qd]
write.table( dcast.data.table( pen_inv_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qd_sgo_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( pen_inv_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qd_sgo_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

# Probability of Death For a Disability Pensioner (s,x,t)[qd] --Solo discapacidad
write.table( dcast.data.table( pen_dis_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qdd_sgo_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( pen_dis_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qdd_sgo_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

# Probability of death for an inactive contributor or an old-age pensioner (s,x,t)[qi]
write.table( dcast.data.table( pen_vej_inac_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qi_sgo_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( pen_vej_inac_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qi_sgo_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

# Probability of death (including other reasons for exit such as marriage) for a widow(er) (s,x,t)[qw]
write.table( dcast.data.table( pen_viu_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qw_sgo_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( pen_viu_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qw_sgo_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

# Probability of death (including other reasons for exit such as turning the majority of age) for an orphan (s,x,t)[qo]
write.table( dcast.data.table( pen_orf_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qo_sgo_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( pen_orf_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_qo_sgo_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

# Probability of death  for no afiliados
write.table( dcast.data.table( noafi_mor[ t>=2020 & t<=2060 & sexo=='F'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_noafi_female_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

write.table( dcast.data.table( noafi_mor[ t>=2020 & t<=2060 & sexo=='M'], sexo + x ~ t, value.var = 'qx' )[,-1], 
             file=paste0(parametros$Data_seg,
                         'INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/',
                         "base_noafi_male_50.csv"),
             quote=FALSE, sep="\t", eol = "\n", dec=".", row.names = FALSE, col.names = TRUE)

#-------------------------------------------------------------------------------

save( list = lista ,
      file = paste0( parametros$RData_seg, 'IESS_IVM_tablas_biometricas_mortalidad_todos_estados.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
