;los trace son:
;traceLDEM_CR2208_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_vectors_LDEM_CR2208_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav

;traceLDEM_CR2208_demt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_vectors_LDEM_CR2208_demt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav

;traceLDEM_CR2082_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_vectors_LDEM_CR2082_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav

;traceLDEM_CR2082_hollow_demt__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_vectors_LDEM_CR2082_hollow_demt__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav


;traceLDEM_CR2082_test_ldem_6alturas_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_vectors_LDEM_CR2082_test_ldem_6alturas_unifgrid_v2.heating.sampled.v2.DIEGO.dat_2.sav

;traceLDEM_CR2082_hollow_demt-data_pfss_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_vectors_LDEM_CR2082_hollow_demt-data_pfss_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.NUEVASTAT.dat.sav

;modo struct
;trace_struct_LDEM_CR2082_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav
;trace_struct_LDEM_CR2082_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav

pro estadistica_diego

  restore,'trace_struct_LDEM_CR2082_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  demt2082 = datos
  restore,'trace_struct_LDEM_CR2082_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  awsom2082 = datos
  restore,'trace_struct_LDEM_CR2208_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  demt2208 = datos
  restore,'trace_struct_LDEM_CR2208_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  awsom2208 =datos
  
histoplot,demt2082.gradt_erry(where(abs(demt2082.pearson_t) gt 0.5 and demt2082.opclstat gt 0. and demt2082.hip_chi_pv2_t ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1.))/1.e6,win=1,min=-10,max=10
histoplot,demt2082.gradt_erry( demt2082.opclstat gt 0. and demt2082.hip_chi_pv2_t ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1.))/1.e6,win=1,min=-10,max=10
histoplot,demt2082.gradt_erry(where(abs(demt2082.pearson_t) lt 0.5 and demt2082.opclstat gt 0. and demt2082.hip_chi_pv2_t ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry lt 1.))/1.e6,win=1,min=-10,max=10
 
 histoplot,awsom2082.gradt_erry(where(abs(awsom2082.pearson_t) gt 0.5 and awsom2082.opclstat gt 0. and awsom2082.hip_chi_pv2_t ge 0.7 ))/1.e6,win=2,min=0,max=10  

;filtros de temp
 histoplot,demt2082.ft(where(demt2082.hip_chi_pv2_t ne -555.)),win=1
 histoplot,demt2082.hip_chi_pv2_t(where(demt2082.ft ne -555.)),win=2
 histoplot,demt2082.ft(where(demt2082.hip_chi_pv2_t ge 0.5)),win=3
 histoplot,demt2082.hip_chi_pv2_t(where(demt2082.ft ge 0.7)),win=4

 rpoint_map,where(demt2082.hip_chi_pv2_t ne -555.),demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0]
 rpoint_map,where(demt2082.hip_chi_pv2_t ne -555.),demt2082.rp_alto.lon,demt2082.rp_alto.lat,win=6,vec_color=[0]
;

;doble histos 2082
  ok_demt  = where(abs(demt2082.pearson_t) gt 0.5 and demt2082.opclstat gt 0. and demt2082.hip_chi_pv2_t ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1.)
  ok_awsom = where(abs(awsom2082.pearson_t) gt 0.5 and awsom2082.opclstat gt 0. and awsom2082.hip_chi_pv2_t ge 0.7 )
  histoplot,demt2082.gradt_erry(ok_demt)/1.e6,data2=awsom2082.gradt_erry(ok_awsom)/1.e6,min=-10,max=10,win=1,xtit='MK/Rsun',tit='Gradientes de Temperatura -CR2082',label1='DEMT',label2='AWSOM',filename='2082_grad_temp'

  ne_demt_aux  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom_aux = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  histoplot,ne_demt_aux(ok_demt)/1.e8,data2=ne_awsom_aux(ok_awsom)/1.e8,min=0.4,max=1.2,win=1,xtit='Ne [1.e8 cm^-3]',tit='Densidad - 1.055 Rsun -CR2082',label1='DEMT',label2='AWSOM',filename='2082_ne_1055'

  histoplot,demt2082.tmmean(ok_demt)/1.e6,data2=awsom2082.tmmean(ok_awsom)/1.e6,win=1,xtit='MK',tit='Temperatura promedio -CR2082',label1='DEMT',label2='AWSOM',filename='2082_temp_media'
  histoplot,demt2082.lambda_n(ok_demt),data2=awsom2082.lambda_n(ok_awsom),min=0.03,max=0.16,win=1,xtit='',tit='Escala de altura (Ne) -CR2082',label1='DEMT',label2='AWSOM',filename='2082_lambda_n'


;cerrados chicos
  ok_demt1  = where(demt2082.gradt ne -555. and demt2082.opclstat eq 2. and demt2082.r2n gt 0.7 and demt2082.footlat gt -30 and demt2082.footlat lt 30 and demt2082.ft ge 0.7 and demt2082.iso_erry gt 1.)

  ok_demt1  = where(demt2082.opclstat  eq 2. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1. and abs(demt2082.footlat) le 30)
  ok_demt1  = where(demt2082.opclstat  eq 2. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and abs(demt2082.footlat) le 30)
  ok_awsom1 = where(awsom2082.opclstat eq 2. and awsom2082.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2082.footlat) le 30)

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='histo_2082_demt_awsom_streamer_fulliso'
  histoplot, demt2082.tmmean(ok_demt1 )/1.e6,data2=awsom2082.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demt1 ),data2=awsom2082.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demt1)/1.e8,data2=awsom2082.nebasal(ok_awsom1)/1.e8,win=3,tit='Ne Basal',xtit='10^8 cm-3' ,filename=suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,ne_demt(ok_demt1)/1.e8,data2=ne_awsom(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'
  suf1='rpoint_2082_streamer_fulliso'
  rpoint_map,ok_demt1,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'demt'
  rpoint_map,ok_awsom1,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'awsom'

;cerrados grandes + chicos arriba de 30 lat
  ok_demt1  = where(abs(demt2082.pearson_t)  gt 0.5 and demt2082.opclstat  ge 1. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1. and abs(demt2082.footlat) gt 30)
  ok_awsom1 = where(abs(awsom2082.pearson_t) gt 0.5 and awsom2082.opclstat ge 1. and awsom2082.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2082.footlat) gt 30)

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='histo_2082_demt_awsom_bound_'
  histoplot, demt2082.tmmean(ok_demt1 )/1.e6,data2=awsom2082.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demt1 ),data2=awsom2082.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demt1)/1.e8,data2=awsom2082.nebasal(ok_awsom1)/1.e8,win=3,tit='Ne Basal',xtit='10^8 cm-3' ,filename=suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demt1)/1.e8,data2=awsom2082.nebasal(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'
  suf1='rpoint_2082_bound_'
  rpoint_map,ok_demt1,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'demt'
  rpoint_map,ok_awsom1,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'awsom'

;abiertos
  ok_demt1  = where(abs(demt2082.pearson_t)  gt 0.5 and demt2082.opclstat  eq 0. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1. and abs(demt2082.footlat) ge 65)
  ok_awsom1 = where(abs(awsom2082.pearson_t) gt 0.5 and awsom2082.opclstat eq 0. and awsom2082.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2082.footlat) ge 65)

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='histo_2082_demt_awsom_CH_'
  histoplot, demt2082.tmmean(ok_demt1 )/1.e6,data2=awsom2082.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demt1 ),data2=awsom2082.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demt1)/1.e8,data2=awsom2082.nebasal(ok_awsom1)/1.e8,win=3,tit='Ne Basal',xtit='10^8 cm-3' ,filename=suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demt1)/1.e8,data2=awsom2082.nebasal(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'
  suf1='rpoint_2082_CH_'
  rpoint_map,ok_demt1,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'demt'
  rpoint_map,ok_awsom1,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'awsom'

  
;doble histos 2208

;cerrados chicos
 ok_demt1  = where(demt2208.gradt ne -555. and demt2208.opclstat eq 2. and demt2208.r2n gt 0.7 and demt2208.footlat gt -30 and demt2208.footlat lt 30 and demt2208.ft ge 0.7 and demt2208.iso_erry gt 1.)

  ok_demt1  = where(abs(demt2208.pearson_t)  gt 0.5 and demt2208.opclstat  eq 2. and demt2208.hip_chi_pv2_t  ge 0.7 and demt2208.ft ne -555. and demt2208.iso_erry gt 1. and abs(demt2208.footlat) le 30)
  ok_awsom1 = where(abs(awsom2208.pearson_t) gt 0.5 and awsom2208.opclstat eq 2. and awsom2208.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2208.footlat) le 30)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='histo_2208_demt_awsom_streamer_'
  histoplot, demt2208.tmmean(ok_demt1 )/1.e6,data2=awsom2208.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demt1 ),data2=awsom2208.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demt1)/1.e8,data2=awsom2208.nebasal(ok_awsom1)/1.e8,win=3,tit='Ne Basal',xtit='10^8 cm-3' ,filename=suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demt1)/1.e8,data2=awsom2208.nebasal(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'
  suf1='rpoint_2208_streamer_'
  rpoint_map,ok_demt1,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'demt'
  rpoint_map,ok_awsom1,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'awsom'


;cerrados grandes + chicos arriba de 30 lat                                                                                                                                                                        
  ok_demt1  = where(abs(demt2208.pearson_t)  gt 0.5 and demt2208.opclstat  ge 1. and demt2208.hip_chi_pv2_t  ge 0.7 and demt2208.ft ne -555. and demt2208.iso_erry gt 1. and abs(demt2208.footlat) gt 30)
  ok_awsom1 = where(abs(awsom2208.pearson_t) gt 0.5 and awsom2208.opclstat ge 1. and awsom2208.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2208.footlat) gt 30)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='histo_2208_demt_awsom_bound_'
  histoplot, demt2208.tmmean(ok_demt1 )/1.e6,data2=awsom2208.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demt1 ),data2=awsom2208.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demt1)/1.e8,data2=awsom2208.nebasal(ok_awsom1)/1.e8,win=3,tit='Ne Basal',xtit='10^8 cm-3' ,filename=suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demt1)/1.e8,data2=awsom2208.nebasal(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'
  suf1='rpoint_2208_bound_'
  rpoint_map,ok_demt1,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'demt'
  rpoint_map,ok_awsom1,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'awsom'


;abiertos                                                                                                                                                                                                          
  ok_demt1  = where(abs(demt2208.pearson_t)  gt 0.5 and demt2208.opclstat  eq 0. and demt2208.hip_chi_pv2_t  ge 0.7 and demt2208.ft ne -555. and demt2208.iso_erry gt 1. and abs(demt2208.footlat) ge 65)
  ok_awsom1 = where(abs(awsom2208.pearson_t) gt 0.5 and awsom2208.opclstat eq 0. and awsom2208.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2208.footlat) ge 65)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='histo_2208_demt_awsom_CH_'
  histoplot, demt2208.tmmean(ok_demt1 )/1.e6,data2=awsom2208.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demt1 ),data2=awsom2208.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demt1)/1.e8,data2=awsom2208.nebasal(ok_awsom1)/1.e8,win=3,tit='Ne Basal',xtit='10^8 cm-3' ,filename=suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demt1)/1.e8,data2=awsom2208.nebasal(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'
  suf1='rpoint_2208_CH_'
  rpoint_map,ok_demt1,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'demt'
  rpoint_map,ok_awsom1,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0],filename=suf1+'awsom'

;proceeding

  rpoint_map,where(demt2082.hip_chi_pv2_t ne -555. and demt2082.opclstat gt 0.),data2=where(demt2082.hip_chi_pv2_t ne -555. and demt2082.opclstat eq 0.),demt2082.rp_alto.lon,demt2082.rp_alto.lat,win=7,vec_color=[0,1],title='CR-2082 Localizacion fisica de arcos magneticos a R=1.105',filename='proceeding2019_1105_2082_demt'

rpoint_map,where(demt2208.hip_chi_pv2_t ne -555. and demt2208.opclstat gt 0.),data2=where(demt2208.hip_chi_pv2_t ne -555. and demt2208.opclstat eq 0.),demt2208.rp_alto.lon,demt2208.rp_alto.lat,win=7,vec_color=[0,1],title='CR-2208 Localizacion fisica de arcos magneticos a R=1.105',filename='proceeding2019_1105_2208_demt'

;CR-2082
;abierto
  ok_demt1  = where(abs(demt2082.pearson_t)  gt 0.5 and demt2082.opclstat  eq 0. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1. and abs(demt2082.footlat) ge 65)
  ok_awsom1 = where(abs(awsom2082.pearson_t) gt 0.5 and awsom2082.opclstat eq 0. and awsom2082.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2082.footlat) ge 65)

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2082_demt_awsom_CH_'
  histoplot, demt2082.tmmean_alto(ok_demt1 )/1.e6,data2=awsom2082.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demt1 ),data2=awsom2082.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,ne_demt(ok_demt1)/1.e8,data2=ne_awsom(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'

;cerrados
  ok_demt1  = where(abs(demt2082.pearson_t)  gt 0.5 and demt2082.opclstat  ge 1. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1.)
  ok_awsom1 = where(abs(awsom2082.pearson_t) gt 0.5 and awsom2082.opclstat ge 1. and awsom2082.hip_chi_pv2_t ge 0.7                                                     )

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2082_demt_awsom_streamer_'
  histoplot, demt2082.tmmean(ok_demt1 )/1.e6,data2=awsom2082.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demt1 ),data2=awsom2082.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,ne_demt(ok_demt1)/1.e8,data2=ne_awsom(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'

;CR2208
;abiertos
    ok_demt1  = where(abs(demt2208.pearson_t)  gt 0.5 and demt2208.opclstat  eq 0. and demt2208.hip_chi_pv2_t  ge 0.7 and demt2208.ft ne -555. and demt2208.iso_erry gt 1. and abs(demt2208.footlat) ge 65)
  ok_awsom1 = where(abs(awsom2208.pearson_t) gt 0.5 and awsom2208.opclstat eq 0. and awsom2208.hip_chi_pv2_t ge 0.7                                                      and abs(awsom2208.footlat) ge 65)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2208_demt_awsom_CH_'
  histoplot, demt2208.tmmean(ok_demt1 )/1.e6,data2=awsom2208.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demt1 ),data2=awsom2208.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,ne_demt(ok_demt1)/1.e8,data2=ne_awsom(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'

;cerrados, todo
  ok_demt1  = where(abs(demt2208.pearson_t)  gt 0.5 and demt2208.opclstat  ge 1. and demt2208.hip_chi_pv2_t  ge 0.7 and demt2208.ft ne -555. and demt2208.iso_erry gt 1. );and abs(demt2208.footlat) gt 30)
  ok_awsom1 = where(abs(awsom2208.pearson_t) gt 0.5 and awsom2208.opclstat ge 1. and awsom2208.hip_chi_pv2_t ge 0.7                                                      );and abs(awsom2208.footlat) gt 30)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2208_demt_awsom_streamer_'
  histoplot, demt2208.tmmean(ok_demt1 )/1.e6,data2=awsom2208.tmmean(ok_awsom1)/1.e6,win=1,tit='Temp media ',xtit='[MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demt1 ),data2=awsom2208.lambda_n(ok_awsom1)      ,win=2,min=-0.05,max=0.2,tit='lambda N',filename=suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,ne_demt(ok_demt1)/1.e8,data2=ne_awsom(ok_awsom1)/1.e8,win=4,tit='Ne 1.055',xtit='10^8 cm-3' ,filename=suf+'ne_1055',label1='demt',label2='awsom'


  

  return
end


pro old_estadistica_diego
  file1 = 'trace_vectors_LDEM_CR2082_hollow_demt__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  file2 = 'trace_vectors_LDEM_CR2082_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  file3 = 'trace_vector_2082_ceci_10alturas.sav'
  file4 = 'trace_vectors_LDEM_CR2082_hollow_demt-data_pfss_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  
;restore del trzado.sav
restore,file1
ok = where(gradt ne -555. and opclstat eq 0. and r2n gt 0.7 and footlat gt 70)
lamb1 = lambda_n(ok)
nbas1 = nebasal(ok)
tbas1 = te_base(ok)
grad1 = gradt(ok)
rad = 1.025 + 0.012 *findgen(20)
N_e1 = median(Nbas1)* exp(-1/median(lamb1)* (1. - 1./rad))
te1 = median(tbas1) + median(grad1) * rad

ok = where(gradt ne -555. and opclstat eq 2. and r2n gt 0.7 and footlat gt -30 and footlat lt 30)
lamb2 = lambda_n(ok)
nbas2 = nebasal(ok)
tbas2 = te_base(ok)
grad2 = gradt(ok)
rad = 1.025 + 0.012 *findgen(20)
N_e2 = median(Nbas2)* exp(-1/median(lamb2)* (1. - 1./rad))
te2 = median(tbas2) + median(grad2) * rad

restore,file2

ok = where(gradt ne -555. and opclstat eq 0. and r2n gt 0.7 and footlat gt 70)
lamb11 = lambda_n(ok)
nbas11 = nebasal(ok)
tbas11 = te_base(ok)
grad11 = gradt(ok)
N_e11 = median(Nbas11)* exp(-1/median(lamb11)* (1. - 1./rad))
te11 = median(tbas11) + median(grad11) * rad

ok = where(gradt ne -555. and opclstat eq 2. and r2n gt 0.7 and footlat gt -30 and footlat lt 30)
lamb22 = lambda_n(ok)
nbas22 = nebasal(ok)
tbas22 = te_base(ok)
grad22 = gradt(ok)
N_e22 = median(Nbas22)* exp(-1/median(lamb22)* (1. - 1./rad))
te22 = median(tbas22) + median(grad22) * rad

stop

perfil,rad,n_e1,vec2=n_e11,label1='demt',label2='awsom',ytit='Ne',tit='CR-2082 Radial Profile Open Region',filename='2082_Ne_openN'
perfil,rad,n_e2,n_e22,label1='demt',label2='awsom',ytit='Ne',tit='CR-2082 Radial Profile Open Region',filename='2082_Ne_streamer'

goto, final



  
r2crit_t=0.7
r2crit_N=0.75
ok = where(gradt ne -555. and opclstat eq 0. and r2n gt 0.7 )
histoplot,nebasal(ok),min=0
histoplot,pearson_n(ok),min=-1
histoplot, lambda_n(ok)
rpoint_map,ok,[0],footlon, footlat

ok = where(gradt ne -555. and opclstat eq 0. and ft gt 0.5 )
histoplot,pearson_t(ok),min=-1
histoplot,te_base(ok)
histoplot,tmmean(ok)
rpoint_map,ok,[0],footlon, footlat

histoplot,r2n(ok),min=0
histoplot,r2t(ok),min=0

rpoint_map,ok,[0],footlon, footlat

stop

lowlatR=30
lowlatL=-27
lonmax=360

lowlatR=30
lowlatL=-27
i_ok = where(gradT ne -555. AND opclstat eq 2. and r2N_erry gt 0.7 and footlat le lowlatR and footlat ge lowlatL  and ft gt 0.6  )
histoplot,nebasal(i_ok),win=1
rpoint_map,i_ok,footlon,footlat,vec_color=0
i_ok2= where(gradT ne -555. AND opclstat eq 2. and r2N_erry gt 0.7 and footlat le lowlatR and footlat ge lowlatL  and abs(pearson_t) ge 0.4  and hip_chi_pv2_t ge 0.8)
;chequeos globales
histoplot,pearson_t    (where(pearson_t ne -555.)),win=0
histoplot,pearson_n    (where(pearson_n ne -555.)),win=1
histoplot,hip_chi_pv_t (where(r2n       ne -555.)),win=8
histoplot,hip_chi_pv2_t(where(r2n       ne -555.)),win=8
histoplot,iso_erry     (where(r2n       ne -555.)),win=8
histoplot,r2t_erry     (where(r2n       ne -555.)),win=2,min=-1
histoplot,ft           (where(r2n       ne -555.)),win=2

histoplot,gradt_erry(where(abs(pearson_t) lt 0.5 and opclstat gt 0. and hip_chi_pv2_t ge 0.7 and ft ne -555. and iso_erry le 1.))/1.e6,win=1,min=-10,max=10
histoplot,gradt_erry(where(abs(pearson_t) ge 0.5 and opclstat gt 0. and hip_chi_pv2_t ge 0.7 and ft ne -555. and iso_erry gt 1.))/1.e6,win=2,min=-10,max=10
lowlatR=30
lowlatL=-27
lowlat_gradneg = where(gradT ne -555. AND opclstat eq 2. and r2N_erry gt 0.7 and footlat le lowlatR and footlat ge lowlatL and iso_erry ge 1. and hip_chi_pv2_t ge 0.7 and pearson_t le 0.)
histoplot,gradt_erry(lowlat_gradneg)/1.e6,win=1
histoplot,gradt_erry(lowlat_gradneg)/1.e6,win=1
histoplot,gradt_erry(lowlat_gradneg)/1.e6,win=1

rpoint_map,lowlat_gradneg,rp_medio.lon,rp_medio.lat,vec_color=0
lowlatR=30
lowlatL=-27
St_LIN=30
St_LIS=-27
St_FN=48
CH_LBS=-71
CH_LAS=-71
CH_LBN=73
CH_LAN=73
lonmax=360
i_lowlat             = where(gradT ne -555. AND opclstat eq 2.  and footlat le lowlatR and footlat ge lowlatL  and footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry le 0. )
i_midlat_small_norte = where(gradT ne -555. AND opclstat eq 2.  and footlat gt St_LIN  and                         footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_midlat_small_sur   = where(gradT ne -555. AND opclstat eq 2.  and footlat le St_LIS  and                         footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_large_OCN          = where(gradT ne -555. AND opclstat eq 1.  and footlat gt St_FN   and                         footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_large_OCS          = where(gradT ne -555. AND opclstat eq 0.  and footlat gt CH_LBS  and footlat le   0. and     footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_open_sur_L         = where(gradT ne -555. AND opclstat eq 0.  and footlat gt CH_LBS  and footlat le   0. and     footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_open_sur_H         = where(gradT ne -555. AND opclstat eq 0.  and footlat le CH_LAS  and footlat ge -80. and     footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_open_norte_L       = where(gradT ne -555. AND opclstat eq 0.  and footlat lt CH_LBN  and footlat gt   0. and     footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )
i_open_norte_H       = where(gradT ne -555. AND opclstat eq 0.  and footlat ge CH_LAN  and footlat le  80. and     footlon le lonmax and hip_chi_pv2_t ge 0.6 );and  gradt_erry gt 0. )

histoplot,tmmean(i_lowlat)/1.e6,win=1
histoplot,lambda_n(i_lowlat),win=2
histoplot,nebasal(i_lowlat)/1.e8,win=3

rpoint_map,i_lowlat,rp_medio.lon,rp_medio.lat,data2=i_midlat_small_norte,data3=i_midlat_small_sur,data4=i_large_OCN,data5=i_large_OCS,data6=i_open_sur_L,data7=i_open_sur_H,data8=i_open_norte_L,data9=i_open_norte_H
final:

return
end
