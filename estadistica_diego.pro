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

pro estadistica_diego,proceeding=proceeding,paper=paper,up=up,cr2082=cr2082,cr2208=cr2208,solo_demt=solo_demt

  restore,'trace_struct_LDEM_CR2082_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  demt2082 = datos
  restore,'trace_struct_LDEM_CR2082_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  awsom2082 = datos
  restore,'trace_struct_LDEM_CR2208_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  demt2208 = datos
  restore,'trace_struct_LDEM_CR2208_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  awsom2208 =datos

  
if keyword_set(paper) then begin ;PAPER
   if keyword_set(cr2082)then begin
;      CR - 2082
;cerrados chicos
;  ok_demt1  = where(demt2082.gradt ne -555. and demt2082.opclstat eq 2. and demt2082.r2n gt 0.7 and demt2082.footlat gt -30 and demt2082.footlat lt 30 and demt2082.ft ge 0.7 and demt2082.iso_erry gt 1.)
;  ok_demtcc  = where(demt2082.opclstat  eq 2. and demt2082.hip_chi_pv2_t  ge 0.7 and demt2082.ft ne -555. and demt2082.iso_erry gt 1. and abs(demt2082.footlat) le 30)
  ok_demtcc  = where(demt2082.opclstat  eq 2. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.gradt_erry  ne -555. );and demt2082.lincorr_pearson_t ge 0.5)
  ok_awsomcc = where(awsom2082.opclstat eq 2. and awsom2082.lincorr_pvalue_t le 0.05 and awsom2082.gradt_erry ne -555. )
  suf='_2082_demt_awsom_streamer_updown_'

  if keyword_set(up) then begin
  ok_demtcc  = where(demt2082.opclstat  eq 2. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.gradt_erry  ne -555. and demt2082.lincorr_pearson_t ge 0.5) 
  ok_awsomcc = where(awsom2082.opclstat eq 2. and awsom2082.lincorr_pvalue_t le 0.05 and awsom2082.gradt_erry ne -555. and awsom2082.lincorr_pearson_t ge 0.5)
  suf='_2082_demt_awsom_streamer_up_'
  endif
  
  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  histoplot, demt2082.tmmean(ok_demtcc )/1.e6,data2=awsom2082.tmmean(ok_awsomcc)/1.e6,win=1,tit='CR2082 - Streamer',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demtcc ),data2=awsom2082.lambda_n(ok_awsomcc)    ,win=2,min=-0.05,max=0.2,tit='CR2082 - Streamer',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demtcc)/1.e8,data2=awsom2082.nebasal(ok_awsomcc)/1.e8,win=3,tit='CR2082 - Streamer',xtit='Ne 1.025Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,ne_demt(ok_demtcc)/1.e8,data2=ne_awsom(ok_awsomcc)/1.e8,win=4,tit='CR2082 - Streamer',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='demt',label2='awsom'
  histoplot, demt2082.gradt_erry(ok_demtcc )/1.e6,data2=awsom2082.gradt_erry(ok_awsomcc)/1.e6,win=1,tit='CR2082 - Streamer',xtit='Temperature gradient [MK/Rsun]'   ,filename='histo'+suf+'gradt',label1='demt',label2='awsom',min=-10,max=10
  
  rpoint_map,ok_demtcc,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'demt',title='Physical location of loop at R=1.075'
  rpoint_map,ok_awsomcc,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'awsom',title='Physical location of loop at R=1.075'

;cerrados grandes 
  ok_demtcg  = where(demt2082.opclstat  eq 1. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.gradt_erry  ne -555. and abs(demt2082.footlat)   gt 30); and demt2082.lincorr_pearson_t ge 0.5)
  ok_awsomcg = where(awsom2082.opclstat eq 1. and awsom2082.lincorr_pvalue_t le 0.05 and awsom2082.gradt_erry ne -555. and abs(awsom2082.footlat)  gt 30)
  suf='_2082_demt_awsom_bound_updown_'

  if keyword_set(up) then begin
  ok_demtcg  = where(demt2082.opclstat  eq 1. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.gradt_erry  ne -555. and abs(demt2082.footlat)   gt 30 and demt2082.lincorr_pearson_t ge 0.5)   
  ok_awsomcg = where(awsom2082.opclstat eq 1. and awsom2082.lincorr_pvalue_t le 0.05 and awsom2082.gradt_erry ne -555. and abs(awsom2082.footlat)  gt 30 and awsom2082.lincorr_pearson_t ge 0.5)   
  suf='_2082_demt_awsom_bound_up_'
  endif
     
  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  histoplot, demt2082.tmmean(ok_demtcg )/1.e6,data2=awsom2082.tmmean(ok_awsomcg)/1.e6,win=1,tit='CR2082 - Boundary',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demtcg ),data2=awsom2082.lambda_n(ok_awsomcg)      ,win=2,min=-0.05,max=0.2,tit='CR2082 - Boundary',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demtcg)/1.e8,data2=awsom2082.nebasal(ok_awsomcg)/1.e8,win=3,tit='CR2082 - Boundary',xtit='Ne 1.025Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demtcg)/1.e8,data2=awsom2082.nebasal(ok_awsomcg)/1.e8,win=4,tit='CR2082 - Boundary',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='demt',label2='awsom'
  histoplot, demt2082.gradt_erry(ok_demtcg)/1.e6,data2=awsom2082.gradt_erry(ok_awsomcg)/1.e6,win=1,tit='CR2082 - Boundary',xtit='Temperature gradient [MK/Rsun]'   ,filename='histo'+suf+'gradt',label1='demt',label2='awsom',min=-10,max=10
  
  rpoint_map,ok_demtcg,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'demt'
  rpoint_map,ok_awsomcg,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'awsom'

;abiertos
  ok_demta  = where( demt2082.opclstat  eq 0. and demt2082.lincorr_pvalue_t   le 0.05 and demt2082.gradt_erry  ne -555. and abs(demt2082.footlat)  ge 60); and demt2082.lincorr_pearson_t ge 0.5)
  ok_awsoma = where( awsom2082.opclstat eq 0. and awsom2082.lincorr_pvalue_t  le 0.05 and awsom2082.gradt_erry ne -555. and abs(awsom2082.footlat) ge 60)
  suf='_2082_demt_awsom_CH_updown_'

  if keyword_set(up) then begin
     ok_demta  = where( demt2082.opclstat  eq 0. and demt2082.lincorr_pvalue_t   le 0.05 and demt2082.gradt_erry  ne -555. and abs(demt2082.footlat)  ge 60 and demt2082.lincorr_pearson_t ge 0.5)
     ok_awsoma = where( awsom2082.opclstat eq 0. and awsom2082.lincorr_pvalue_t  le 0.05 and awsom2082.gradt_erry ne -555. and abs(awsom2082.footlat) ge 60 and awsom2082.lincorr_pearson_t ge 0.5)
     suf='_2082_demt_awsom_CH_up_'
  endif
  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
 
  histoplot, demt2082.tmmean(ok_demta )/1.e6,data2=awsom2082.tmmean(ok_awsoma)/1.e6,win=1,tit='CR2082 - CH',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2082.lambda_n(ok_demta ),data2=awsom2082.lambda_n(ok_awsoma)      ,win=2,min=-0.05,max=0.2,tit='CR2082 - CH',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demta)/1.e8,data2=awsom2082.nebasal(ok_awsoma)/1.e8,win=3,tit='CR2082 - CH',xtit='Ne 1.025Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2082.nebasal(ok_demta)/1.e8,data2=awsom2082.nebasal(ok_awsoma)/1.e8,win=4,tit='CR2082 - CH',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='demt',label2='awsom'
  histoplot, demt2082.gradt_erry(ok_demta)/1.e6,data2=awsom2082.gradt_erry(ok_awsoma)/1.e6,win=1,tit='CR2082 - CH',xtit='Temperature gradient [MK/Rsun]'   ,filename='histo'+suf+'gradt',label1='demt',label2='awsom',min=-10,max=10
  
  rpoint_map,ok_demta,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'demt'
  rpoint_map,ok_awsoma,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'awsom'

;-> triple midpointmap
  suf1='_cr2082_updown'
  if keyword_set(up) then   suf1='_cr2082_up'

rpoint_map,ok_demtcc,data2=ok_demtcg,data3=ok_demta,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=6,vec_color=[0,1,2],title='Physical location of loop at R=1.075',filename='Midpoint_2082_demt_paper'+suf1
rpoint_map,ok_awsomcc,data2=ok_awsomcg,data3=ok_awsoma,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0,1,2],title='Physical location of loop at R=1.075',filename='Midpoint_2082_awsom_paper'+suf1
  
rpoint_map,ok_demtcc,data2=ok_demtcg,data3=ok_demta,demt2082.rp_alto.lon,demt2082.rp_alto.lat,win=6,vec_color=[0,1,2],title='Physical location of loop at R=1.105',filename='Highpoint_2082_demt_paper'+suf1
rpoint_map,ok_awsomcc,data2=ok_awsomcg,data3=ok_awsoma,awsom2082.rp_alto.lon,awsom2082.rp_alto.lat,win=7,vec_color=[0,1,2],title='Physical location of loop at R=1.105',filename='Highpoint_2082_awsom_paper'+suf1

;----------> Perfil radial promedio.
  vec_rad=1.025 + 0.02 *findgen(10)
  ne_demtcc  = median(demt2082.nebasal(ok_demtcc)) * exp(-1/median(demt2082.lambda_n(ok_demtcc)) * (1. - 1./vec_rad))
  ne_awsomcc = median(awsom2082.nebasal(ok_demtcc))* exp(-1/median(awsom2082.lambda_n(ok_demtcc))* (1. - 1./vec_rad))
  ne_demtcg  = median(demt2082.nebasal(ok_demtcg)) * exp(-1/median(demt2082.lambda_n(ok_demtcg)) * (1. - 1./vec_rad))
  ne_awsomcg = median(awsom2082.nebasal(ok_demtcg))* exp(-1/median(awsom2082.lambda_n(ok_demtcg))* (1. - 1./vec_rad))
  ne_demta   = median(demt2082.nebasal(ok_demta )) * exp(-1/median(demt2082.lambda_n(ok_demta )) * (1. - 1./vec_rad))
  ne_awsoma  = median(awsom2082.nebasal(ok_demta ))* exp(-1/median(awsom2082.lambda_n(ok_demta ))* (1. - 1./vec_rad))
    
  tm_demtcc  = median(demt2082.tm0_erry(ok_demtcc))   + median(demt2082.gradt_erry(ok_demtcc))   * vec_rad
  tm_awsomcc = median(awsom2082.tm0_erry(ok_awsomcc)) + median(awsom2082.gradt_erry(ok_awsomcc)) * vec_rad
  tm_demtcg  = median(demt2082.tm0_erry(ok_demtcg))   + median(demt2082.gradt_erry(ok_demtcg))   * vec_rad
  tm_awsomcg = median(awsom2082.tm0_erry(ok_awsomcg)) + median(awsom2082.gradt_erry(ok_awsomcg)) * vec_rad
  tm_demta   = median(demt2082.tm0_erry(ok_demta ))   + median(demt2082.gradt_erry(ok_demta ))   * vec_rad
  tm_awsoma  = median(awsom2082.tm0_erry(ok_awsoma )) + median(awsom2082.gradt_erry(ok_awsoma )) * vec_rad

perfil_paper,ne_demtcc,vec_rad,v1=ne_awsomcc,v2=ne_demtcg,v3=ne_awsomcg,v4=ne_demta,v5=ne_awsoma,win=1,ytit='Ne [10!U8!Ncm!U-3!N]',units=1.e8,tit='CR2082 - Radial Profile',filename='_ne'+suf1
perfil_paper,tm_demtcc,vec_rad,v1=tm_awsomcc,v2=tm_demtcg,v3=tm_awsomcg,v4=tm_demta,v5=tm_awsoma,win=2,ytit='Te [MK]',tit='CR2082 - Radial Profile',units=1.e6,filename='_te'+suf1

vec1=demt2082.gradt_erry(ok_demtcc)
vec2=awsom2082.gradt_erry(ok_awsomcc)
vec3=demt2082.gradt_erry(ok_demtcg)
vec4=awsom2082.gradt_erry(ok_awsomcg)
vec5=demt2082.gradt_erry(ok_demta)
vec6=awsom2082.gradt_erry(ok_awsoma)
histo_gradt_paper2,v1=vec1,v2=vec2,v3=vec3,v4=vec4,v5=vec5,v6=vec6,win=3,tit='CR2082',xtit='Temperature gradient [MK/Rsun]',label1='demt',label2='awsom',min=-10,max=10,filename='histo'+suf1+'triple_gradt'
  
endif  
  
;doble histos 2208
;cerrados chicos

   if keyword_set(cr2208) then begin
;   CR - 2208     
  ok_demtcc  = where( demt2208.opclstat  eq 2. and demt2208.lincorr_pvalue_t   le 0.05 and demt2208.gradt_erry  ne -555. );and abs(demt2208.footlat)  le 30)
  ok_awsomcc = where( awsom2208.opclstat eq 2. and awsom2208.lincorr_pvalue_t  le 0.05 and awsom2208.gradt_erry ne -555. );and abs(awsom2208.footlat) le 30)
  suf='_2208_demt_awsom_streamer_'
  suf1='rpoint_2208_streamer_'  
  if keyword_set(up) then begin
  ok_demtcc  = where(demt2208.opclstat  eq 2. and demt2208.lincorr_pvalue_t  le 0.05 and demt2208.gradt_erry  ne -555. and demt2208.lincorr_pearson_t ge 0.5)
  ok_awsomcc = where(awsom2208.opclstat eq 2. and awsom2208.lincorr_pvalue_t le 0.05 and awsom2208.gradt_erry ne -555. and awsom2208.lincorr_pearson_t ge 0.5)
  suf='_2208_demt_awsom_streamer_up_'
  endif
  
  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))

  histoplot, demt2208.tmmean(ok_demtcc )/1.e6,data2=awsom2208.tmmean(ok_awsomcc)/1.e6,win=1,tit='CR2082 - Streamer',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demtcc ),data2=awsom2208.lambda_n(ok_awsomcc)      ,win=2,min=-0.05,max=0.2,tit='CR2082 - Streamer',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demtcc)/1.e8,data2=awsom2208.nebasal(ok_awsomcc)/1.e8,win=3,tit='CR2082 - Streamer',xtit='Ne 1.025Rsun[10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demtcc)/1.e8,data2=awsom2208.nebasal(ok_awsomcc)/1.e8,win=4,tit='CR2082 - Streamer',xtit='Ne 1.055Rsun[10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='demt',label2='awsom'
  histoplot,demt2208.gradt_erry(ok_demtcc )/1.e6,data2=awsom2208.gradt_erry(ok_awsomcc)/1.e6,win=1,tit='CR2208 - Streamer',xtit='Temperature gradient [MK/Rsun]'   ,filename='histo'+suf+'gradt',label1='demt',label2='awsom',min=-10,max=10
  
  rpoint_map,ok_demtcc,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'demt'
  rpoint_map,ok_awsomcc,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'awsom'


;cerrados grandes + chicos arriba de 30 lat                                                                                                                                                                        
  ok_demtcg  = where( demt2208.opclstat  eq 1. and demt2208.lincorr_pvalue_t   le 0.05 and demt2208.gradt_erry  ne -555. and abs(demt2208.footlat)  gt 30)
  ok_awsomcg = where( awsom2208.opclstat eq 1. and awsom2208.lincorr_pvalue_t  le 0.05 and awsom2208.gradt_erry ne -555. and abs(awsom2208.footlat) gt 30)
  suf='_2208_demt_awsom_bound_updown_'
  suf1='rpoint_2208_bound_'
  if keyword_set(up) then begin
     ok_demtcg  = where( demt2208.opclstat  eq 1. and demt2208.lincorr_pvalue_t   le 0.05 and demt2208.gradt_erry  ne -555. and abs(demt2208.footlat)  gt 30 and demt2208.lincorr_pearson_t ge 0.5)
     ok_awsomcg = where( awsom2208.opclstat eq 1. and awsom2208.lincorr_pvalue_t  le 0.05 and awsom2208.gradt_erry ne -555. and abs(awsom2208.footlat) gt 30 and awsom2208.lincorr_pearson_t ge 0.5)
     suf='_2208_demt_awsom_bound_up_'
  endif
  
  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  
  histoplot, demt2208.tmmean(ok_demtcg )/1.e6,data2=awsom2208.tmmean(ok_awsomcg)/1.e6,win=1,tit='CR2082 - Boundary',xtit='[MK]',filename='histo'+suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demtcg ),data2=awsom2208.lambda_n(ok_awsomcg)      ,win=2,min=-0.05,max=0.2,tit='CR2082 - Boundary',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demtcg)/1.e8,data2=awsom2208.nebasal(ok_awsomcg)/1.e8,win=3,tit='CR2082 - Boundary',xtit='Ne 1.025Rsun[10!U8!Ncm!U-3!N]',filename='histo'+suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demtcg)/1.e8,data2=awsom2208.nebasal(ok_awsomcg)/1.e8,win=4,tit='CR2082 - Boundary',xtit='Ne 1.055Rsun[10!U8!Ncm!U-3!N]',filename='histo'+suf+'ne_1055',label1='demt',label2='awsom'
  histoplot,demt2208.gradt_erry(ok_demtcg )/1.e6,data2=awsom2208.gradt_erry(ok_awsomcg)/1.e6,win=1,tit='CR2208 - Boundary',xtit='Temperature gradient [MK/Rsun]'   ,filename='histo'+suf+'gradt',label1='demt',label2='awsom',min=-10,max=10

  rpoint_map,ok_demtcg,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'demt'
  rpoint_map,ok_awsomcg,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'awsom'


;abiertos                                                                                                                                                                                                          
  ok_demta  = where( demt2208.opclstat  eq 0. and demt2208.lincorr_pvalue_t   le 0.05 and demt2208.gradt_erry  ne -555. and abs(demt2208.footlat)  ge 60)
  ok_awsoma = where( awsom2208.opclstat eq 0. and awsom2208.lincorr_pvalue_t  le 0.05 and awsom2208.gradt_erry ne -555. and abs(awsom2208.footlat) ge 60)
  suf='_2208_demt_awsom_CH_updown_'
  suf1='rpoint_2208_CH_'  
  if keyword_set(up) then begin
     ok_demta  = where( demt2208.opclstat  eq 0. and demt2208.lincorr_pvalue_t   le 0.05 and demt2208.gradt_erry  ne -555. and abs(demt2208.footlat)  ge 60 and demt2208.lincorr_pearson_t ge 0.5)
     ok_awsoma = where( awsom2208.opclstat eq 0. and awsom2208.lincorr_pvalue_t  le 0.05 and awsom2208.gradt_erry ne -555. and abs(awsom2208.footlat) ge 60 and awsom2208.lincorr_pearson_t ge 0.5)
     suf='_2208_demt_awsom_CH_up_'
  endif
  
  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  
  histoplot, demt2208.tmmean(ok_demta )/1.e6,data2=awsom2208.tmmean(ok_awsoma)/1.e6,win=1,tit='CR2082 - CH',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='demt',label2='awsom'
  histoplot, demt2208.lambda_n(ok_demta ),data2=awsom2208.lambda_n(ok_awsoma)      ,win=2,min=-0.05,max=0.2,tit='CR2082 - CH',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demta)/1.e8,data2=awsom2208.nebasal(ok_awsoma)/1.e8,win=3,tit='CR2082 - CH',xtit='Ne 1.025Rsun[10!U8!Ncm!U-3!N]',filename='histo'+suf+'ne_1025',label1='demt',label2='awsom'
  histoplot,demt2208.nebasal(ok_demta)/1.e8,data2=awsom2208.nebasal(ok_awsoma)/1.e8,win=4,tit='CR2082 - CH',xtit='Ne 1.055Rsun[10!U8!Ncm!U-3!N]',filename='histo'+suf+'ne_1055',label1='demt',label2='awsom'
  histoplot,demt2208.gradt_erry(ok_demta )/1.e6,data2=awsom2208.gradt_erry(ok_awsoma)/1.e6,win=1,tit='CR2208 - CH',xtit='Temperature gradient [MK/Rsun]'   ,filename='histo'+suf+'gradt',label1='demt',label2='awsom',min=-10,max=10
  
  rpoint_map,ok_demta,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'demt'
  rpoint_map,ok_awsoma,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0],filename='rpoint'+suf+'awsom'

;-> triple midpointmap                 
  suf1='_cr2208_updown'
  if keyword_set(up) then   suf1='_cr2208_up'

rpoint_map,ok_demtcc,data2=ok_demtcg,data3=ok_demta,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=6,vec_color=[0,1,2],title='Physical location of loop at R=1.075',filename='Midpoint_2208_demt_paper'+suf1
rpoint_map,ok_awsomcc,data2=ok_awsomcg,data3=ok_awsoma,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0,1,2],title='Physical location of loop at R=1.075',filename='Midpoint_2208_awsom_paper'+suf1

rpoint_map,ok_demtcc,data2=ok_demtcg,data3=ok_demta,demt2208.rp_alto.lon,demt2208.rp_alto.lat,win=6,vec_color=[0,1,2],title='Physical location of loop at R=1.105',filename='Highpoint_2208_demt_paper'+suf1
rpoint_map,ok_awsomcc,data2=ok_awsomcg,data3=ok_awsoma,awsom2208.rp_alto.lon,awsom2208.rp_alto.lat,win=7,vec_color=[0,1,2],title='Physical location of loop at R=1.105',filename='Highpoint_2208_awsom_paper'+suf1
;--->
  vec_rad=1.025 + 0.02 *findgen(10)
  ne_demtcc  = median(demt2208.nebasal(ok_demtcc)) * exp(-1/median(demt2208.lambda_n(ok_demtcc)) * (1. - 1./vec_rad))
  ne_awsomcc = median(awsom2208.nebasal(ok_demtcc))* exp(-1/median(awsom2208.lambda_n(ok_demtcc))* (1. - 1./vec_rad))
  ne_demtcg  = median(demt2208.nebasal(ok_demtcg)) * exp(-1/median(demt2208.lambda_n(ok_demtcg)) * (1. - 1./vec_rad))
  ne_awsomcg = median(awsom2208.nebasal(ok_demtcg))* exp(-1/median(awsom2208.lambda_n(ok_demtcg))* (1. - 1./vec_rad))
  ne_demta   = median(demt2208.nebasal(ok_demta )) * exp(-1/median(demt2208.lambda_n(ok_demta )) * (1. - 1./vec_rad))
  ne_awsoma  = median(awsom2208.nebasal(ok_demta ))* exp(-1/median(awsom2208.lambda_n(ok_demta ))* (1. - 1./vec_rad))

  tm_demtcc  = median(demt2208.tm0_erry(ok_demtcc))   + median(demt2208.gradt_erry(ok_demtcc))   * vec_rad
  tm_awsomcc = median(awsom2208.tm0_erry(ok_awsomcc)) + median(awsom2208.gradt_erry(ok_awsomcc)) * vec_rad
  tm_demtcg  = median(demt2208.tm0_erry(ok_demtcg))   + median(demt2208.gradt_erry(ok_demtcg))   * vec_rad
  tm_awsomcg = median(awsom2208.tm0_erry(ok_awsomcg)) + median(awsom2208.gradt_erry(ok_awsomcg)) * vec_rad
  tm_demta   = median(demt2208.tm0_erry(ok_demta ))   + median(demt2208.gradt_erry(ok_demta ))   * vec_rad
  tm_awsoma  = median(awsom2208.tm0_erry(ok_awsoma )) + median(awsom2208.gradt_erry(ok_awsoma )) * vec_rad

perfil_paper,ne_demtcc,vec_rad,v1=ne_awsomcc,v2=ne_demtcg,v3=ne_awsomcg,v4=ne_demta,v5=ne_awsoma,win=1,ytit='Ne [10!U8!Ncm!U-3!N]',units=1.e8,tit='CR2208 - Radial Profile',filename='_ne'+suf1

perfil_paper,tm_demtcc,vec_rad,v1=tm_awsomcc,v2=tm_demtcg,v3=tm_awsomcg,v4=tm_demta,v5=tm_awsoma,win=2,ytit='Te [MK]',tit='CR2208 - Radial Profile',units=1.e6,filename='_te'+suf1

vec1=demt2208.gradt_erry(ok_demtcc)
vec2=awsom2208.gradt_erry(ok_awsomcc)
vec3=demt2208.gradt_erry(ok_demtcg)
vec4=awsom2208.gradt_erry(ok_awsomcg)
vec5=demt2208.gradt_erry(ok_demta)
vec6=awsom2208.gradt_erry(ok_awsoma)
histo_gradt_paper2,v1=vec1,v2=vec2,v3=vec3,v4=vec4,v5=vec5,v6=vec6,win=3,tit='CR2208',xtit='Temperature gradient [MK/Rsun]',label1='demt',label2='awsom',min=-10,max=10,filename='histo'+suf1+'triple_gradt'

endif

   if keyword_set(solo_demt) then begin
      suf='_2082_2208_fulldemt_streamer_up_'      
      ok_demtcc1  = where(demt2082.opclstat  eq 2. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.gradt_erry  ne -555. and demt2082.lincorr_pearson_t ge 0.5)
      ok_demtcc2  = where(demt2208.opclstat  eq 2. and demt2208.lincorr_pvalue_t  le 0.05 and demt2208.gradt_erry  ne -555. and demt2208.lincorr_pearson_t ge 0.5)

      ne_demt1  = (demt2082.nebasal)* exp(-1/(demt2082.lambda_n)* (1. - 1./1.055))
      ne_demt2  = (demt2208.nebasal)* exp(-1/(demt2208.lambda_n)* (1. - 1./1.055))
histoplot,demt2082.tmmean(ok_demtcc1)/1.e6,data2=demt2208.tmmean(ok_demtcc2)/1.e6,win=1,tit='CR2082 vs CR2208 Streamer',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='CR2082',label2='CR2208'
histoplot,demt2082.lambda_n(ok_demtcc1),data2=demt2208.lambda_n(ok_demtcc2),win=2,min=-.05,max=0.2,tit='CR2082 vs CR2208 Streamer',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='CR2082',label2='CR2208'
histoplot,ne_demt1(ok_demtcc1)/1.e8,data2=ne_demt2(ok_demtcc2)/1.e8,win=4,tit='CR2082 vs CR2208 Streamer',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='CR2082',label2='CR2208'
 
      suf='_2082_2208_fulldemt_bound_up_'
      ok_demtcg1  = where(demt2082.opclstat  eq 1. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.gradt_erry  ne -555. and abs(demt2082.footlat)   gt 30 and demt2208.lincorr_pearson_t ge 0.5)
      ok_demtcg2  = where(demt2208.opclstat  eq 1. and demt2208.lincorr_pvalue_t  le 0.05 and demt2208.gradt_erry  ne -555. and abs(demt2208.footlat)   gt 30 and demt2082.lincorr_pearson_t ge 0.5)
      
histoplot,demt2082.tmmean(ok_demtcg1 )/1.e6,data2=demt2208.tmmean(ok_demtcg2)/1.e6,win=1,tit='CR2082 vs CR2208 Boundary',xtit='Mean Temperature [MK]'   ,filename='histo'+suf+'Tm',label1='CR2082',label2='CR2208'
histoplot,demt2082.lambda_n(ok_demtcg1),data2=demt2208.lambda_n(ok_demtcg2),win=2,min=-.05,max=0.2,tit='CR2082 vs CR2208 Boundary',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='CR2082',label2='CR2208'
histoplot,ne_demt2(ok_demtcg1)/1.e8,data2=ne_demt2(ok_demtcg2)/1.e8,win=4,tit='CR2082 vs CR2208 Boundary',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='CR2082',label2='CR2208'
      
      suf='_2082_2208_fulldemt_CH_up_'
      ok_demta1  = where( demt2082.opclstat  eq 0. and demt2082.lincorr_pvalue_t   le 0.05 and demt2082.gradt_erry  ne -555. and abs(demt2082.footlat)  ge 60 and demt2208.lincorr_pearson_t ge 0.5)
      ok_demta2  = where( demt2208.opclstat  eq 0. and demt2208.lincorr_pvalue_t   le 0.05 and demt2208.gradt_erry  ne -555. and abs(demt2208.footlat)  ge 60 and demt2082.lincorr_pearson_t ge 0.5)
      
histoplot, demt2082.tmmean(ok_demta1 )/1.e6,data2=demt2208.tmmean(ok_demta2)/1.e6,win=1,tit='CR2082 vs CR2208 CH',xtit='Mean Temperature [MK]',filename='histo'+suf+'Tm',label1='CR2082',label2='CR2208'
histoplot, demt2082.lambda_n(ok_demta1 ),data2=demt2208.lambda_n(ok_demta2),win=2,min=-0.05,max=0.2,tit='CR2082 vs CR2208 CH',xtit='lambda N',filename='histo'+suf+'lambda_n',label1='CR2082',label2='CR2208'
histoplot,ne_demt1(ok_demta1)/1.e8,data2=ne_demt2(ok_demta2)/1.e8,win=4,tit='CR2082 vs CR2208 CH',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]' ,filename='histo'+suf+'ne_1055',label1='CR2082',label2='CR2208'
          
      
   endif
   if keyword_set(energia) then begin

      for i=0,n_elements(demt2082.opclstat)-1 do begin

         if demt2082.opclstat(i) eq 0.

         if demt2082.opclstat(i) eq 1. and demt2082.opclstat(i+1) eq 1.
         
         if demt2082.opclstat(i) eq 2. and demt2082.opclstat(i+1) eq 2.

         histoplot, demt2082.phi_r_total(ok_demtcc1)*2,min=2.e4,max=3.e5

 histoplot, demt2082.phi_c_total(ok_demtcg1)*2/1.e5,win=3,min=-1,max=5   
 histoplot, demt2082.phi_r_total(ok_demtcg1)*2/1.e5,win=3,min=-1,max=5



endif

   stop

endif


  
;PROCEEDING
if keyword_set(proceeding) then begin
;  rpoint_map,where(demt2082.hip_chi_pv2_t ne -555. and demt2082.opclstat gt 0.),data2=where(demt2082.hip_chi_pv2_t ne -555. and demt2082.opclstat eq 0.),demt2082.rp_alto.lon,demt2082.rp_alto.lat,win=7,vec_color=[0,1],title='CR-2082 Localizacion fisica de arcos magneticos a R=1.105',filename='proceeding2019_1105_2082_demt'

;rpoint_map,where(demt2208.hip_chi_pv2_t ne -555. and demt2208.opclstat gt 0.),data2=where(demt2208.hip_chi_pv2_t ne -555. and demt2208.opclstat eq 0.),demt2208.rp_alto.lon,demt2208.rp_alto.lat,win=7,vec_color=[0,1],title='CR-2208 Localizacion fisica de arcos magneticos a R=1.105',filename='proceeding2019_1105_2208_demt'

;CR-2082
;abierto
  ok_demta  = where( demt2082.opclstat  eq 0. and demt2082.lincorr_pvalue_t  le 0.05 and demt2082.r2t_erry ne -555. and abs(demt2082.footlat) ge 65); and demt2082.lincorr_pearson_t ge 0.5)
  ok_awsoma = where( awsom2082.opclstat eq 0. and awsom2082.lincorr_pvalue_t le 0.05 and demt2082.r2t_erry ne -555. and abs(awsom2082.footlat) ge 65)

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2082_demt_awsom_CH_'
  histoplot, demt2082.tmmean_alto(ok_demta )/1.e6,data2=awsom2082.tmmean(ok_awsoma)/1.e6,win=1,tit='CR2082 Agujero Coronal',$
             xtit='Temperatura media [MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom',max=1.3,ytit = 'Histograma de Frec.'
  histoplot, demt2082.lambda_n(ok_demta ),data2=awsom2082.lambda_n(ok_awsoma) ,win=2,max=0.15,tit='CR2082 Agujero Coronal',$
             xtit='Escala de altura',filename=suf+'lambda_n',label1='demt',label2='awsom',min=0.03,ytit = 'Histograma de Frec.'
  histoplot,ne_demt(ok_demta)/1.e8,data2=ne_awsom(ok_awsoma)/1.e8,win=4,tit='CR2082 Agujero Coronal',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]',$
            filename=suf+'ne_1055',label1='demt',label2='awsom',max=0.8,ytit = 'Histograma de Frec.'

;cerrados
  ok_demtc  = where(demt2082.opclstat  ge 1. and demt2082.lincorr_pvalue_t le 0.05 and demt2082.r2t_erry ne -555.)
  ok_awsomc = where(awsom2082.opclstat ge 1. and awsom2082.lincorr_pvalue_t le 0.05 and demt2082.r2t_erry ne -555.)

  ne_demt  = (demt2082.nebasal) * exp(-1/(demt2082.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2082.nebasal)* exp(-1/(awsom2082.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2082_demt_awsom_streamer_'
  histoplot, demt2082.tmmean(ok_demtc )/1.e6,data2=awsom2082.tmmean(ok_awsomc)/1.e6,win=1,tit='CR2082 Streamer',$
             xtit='Temperatura media [MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom',max=2,ytit = 'Histograma de Frec.'
  histoplot, demt2082.lambda_n(ok_demtc ),data2=awsom2082.lambda_n(ok_awsomc)      ,win=2,max=0.15,tit='CR2082 Streamer',$
             xtit='Escala de altura',filename=suf+'lambda_n',label1='demt',label2='awsom',min=0.01,ytit = 'Histograma de Frec.'
  histoplot,ne_demt(ok_demtc)/1.e8,data2=ne_awsom(ok_awsomc)/1.e8,win=4,tit='CR2082 Streamer',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]',$
            filename=suf+'ne_1055',label1='demt',label2='awsom',max=1.4,ytit = 'Histograma de Frec.'

  rpoint_map,ok_demtc,data2=ok_demta,demt2082.rp_medio.lon,demt2082.rp_medio.lat,win=6,vec_color=[0,1],title='Localizacion fisica de arcos (1.075)',filename='Midpoint_2082_demt-proceeding'
  rpoint_map,ok_awsomc,data2=ok_awsoma,awsom2082.rp_medio.lon,awsom2082.rp_medio.lat,win=7,vec_color=[0,1],title='Localizacion fisica de arcos (1.075)',filename='Midpoint_2082_awsom_proceeding'

  
;CR2208
;abiertos
  ok_demta  = where( demt2208.opclstat  eq 0. and demt2208.lincorr_pvalue_t  le 0.05 and demt2208.r2t_erry ne -555.  and abs(demt2208.footlat) ge 65)
  ok_awsoma = where( awsom2208.opclstat eq 0. and awsom2208.lincorr_pvalue_t le 0.05 and demt2208.r2t_erry ne -555.  and abs(awsom2208.footlat) ge 65)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2208_demt_awsom_CH_'
  histoplot, demt2208.tmmean(ok_demta )/1.e6,data2=awsom2208.tmmean(ok_awsoma)/1.e6,win=1,tit='CR2208 Agujero Coronal',$
             xtit='Temperatura media [MK]'   ,filename=suf+'Tm',label1='demt',label2='awsom',max=2,ytit = 'Histograma de Frec.'
  histoplot, demt2208.lambda_n(ok_demta ),data2=awsom2208.lambda_n(ok_awsoma) ,win=2,min=0.01,tit='CR2208 Agujero Coronal',$
             max=0.15,xtit='Escala de altura',filename=suf+'lambda_n',label1='demt',label2='awsom',ytit = 'Histograma de Frec.'
  histoplot,ne_demt(ok_demta)/1.e8,data2=ne_awsom(ok_awsoma)/1.e8,win=4,tit='CR2208 Agujero Coronal',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]',$
            filename=suf+'ne_1055',label1='demt',label2='awsom',max=.8,ytit = 'Histograma de Frec.'

;cerrados, todo
  ok_demtc  = where( demt2208.opclstat  ge 1. and demt2208.lincorr_pvalue_t  le 0.05 and demt2208.r2t_erry ne -555. );and abs(demt2208.footlat) gt 30)
  ok_awsomc = where( awsom2208.opclstat ge 1. and awsom2208.lincorr_pvalue_t le 0.05 and demt2208.r2t_erry ne -555. );and abs(awsom2208.footlat) gt 30)

  ne_demt  = (demt2208.nebasal) * exp(-1/(demt2208.lambda_n) * (1. - 1./1.055))
  ne_awsom = (awsom2208.nebasal)* exp(-1/(awsom2208.lambda_n)* (1. - 1./1.055))
  suf='proceeding_2208_demt_awsom_streamer_'
  histoplot, demt2208.tmmean(ok_demtc )/1.e6,data2=awsom2208.tmmean(ok_awsomc)/1.e6,win=1,tit='CR2208 Streamer',xtit='Temperatura media [MK]',$
             filename=suf+'Tm',label1='demt',label2='awsom',max=2,ytit = 'Histograma de Frec.'
  histoplot, demt2208.lambda_n(ok_demtc ),data2=awsom2208.lambda_n(ok_awsomc)      ,win=2,min=0.01,max=0.2,tit='CR2208 Streamer',$
             xtit='Escala de altura',filename=suf+'lambda_n',label1='demt',label2='awsom',ytit = 'Histograma de Frec.'
  histoplot,ne_demt(ok_demtc)/1.e8,data2=ne_awsom(ok_awsomc)/1.e8,win=4,tit='CR2208 Streamer',xtit='Ne 1.055Rsun [10!U8!Ncm!U-3!N]',$
            filename=suf+'ne_1055',label1='demt',label2='awsom',max=1.5,ytit = 'Histograma de Frec.'



  rpoint_map,ok_demtc,data2=ok_demta,demt2208.rp_medio.lon,demt2208.rp_medio.lat,win=6,vec_color=[0,1],title='Localizacion fisica de arcos (1.075)',filename='Midpoint_2208_demt-proceeding'
  rpoint_map,ok_awsomc,data2=ok_awsoma,awsom2208.rp_medio.lon,awsom2208.rp_medio.lat,win=7,vec_color=[0,1],title='Localizacion fisica de arcos (1.075)',filename='Midpoint_2208_awsom_proceeding'

return
endif

  

  return
end

