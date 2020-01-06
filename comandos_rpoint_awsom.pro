  restore,'trace_struct_LDEM_CR2082_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  awsom2082 = datos
  restore,'trace_struct_LDEM_CR2208_awsom-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  awsom2208 =datos

  c_2082 = awsom2082.long_s(where(awsom2082.opclstat ne 0 and awsom2082.long_s ne -555.))
  c_2208 = awsom2208.long_s(where(awsom2208.opclstat ne 0 and awsom2208.long_s ne -555.))
;  histoplot,c_2082,win=0


;  Ls_2082 = 0.26; 0.28
;  Ll_2082 = 0.26;Ls_2082

; c_s_u_2082 = where(awsom2082.opclstat ne 0 and awsom2082.long_s ne -555. and awsom2082.long_s lt Ls_2082 and awsom2082.pearson_t ge +0.4 and abs(awsom2082.rp_base.lat) lt 50)
; c_e_2082   = where(awsom2082.opclstat ne 0 and awsom2082.long_s ne -555. and awsom2082.long_s ge Ll_2082 and awsom2082.pearson_t ge +0.4 and abs(awsom2082.rp_base.lat) gt 40)
; op_2082    = where(awsom2082.opclstat eq 0 and awsom2082.long_s ne -555. and                           awsom2082.pearson_t ge +0.4 and abs(awsom2082.rp_base.lat) gt 40)
; rpoint_map,c_s_u_2082,data2=c_e_2082,data3=op_2082,awsom2082.rp_alto.lon,awsom2082.rp_alto.lat,win=1,vec_color=[1,5,4],tit='CR-2082'
 
 Ls_2208 = 0.25
 Ll_2208 = 0.25;Ls_2208
  c_2208 = awsom2208.long_s(where(awsom2208.opclstat ne 0 and awsom2208.long_s ne -555.))
  histoplot,c_2208,win=1

  c_s_u_2208 = where(awsom2208.opclstat ne 0 and awsom2208.long_s ne -555. and awsom2208.long_s lt Ls_2208 and awsom2208.pearson_t ge +0.4 and abs(awsom2208.rp_base.lat) lt 50)
 c_e_2208   = where(awsom2208.opclstat ne 0 and awsom2208.long_s ne -555. and awsom2208.long_s ge Ll_2208 and awsom2208.pearson_t ge +0.4 and abs(awsom2208.rp_base.lat) gt 40)
 op_2208    = where(awsom2208.opclstat eq 0 and awsom2208.long_s ne -555. and                           awsom2208.pearson_t ge +0.4 and abs(awsom2208.rp_base.lat) gt 40)
 rpoint_map,c_s_u_2208,data2=c_e_2208,data3=op_2208,awsom2208.rp_alto.lon,awsom2208.rp_alto.lat,win=2,vec_color=[1,5,4],tit='CR-2208'

