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
pro estadistica_diego,file

;restore del trzado.sav
  restore,file
  
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
i_lowlat_gradneg = where(gradT ne -555. AND opclstat eq 2. and r2N gt r2crit_N and footlat le lowlatR and footlat ge lowlatL  and footlon le lonmax and r2T gt r2cr)


return
end
