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
pro estadistica_diego
  file1 = 'trace_vectors_LDEM_CR2082_hollow_demt__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  file2 = 'trace_vectors_LDEM_CR2082_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  file2= 'trace_vectors_LDEM_CR2208_con_awsomdata__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
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
i_lowlat_gradneg = where(gradT ne -555. AND opclstat eq 2. and r2N gt r2crit_N and footlat le lowlatR and footlat ge lowlatL  and footlon le lonmax and r2T gt r2cr)
final:

return
end
