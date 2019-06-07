;file='trace_vectors_LDEM_CR2082_test_ldem_6alturas_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
pro estadistica_diego,file

;restore del trzado.sav
  restore,file
  
fcrit_t=0.7
fcrit_n=0.75
r2crit_t=0.7
r2crit_N=0.75
ok = where(gradt ne -555.)
histoplot,ft(ok),min=0
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
