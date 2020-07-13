;statloop_compuesto,file='traceLDEM_CR2208_awsom_test_mucha_altura_vr_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'


pro statloop_compuesto,file=file

;la idea es que sea similar al statloop original pero en principio
;sin hacer fiteos. La idea es usar trazados a mucha altura que
;que involucren resultados DEMT + AWSOM + lasco. Ver como se
;relacionan con la vel terminal, el factor de expansion, etc.

  !except=2
  device, retain     = 2
  device, true_color = 24
  device, decomposed = 0

; Physical constants needed for the HS fits:
  rsun = 6.955e10              ; cm                                                                                                                                                                                                          
  gsun = 2.74e4                ; cm/sec²
  kB = 1.38e-16                ; erg/K
  mH = 1.6726e-24              ; gr
  a = 0.08                     ; N_He / N_H
  mu = (1.+4.*a)/(1.+2.*a)
  bb = (1.+2.*a)/(2.+3.*a)
  kappa = 9.2e-7                ; erg s ^-1 cm ^-1 K ^-7/2   

  restore,file

;different types of loops/legs
  Nloop = n_elements(loopL)
  index0 = where(opcls eq 0.)
  index1 = where(opcls eq 1.)
  index2 = where(opcls eq 2.)

  Nloop0 = n_elements(index0) & if index0(0) eq -1 then Nloop0=0
  Nloop1 = n_elements(index1) & if index1(0) eq -1 then Nloop1=0
  Nloop2 = n_elements(index2) & if index2(0) eq -1 then Nloop2=0

  if Nloop0 + Nloop1 + Nloop2 ne Nloop then stop
  Nlegs = Nloop0 + 2*Nloop1 + 2*Nloop2

  
;paso los valores de las amtrices del trace a vectores faciles de usar
;y leer que no tienen elementos de relleno

  for il=0L,Nloop-1 do begin

; Analysis for OPEN loops:                                                                                                                                                                                                                    
     if opcls(il) eq 0. then begin

        Ne_l = reform ( Ne_v(0:Npts_v(il)-1,il))
        Tm_l = reform ( Tm_v(0:Npts_v(il)-1,il))
        Vr_l = reform ( Vr_v(0:Npts_v(il)-1,il))
        rad_l = reform (rad_v(0:Npts_v(il)-1,il))
        lat_l = reform (lat_v(0:Npts_v(il)-1,il))
        lon_l = reform (lon_v(0:Npts_v(il)-1,il))
        s_l = reform (  s_v(0:Npts_v(il)-1,il))
        B_l = reform (  B_v(0:Npts_v(il)-1,il))
        Br_l = reform ( Br_v(0:Npts_v(il)-1,il))
        Bth_l = reform ( Bth_v(0:Npts_v(il)-1,il))
        Bph_l = reform ( Bph_v(0:Npts_v(il)-1,il))
        Er_l = reform ( Er_v(0:Npts_v(il)-1,il))
        
                                ;obs: depende que estemos evaluando,
                                ;ver si es necesario recortar valores
                                ;con algo como p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999.)
        
     endfor
  endif
  

  return
end
