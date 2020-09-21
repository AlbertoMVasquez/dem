;statloop_compuesto,file='traceLDEM_CR2208_awsom_test_mucha_altura_vr_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
;statloop_compuesto,file='traceLDEM_CR2208_awsom_test_multitrace_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
;statloop_compuesto,file='traceLDEM_CR2219_awsom_demt_multitrace_radstart-5.505Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav',/demt,/awsom
;nueva reforma
;statloop_compuesto,file='traceLDEM_CR2219_demt_campo_awsom_radstart-5.995Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav',/demt,file_out='teste_covid'
;statloop_compuesto,file='traceLDEM_CR2219_awsom_campo_awsom_radstart-5.995Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav',/awsom,file_out=''
pro statloop_compuesto,file=file,demt=demt,awsom=awsom,lasco=lasco,kcor=kcor,file_out=file_out

;la idea es que sea similar al statloop original pero en principio
;sin hacer fiteos. La idea es usar trazados a mucha altura que
;que involucren resultados DEMT + AWSOM + lasco. Ver como se
;relacionan con la vel terminal, el factor de expansion, etc.
longitud = strlen(file)-5-4
if not keyword_set(file_out) then file_out = strmid(file,5,longitud)
  
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


  if not keyword_set(corte_awsom) then corte_awsom = 1.055
  
  restore,file

;different types of loops/legs
  Nloop = n_elements(loopL)
  index0 = where(opcls eq 0.)
  index1 = where(opcls eq 1.)
  index2 = where(opcls eq 2.)

  Nloop0 = n_elements(index0) & if index0(0) eq -1 then Nloop0=0;abierto
  Nloop1 = n_elements(index1) & if index1(0) eq -1 then Nloop1=0
  Nloop2 = n_elements(index2) & if index2(0) eq -1 then Nloop2=0

  if Nloop0 + Nloop1 + Nloop2 ne Nloop then stop
  Nlegs = Nloop0 + 2*Nloop1 + 2*Nloop2

  
;paso los valores de las amtrices del trace a vectores faciles de usar
;y leer que no tienen elementos de relleno

;--- Ajustes de temperatura
  Tmmean = fltarr(Nlegs)-555.
  Tm0_erry  = fltarr(Nlegs)-555.
  gradT_erry  = fltarr(Nlegs)-555.
  lincorr_pearson_t  = fltarr(Nlegs)-555.
  lincorr_pvalue_t  = fltarr(Nlegs)-555.
  hip_chi_pv_t  = fltarr(Nlegs)-555.
  hip_chi_ch_t  = fltarr(Nlegs)-555.
  hip_chi_pv2_t  = fltarr(Nlegs)-555.
  hip_chi_ch2_t  = fltarr(Nlegs)-555.

  
;--- Ajustes de Densidad  
  Ne0 = fltarr(Nlegs)-555.
  lambda_N = fltarr(Nlegs)-555.
  lincorr_pearson_n  = fltarr(Nlegs)-555.
  lincorr_pvalue_n  = fltarr(Nlegs)-555.
  hip_chi_pv2_n  = fltarr(Nlegs)-555.
  hip_chi_ch2_n  = fltarr(Nlegs)-555.

;--Ajustes energia
  Eh      = fltarr(Nlegs)-555.
  Phi_r    = fltarr(Nlegs)-555.
  Fcb     = fltarr(Nlegs)-555.
  phi_r_total = fltarr(Nlegs)-555.
  phi_c_total = fltarr(Nlegs)-555.
  
  
;--- longitudes de lineas  
  long_r = fltarr(Nlegs)-555.
  long_s = fltarr(Nlegs)-555.

  if keyword_set(demt) then begin
;--- errores demt
  error_ne = fltarr(Nlegs)-555.
  error_t  = fltarr(Nlegs)-555.
  scoreR = fltarr(Nlegs)-555.
endif
  
;--- Posisiones a distintas alturas
  Rp_0     = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.} ;1rsun
  Rp_base  = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.} ;1.025rsun
  Rp_mhd   = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.} ;1.055rsun
  Rp_medio = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.} ;1.105rsun
  Rp_alto  = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.} ;6rsun

;starting points de los loops correspondientes a cada leg 
  strad = fltarr(Nlegs)-555.
  stlat = fltarr(Nlegs)-555.
  stlon = fltarr(Nlegs)-555.
  
  
  opclstat   = fltarr(Nloop)-555.
  indexloop  = fltarr(Nloop)-555.


  ne_base_mhd  = fltarr(Nloop)-555.
  ne_base_demt = fltarr(Nloop)-555.
  ne_final  = fltarr(Nloop)-555.
  te_base_mhd   = fltarr(Nloop)-555.
  te_base_demt  = fltarr(Nloop)-555.
  te_final   = fltarr(Nloop)-555.

  ne_demt_base_demt = fltarr(Nloop)-555.
  ne_demt_base_mhd  = fltarr(Nloop)-555.
  ne_demt_final  = fltarr(Nloop)-555.
  te_demt_base_demt = fltarr(Nloop)-555.
  te_demt_base_mhd  = fltarr(Nloop)-555.
  te_demt_final  = fltarr(Nloop)-555.

  vr_base_demt  = fltarr(Nloop)-555.
  vth_base_demt = fltarr(Nloop)-555.
  vph_base_demt = fltarr(Nloop)-555.
  vr_base_mhd   = fltarr(Nloop)-555.
  vth_base_mhd  = fltarr(Nloop)-555.
  vph_base_mhd  = fltarr(Nloop)-555.
  vr_final      = fltarr(Nloop)-555.
  vth_final     = fltarr(Nloop)-555.
  vph_final     = fltarr(Nloop)-555.
  fexp_base_mhd   = fltarr(Nloop)-555.
  fexpr_base_mhd  = fltarr(Nloop)-555.
  fexp_base_demt  = fltarr(Nloop)-555.
  fexpr_base_demt = fltarr(Nloop)-555.
  fexp_f      = fltarr(Nloop)-555.
  fexpr_f     = fltarr(Nloop)-555.
  fexp        = fltarr(Nloop)-555.
  fexpr       = fltarr(Nloop)-555.
  r_base_mhd  = fltarr(Nloop)-555.
  r_base_demt = fltarr(Nloop)-555.
  r_final     = fltarr(Nloop)-555.
  
  fexpr_0    = fltarr(Nloop)-555.
  foot_lat_0 = fltarr(Nloop)-555.
  foot_lon_0 = fltarr(Nloop)-555.
  
  foot_lat_base_demt = fltarr(Nloop)-555.
  foot_lon_base_demt = fltarr(Nloop)-555.
  
  foot_lat_base_mhd = fltarr(Nloop)-555.
  foot_lon_base_mhd = fltarr(Nloop)-555.

  end_lat = fltarr(Nloop)-555.
  end_lon = fltarr(Nloop)-555.

  B_final    = fltarr(Nloop)-555.
  Br_final   = fltarr(Nloop)-555.

  ;matrices y alturas fijas
  alturas = [1.0,1.025,1.065,1.1,1.15,1.25,1.5,2.,2.5,3.,3.5,4.,4.5,5,5.5,6]
  extra = 1;valor primero / ultimo de cada pierna
  mat_r   = fltarr(n_elements(alturas+extra),nloop)
  mat_br  = fltarr(n_elements(alturas+extra),nloop)
  mat_ne  = fltarr(n_elements(alturas+extra),nloop)
  mat_te  = fltarr(n_elements(alturas+extra),nloop)
  mat_vr  = fltarr(n_elements(alturas+extra),nloop)
  mat_vth = fltarr(n_elements(alturas+extra),nloop)
  mat_vph = fltarr(n_elements(alturas+extra),nloop)
  mat_lat = fltarr(n_elements(alturas+extra),nloop)
  mat_lon = fltarr(n_elements(alturas+extra),nloop)


  
  err_ne = 4.e6 *2. ;doble error para demt
  err_tm = 7.e4 *2.




;ileg es la cantidad de piernas totales, incluyendo cerradas y abiertas
  ileg =0L
  
  for il=0L,Nloop-1 do begin

; Analysis for OPEN loops:
     if opcls(il) eq 0. then begin
        ;awsom
        if keyword_set (awsom) then begin
           Ne_awsom_l  = reform ( Ne_v(0:Npts_v(il)-1,il))
           Tm_awsom_l  = reform ( Tm_v(0:Npts_v(il)-1,il))
           Er_awsom_l  = reform ( Er_v(0:Npts_v(il)-1,il))
           Vr_l        = reform ( Vr_v(0:Npts_v(il)-1,il))
           Vth_l       = reform ( Vth_v(0:Npts_v(il)-1,il))
           Vph_l       = reform ( Vph_v(0:Npts_v(il)-1,il))
        endif
        ;demt
        if keyword_set(demt) then begin
           Ne_demt_l = reform ( Ne_demt_v(0:Npts_v(il)-1,il))
           Tm_demt_l = reform ( Tm_demt_v(0:Npts_v(il)-1,il))
           Er_demt_l = reform ( Er_demt_v(0:Npts_v(il)-1,il))
           scoreR_l  = reform ( scoreR_v(0:Npts_v(il)-1,il))
        endif
        ;Lasco
        if keyword_set(lasco) then begin
           Ne_lasco_l = reform ( Ne_lasco_v(0:Npts_v(il)-1,il))
        endif
        ;kcor
        if keyword_set(kcor) then begin
           Ne_kcor_l = reform ( Ne_kcor_v(0:Npts_v(il)-1,il))
        endif

        
        rad_l = reform (rad_v(0:Npts_v(il)-1,il))
        lat_l = reform (lat_v(0:Npts_v(il)-1,il))
        lon_l = reform (lon_v(0:Npts_v(il)-1,il))
        s_l   = reform (  s_v(0:Npts_v(il)-1,il))
        B_l   = reform (  B_v(0:Npts_v(il)-1,il))
        Br_l  = reform ( Br_v(0:Npts_v(il)-1,il))
        Bth_l = reform ( Bth_v(0:Npts_v(il)-1,il))
        Bph_l = reform ( Bph_v(0:Npts_v(il)-1,il))

        foot_lat_0(il) = lat_v(0,il)
        foot_lon_0(il) = lon_v(0,il)
        

 ;matrices
        for i=0,n_elements(alturas)-1 do begin ;  alturas = [1.0,1.05,1.1,1.15,1.25,1.5,2.,2.5,3.,3.5,4.,4.5,5,5.5,6]
           
           min_rad = where( abs(rad_l - alturas(i)) eq min(abs(rad_l-alturas(i))))
           r_pos = min_rad(0)
           
           if keyword_set(awsom) then begin
              mat_ne(i,il)  = Ne_demt_l(r_pos)
              mat_te(i,il)  = Tm_demt_l(r_pos)
              mat_br(i,il)  = Br_l(r_pos)
              mat_vr(i,il)  = Vr_l(r_pos)
              mat_vth(i,il) = Vth_l(r_pos)
              mat_vph(i,il) = Vph_l(r_pos)
              mat_ne(i,il)  = Ne_awsom_l(r_pos)
              mat_te(i,il)  = Tm_awsom_l(r_pos)
           endif
           mat_r(i,il)   = rad_l(r_pos)
           mat_lat(i,il) = lat_l(r_pos)
           mat_lon(i,il) = lon_l(r_pos)
           if keyword_set(demt) then begin
              mat_ne(i,il) = Ne_demt_l(r_pos)
              mat_te(i,il) = Tm_demt_l(r_pos)
           endif
        endfor

;celda inicial de la pierna?


;celda final de la pierna
        if keyword_set(awsom) then begin
           if (where(Ne_awsom_l eq -666))(0) eq -1 then sobrante =0.
           if (where(Ne_awsom_l eq -666))(0) ne -1 then sobrante =n_elements(where(ne_awsom_l eq -666))
        endif
        if keyword_set(demt) then begin
           if (where(Ne_demt_l eq -666))(0) eq -1 then sobrante =0.
           if (where(Ne_demt_l eq -666))(0) ne -1 then sobrante =n_elements(where(ne_demt_l eq -666))           
        endif

           if keyword_set(awsom) then begin
              mat_ne(n_elements(alturas+extra),il)  = Ne_awsom_l(n_elements(ne_awsom_l) - sobrante)
              mat_te(n_elements(alturas+extra),il)  = Tm_awsom_l(n_elements(ne_awsom_l) - sobrante)
              mat_br(n_elements(alturas+extra),il)  = Br_l(n_elements(ne_awsom_l) - sobrante)
              mat_vr(n_elements(alturas+extra),il)  = Vr_l(n_elements(ne_awsom_l) - sobrante)
              mat_vth(n_elements(alturas+extra),il) = Vth_l(n_elements(ne_awsom_l) - sobrante)
              mat_vph(n_elements(alturas+extra),il) = Vph_l(n_elements(ne_awsom_l) - sobrante)
           endif
           mat_r(n_elements(alturas+extra),il)   = rad_l()
           mat_lat(n_elements(alturas+extra),il) = lat_l()
           mat_lon(n_elements(alturas+extra),il) = lon_l()
           if keyword_set(demt) then begin
	      mat_ne(n_elements(alturas+extra),il) = Ne_demt_l(n_elements(ne_demt_l) - sobrante)
              mat_te(n_elements(alturas+extra),il) = Tm_demt_l(n_elements(ne_demt_l) - sobrante)
           endif

;        goto,hoy_no

        if keyword_set(demt)then begin
           p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. and scoreR_l lt 0.25)
           if p(0) eq -1             then demt_leg_sin_datos = 1;flag
           if n_elements(p) lt Ndata then demt_leg_sin_datos = 1
                                ;_tbf significa to be fitted, son los
                                ;vectores que luego se fitearan
           Ne_l_tbf  =  Ne_demt_l (p)
           Tm_l_tbf  =  Tm_demt_l (p)
           Er_l_tbf  =  Er_demt_l (p)
           WT_l_tbf  =  WT_l (p)
           rad_l_tbf = rad_l (p)
           lat_l_tbf = lat_l (p)
           lon_l_tbf = lon_l (p)
           s_l_tbf   =   s_l (p)
           B_l_tbf   =   B_l (p)
           p_l_tbf   = kB/bb *Ne_demt_l*Tm_demt_l
           beta_l    = p_l/(B_l^2/(8*!pi))

           Tmmean(ileg) =  mean(Tm_demt_l(p))

;IMP:aca viene la parte de los tercios

;IMP:si no hay ajuste alto ni bajo entonces

           sfit = s_l_tbf
           xfit = rad_l_tbf
           yfit = Ne_l_tbf
           zfit = p_l_tbf
           wfit = Tm_l_tbf
           rfit = Er_l_tbf
           min_s = min(s_l_tbf)
           max_s = max(s_l_tbf)
           min_r = rad_l(findel(rmin,rad_l))
           max_r = rad_l(findel(rmax,rad_l))
          
;---Ajuste densidad
           x_max = 1./min_r
           x_min = 1./max_r
           p1 = where(1./xfit ge x_min and 1./xfit le x_max)
           xxfit = xfit(p1)
           yyfit = yfit(p1)
           
           sigma_n(ileg) = stddev(yyfit)
           
           linear_fit,1/xxfit,alog(yyfit),A,r2,salidafit,/theilsen
           Ne0(ileg) = exp(A[0]+A[1])
           lambda_N(ileg) = 1./A[1]
           r2N(ileg) = r2
;nuevo
           linear_fit,1/xxfit,alog(yyfit),A,r2,salidafit2,/linfit_err,err_y=((xxfit*0)+alog(err_ne))
           Ne0_erry     (ileg) = exp(A[0]+A[1])
           lambda_N_erry(ileg) = 1./A[1]
           r2N_erry     (ileg) = r2

           sta1 = lincorr(1/xxfit,alog(yyfit),/double,T_STAT=t1)
           lincorr_pearson_n   (ileg) = sta1(0) ;se calcula con 1/xfit
           lincorr_pvalue_n    (ileg) = sta1(1)
           lincorr_tstatistic_n(ileg) = sta1(2)
           
           sta2 = hipotesis_chi(alog(yyfit),salidafit,error_y=(xxfit*0)+alog(err_ne))
           hip_chi_pv_n (ileg) = sta2(0)
           hip_chi_ch_n (ileg) = sta2(1)
           
           sta3 = hipotesis_chi(alog(yyfit),salidafit2,error_y=(xxfit*0)+alog(err_ne))
           hip_chi_pv2_n (ileg) = sta3(0)
           hip_chi_ch2_n (ileg) = sta3(1)

;IMP:Ajuste sobre s


;---Ajuste Temperatura
           
           p3 = where(xfit ge min_r and xfit le max_r)
           xxfit = xfit(p3)
           wwfit = wfit(p3)

           linear_fit,xxfit,wwfit,A,r2,salidafit2,/linfit_err,err_y=((xxfit*0)+err_tm),prob=prob
           Tm0_erry     (ileg) = A[0]
           gradT_erry(ileg)    = A[1]
           r2t_erry     (ileg) = r2
           prob_t        (ileg)=prob;?

           sta1 = lincorr(xxfit,wwfit,/double,T_STAT=t1)
           lincorr_pearson_t   (ileg) = sta1(0) 
           lincorr_pvalue_t    (ileg) = sta1(1)
           lincorr_tstatistic_t(ileg) = sta1(2)
           
           sta2 = hipotesis_chi(wwfit,salidafit,error_y=(xxfit*0)+err_tm)
           hip_chi_pv_t (ileg) = sta2(0)
           hip_chi_ch_t (ileg) = sta2(1)
           
           sta3 = hipotesis_chi(wwfit,salidafit2,error_y=(xxfit*0)+err_tm)
           hip_chi_pv2_t (ileg) = sta3(0)
           hip_chi_ch2_t (ileg) = sta3(1)
           
           long_r(ileg)  = max_r - min_r
           long_s(ileg)  = max_s - min_s
           ;isotermicos
           iso_erry(ileg)= abs(gradT_erry (ileg)    * long_r(ileg)) / (2 * error_t(ileg))
           iso_s(ileg)   = abs(gradT_s(ileg)        * long_s(ileg)) / (2 * error_t(ileg))
           
           skipfitloop_open:
           opclstat(ileg) = opcls(il)
           loop_length(ileg) = loopL(il)
           strad(ileg) = str_v(il)
           stlat(ileg) = (90-stth_v(il)/!dtor)
           stlon(ileg) = stph_v(il)/!dtor
           skipnextloop_open:
           indexloop(ileg) = il
           ileg = ileg+1
;IMP:Luego vienen los cerrados.    
        endif
        
        



hoy_no:
     endif
  endfor
if keyword_set(awsom) then begin matriz_awsom={mat_ne:mat_ne,mat_te:mat_te,mat_r:mat_r,mat_lat:mat_lat,mat_lon:mat_lon,$
                                                alturas:alturas}
if keyword_set(demt)  then begin matriz_demt ={mat_ne:mat_ne,mat_te:mat_te,mat_br:mat_br,mat_vr:mat_vr,mat_vth:mat_vth,mat_vph:mat_vph,mat_r:mat_r,mat_lat:mat_lat,mat_lon:mat_lon,$
                alturas:alturas}



  
if keyword_set(awsom) then begin
  datos = {Nemean:Nemean, Tmmean:Tmmean, Nestddev:Nestddev, Tmstddev:Tmstddev, WTmean:WTmean, WTstddev:WTstddev,$
           Pmean:Pmean, Ne2mean:Ne2mean, Ermean:Ermean ,Bmean:Bmean ,$
           Ne0:Ne0 ,lambda_N:lambda_N, r2N:r2N ,Ne0_s:Ne0_s, lambda_N_s:lambda_N_s, r2N_s:r2N_s, P0:P0, lambda_P:lambda_P, r2P:r2P,$
           gradT:gradT, Tm0:Tm0, r2T:r2T, gradT_s:gradT_s, Tm0_s:Tm0_s, r2t_s:r2t_s,$
           ft:ft, ft_s:ft_s, fne:fne ,fne_s:fne_s, Tefit:Tefit, Tefit_ts:Tefit_ts, Te_base:Te_base, pearson_ns:pearson_ns, pearson_ts:pearson_ts,$
           betamean:betamean, betaapex:betaapex, Br0:Br0, B_base:B_base, Nebasal:Nebasal, opclstat:opclstat, indexloop:indexloop,$
           footrad:footrad, footlat:footlat, footlon:footlon, iso:iso, iso_s:iso_s, iso_erry:iso_erry, long_r:long_r, long_s:long_s, scoreR_v:scoreR_v, npts_v:npts_v,$
           pearson_n:pearson_n, lincorr_pearson_n:lincorr_pearson_n, lincorr_pvalue_n:lincorr_pvalue_n, hip_chi_pv_n:hip_chi_pv_n, hip_chi_pv2_n:hip_chi_pv2_n, r2n_erry:r2n_erry,$
           pearson_t:pearson_t, lincorr_pearson_t:lincorr_pearson_t, lincorr_pvalue_t:lincorr_pvalue_t, hip_chi_pv_t:hip_chi_pv_t, hip_chi_pv2_t:hip_chi_pv2_t, r2t_erry:r2t_erry,$
           lincorr_tstatistic_t:lincorr_tstatistic_t,$
           sigma_n:sigma_n, sigma_t:sigma_t, Ne0_erry:Ne0_erry, lambda_N_erry:lambda_N_erry, Tm0_erry:Tm0_erry, gradT_erry:gradT_erry,prob_t:prob_t,$
           Rp_base:Rp_base, Rp_medio:Rp_medio, Rp_alto:Rp_alto, er0_s:er0_s, lambda_er_s:lambda_er_s, r2er_s:r2er_s, pearson_ers:pearson_ers,$
           s_base:s_base, fc_l:fc_l,fcb:fcb,phi_r_total:phi_r_total,$
           phi_c_total:phi_c_total,Tmmean_alto:Tmmean_alto,phi_r:phi_r,$
           matriz_awsom=matriz_awsom}
  save, datos, FILENAME = 'trace_struct_'+ffile_out+'.sav'


endif

  if keyword_set(demt) then begin
  datos = {Nemean:Nemean, Tmmean:Tmmean, Nestddev:Nestddev, Tmstddev:Tmstddev, WTmean:WTmean, WTstddev:WTstddev,$
           Pmean:Pmean, Ne2mean:Ne2mean, Ermean:Ermean ,Bmean:Bmean ,$
           Ne0:Ne0 ,lambda_N:lambda_N, r2N:r2N ,Ne0_s:Ne0_s, lambda_N_s:lambda_N_s, r2N_s:r2N_s, P0:P0, lambda_P:lambda_P, r2P:r2P,$
           gradT:gradT, Tm0:Tm0, r2T:r2T, gradT_s:gradT_s, Tm0_s:Tm0_s, r2t_s:r2t_s,$
           ft:ft, ft_s:ft_s, fne:fne ,fne_s:fne_s, Tefit:Tefit, Tefit_ts:Tefit_ts, Te_base:Te_base, pearson_ns:pearson_ns, pearson_ts:pearson_ts,$
           betamean:betamean, betaapex:betaapex, Br0:Br0, B_base:B_base, Nebasal:Nebasal, opclstat:opclstat, indexloop:indexloop,$
           footrad:footrad, footlat:footlat, footlon:footlon, iso:iso, iso_s:iso_s, iso_erry:iso_erry, long_r:long_r, long_s:long_s, scoreR_v:scoreR_v, npts_v:npts_v,$
           pearson_n:pearson_n, lincorr_pearson_n:lincorr_pearson_n, lincorr_pvalue_n:lincorr_pvalue_n, hip_chi_pv_n:hip_chi_pv_n, hip_chi_pv2_n:hip_chi_pv2_n, r2n_erry:r2n_erry,$
           pearson_t:pearson_t, lincorr_pearson_t:lincorr_pearson_t, lincorr_pvalue_t:lincorr_pvalue_t, hip_chi_pv_t:hip_chi_pv_t, hip_chi_pv2_t:hip_chi_pv2_t, r2t_erry:r2t_erry,$
           lincorr_tstatistic_t:lincorr_tstatistic_t,$
           sigma_n:sigma_n, sigma_t:sigma_t, Ne0_erry:Ne0_erry, lambda_N_erry:lambda_N_erry, Tm0_erry:Tm0_erry, gradT_erry:gradT_erry,prob_t:prob_t,$
           Rp_base:Rp_base, Rp_medio:Rp_medio, Rp_alto:Rp_alto, er0_s:er0_s, lambda_er_s:lambda_er_s, r2er_s:r2er_s, pearson_ers:pearson_ers,$
           s_base:s_base, fc_l:fc_l,fcb:fcb,phi_r_total:phi_r_total,$
           phi_c_total:phi_c_total,Tmmean_alto:Tmmean_alto,phi_r:phi_r,$
           matriz_demt:matriz_demt}
  save, datos, FILENAME = 'trace_struct_'+ffile_out+'.sav'
  endif














;-555 sucede en los cerrados

;p =  where(ne_basal ne -555. and abs(foot_lat) ge 60 )
  suf = 'CR2219'
;  suf = 'cr2208'
    stop










    
;p = where(ne_demt_basal ne -555. and ne_demt_basal ne -999. and abs(foot_lat) ge 60)
p = where(opcls eq 0. and ne_demt_base_demt ne -999. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = foot_lat_0(p)
scatter_plot,vr_final(p),ne_demt_base_demt(p)/1.e8,data_aux=data_aux,table=33,xtitle='V!Dr!N[km/s] at 6 Rs',ytitle='(N!Dm!N!U2!N)!U1/2!N DEMT[10!U8!Ncm!U-3!N] (r=1.025R!DSUN!N)',$
             filename='_vr_nedemt_'+suf+'colbarlat1',title=suf+' - colorbar indicates Latitude at 1R!DSUN!N',min_color=0,max_color=255,/pearson,min_y=0,max_y=2.5


p = where(opcls eq 0. and ne_demt_base_demt ne -999. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = abs(foot_lat_0(p))
signo = br_final(p) / abs(br_final(p))
scatter_plot,vr_final(p)*signo,ne_demt_base_demt(p)/1.e8,data_aux=data_aux,table=33,xtitle='sg(B!Dr!N)*V!Dr!N[km/s] (r=6R!DSUN!N)',$
             ytitle='(N!Dm!N!U2!N)!U1/2!N DEMT[10!U8!Ncm!U-3!N] (r=1.025R!DSUN!N)',filename='_signvr_nedemt_'+suf+'colbarlat1',$
             title=suf+' - colorbar indicates |Latitude| at 1R!DSUN!N',min_color=0,max_color=255,min_y=0,max_y=1.6



p = where(opcls eq 0. and ne_demt_base_demt ne -999. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = abs(foot_lat_0(p))
signo = br_final(p) / abs(br_final(p))
scatter_plot,vr_final(p)*signo,te_demt_base_demt(p)/1.e6,data_aux=data_aux,table=33,xtitle='sg(B!Dr!N)*V!Dr!N[km/s] (r=6R!DSUN!N)',$
             ytitle='T!Dm!N DEMT[MK] (r=1.025R!DSUN!N)',filename='_signvr_tmdemt_'+suf+'colbarlat1',$
             title=suf+' - colorbar indicates |Latitude| at 1R!DSUN!N',min_color=0,max_color=255,min_y=0,max_y=2

;-----fs
p =  where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = abs(foot_lat_0(p))
signo = br_final(p) / abs(br_final(p))
scatter_plot,vr_final(p)*signo,fexpr(p),data_aux=data_aux,table=33,xtitle='sg(B!Dr!N)*V!Dr!N[km/s] (r=6R!DSUN!N)',ytitle='fs (r=6R!DSUN!N)',$
             filename='_signvr_fexp_'+suf+'colbarlat1',title=suf+' - colorbar indicates |Latitude| at 1 R!DSUN!N',min_color=0,max_color=255,max_y=20

;---- lat - lat
p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = vr_final(p)
scatter_plot,end_lat(p),foot_lat_0(p),data_aux=data_aux,table=33,ytitle='Latitude at 1 R!DSUN!N',xtitle='Latitude at 6 R!DSUN!N',$
             filename='_lat_lat_'+suf+'colbarvrfinal',title=suf+' - colorbar indicates V!Dr!N[km/s] at 6R!DSUN!N',min_color=0,max_color=255


;---- lon - lon
p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = vr_final(p)
scatter_plot,foot_lon_0(p),ne_demt_base_demt(p),data_aux=data_aux,table=33,ytitle='Latitude at 1 R!DSUN!N',xtitle='Latitude at 6 R!DSUN!N',$
             filename='_lon_lon_'+suf+'colbarvrfinal',title=suf+' - colorbar indicates V!Dr!N[km/s] at 6R!DSUN!N',min_color=0,max_color=255




p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = foot_lat_0(p)
scatter_plot,foot_lat_0(p),vr_final(p),data_aux=data_aux,table=33,ytitle='V!Dr!N[km/s] (r=6R!DSUN!N)',xtitle='Latitude at 1 R!DSUN!N',$
             filename='_lat_vr_'+suf+'colbarlat0',title=suf+' - colorbar indicates Latitude at 1R!DSUN!N',min_color=0,max_color=255

;mapas te y Ne

p = where(opcls eq 0. and ne_demt_base_demt ne -999. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = vr_final(p)
scatter_plot,foot_lat_0(p),te_demt_base_demt(p)/1.e6,data_aux=data_aux,table=33,ytitle='T!Dm!N DEMT[MK] (r=1.025R!DSUN!N)',xtitle='Latitude at 1 R!DSUN!N',$
             filename='_lat_tm_'+suf+'colbarvr',title=suf+' - colorbar indicates V!Dr!N[km/s] at 6R!DSUN!N',min_color=0,max_color=255

p = where(opcls eq 0. and ne_demt_base_demt ne -999. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = vr_final(p)
scatter_plot,foot_lat_0(p),ne_demt_base_demt(p)/1.e8,data_aux=data_aux,table=33,ytitle='(N!Dm!N!U2!N)!U1/2!N DEMT[10!U8!Ncm!U-3!N] (r=1.025R!DSUN!N)',$
             xtitle='Latitude at 1 R!DSUN!N',filename='_lat_ne_'+suf+'colbarvr',title=suf+' - colorbar indicates V!Dr!N[km/s] at 6R!DSUN!N',min_color=0,max_color=255.

;mapas lat lon

p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = vr_final(p)
scatter_plot,foot_lon_0(p),foot_lat_0(p),data_aux=data_aux,table=33,xtitle='Longitude (1R!DSUN!N)',$
             ytitle='Latitude (1R!DSUN!N)',filename='_lat_long_'+suf+'colbarvr',title=suf+' - colorbar indicates V!Dr!N[km/s] at 6R!DSUN!N',min_color=0,max_color=255.


p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = fexpr(p) <20
scatter_plot,foot_lon_0(p),foot_lat_0(p),data_aux=data_aux,table=33,xtitle='Longitude (1R!DSUN!N)',$
             ytitle='Latitude (1R!DSUN!N)',filename='_lat_long_'+suf+'fexprr',title=suf+' - colorbar indicates fs at 6R!DSUN!N',min_color=0,max_color=255.


p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = fexpr_f(p)
scatter_plot,foot_lon_0(p),foot_lat_0(p),data_aux=data_aux,table=33,xtitle='Longitude (1R!DSUN!N)',$
             ytitle='Latitude (1R!DSUN!N)',filename='_lat_long_'+suf+'_test',title=suf+' - colorbar indicates fs at 6R!DSUN!N',min_color=0,max_color=255.

;-----chequeo fs

p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = matriz(0,p)/matriz(14,p)
scatter_plot,end_lon(p),end_lat(p),data_aux=data_aux,table=33,xtitle='Longitude (6R!DSUN!N)',$
             ytitle='Latitude (6R!DSUN!N)',filename='_lat_long_'+suf+'_test',title=suf+' - colorbar indicates fs',$
             min_color=0,max_color=255.,win=3


p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) )
data_aux = matriz(2,p)/matriz(6,p)<8
scatter_plot,foot_lon_0(p),foot_lat_0(p),data_aux=data_aux,table=33,xtitle='Longitude (1R!DSUN!N)',$
             ytitle='Latitude (1R!DSUN!N)',filename='_lat_long_'+suf+'_test',title=suf+' - colorbar indicates fs ',$
             min_color=0,max_color=255.,win=5


p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) and (foot_lon_0 le 100 or foot_lon_0 ge 200))
data_aux = abs(foot_lat_0(p))
signo = br_final(p) / abs(br_final(p))
scatter_plot,vr_final(p)*signo,matriz(2,p)/matriz(14,p),data_aux=data_aux,table=33,xtitle='vr*signo (6R!DSUN!N)',$
             ytitle='fs (R!DSUN!N)',filename='_lat_long_'+suf+'_test2',title=suf+' - colorbar indicates Lat at 1R!DSUN!N',$
             min_color=0,max_color=255.,max_y=15,min_y=0

plot,matriz(2,p)/matriz(14,p)<10,matriz(2,p)/matriz(6,p) <10,psym=4

p = where(opcls eq 0. and matriz(2,*)/matriz(14,*) le matriz(2,*)/matriz(6,*))
data_aux = vr_final(p)
scatter_plot,foot_lon_0(p),foot_lat_0(p),data_aux=data_aux,table=33,xtitle='Longitude (1R!DSUN!N)',$
             ytitle='Latitude (1R!DSUN!N)',filename='_lat_long_'+suf+'_test2',title=suf+' - colorbar indicates fs at 6R!DSUN!N',$
             min_color=0,max_color=255.,win=3




p = where(opcls eq 0. and (foot_lat_0 ge 50. or foot_lat_0 le -50.) and (foot_lon_0 le 100 or foot_lon_0 ge 200))
data_aux = vr_final(p)
scatter_plot,foot_lon_0(p),foot_lat_0(p),data_aux=data_aux,table=33,xtitle='Longitude (1R!DSUN!N)',$
             ytitle='Latitude (1R!DSUN!N)',filename='_lat_long_'+suf+'_test2',title=suf+' - colorbar indicates fs at 6R!DSUN!N',$
             min_color=0,max_color=255.,win=3








;-----
p = where(ne_basal ne -555.)
data_aux = foot_lat(p)
scatter_plot,vr_final(p),ne_base_mhd(p)/1.e8,data_aux=data_aux,table=6,xtitle='Vr[km/s] at 6 Rs',ytitle='Ne AWSoM[10!U8!Ncm!U-3!N] at 1.065 Rs',$
             filename='_vr_ne_'+suf,title=suf,min_color=50,max_color=200,/pearson,min_y=0,max_y=1.6,/inverse







;----------


return
end
