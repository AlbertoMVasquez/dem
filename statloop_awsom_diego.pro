;statloop_awsom_diego,file='traceLDEM_CR2082_test_DEMT_full_asd_diarrea_putrefacta2_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',alturas=6,/demt
;statloop_awsom_diego,file='traceLDEM_CR2082_test_ldem_6alturas_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav',alturas=6,/demt
pro statloop_awsom_diego,rmin=rmin,rmax=rmax,alturas=alturas,ajuste_alto=ajuste_alto,ajuste_bajo=ajuste_bajo,demt=demt,file=file
;    common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
;  common B_sampled,B_v,Br_v,Bth_v,Bph_v
;  common opclstatus,opcls,loopL,WTc


;si se usa un antiguo read_trace, es necesario usar commons, si se usa
;un restore, no es necesario.  
  !except=2
  device, retain     = 2
  device, true_color = 24
  device, decomposed = 0
  
; Physical constants needed for the HS fits:             
  rsun = 6.955e10               ; cm                                             
  gsun = 2.74e4      ; cm/sec²                                
    kB = 1.38e-16    ; erg/K                                           
    mH = 1.6726e-24  ; gr                                    
     a = 0.08        ; N_He / N_H                       
    mu = (1.+4.*a)/(1.+2.*a)
    bb = (1.+2.*a)/(2.+3.*a)
    kappa = 9.2e-7              ; erg s ^-1 cm ^-1 K ^-7/2                

    if not keyword_set(rmin)        then rmin = 1.025
    if not keyword_set(rmax)        then rmax = 1.20
    if not keyword_set(rloopmin)    then rloopmin = 1.07
    if not keyword_set(corte_awsom) then corte_awsom = 1.055
    rminloop=rloopmin
;Para que estan estos valores de rloopmin???

;    if not keyword_set (alturas) then read_trace_diego,file,0
;    if     keyword_set (alturas) then read_trace_diego,file,alturas
    restore,file
;cambiar el read_trace por un restore!

Nloop = n_elements(loopL)

index0 = where(opcls eq 0.)
index1 = where(opcls eq 1.)
index2 = where(opcls eq 2.)

Nloop0 = n_elements(index0) & if index0(0) eq -1 then Nloop0=0
Nloop1 = n_elements(index1) & if index1(0) eq -1 then Nloop1=0
Nloop2 = n_elements(index2) & if index2(0) eq -1 then Nloop2=0

if Nloop0 + Nloop1 + Nloop2 ne Nloop then stop

Nlegs = Nloop0 + 2*Nloop1 + 2*Nloop2

;se definen variables estadisticas!
; Mean and standard desviation values for each leg
Nemean = fltarr(Nlegs)-555.
Tmmean = fltarr(Nlegs)-555.
Nestddev= fltarr(Nlegs)-555.
Tmstddev= fltarr(Nlegs)-555.
WTmean = fltarr(Nlegs)-555.
WTstddev= fltarr(Nlegs)-555.
 Pmean = fltarr(Nlegs)-555.
Ne2mean = fltarr(Nlegs)-555.
Ermean  = fltarr(Nlegs)-555.
Bmean = fltarr(Nlegs)-555.
betamean = fltarr(Nlegs)-555.
;fiteos densidad y temperatura en funcion de r y s
Ne0 = fltarr(Nlegs)-555.
lambda_N = fltarr(Nlegs)-555.
     r2N = fltarr(Nlegs)-555.
Ne0_s = fltarr(Nlegs)-555.
lambda_N_s = fltarr(Nlegs)-555.
r2N_s = fltarr(Nlegs)-555.

     P0 = fltarr(Nlegs)-555.
lambda_P = fltarr(Nlegs)-555.
     r2P = fltarr(Nlegs)-555.

     gradT = fltarr(Nlegs)-555.
     Tm0 = fltarr(Nlegs)-555.
     r2T = fltarr(Nlegs)-555.
     gradT_s = fltarr(Nlegs)-555.
     Tm0_s = fltarr(Nlegs)-555.
     r2t_s = fltarr(Nlegs)-555.
;r2 o chi-square coefficient
     ft  = fltarr(Nlegs)-555.
     ft_s  = fltarr(Nlegs)-555.
     fne = fltarr(Nlegs)-555.
     fne_s = fltarr(Nlegs)-555.

     Tefit = fltarr(Nlegs)-555.
   Tefit_ts = fltarr(Nlegs)-555.
   Te_base = fltarr(Nlegs)-555
;pearson correlation coefficient
   pearson_n  = fltarr(Nlegs)-555. 
   pearson_ns = fltarr(Nlegs)-555.
   pearson_t  = fltarr(Nlegs)-555.
   pearson_ts = fltarr(Nlegs)-555.

;pedidos especiales por ceci
   dTmds = fltarr(Nlegs)-555.
      Eh = fltarr(Nlegs)-555.
      Smaxxx = fltarr(Nlegs)-555. 
      Sminnn = fltarr(Nlegs)-555.
      Phir =fltarr(Nlegs)-555.; flujo radiativo 
      Fcb=fltarr(Nlegs)-555.  ;flujo conductivo enla base
      Phirfit = fltarr(Nlegs)-555. ; flujo radiativo fiteado
      eplegT = fltarr(Nlegs)-555.
      s_r0_a = fltarr(Nlegs)-555.
;----------------------------
      
 deltaEh = fltarr(Nlegs)-555. ;??
betamean = fltarr(Nlegs)-555.
betaapex = fltarr(Nlegs)-555.
   Br0 = fltarr(Nlegs)-555.
   
  B_base = fltarr(Nlegs)-555.
 Nebasal = fltarr(Nlegs)-555.
betabase = fltarr(Nlegs)-555.
; opclstat=0. if loop is open, opclstat=1. if closed large; opclstat=2 if closed small.
opclstat  = fltarr(Nlegs)-555.
indexloop = fltarr(Nlegs)-555.
; The following arrays will contain the LOOP length (in Rsun) to which each LEG belongs:
loop_length=fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of     
; the starting point used for the LOOP to which the LEG belongs: 
strad = fltarr(Nlegs)-555.
stlat = fltarr(Nlegs)-555.
stlon = fltarr(Nlegs)-555.

;valores iniciales y finales de rad, lat y lon de cada loop
rad_ini = fltarr(Nlegs)-555.
lat_ini = fltarr(Nlegs)-555.
lon_ini = fltarr(Nlegs)-555.
rad_fin = fltarr(Nlegs)-555.
lat_fin = fltarr(Nlegs)-555.
lon_fin = fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of      
; the FOOT-POINT of each LEG: 
footrad = fltarr(Nlegs)-555.
footlat = fltarr(Nlegs)-555.
footlon = fltarr(Nlegs)-555.
;Isotermalidad
iso = fltarr(Nlegs)-555.
iso_s = fltarr(Nlegs)-555.
long_r = fltarr(Nlegs)-555.
long_s = fltarr(Nlegs)-555.

error_ne = fltarr(Nlegs)-555.
error_t  = fltarr(Nlegs)-555.
;ver si esto aun sirve
Rp0_rad = fltarr(Nlegs)-555.
Rp0_lat = fltarr(Nlegs)-555.
Rp0_lon = fltarr(Nlegs)-555.

Rp_rad = fltarr(Nlegs)-555.
Rp_lat = fltarr(Nlegs)-555.
Rp_lon = fltarr(Nlegs)-555.

Rp_base  = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.}
Rp_medio = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.}
Rp_alto  = {lat:fltarr(Nlegs)-555.,lon:fltarr(Nlegs)-555.}

; This array will code the "STATUS of LEG" so that:
; leg_status=1 if leg contains the starting point used for the loop,     
; leg_status=2 if not.  
  leg_status = fltarr(Nlegs) + 1.
 ileg = 0L
; Define minimum number of data points required by leg
  Ndata=5

;Error segun Lloveras et. al 2017
  error_euvi=1;seteo estos errores por ahora
  if keyword_set(error_euvi) then begin
     st_lb_ne= 3.8* 1.e6
     st_li_ne= 2.9* 1.e6
     st_f_ne = 2.7* 1.e6
     ch_lb_ne= 3.0* 1.e6
     ch_la_ne= 2.2* 1.e6
     
     st_lb_t = 6.4* 1.e4
     st_li_t = 5.1* 1.e4
     st_f_t  = 5.4* 1.e4
     ch_lb_t = 6.4* 1.e4
     ch_la_t = 4.8* 1.e4
  endif
  if keyword_set(error_eit) then begin
     st_lb_ne= 3.9* 1.e6
     st_li_ne= 4.0* 1.e6
     st_f_ne = 3.6* 1.e6
     ch_lb_ne= 2.8* 1.e6
     ch_la_ne= 2.7* 1.e6
     
     st_lb_t = 17.3* 1.e4
     st_li_t = 14.3* 1.e4
     st_f_t  = 18.9* 1.e4
     ch_lb_t = 13.2* 1.e4
     ch_la_t = 5.3* 1.e4
  endif
;seteo variables de fiteo
  
cr2081 = 1 ;seteo las latitudes del paper con 2081
  if keyword_set(cr2081) then begin
;coronal holes latitudinal sets
     chlan_i =  73.
     chlan_f =  90.
     chlas_f = -71.
     chlas_i = -90.
     chlbn_i =   0.
     chlbn_f =  73.
     chlbs_i = -71.
     chlbs_f =   0.
;streamer
     stlb_i  = -27.
     stlb_f  =  30.
     stlin_i =  30.
;     stlin_f =
;     stlis_i =
     stlis_f = -27.
     stfn_i  =  48.
     stfn_f  =  80.
     stfs_i  = -80.
     stfs_f  = -45.
  endif
  

  
  for il=0L,Nloop-1 do begin

; Analysis for OPEN loops:                     
     if opcls(il) eq 0. then begin

        Ne_l = reform ( Ne_v(0:Npts_v(il)-1,il))
        Tm_l = reform ( Tm_v(0:Npts_v(il)-1,il))
        rad_l = reform (rad_v(0:Npts_v(il)-1,il))
        lat_l = reform (lat_v(0:Npts_v(il)-1,il))
        lon_l = reform (lon_v(0:Npts_v(il)-1,il))
        s_l = reform (  s_v(0:Npts_v(il)-1,il))
        B_l = reform (  B_v(0:Npts_v(il)-1,il))
        Br_l = reform ( Br_v(0:Npts_v(il)-1,il))

        if keyword_set(demt) then begin
           WT_l = reform ( WT_v(0:Npts_v(il)-1,il))
;           Er_l = reform ( Er_v(0:Npts_v(il)-1,il))
           scoreR_l = reform ( scoreR_v(0:Npts_v(il)-1,il))
        endif
        
        rad_ini(ileg) = rad_l(0)
        lat_ini(ileg) = lat_l(0)
        lon_ini(ileg) = lon_l(0)
        Br0(ileg) =  Br_l(0)
       
        rad_fin(ileg) = enrad_v(il) ;no seria il-1??
        lat_fin(ileg) = enlat_v(il)
        lon_fin(ileg) = enlon_v(il)
        
        footrad(ileg) = rad_ini(ileg)
        footlat(ileg) = lat_ini(ileg)
        footlon(ileg) = lon_ini(ileg)

        rrr0=findel(1.025,rad_l) ;se usa para evaluar Nebasal
        B_base (ileg) = B_l (rrr0)
;select usefull data                                                     
        p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. and scoreR_l lt 0.10)
;podria relajarse a 0.25??
;hacer estadistica de scoreR_l  !!!
        if p(0) eq -1 then goto,skipnextloop_open

        Ne_l =  Ne_l (p)
        Tm_l =  Tm_l (p)
        if keyword_set (demt) then begin
           WT_l =  WT_l (p)
;           Er_l =  Er_l (p)
        endif
        rad_l = rad_l (p)
        lat_l = lat_l (p)
        lon_l = lon_l (p)
        s_l =   s_l (p)
        B_l =   B_l (p)
      
        p_l   = kB/bb *Ne_l*Tm_l
        beta_l = p_l/(B_l^2/(8*!pi));valor crudo
        
        Nemean(ileg) =   mean(Ne_l)
        betamean(ileg) =   mean(beta_l)
        Bmean(ileg) =   mean(B_l)
        Pmean(ileg) =   mean(p_l)                                  
        Ne2mean(ileg) =   mean((Ne_l)^2)                                   
        ;Ermean(ileg) =   mean(Er_l)
        case 1 of
           keyword_set(ajuste_alto) eq 1: Tmmean(ileg) =  mean(Tm_l(where(rad_l ge corte_awsom)))
           keyword_set(ajuste_bajo) eq 1: Tmmean(ileg) =  mean(Tm_l(where(rad_l le corte_awsom)))
           else: Tmmean(ileg) =   mean(Tm_l)
        endcase

;se guardan valores pero no hay fiteo si hay menos de 5 puntos.
        if n_elements(p) lt Ndata then goto,skipfitloop_open

; La idea es considerar tercios sobre 1.05  

     if not keyword_set(ajuste_alto) || not keyword_set(ajuste_awsom_bajo) then begin
        rr1 = 1.07              ;rr1 = 1.06
        rr2 = 1.12              ;1.16
        lefts  = (where(rad_l le rr1))
        rights = (where(rad_l ge rr2))
        diomes = (where(rad_l gt rr1 and rad_l lt rr2))
     endif

;     esto en realidad no es necesario, ya que awsom no tiene zda. pero eliminara
;     loops abiertos que sean chicos (cosas raras)
     if  keyword_set(ajuste_alto) then begin 
       rr1 = 1.10
       rr2 = 1.13
       lefts  = (where(rad_l ge corte_awsom and rad_l le rr1))
       rights = (where(rad_l ge rr2))
       diomes = (where(rad_l gt rr1 and rad_l lt rr2))
       if lefts(0) eq -1 || diomes(0) eq -1 || rights(0) eq -1 then stop;quiero ver si esto pasa.
    endif
     
     if lefts(0) eq -1 || diomes(0) eq -1 || rights(0) eq -1 then goto,skipfitloop_open

;se setean los errores segun la region de donde sea el loop abierto
        if (footlat(il) le chlbn_f && footlat(il) ge chlbn_i) || (footlat(il) ge chlbs_i && footlat(il) le chlbs_f) then begin
           error_ne(ileg) = ch_lb_ne
           error_t(ileg)  = ch_lb_t
        endif

        if (footlat(il) gt chlan_i) || (footlat(il) lt chlas_f) then begin
           error_ne(ileg) = ch_la_ne
           error_t(ileg)  = ch_la_t
        endif


   ;Make HS-fit to Ne(r) for each open leg/loop                                                
   ;Como no le creemos a awsom por debajo                                        
   ;de 1.05 entonces partimos el ajuste en 2                           

    case 1 of
       keyword_set(ajuste_alto) eq 1: begin
          sfit =  s_l  (where(rad_l ge findel(val_corte,rad_l)))
          xfit =  rad_l(where(rad_l ge val_corte))
          yfit =  Ne_l (where(rad_l ge val_corte))
          zfit =  p_l  (where(rad_l ge val_corte))
          wfit =  Tm_l (where(rad_l ge val_corte))
;          min_s = s_l(findel(val_corte,rad_l))
;          max_s = s_l(findel(1.2 ,rad_l))
;Usar definiciones como las de arriba pueden ocasionar errores cuando
;rad_l rad_l1/2 no son monotonamente crecientes/decrecientes.
          min_s = min(s_l)
          max_s = max(s_l)
          min_r = rad_l(findel(val_corte,rad_l))
          max_r = rad_l(findel(1.2 ,rad_l))
       end
       keyword_set(ajuste_bajo) eq 1: begin
          sfit =  s_l  (where(rad_l le findel(val_corte,rad_l)))
          xfit =  rad_l(where(rad_l le val_corte))
          yfit =  Ne_l (where(rad_l le val_corte))
          zfit =  P_l  (where(rad_l le val_corte))
          wfit =  Tm_l (where(rad_l le val_corte))
          min_s = min(s_l)
          max_s = max(s_l)
          min_r = rad_l(findel(1.025,rad_l))
          max_r = rad_l(findel(val_corte ,rad_l))

       end
       else: begin
          sfit = s_l
          xfit = rad_l
          yfit = Ne_l
          zfit = p_l
          wfit = Tm_l
          min_s = min(s_l)
          max_s = max(s_l)
          min_r = rad_l(findel(1.025,rad_l))
          max_r = rad_l(findel(1.2 ,rad_l))

       end
    endcase

;Fiteando Ne
    linear_fit,1/xfit,alog(yfit),min_r,max_r,A,r2,salidafit,/theilsen,/xinverted
    Ne0(ileg) = exp(A[0]+A[1])
    lambda_N(ileg) = 1./A[1]
    r2N(ileg) = r2
    pearson_n(ileg) = correlate(xfit,alog(yfit))
    
    Tefit_ts(ileg) = bb* mu * mH * gsun * (lambda_N(ileg)*rsun) / kB
    Nebasal(ileg) = Ne0(ileg) * exp(-1/lambda_n(ileg)* (1. - 1./1.025))
    ;Estas 2 NO son necesarias, podria armarse luego.
    franja_lineal,yfit,salidafit,min_r,max_r,error_ne(ileg),fraccion
    fne (ileg) = fraccion

    linear_fit,1/sfit,alog(yfit),min_s,max_s,A,r2,salidafit,/theilsen,/xinverted
    Ne0_s(ileg) = exp(A[0]+A[1])
    lambda_N_s(ileg) = 1./A[1]
    r2N_s(ileg) = r2
    pearson_ns(ileg) = correlate(sfit,alog(yfit))

    franja_lineal,yfit,salidafit,min_s,max_s,error_ne(ileg),fraccion
    fne_s (ileg) = fraccion

    
;Fiteando temperatura
    linear_fit,xfit,wfit,min_r,max_r,A,r2,salidafit,/theilsen
    Tm0(ileg)   = A[0]
    gradT(ileg) = A[1]
    r2t (ileg)  = r2;si es isotermico va a dar bajo.  
    franja_lineal,wfit,salidafit,min_r,max_r,error_t(ileg),fraccion
    ft (ileg) = fraccion
    pearson_t(ileg) = correlate(xfit,wfit)
    
    linear_fit,sfit,wfit,min_s,max_s,A,r2,salidafit,/theilsen
    Tm0_s(ileg)   = A[0]
    gradT_s(ileg) = A[1]
    r2t_s (ileg)  = r2
    franja_lineal,wfit,salidafit,min_s,max_s,error_t(ileg),fraccion
    ft_s (ileg) = fraccion
    pearson_ts(ileg) = correlate(sfit,wfit)
    
    Te_base(ileg) = gradT(ileg) * 1.025 + Tm0(ileg)
    betabase(ileg) = (kb/bb * nebasal(ileg) * te_base(ileg)) /(B_base(ileg)^2/(8*!pi))
    ;Estas 2 NO son necesarias, podria armarse luego. Hacer lo mismo calcu
    ;lando sobre el loop s!
    ;betabase puede ser negativo para los malos fiteos de temperatura

;    if not keyword_set(ajuste_bajo) && betabase(ileg) lt 0. && ft_s(ileg) ge 0.7 && r2N(ileg) ge 0.8 then stop
    
    long_r(ileg)  = max_r - min_r
    long_s(ileg)  = max_s - min_s
    iso  (ileg)   = abs(gradT (ileg)    * long_r(ileg)) / (2 * error_t(ileg))
    iso_s(ileg)   = abs(gradT_s(ileg)   * long_s(ileg)) / (2 * error_t(ileg))
    

    skipfitloop_open:
    opclstat(ileg) = opcls(il)
    loop_length(ileg) = loopL(il)
    strad(ileg) = str_v(il)
    stlat(ileg) = (90-stth_v(il)/!dtor)
    stlon(ileg) = stph_v(il)/!dtor
    skipnextloop_open:
    indexloop(ileg) = il
    ileg = ileg+1
   
 endif else begin
    

       if max(rad_v(0:Npts_v(il)-1,il)) lt rminloop then goto,skipnextloop

       if opcls(il) eq 2 then begin
          ifirs_1 = 0
          ilast_1 = midcell_v(il)
          ifirs_2 = midcell_v(il)
          ilast_2 = Npts_v(il)-1
       endif
       if opcls(il) eq 1 then begin
          ifirs_1 = 0
          ilast_1 = midcell_v(il)-1
          while Ne_v(ilast_1,il) eq -666. do ilast_1=ilast_1-1
          ifirs_2 = midcell_v(il)
          ilast_2 = Npts_v(il)-1
       endif
       
       Ne_l1 = reform ( Ne_v(ifirs_1:ilast_1,il))
       Ne_l2 = reform ( Ne_v(ifirs_2:ilast_2,il))
       Tm_l1 = reform ( Tm_v(ifirs_1:ilast_1,il))
       Tm_l2 = reform ( Tm_v(ifirs_2:ilast_2,il))
       if keyword_set(demt) then begin
          WT_l1 = reform ( WT_v(ifirs_1:ilast_1,il))
          WT_l2 = reform ( WT_v(ifirs_2:ilast_2,il))
        ;  Er_l1 = reform ( Er_v(ifirs_1:ilast_1,il))
         ; Er_l2 = reform ( Er_v(ifirs_2:ilast_2,il))
          scoreR_l1 = reform ( scoreR_v(ifirs_1:ilast_1,il))
          scoreR_l2 = reform ( scoreR_v(ifirs_2:ilast_2,il))
       endif
       rad_l1 = reform( rad_v(ifirs_1:ilast_1,il))
       rad_l2 = reform( rad_v(ifirs_2:ilast_2,il))
       lat_l1 = reform( lat_v(ifirs_1:ilast_1,il))
       lat_l2 = reform( lat_v(ifirs_2:ilast_2,il))
       lon_l1 = reform( lon_v(ifirs_1:ilast_1,il))
       lon_l2 = reform( lon_v(ifirs_2:ilast_2,il))
       s_l1 = reform(   s_v(ifirs_1:ilast_1,il))
       s_l2 = loopL(il) - reform(   s_v(ifirs_2:ilast_2,il))
       B_l1 = reform(   B_v(ifirs_1:ilast_1,il))
       B_l2 = reform(   B_v(ifirs_2:ilast_2,il))
;quiero ver si rad_l2 y s_l2 hay que hacerles un reverse()  
;SI, ESTAN INVERTIDOS
       switching = 'no'
       leg_status(ileg+1) = 2.
       if switching eq 'no' then begin
          rad_ini(ileg)   = rad_l1(0)
          rad_ini(ileg+1) = rad_l1(0)
          rad_fin(ileg)   = rad_l2(n_elements(rad_l2)-1)
          rad_fin(ileg+1) = rad_l2(n_elements(rad_l2)-1)
          lat_ini(ileg)   = lat_l1(0)
          lat_ini(ileg+1) = lat_l1(0)
          lat_fin(ileg)   = lat_l2(n_elements(lat_l2)-1)
          lat_fin(ileg+1) = lat_l2(n_elements(lat_l2)-1)
          lon_ini(ileg)   = lon_l1(0)
          lon_ini(ileg+1) = lon_l1(0)
          lon_fin(ileg)   = lon_l2(n_elements(lon_l2)-1)
          lon_fin(ileg+1) = lon_l2(n_elements(lon_l2)-1)
       endif
       if switching eq 'yes' then begin
          rad_ini(ileg)   = rad_l1(n_elements(rad_l1)-1)
          rad_ini(ileg+1) = rad_l1(n_elements(rad_l1)-1)
          rad_fin(ileg)   = rad_l2(0)
          rad_fin(ileg+1) = rad_l2(0)
          lat_ini(ileg)   = lat_l1(n_elements(lat_l1)-1)
          lat_ini(ileg+1) = lat_l1(n_elements(lat_l1)-1)
          lat_fin(ileg)   = lat_l2(0)
          lat_fin(ileg+1) = lat_l2(0)
          lon_ini(ileg)   = lon_l1(n_elements(lon_l1)-1)
          lon_ini(ileg+1) = lon_l1(n_elements(lon_l1)-1)
          lon_fin(ileg)   = lon_l2(0)
          lon_fin(ileg+1) = lon_l2(0)
       endif
       

       Footrad(ileg)   = rad_ini(ileg)
       Footrad(ileg+1) = rad_fin(ileg+1)
       Footlat(ileg)   = lat_ini(ileg)
       Footlat(ileg+1) = lat_fin(ileg+1)
       Footlon(ileg)   = lon_ini(ileg)
       Footlon(ileg+1) = lon_fin(ileg+1)
       

       rrr01=findel(1.025,rad_l1)
       Rp_base.lat(ileg)  = lat_l1(rrr01)
       Rp_base.lon(ileg)  = lon_l1(rrr01)
       
       rrr1=findel(1.075,rad_l1)
       Rp_medio.lat(ileg) = lat_l1(rrr1)
       Rp_medio.lon(ileg) = lon_l1(rrr1)
       
       rrr2=findel(1.105,rad_l1)
       Rp_alto.lat(ileg)  = lat_l1(rrr2)
       Rp_alto.lon(ileg)  = lon_l1(rrr2)
       
       rrr02=findel(1.025,rad_l2)
       Rp_base.lat(ileg+1)  = lat_l2(rrr02)
       Rp_base.lon(ileg+1)  = lon_l2(rrr02)
       
       rrr1=findel(1.075,rad_l2)
       Rp_medio.lat(ileg+1) = lat_l2(rrr1)
       Rp_medio.lon(ileg+1) = lon_l2(rrr1)
       
       rrr2=findel(1.105,rad_l2)
       Rp_alto.lat(ileg+1)  = lat_l2(rrr2)
       Rp_alto.lon(ileg+1)  = lon_l2(rrr2)
       
       rad_l1_orig = rad_l1;donde se usa?
       rad_l2_orig = rad_l2
       B_base (ileg  ) = B_l1 (rrr01)
       B_base (ileg+1) = B_l2 (rrr02)

;stop;falta invertir...
  p1 = where ( rad_l1 ge rmin and rad_l1 le rmax and Ne_l1 ne -999. and scoreR_l1 lt 0.1 and WT_l1 ge WTc*1.e6)
  p2 = where ( rad_l2 ge rmin and rad_l2 le rmax and Ne_l2 ne -999. and scoreR_l2 lt 0.1 and WT_l2 ge WTc*1.e6)

  if  p1(0) eq -1 || p2(0) eq -1 then goto,skipnextloop

  Ne_l1 =  Ne_l1 (p1)
  Ne_l2 =  Ne_l2 (p2)
  Tm_l1 =  Tm_l1 (p1)
  Tm_l2 =  Tm_l2 (p2)
  if keyword_set(demt) then begin
     WT_l1 =  WT_l1 (p1)
     WT_l2 =  WT_l2 (p2)
   ;  Er_l1 =  Er_l1 (p1)
   ;  Er_l2 =  Er_l2 (p2)
  endif
  rad_l1 = rad_l1 (p1)
  rad_l2 = rad_l2 (p2)
  s_l1 =   s_l1 (p1)
  s_l2 =   s_l2 (p2)
  B_l1 =   B_l1 (p1)
  B_l2 =   B_l2 (p2)
; tomographic pressure     
  p_l1 = kB/bb *Ne_l1*Tm_l1
  p_l2 = kB/bb *Ne_l2*Tm_l2
;make Beta plasma parameter, valor crudo            
  beta_l1 = p_l1/(B_l1^2/(8*!pi))
  beta_l2 = p_l2/(B_l2^2/(8*!pi))
  
  Nemean(ileg)   = mean(Ne_l1)
  Nemean(ileg+1) = mean(Ne_l2)
  Ne2mean(ileg)   = mean((Ne_l1)^2)
  Ne2mean(ileg+1) = mean((Ne_l2)^2)

  if keyword_set(demt) then begin
     WTmean(ileg)   = mean(WT_l1)
     WTmean(ileg+1) = mean(WT_l2)
     WTstddev(ileg)   = stddev(WT_l1)
     WTstddev(ileg+1) = stddev(WT_l2)
   ;  Ermean(ileg)   = mean(Er_l1)
   ;  Ermean(ileg+1) = mean(Er_l2)
  endif
  Nestddev(ileg)   = stddev(Ne_l1)
  Nestddev(ileg+1) = stddev(Ne_l2)
  Tmstddev(ileg)   = stddev(Tm_l1)
  Tmstddev(ileg+1) = stddev(Tm_l2)  
  betamean(ileg)   = mean(beta_l1)
  betamean(ileg+1) = mean(beta_l2)
  betaapex(ileg)   = beta_l1(n_elements(rad_l1(p1))-1)
  betaapex(ileg+1) = beta_l2(0)
  Bmean(ileg)   = mean(B_l1)
  Bmean(ileg+1) = mean(B_l2)
  Pmean(ileg)   = mean(p_l1)
  Pmean(ileg+1) = mean(p_l2)
  case 1 of
     keyword_set(ajuste_alto) eq 1: begin
        Tmmean(ileg)   =  mean(Tm_l1(where(rad_l1 ge corte_awsom)))
        Tmmean(ileg+1) =  mean(Tm_l2(where(rad_l2 ge corte_awsom)))
     end
     keyword_set(ajuste_bajo) eq 1: begin
        Tmmean(ileg)   =  mean(Tm_l1(where(rad_l1 le corte_awsom)))
        Tmmean(ileg+1) =  mean(Tm_l2(where(rad_l2 le corte_awsom)))
     end
     else: begin
        Tmmean(ileg)   =   mean(Tm_l1)
        Tmmean(ileg+1) =   mean(Tm_l2)
     end
  endcase
  
  if n_elements(p1) lt Ndata || n_elements(p2) lt Ndata then goto,skipfitloop
  if keyword_set(demt) then begin ;no quiero que entre aca si lo trazados son datos de awsom
     rrr_min = 1.025              ;antes decia 1.035 pero ahora tenemos datos ahi abajo.
     if max(rad_l1) ge 1.2 then begin  ;igual que las piernas baiertas     
        rr1_l1 = 1.07             ;(max(rad_l1)-1.)*0.25 +1.0                                      
        rr2_l1 = 1.12             ;(max(rad_l1)-1.)*0.5 +1.0            
        if     keyword_set(ajuste_bajo) then lefts_l1  = where(rad_l1 le rr1_l1 and rad_l1 ge 1.05)
        if not keyword_set(ajuste_bajo) then lefts_l1  = where(rad_l1 le rr1_l1)
        rights_l1 = where(rad_l1 ge rr2_l1)
        diomes_l1 = where(rad_l1 gt rr1_l1 and rad_l1 lt rr2_l1)
     endif
     if max(rad_l1) lt 1.2 then begin
        Drr = max(rad_l1) - rrr_min ;se asume un pto en 1.035                                   
        rr1_l1  = rrr_min + Drr * 1./3.
        rr2_l1  = rrr_min + Drr * 2./3.
    ;rr1_l1 = 1.07                                                                                    
    ;rr2_l1 = (max(rad_l1)-1.)*0.75 +1.0                                                                
        if     keyword_set(ajuste_bajo) then lefts_l1  = where(rad_l1 le rr1_l1 and rad_l1 ge 1.05)
        if not keyword_set(ajuste_bajo) then lefts_l1  = where(rad_l1 le rr1_l1)
        rights_l1 = where(rad_l1 le rr2_l1)
        diomes_l1 = where(rad_l1 gt rr1_l1 and rad_l1 lt rr2_l1)
     endif

     if max(rad_l2) ge 1.2 then begin
        rr1_l2 = 1.07           ;(max(rad_l2)-1.)*0.25 +1.0                                      
        rr2_l2 = 1.12           ;(max(rad_l2)-1.)*0.5 +1.0                             
        if     keyword_set(ajuste_bajo) then lefts_l2  = where(rad_l2 le rr1_l2 and rad_l2 ge 1.05)
        if not keyword_set(ajuste_bajo) then lefts_l2  = where(rad_l2 le rr1_l2)
        rights_l2 = where(rad_l2 ge rr2_l2)
        diomes_l2 = where(rad_l2 gt rr1_l2 and rad_l2 lt rr2_l2)
     endif

     if max(rad_l2) lt 1.2 then begin
        Drr = max(rad_l2) - rrr_min 
        rr1_l2  = rrr_min + Drr * 1./3.
        rr2_l2  = rrr_min + Drr * 2./3.
;    rr1_l2 = 1.07                     
;    rr2_l2 = (max(rad_l2)-1.)*0.75 +1.0                 
        if     keyword_set(ajuste_bajo) then lefts_l2  = where(rad_l2 le rr1_l2 and rad_l2 ge 1.05)
        if not keyword_set(ajuste_bajo) then lefts_l2  = where(rad_l2 le rr1_l2)
        rights_l2 = where(rad_l2 ge rr2_l2)
        diomes_l2 = where(rad_l2 gt rr1_l2 and rad_l2 lt rr2_l2)
     endif
  endif
  
     if lefts_l1(0) eq -1 or diomes_l1(0) eq -1 or rights_l1(0) eq -1 then goto,skipfitloop
     if lefts_l2(0) eq -1 or diomes_l2(0) eq -1 or rights_l2(0) eq -1 then goto,skipfitloop

  
        CASE opcls(il) OF
           1: begin
              if (footlat(ileg) ge stfn_i && footlat(ileg) le stfn_f) || (footlat(ileg) le stfs_f && footlat(ileg) ge stfs_i) then begin
                 error_ne (ileg) = st_f_ne
                 error_t  (ileg) = st_f_t
              endif
;en este caso, ileg y ileg+1 deberian caer aca entonces mismo error para
;ambas piernas
           end
           2: begin
              if footlat(ileg) le stlb_f && footlat(ileg) ge stlb_i then begin
                 error_ne (ileg) = st_lb_ne
                 error_t  (ileg) = st_lb_t
              endif
              
              if footlat(ileg) ge stlin_i || footlat(ileg) le stlis_f then begin
                 error_ne (ileg) = st_li_ne
                 error_t  (ileg) = st_li_t
              endif
           end
;en este caso quizas una pierna ileg tiene un error pero ileg+1 tiene
;un error diferente
        endcase
        
        CASE opcls(il) OF
;Se vuelve a evaluar opcls(il) xq aun no se definio que la 2da pata
;tenga el mismo valor de opcls que la primera. Para que quede claro,
;il e ileg no tienen igual dimension. Es decir que por cada il cerrado
;tengo dos ileg. Pero a la segunda ileg aun no se le asigno el valor
;que le corresponde de opcls. (esto se hace mas abajo)           
           1: begin
              if (footlat(ileg+1) ge stfn_i && footlat(ileg+1) le stfn_f) || (footlat(ileg+1) le stfs_f and footlat(ileg+1) ge stfs_i) then begin
                 error_ne (ileg+1) = st_f_ne
                 error_t  (ileg+1) = st_f_t
;deberian valer igual que ileg xq son cerrados chicos y deberian estar
;en la misma franja                 
              endif
           end
           2: begin
              if footlat(ileg+1) le stlb_f && footlat(ileg+1) ge stlb_i then begin
                 error_ne (ileg+1) = st_lb_ne
                 error_t  (ileg+1) = st_lb_t
              endif
              if footlat(ileg+1) ge stlin_i || footlat(ileg+1) le stlis_f then begin
                 error_ne (ileg+1) = st_li_ne
                 error_t  (ileg+1) = st_li_t
              endif
           end
        endcase
                
        case 1 of
           keyword_set(ajuste_alto) eq 1: begin
              sfit1  = s_l1  (where(rad_l1 ge findel(val_corte,rad_l1)))
              xfit1  = rad_l1(where(rad_l1 ge val_corte))
              yfit1  = Ne_l1 (where(rad_l1 ge val_corte))
              zfit1  = p_l1  (where(rad_l1 ge val_corte))
              wfit1  = Tm_l1 (where(rad_l1 ge val_corte))
              min_s1 = min(s_l1)
              max_s1 = max(s_l1) 
              min_r1 = rad_l1(findel(val_corte,rad_l1))
              max_r1 = rad_l1(findel(1.2 ,rad_l1))
              sfit2  = s_l2  (where(rad_l2 ge findel(val_corte,rad_l2)))
              xfit2  = rad_l2(where(rad_l2 ge val_corte))
              yfit2  = Ne_l2 (where(rad_l2 ge val_corte))
              zfit2  = p_l2  (where(rad_l2 ge val_corte))
              wfit2  = Tm_l2 (where(rad_l2 ge val_corte))
              min_s2 = min(s_l2)
              max_s2 = max(s_l2)
              min_r2 = rad_l2(findel(val_corte,rad_l2))
              max_r2 = rad_l2(findel(1.2 ,rad_l2))
           end
           keyword_set(ajuste_bajo) eq 1: begin
              sfit1 =  s_l1  (where(rad_l1 le findel(val_corte,rad_l1)))
              xfit1 =  rad_l1(where(rad_l1 le val_corte))
              yfit1 =  Ne_l1 (where(rad_l1 le val_corte))
              zfit1 =  P_l1  (where(rad_l1 le val_corte))
              wfit1 =  Tm_l1 (where(rad_l1 le val_corte))
              min_s1 = min(s_l1)
              max_s1 = max(s_l1)
              min_r1 = rad_l1(findel(1.025,rad_l1))
              max_r1 = rad_l1(findel(val_corte ,rad_l1))
              sfit2 =  s_l2  (where(rad_l2 le findel(val_corte,rad_l2)))
              xfit2 =  rad_l2(where(rad_l2 le val_corte))
              yfit2 =  Ne_l2 (where(rad_l2 le val_corte))
              zfit2 =  P_l2  (where(rad_l2 le val_corte))
              wfit2 =  Tm_l2 (where(rad_l2 le val_corte))
              min_s2 = min(s_l2)
              max_s2 = max(s_l2)
              min_r2 = rad_l2(findel(1.025,rad_l2))
              max_r2 = rad_l2(findel(val_corte ,rad_l2))
           end
           else: begin
              sfit1 = s_l1
              xfit1 = rad_l1
              yfit1 = Ne_l1
              zfit1 = p_l1
              wfit1 = Tm_l1
              min_s1 = min(s_l1)
              max_s1 = max(s_l1)
              min_r1 = rad_l1(findel(1.025,rad_l1))
              max_r1 = rad_l1(findel(1.2 ,rad_l1))
              sfit2 = s_l2
              xfit2 = rad_l2
              yfit2 = Ne_l2
              zfit2 = p_l2
              wfit2 = Tm_l2
              min_s2 = min(s_l2)
              max_s2 = max(s_l2)
              min_r2 = rad_l2(findel(1.025,rad_l2))
              max_r2 = rad_l2(findel(1.2 ,rad_l2))
           end
        endcase

;Fiteando Ne 
; pata l1
    linear_fit,1/xfit1,alog(yfit1),min_r1,max_r1,A,r2,salidafit,/theilsen,/xinverted
    Ne0(ileg) = exp(A[0]+A[1])
    lambda_N(ileg) = 1./A[1]
    r2N(ileg) = r2
    pearson_n(ileg) = correlate(xfit1,alog(yfit1))
    
    Tefit_ts(ileg) = bb* mu * mH * gsun * (lambda_N(ileg)*rsun) / kB
    Nebasal(ileg) = Ne0(ileg) * exp(-1/lambda_n(ileg)* (1. - 1./1.025))
    franja_lineal,yfit1,salidafit,min_r1,max_r1,error_ne(ileg),fraccion
    fne (ileg) = fraccion
;s    
    linear_fit,1/sfit1,alog(yfit1),min_s1,max_s1,A,r2,salidafit,/theilsen,/xinverted
    Ne0_s(ileg) = exp(A[0]+A[1])
    lambda_N_s(ileg) = 1./A[1]
    r2N_s(ileg) = r2
    pearson_ns(ileg) = correlate(sfit1,alog(yfit1))
    
    if n_elements(yfit1) ne n_elements(salidafit) then stop
    franja_lineal,yfit1,salidafit,min_s1,max_s1,error_ne(ileg),fraccion
    fne_s (ileg) = fraccion

;pata l2    
    linear_fit,1/xfit2,alog(yfit2),min_r2,max_r2,A,r2,salidafit,/theilsen,/xinverted
    Ne0(ileg+1) = exp(A[0]+A[1])
    lambda_N(ileg+1) = 1./A[1]
    r2N(ileg+1) = r2
    pearson_n(ileg+1) = correlate(xfit2,alog(yfit2))
    
    Tefit_ts(ileg+1) = bb* mu * mH * gsun * (lambda_N(ileg+1)*rsun) / kB
    Nebasal(ileg+1) = Ne0(ileg+1) * exp(-1/lambda_n(ileg+1)* (1. - 1./1.025))
    franja_lineal,yfit2,salidafit,min_r2,max_r2,error_ne(ileg+1),fraccion
    fne (ileg+1) = fraccion

    linear_fit,1/sfit2,alog(yfit2),min_s2,max_s2,A,r2,salidafit,/theilsen,/xinverted
    Ne0_s(ileg+1) = exp(A[0]+A[1])
    lambda_N_s(ileg+1) = 1./A[1]
    r2N_s(ileg+1) = r2
    pearson_ns(ileg+1) = correlate(sfit2,alog(yfit2))
    
    if n_elements(yfit2) ne n_elements(salidafit) then stop
    franja_lineal,yfit2,salidafit,min_s2,max_s2,error_ne(ileg+1),fraccion
    fne_s (ileg+1) = fraccion

    
;Fiteando temperatura          
;para l1
    linear_fit,xfit1,wfit1,min_r1,max_r1,A,r2,salidafit,/theilsen
    Tm0(ileg)   = A[0]
    gradT(ileg) = A[1]
    r2t (ileg)  = r2;si es isotermico va a dar bajo.      
    pearson_t(ileg) = correlate(xfit1,alog(wfit1))

    franja_lineal,wfit1,salidafit,min_r1,max_r1,error_t(ileg),fraccion
    ft (ileg) = fraccion

    linear_fit,sfit1,wfit1,min_s1,max_s1,A,r2,salidafit,/theilsen
    Tm0_s(ileg)   = A[0]
    gradT_s(ileg) = A[1]
    r2t_s (ileg)  = r2
    pearson_ts(ileg) = correlate(sfit1,alog(wfit1))
    
    Te_base(ileg) = gradT_s(ileg) * 1.025 + Tm0_s(ileg)
    betabase(ileg) = (kb/bb * nebasal(ileg) * te_base(ileg)) /(B_base(ileg)^2/(8*!pi))
;pata l2
    linear_fit,xfit2,wfit2,min_r2,max_r2,A,r2,salidafit,/theilsen
    Tm0(ileg+1)   = A[0]
    gradT(ileg+1) = A[1]
    r2t (ileg+1)  = r2
    pearson_t(ileg+1) = correlate(xfit2,alog(wfit2))

    franja_lineal,wfit2,salidafit,min_r2,max_r2,error_t(ileg+1),fraccion
    ft (ileg+1) = fraccion

    linear_fit,sfit2,wfit2,min_s2,max_s2,A,r2,salidafit,/theilsen
    Tm0_s(ileg+1)   = A[0]
    gradT_s(ileg+1) = A[1]
    r2t_s (ileg+1)  = r2
    pearson_ts(ileg+1) = correlate(sfit2,alog(wfit2))

    Te_base(ileg+1) = gradT_s(ileg+1) * 1.025 + Tm0_s(ileg+1)
    betabase(ileg+1) = (kb/bb * nebasal(ileg+1) * te_base(ileg+1)) /(B_base(ileg+1)^2/(8*!pi))

    iso  (ileg)   = abs(gradT (ileg)    * abs(max_r1 - min_r1)) / (2 * error_t(ileg))
    iso_s(ileg)   = abs(gradT_s(ileg)   * abs(max_s1 - min_s1)) / (2 * error_t(ileg))
    iso  (ileg+1) = abs(gradT (ileg+1)   * abs(max_r2 - min_r2)) / (2 * error_t(ileg+1))
    iso_s(ileg+1) = abs(gradT_s(ileg+1)  * abs(max_s2 - min_s2)) / (2 * error_t(ileg+1))


    long_r (ileg)   = abs(max_r1 - min_r1)
    long_s (ileg)   = abs(max_s1 - min_s1)
    long_r (ileg+1) = abs(max_r2 - min_r2)
    long_s (ileg+1) = abs(max_s2 - min_s2)

;fit s_l1_base = m1 * rad_l1_base + s0r1
  linear_fit,sfit1,wfit1,min(s_l1),max(s_l1),A,r2,salidafit,/theilsen
  S0r1 = A[0]
  m1   = A[1]
  r2s1 = r2;si m1 es chica entonces r2 no sirve como estimador

;fit s_l2_base = m2 * rad_l2_base + s0r2 
  linear_fit,sfit2,wfit2,min(s_l2),max(s_l2),A,r2,salidafit,/theilsen
  S0r2 = A[0]
  m2   = A[1]
  r2s2 = r2
  r0 = 1.025
  s_r0_a(ileg  ) = m1 * r0 + s0r1
  s_r0_a(ileg+1) = m2 * r0 + s0r2
    
;FCb calculo
  b_l1_r0 = b_l1(0)
  b_l2_r0 = b_l2(0)  
  Fc2_l1 = kappa*Te_base(ileg)^(5./2)*gradt_s(ileg)
  Fc2_l2 = kappa*Te_base(ileg+1)^(5./2)*gradt_s(ileg+1)
  Fcb(ileg  ) = Fc2_l1 * B_l2_r0/(B_l1_r0+B_l2_r0);b_l_0 depende y no es necesariamente en 1.025
  Fcb(ileg+1) = Fc2_l2 * B_l1_r0/(B_l1_r0+B_l2_r0)
;ADEMAS PODRIA FITEARSE EL ER, VER!
  skipfitloop:
     opclstat(ileg)   =      opcls(il)
     opclstat(ileg+1) =      opcls(il)
  loop_length(ileg)   =      loopL(il)
  loop_length(ileg+1) =      loopL(il)
        strad(ileg)   =      str_v(il)
        stlat(ileg)   = (90-stth_v(il)/!dtor)
        stlon(ileg)   =     stph_v(il)/!dtor
        strad(ileg+1) =      str_v(il)
        stlat(ileg+1) = (90-stth_v(il)/!dtor)
        stlon(ileg+1) =     stph_v(il)/!dtor
  skipnextloop:
  indexloop(ileg)  = il
  indexloop(ileg+1)= il
;  if gradT(ileg)*gradT(ileg+1) lt 0. then begin
;     indexloop(ileg)   = -678.
;     indexloop(ileg+1) = -678.
;  endif
  
  ileg = ileg+2
endelse
endfor
  Rp_full  = {base:Rp_base,medio:Rp_medio,alto:Rp_alto}
  stop
;ACA VA UN SAVE!!!  
  save,Nemean,Tmmean,Nestddev,Tmstddev,WTmean,WTstddev,Pmean,Ne2mean,Ermean,Bmean,betamean,$
       Ne0,lambda_N,r2N,Ne0_s,lambda_N_s,r2N_s,P0,lambda_P,r2P,gradT,Tm0,r2T,gradT_s,Tm0_s,r2t_s,$
       ft,ft_s,fne,fne_s,Tefit,Tefit_ts,Te_base,pearson_n,pearson_ns,pearson_t,pearson_ts,$
       betamean,betaapex,Br0,B_base,Nebasal,B_base,opclstat,indexloop,$
       footrad,footlat,footlon,iso,iso_s,long_r,long_s,$
       Rp_base,Rp_medio,Rp_alto,FILENAME = 'trace_vectors_'+file_out+'.sav'
  return
end
