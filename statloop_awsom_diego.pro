pro statloop_awsom_diego


!except=2
device, retain     = 2
device, true_color = 24
device, decomposed = 0

; Physical constants needed for the HS fits:             
  rsun = 6.955e10    ; cm                                             
  gsun = 2.74e4      ; cm/sec²                                
    kB = 1.38e-16    ; erg/K                                           
    mH = 1.6726e-24  ; gr                                    
     a = 0.08        ; N_He / N_H                       
    mu = (1.+4.*a)/(1.+2.*a)
    bb = (1.+2.*a)/(2.+3.*a)
 kappa = 9.2e-7      ; erg s ^-1 cm ^-1 K ^-7/2                

if not keyword_set(rmin) then rmin = 1.025
if not keyword_set(rmax) then rmax = 1.20
if not keyword_set(rloopmin) then rloopmin = 1.07
rminloop=rloopmin
;Para que estan estos valores de rloopmin???

if not keyword_set (altura) then read_trace_sampled,file,0
if     keyword_set (altura) then read_trace_sampled,file,alturas
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


    Ne0 = fltarr(Nlegs)-555.
lambda_N = fltarr(Nlegs)-555.
   Tefit = fltarr(Nlegs)-555.
     r2N = fltarr(Nlegs)-555.
      P0 = fltarr(Nlegs)-555.
lambda_P = fltarr(Nlegs)-555.
     r2P = fltarr(Nlegs)-555.
   gradT = fltarr(Nlegs)-555.
     Tm0 = fltarr(Nlegs)-555.

gradT_2 = fltarr(Nlegs)-555.
     r2T_2 = fltarr(Nlegs)-555.
     Tm0_2 = fltarr(Nlegs)-555.
     error_ladfit = fltarr(Nlegs)-555.
     r2T_new = fltarr(Nlegs)-555.
     r2T_2_new = fltarr(Nlegs)-555.
     r2T_3_new = fltarr(Nlegs)-555.
     gradT_3 = fltarr(Nlegs)-555.
     Tm0_3 = fltarr(Nlegs)-555.
     r2T_3 = fltarr(Nlegs)-555.
     r2N_ts = fltarr(Nlegs)-555.
     lambda_N_ts = fltarr(Nlegs)-555.
     Ne0_ts = fltarr(Nlegs)-555.

     gradT_3s = fltarr(Nlegs)-555.
     Tm0_3s = fltarr(Nlegs)-555.

     f_T  = fltarr(Nlegs)-555.
     f_ne = fltarr(Nlegs)-555.
f_ne_cuad = fltarr(Nlegs)-555.
     r2_lin = fltarr(Nlegs)-555.
     r2_lin_ts = fltarr(Nlegs)-555.

   Tefit_ts = fltarr(Nlegs)-555.

Te_base = fltarr(Nlegs)-555.

Tm0s = fltarr(Nlegs)-555.                                                             

     r2T = fltarr(Nlegs)-555.
   dTmds = fltarr(Nlegs)-555.
    r2Ts = fltarr(Nlegs)-555.
r2Tcuadr = fltarr(Nlegs)-555.
Acuadr_a = fltarr(Nlegs,3)-555.
  s_r0_a = fltarr(Nlegs)-555.
      Eh = fltarr(Nlegs)-555.

Phir =fltarr(Nlegs)-963.
                                                                                                 
Fcb=fltarr(Nlegs)-963.                                                 

 deltaEh = fltarr(Nlegs)-555.
      sH = fltarr(Nlegs)-555.
    r2sH = fltarr(Nlegs)-555.
betamean = fltarr(Nlegs)-555.
betaapex = fltarr(Nlegs)-555.
   Bmean = fltarr(Nlegs)-555.
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
  corte_awsom = 1.055

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
           Er_l = reform ( Er_v(0:Npts_v(il)-1,il))
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


;select usefull data
                                                                                              
        p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. and scoreR_l lt 0.10)
;podria relajarse a 0.25??
;hacer estadistica de scoreR_l  !!!
if p(0) eq -1 then goto,skipnextloop_open

     Ne_l =  Ne_l (p)
     Tm_l =  Tm_l (p)
     if keyword_set (demt) then begin
        WT_l =  WT_l (p)
        Er_l =  Er_l (p)
     endif
     rad_l = rad_l (p)
     lat_l = lat_l (p)
     lon_l = lon_l (p)
     s_l =   s_l (p)
     B_l =   B_l (p)
     
     p_l   = kB/bb *Ne_l*Tm_l
     beta_l = p_l/(B_l^2/(8*!pi))
    
     Nemean(ileg) =   mean(Ne_l)
     betamean(ileg) =   mean(beta_l)
     Bmean(ileg) =   mean(B_l)

     case 1 of
        keyword_set(ajuste_awsom_alto) eq 1: Tmmean(ileg) =  mean(Tm_l(where(rad_l ge corte_awsom)))
        keyword_set(ajuste_awsom_bajo) eq 1: Tmmean(ileg) =  mean(Tm_l(where(rad_l le corte_awsom)))
        else: Tmmean(ileg) =   mean(Tm_l)
     endcase
     ;se guardan valores pero no hay fiteo si hay menos de 5 puntos.
     if n_elements(p) lt Ndata then goto,skipfitloop_open

; La idea es considerar tercios sobre 1.05  

     if not keyword_set(ajuste_awsom_alto) || not keyword_set(ajuste_awsom_bajo) then begin
        rr1 = 1.07              ;rr1 = 1.06
        rr2 = 1.12              ;1.16
        lefts  = (where(rad_l le rr1))
        rights = (where(rad_l ge rr2))
        diomes = (where(rad_l gt rr1 and rad_l lt rr2))
     endif

;     esto en realidad no es necesario, ya que awsom no tiene zda. pero eliminara
;     loops abiertos que sean chicos (cosas raras)
     if  keyword_set(ajuste_awsom_alto) then begin 
       rr1 = 1.10
       rr2 = 1.13
       lefts  = (where(rad_l ge corte_awsom and rad_l le rr1))
       rights = (where(rad_l ge rr2))
       diomes = (where(rad_l gt rr1 and rad_l lt rr2))
       if lefts(0) eq -1 || diomes(0) eq -1 || rights(0) eq -1 then stop;quiero ver si esto pasa.
    endif
     
     if lefts(0) eq -1 || diomes(0) eq -1 || rights(0) eq -1 then goto,skipfitloop_open

;se setean los errores segun la region de donde sea el loop abierto
        if (footlat(il) le chlbn_f && footlat(il) ge chlbn_i) || (footlat(il) ge chlbs_i. && footlat(il) le chlbs_f) then begin
           eps_ne = ch_lb_ne
           eps_t  = ch_lb_t
        endif

        if (footlat(il) gt chlan_i.) || (footlat(il) lt chlas_f) then begin
           eps_ne = ch_la_ne
           eps_t  = ch_la_t
        endif


   ;Make HS-fit to Ne(r) for each open leg/loop                                                
   ;Como no le creemos a awsom por debajo                                        
   ;de 1.05 entonces partimos el ajuste en 2                           

    case 1 of
       keyword_set(ajuste_awsom_alto) eq 1: begin
          sfit =  s_l  (where(rad_l ge corte_awsom))
          xfit =  rad_l(where(rad_l ge corte_awsom))
          yfit =  Ne_l (where(rad_l ge corte_awsom))
          zfit =  p_l  (where(rad_l ge corte_awsom))
          wfit =  Tm_l (where(rad_l ge corte_awsom))
       end
       keyword_set(ajuste_awsom_bajo) eq 1: begin
          sfit =  s_l  (where(rad_l le corte_awsom))
          xfit =  rad_l(where(rad_l le corte_awsom))
          yfit =  Ne_l (where(rad_l le corte_awsom))
          zfit =  P_l  (where(rad_l le corte_awsom))
          wfit =  Tm_l (where(rad_l le corte_awsom))
       end
       else: begin
          sfit = s_l
          xfit = rad_l
          yfit = Ne_l
          zfit = p_l
          wfit = Tm_l
       end
    endcase




    rminhs =min(rad_l)          ;rmin
    rmaxhs = max(rad_l)         ;rmax

    if      keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2,alin,A_ts,corr2_ts,alin_ts,fit_ts,fit_cuad
if not  keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2


;linear fit?






















    

    

endfor




















