suffix   = '-EUVI-2081-Long-Gradientes-19-07-'+strmid(string(Lmin),6,3)+'-'+strmid(string(Lmax),6,3)

R2Crit_Er = 0.8
R2Crit_Ne = 0.75
R2Crit_T  = 0.5
FTscrit   = 0.70
NeCritA   = 1.4
NeCritQ   = 1.4
rsun      = 6.955e10 

;Lmin = 0.
;Lmax = 5.

;===================================================================================================================================================
;FULL CORONA
;================== FUll QS all loops

ff=1.

 i = where(Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ and L_loop ge Lmin and L_loop lt Lmax $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit $
           and    (abs(dTmds1_ts_loop)*rsun*abs(Smaxxx1_loop - Sminnn1_loop) gt ff*(2*eplegT1_loop)        $
           or      abs(dTmds2_ts_loop)*rsun*abs(Smaxxx2_loop - Sminnn2_loop) gt ff*(2*eplegT2_loop) )      $
           and ((dTmds1_ts_loop gt 0. and dTmds2_ts_loop gt 0.) or (dTmds1_ts_loop lt 0. and dTmds2_ts_loop lt 0.)) )
;           and  Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. )

filter_loop,i 
nbins=80

;ISO_loop = (((abs(dTmds1_ts_loop_c)*rsun*abs(Smaxxx1_loop_c - Sminnn1_loop_c))/(2*eplegT1_loop_c))+((abs(dTmds2_ts_loop_c)*rsun*abs(Smaxxx2_loop_c - Sminnn2_loop_c))/(2*eplegT2_loop_c)))*0.5
;help,where(L_loop_c ge 0.1 and L_loop_c lt 0.3)

;======================================================================= UBICACIÓN LOOPS =========================================================
;footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-all'+suffix,indexloop_c,rotacion
;======================================================================== ANÁLISIS L, N, T =======================================================

Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

;PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,5],yr=[0.5,2],ytit='T!Dm!N [MK]'                              ,tit='QS T media vs L'   ,filename='QS-TvsL'   +suffix
;PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,5],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS N basal vs L'   ,filename='QS-NvsL'   +suffix
;PlotVs,Long,Phir,xtit='L [Rsun]',xr=[0,5],yr=[0,5],ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dr!N vs L',filename='QS-PhirvsL'+suffix
;PlotVs,Long,Fcb ,xtit='L [Rsun]',xr=[0,5],yr=[-1,2],ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dc!N vs L',filename='QS-FcbvsL' +suffix
;PlotVs,Temp,Nbas,xtit='T!Dm!N [MK]',xr=[0.8,1.8],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]',tit='QS N!De!N vs T!Dm!N',filename='QS-NvsT' +suffix


xx   = Long
medianlong = median(Long)
N    = n_elements(xx)
mini = 0
maxi = 5;2.5
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram' ,tit='Long QS closed loops',filename='QS-Long-Loops'+suffix

xx   = Nbas
N    = n_elements(xx)
mini = 0.5
maxi = 2.5
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS closed loops',filename='QS-Nbasal'+suffix

xx   = Temp
N    = n_elements(xx)
mini = 0.5
maxi = 2
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS closed loops',filename='QS-Tm'+suffix

;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin) 
   L_filt =    L_loop_c(iin)

lat1_filt = Rp_lat1_loop_c(iin) 
lat2_filt = Rp_lat2_loop_c(iin) 
lon1_filt = Rp_lon1_loop_c(iin) 
lon2_filt = Rp_lon2_loop_c(iin) 

xxr = Phir_filt/1.e5
xxf = -Fcb_filt/1.e5
xxh=xxr+xxf
mini=-1.                    
maxi=3.5                                                                                                                    
;xxxx=xxh/max(xxh)
;data1 = [xxxx,xxxx]
;Fluence_Plot,data1,lat1_filt,lon1_filt

;Fcbmalos=where(Fcb_loop_c lt 0)
;print,n_elements(Fcbmalos)
;x2=abs(dTmds2_ts_loop_c)*rsun*abs(Smaxxx2_loop_c - Sminnn2_loop_c)/(2*eplegT2_loop_c)
;x1=abs(dTmds1_ts_loop_c)*rsun*abs(Smaxxx1_loop_c - Sminnn1_loop_c)/(2*eplegT1_loop_c)
;x3=x2
histoplotNe3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='QS closed loops',filename='QS-all-Phis'+suffix

;Phih_map,box=box,[lat1_filt,lat2_filt],[lon1_filt,lon2_filt],[xxh,xxh],filelabel='QS-',indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH
L_cm   = L_filt*rsun
Phih_L = xxh/L_cm

;openw,2,'Long_phi.txt'
;printf,2,format='(''Longitud'',''  Heating'')'
;printf,2, format='(e10.2,e10.2)',L_cm,Phih_L
;close,2

Nup    = n_elements(Phir_filt/1.e5)
lambda = mean(lambda_N_loop_c)
ff = (0.5*medianlong)/lambda 
print,'all',Lmin,Lmax,medianlong,Nup,lambda,ff
;===============================================================================================================================================
;===============================================================================================================================================
;===============================================================================================================================================
;================================================================================================================================================

;===================================================================================================================================================
;FULL CORONA
;================== FUll QS up loops

ff=1.

 i = where(Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ and L_loop ge Lmin and L_loop lt Lmax $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit $
           and    (abs(dTmds1_ts_loop)*rsun*abs(Smaxxx1_loop - Sminnn1_loop) gt ff*(2*eplegT1_loop)        $
           or      abs(dTmds2_ts_loop)*rsun*abs(Smaxxx2_loop - Sminnn2_loop) gt ff*(2*eplegT2_loop) )      $
           and     -Fcb_loop/1.e5 gt 0.  and dTmds1_ts_loop gt 0. and dTmds2_ts_loop gt 0.)        
;           and ((dTmds1_ts_loop gt 0. and dTmds2_ts_loop gt 0.) or (dTmds1_ts_loop lt 0. and dTmds2_ts_loop lt 0.)) )
;           and  Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. )

filter_loop,i 
nbins=80

;ISO_loop = (((abs(dTmds1_ts_loop_c)*rsun*abs(Smaxxx1_loop_c - Sminnn1_loop_c))/(2*eplegT1_loop_c))+((abs(dTmds2_ts_loop_c)*rsun*abs(Smaxxx2_loop_c - Sminnn2_loop_c))/(2*eplegT2_loop_c)))*0.5

;help,where(L_loop_c ge 0.1 and L_loop_c lt 0.3)

;======================================================================= UBICACIÓN LOOPS =========================================================
;footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion
;;;Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-up'+suffix,indexloop_c,rotacion

;======================================================================== ANÁLISIS L, N, T =======================================================

Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

;PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,5],yr=[0.5,2],ytit='T!Dm!N [MK]'                              ,tit='QS T media vs L'   ,filename='QS-TvsL'   +suffix
;PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,5],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS N basal vs L'   ,filename='QS-NvsL'   +suffix
;PlotVs,Long,Phir,xtit='L [Rsun]',xr=[0,5],yr=[0,5],ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dr!N vs L',filename='QS-PhirvsL'+suffix
;PlotVs,Long,Fcb ,xtit='L [Rsun]',xr=[0,5],yr=[-1,2],ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dc!N vs L',filename='QS-FcbvsL' +suffix
;PlotVs,Temp,Nbas,xtit='T!Dm!N [MK]',xr=[0.8,1.8],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]',tit='QS N!De!N vs T!Dm!N',filename='QS-NvsT' +suffix


xx   = Long
medianlong = median(Long)
N    = n_elements(xx)
mini = 0
maxi = 5;2.5
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram' ,tit='Long QS closed loops',filename='QS-Long-Loops'+suffix

xx   = Nbas
N    = n_elements(xx)
mini = 0.5
maxi = 2.5
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS closed loops',filename='QS-Nbasal'+suffix

xx   = Temp
N    = n_elements(xx)
mini = 0.5
maxi = 2
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS closed loops',filename='QS-Tm'+suffix

;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin) 
   L_filt =    L_loop_c(iin)

lat1_filt = Rp_lat1_loop_c(iin) 
lat2_filt = Rp_lat2_loop_c(iin) 
lon1_filt = Rp_lon1_loop_c(iin) 
lon2_filt = Rp_lon2_loop_c(iin) 

xxr = Phir_filt/1.e5
xxf = -Fcb_filt/1.e5
xxh=xxr+xxf
mini=-0.5                    
maxi=3                                                                                                                    
;xxxx=xxh/max(xxh)
;data1 = [xxxx,xxxx]
;Fluence_Plot,data1,lat1_filt,lon1_filt

;Fcbmalos=where(Fcb_loop_c lt 0)
;print,n_elements(Fcbmalos)
;x2=abs(dTmds2_ts_loop_c)*rsun*abs(Smaxxx2_loop_c - Sminnn2_loop_c)/(2*eplegT2_loop_c)
;x1=abs(dTmds1_ts_loop_c)*rsun*abs(Smaxxx1_loop_c - Sminnn1_loop_c)/(2*eplegT1_loop_c)
;x3=x2
;;;histoplotNe3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='QS closed up loops',filename='QS-up-Phis'+suffix

;Phih_map,box=box,[lat1_filt,lat2_filt],[lon1_filt,lon2_filt],[xxh,xxh],filelabel='QS-',indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH
L_cm   = L_filt*rsun
Phih_L = xxh/L_cm

;openw,2,'Long_phi.txt'
;printf,2,format='(''Longitud'',''  Heating'')'
;printf,2, format='(e10.2,e10.2)',L_cm,Phih_L
;close,2

Nup    = n_elements(Phir_filt/1.e5)
lambda = mean(lambda_N_loop_c)
ff = (0.5*medianlong)/lambda 
;print,'up',Lmin,Lmax,medianlong,Nup,lambda,ff
;===============================================================================================================================================
;===============================================================================================================================================
;===============================================================================================================================================
;================================================================================================================================================
;================== FUll QS down loops

 i = where(Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ and L_loop ge Lmin and L_loop lt Lmax $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit $
           and  (abs(dTmds1_ts_loop)*rsun*abs(Smaxxx1_loop - Sminnn1_loop) gt 2*eplegT1_loop      $
           or    abs(dTmds2_ts_loop)*rsun*abs(Smaxxx2_loop - Sminnn2_loop) gt 2*eplegT2_loop)     $
           and Fcb_loop/1.e5 gt 0.  and dTmds1_ts_loop lt 0. and dTmds2_ts_loop lt 0.) 
;           and Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. )

; i = where(NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ AND Phir_loop gt 0. and L_loop ge Lmin and L_loop lt Lmax $
;           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)

filter_loop,i
nbins=80

;======================================================================= UBICACIÓN LOOPS =========================================================
;footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion
;;;Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-down'+suffix,indexloop_c,rotacion

;======================================================================== ANÁLISIS L, N, T =======================================================

Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

;PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,5],yr=[0.5,2],ytit='T!Dm!N [MK]'                              ,tit='QS T media vs L'   ,filename='QS-TvsL'   +suffix
;PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,5],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS N basal vs L'   ,filename='QS-NvsL'   +suffix
;PlotVs,Long,Phir,xtit='L [Rsun]',xr=[0,5],yr=[0,5],ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dr!N vs L',filename='QS-PhirvsL'+suffix
;PlotVs,Long,Fcb ,xtit='L [Rsun]',xr=[0,5],yr=[-1,2],ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dc!N vs L',filename='QS-FcbvsL' +suffix
;PlotVs,Temp,Nbas,xtit='T!Dm!N [MK]',xr=[0.8,1.8],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]',tit='QS N!De!N vs T!Dm!N',filename='QS-NvsT' +suffix

xx   = Long
medianlong= median(Long)
N    = n_elements(xx)
mini = 0
maxi = 5 ;2.5
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram' ,tit='Long QS closed loops',filename='QS-Long-Loops'+suffix

xx   = Nbas
N    = n_elements(xx)
mini = 0.5
maxi = 2.5
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS closed loops',filename='QS-Nbasal'+suffix

xx   = Temp
N    = n_elements(xx)
mini = 0.5
maxi = 2
;histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS closed loops',filename='QS-Tm'+suffix

;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin) 
   L_filt =    L_loop_c(iin)

xxr = Phir_filt/1.e5
xxf = -Fcb_filt/1.e5
xxh=xxr+xxf
mini=-2.;min(xx)                    
maxi= 6;max(xx)                                                                                                                                     
;;;histoplotNe3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='QS closed down loops',filename='QS-down-Phis'+suffix

Nup    = n_elements(Phir_filt/1.e5)
lambda = mean(lambda_N_loop_c)
ff = (0.5*medianlong)/lambda
;;;print,'down',Lmin,Lmax,medianlong,Nup,lambda,ff
