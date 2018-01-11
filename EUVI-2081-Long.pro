suffix    = '-EUVI-2081-Long';+strmid(string(Lmin),6,3)+'-'+strmid(string(Lmax),6,3)
R2Crit_Er = 0.75
R2Crit_T  = 0.5
NeCritA   = 1.4
NeCritQ   = 1.4

;=========== Prueba
goto,prueba
 i = where(NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ AND Phir_loop gt 0. $ ;and L_loop ge Lmin and L_loop lt Lmax $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)

filter_loop,i
nbins=80

footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion

xx   = [r2Er1_loop_c,r2Er2_loop_c]
N    = n_elements(xx)
mini = 0.5
maxi = 1.5 ;2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='Calidad Ajuste Er',ytit='Frequency Histogram',tit='R2Er QS closed loops',filename='QS-R2Er-Loops'+suffix

prueba:

;===================================================================================================================================================
;FULL CORONA

;================================================================================================================================================
;================== FUll QS 
 i = where(NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ AND Phir_loop gt 0. and L_loop ge Lmin and L_loop lt Lmax $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)

filter_loop,i
nbins=80

;======================================================================= UBICACIÓN LOOPS =========================================================
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS'+suffix,indexloop_c,rotacion

;======================================================================== ANÁLISIS L, N, T =======================================================
Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,2.5],yr=[0.5,2],ytit='T!Dm!N [MK]'                              ,tit='QS T media vs L'   ,filename='QS-TvsL'   +suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,2.5],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS N basal vs L'   ,filename='QS-NvsL'   +suffix
PlotVs,Long,Phir,xtit='L [Rsun]',xr=[0.15,1.3],yr=[0,5],ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dr!N vs L',filename='QS-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]',xr=[0,5],yr=[-1,2],ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dc!N vs L',filename='QS-FcbvsL' +suffix
PlotVs,Temp,Nbas,xtit='T!Dm!N [MK]',xr=[0.8,1.8],yr=[0.5,3],ytit='N!De!N [10!U8!Ncm!U-3!N]',tit='QS N!De!N vs T!Dm!N',filename='QS-NvsT' +suffix

xx   = Long
N    = n_elements(xx)
mini = 0
maxi = 5 ;2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram' ,tit='Long QS closed loops',filename='QS-Long-Loops'+suffix

xx   = Nbas
N    = n_elements(xx)
mini = 0.5
maxi = 2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS closed loops',filename='QS-Nbasal'+suffix

xx   = Temp
N    = n_elements(xx)
mini = 0.5
maxi = 2
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS closed loops',filename='QS-Tm'+suffix

;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin) 
   L_filt =    L_loop_c(iin)

xxr = Phir_filt/1.e5
xxf = Fcb_filt/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)                    
maxi=4.;max(xx)                                                                                                                                     
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',$
tit='QS closed loops',filename='QS-Phis'+suffix


;===================================================================================================================================================
;===================================================================================================================================================
;REGIONES 
;=============== Low Lat QS
 i = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $ 
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T $
           and L_loop ge Lmin and L_loop lt Lmax)
filter_loop,i
nbins=80
;======================================================================= UBICACIÓN LOOPS =========================================================
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Low'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Low'+suffix,indexloop_c,rotacion

;======================================================================== ANÁLISIS L, N, T =======================================================
Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,1.5],yr=[0.8,1.6],ytit='T!Dm!N [MK]'                              ,tit='QS Low T media vs L'   ,filename='QS-Low-TvsL'+suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,1.5],yr=[1,2.5],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS Low N basal vs L'   ,filename='QS-Low-NvsL' +suffix
PlotVs,Long,Phir,xtit='L [Rsun]',xr=[0,1.5],yr=[0,5],ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Low !9f!3!Dr!N vs L',filename='QS-Low-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]',xr=[0,1.5],yr=[-0.5,1.5],ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Low !9f!3!Dc!N vs L',filename='QS-Low-FcbvsL'+suffix
PlotVs,Temp,Nbas,xtit='T!Dm!N [MK]',xr=[0.9,1.4],yr=[1,2.5],ytit='N!De!N [10!U8!Ncm!U-3!N]',tit='QS Low N!De!N vs T!Dm!N',filename='QS-Low-NvsT' +suffix

xx   = Long
N    = n_elements(xx)
mini = 0
maxi = 2
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Long QS Low closed loops',filename='QS-Low-Long-Loops'+suffix

xx   = Nbas
N    = n_elements(xx)
mini = 1
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS Low closed loops',filename='QS-Low-Nbasal'+suffix

xx   = Temp
N    = n_elements(xx)
mini = 0.6
maxi = 1.6
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS Low closed loops',filename='QS-Low-Tm'+suffix

;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)

xxr = Phir_filt/1.e5
xxf = Fcb_filt/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)                    
maxi=4.;max(xx)                                                                                                                                     
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',$
tit='QS Low closed loops',filename='QS-Low-Phis'+suffix

;===================================================================================================================================================
;=============== Mid Lat QS
 i = where(Abs(lat1_loop) gt 30. and Abs(lat2_loop) gt 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T $
           and L_loop ge Lmin and L_loop lt Lmax)
filter_loop,i
nbins=80

;======================================================================= UBICACIÓN LOOPS =========================================================
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Mid'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Mid'+suffix,indexloop_c,rotacion

;======================================================================== ANÁLISIS L, N, T =======================================================
Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,1.5],yr=[1,1.7],ytit='T!Dm!N [MK]'                              ,tit='QS Mid T media vs L'   ,filename='QS-Mid-TvsL'     +suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,1.5],yr=[0.5,2.5],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS Mid N basal vs L'   ,filename='QS-Mid-NvsL'     +suffix
PlotVs,Long,Phir,xtit='L [Rsun]',xr=[0,5],yr=[0,4],ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Mid !9f!3!Dr!N vs L',filename='QS-Mid-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]',xr=[0,5],yr=[-1,1],ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Mid !9f!3!Dc!N vs L',filename='QS-Mid-FcbvsL' +suffix
PlotVs,Temp,Nbas,xtit='T!Dm!N [MK]',xr=[1,1.7],yr=[0.5,2.5],ytit='N!De!N [10!U8!Ncm!U-3!N]',tit='QS Mid N!De!N vs T!Dm!N',filename='QS-Mid-NvsT' +suffix

xx   = Long
N    = n_elements(xx)
mini = 0
maxi = 5 ;2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Long QS Mid closed loops',filename='QS-Mid-Long-Loops'+suffix

xx   = Nbas
N    = n_elements(xx)
mini = 0.5
maxi = 2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS Mid closed loops',filename='QS-Mid-Nbasal'+suffix

xx   = Temp
N    = n_elements(xx)
mini = 0.95
maxi = 1.65
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS Mid closed loops',filename='QS-Mid-Tm'+suffix

;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)

xxr = Phir_filt/1.e5
xxf = Fcb_filt/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)                    
maxi=4.;max(xx)                                                                                                                                     
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',$
tit='QS Mid closed loops',filename='QS-Mid-Phis'+suffix

;===================================================================================================================================================
