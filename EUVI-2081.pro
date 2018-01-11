;pegatina                                                
suffix    = '-EUVI-2081'
R2Crit_Er = 0.8
R2Crit_T  = 0.5
NeCritA   = 1.4
NeCritQ   = 1.4

;=================================================================================================================================================================
;FULL CORONA

;================================================================================================================================================
;================== FUll QS
 i = where(NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ AND Phir_loop gt 0. $
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

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='T!Dm!N [MK]'                              ,tit='QS T media vs L'   ,filename='QS-TvsL'   +suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS N basal vs L'   ,filename='QS-NvsL'   +suffix
PlotVs,Long,Phir,xtit='L [Rsun]',ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dr!N vs L',filename='QS-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]',ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS !9f!3!Dc!N vs L',filename='QS-FcbvsL' +suffix

xx   = Long
mini = 0
maxi = 2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Long QS closed loops',filename='QS-Long-Loops'+suffix

xx   = Nbas
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS closed loops',filename='QS-Nbasal'+suffix

xx   = Temp
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS closed loops',filename='QS-Tm'+suffix

iin=where(TmR12_loop_c gt 0.5 and TmR12_loop_c gt 0.5) 
xx=([TmR12_loop_c(iin),TmR12_loop_c(iin)]/1.e6)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N 1.075 [MK]',ytit='Frequency Histogram',tit='QS closed loops',filename='QS-TmR'+suffix

xx=([NR12_loop_c,NR12_loop_c]/1.e8)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 dat [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='QS closed loops',filename='QS-Ndat'+suffix

xx=([Ner21_loop_c,Ner22_loop_c]/1.e8)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 fit [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='QS closed loops',filename='QS-Nfit'+suffix

;xx1=Ner11_loop_c/1.e8
;xx2=Ner21_loop_c/1.e8
;xx3=Ner31_loop_c/1.e8
;mini=0.;min(xx1)
;maxi=3.;max(xx1)                                                                                                                                                      
;histoplotNe3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ne [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='QS closed loops',filename='QS-NeR'+suffix

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
tit='QS closed loops',filename='QS-Phis'+suffix

;===============================================================================================================================================
;================== FUll AR
 i = where(NR11_loop/1.e8 gt NeCritA and NR12_loop/1.e8 gt NeCritA AND Phir_loop gt 0. $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)
filter_loop,i
nbins=80
;======================================================================= UBICACIÓN LOOPS =========================================================
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='AR'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='AR'+suffix,indexloop_c,rotacion

;======================================================================== ANÁLISIS L, N, T =======================================================
Temp = Tmmean_loop_c/1.e6
Nbas = [Ner01_loop_c,Ner02_loop_c]/1.e8
Long = L_loop_c
Phir = Phir_loop_c/1.e5
Fcb  = Fcb_loop_c/1.e5

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='T!Dm!N [MK]'                              ,tit='AR T media vs L'   ,filename='AR-TvsL'     +suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='AR N basal vs L'   ,filename='AR-NvsL'     +suffix
PlotVs,Long,Phir,xtit='L [Rsun]',ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='AR !9f!3!Dr!N vs L',filename='AR-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]',ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='AR !9f!3!Dc!N vs L',filename='AR-FcbvsL' +suffix

xx   = Long
mini = 0
maxi = 2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Long AR closed loops',filename='AR-Long-Loops'+suffix

xx   = Nbas
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal AR closed loops',filename='AR-Nbasal'+suffix

xx   = Temp
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm AR closed loops',filename='AR-Tm'+suffix

xx=([NR12_loop_c,NR12_loop_c]/1.e8)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 dat [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='AR closed loops',filename='AR-Ndat'+suffix

xx=([Ner21_loop_c,Ner22_loop_c]/1.e8)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 fit [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='AR closed loops',filename='AR-Nfit'+suffix

iin=where(TmR12_loop_c gt 0.5 and TmR12_loop_c gt 0.5) 
xx=([TmR12_loop_c(iin),TmR12_loop_c(iin)]/1.e6)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N 1.075 [MK]',ytit='Frequency Histogram',tit='Ar closed loops',filename='AR-TmR'+suffix

;xx1=Ner11_loop_c/1.e8
;xx2=Ner21_loop_c/1.e8
;xx3=Ner31_loop_c/1.e8
;mini=0.;min(xx1)
;maxi=3.;max(xx1)                                                                                                                                                      
;histoplotNe3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ne [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',$
;tit='AR closed loops',filename='AR-NeR'+suffix

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
tit='AR closed loops',filename='AR-Phis'+suffix


;=================================================================================================================================================================
;=================================================================================================================================================================

;REGIONES 
;=============== Low Lat QS
 i = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $ 
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)
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

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='T!Dm!N [MK]'                              ,tit='QS Low T media vs L'   ,filename='QS-Low-TvsL'     +suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS Low N basal vs L'   ,filename='QS-Low-NvsL'     +suffix
PlotVs,Long,Phir,xtit='L [Rsun]',ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Low !9f!3!Dr!N vs L',filename='QS-Low-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]',ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Low !9f!3!Dc!N vs L',filename='QS-Low-FcbvsL' +suffix

xx   = Long
mini = 0
maxi = 2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Long QS Low closed loops',filename='QS-Low-Long-Loops'+suffix

xx   = Nbas
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS Low closed loops',filename='QS-Low-Nbasal'+suffix

xx   = Temp
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS Low closed loops',filename='QS-Low-Tm'+suffix

xx=([NR12_loop_c,NR12_loop_c]/1.e8)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 dat [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='QS Low closed loops',filename='QS-Low-Ndat'+suffix

xx=([Ner21_loop_c,Ner22_loop_c]/1.e8)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 fit [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='Qs Low closed loops',filename='Qs-Low-Nfit'+suffix

iin=where(TmR12_loop_c gt 0.5 and TmR12_loop_c gt 0.5) 
xx=([TmR12_loop_c(iin),TmR12_loop_c(iin)]/1.e6)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N 1.075 [MK]',ytit='Frequency Histogram',tit='QS Low closed loops',filename='QS-Low-TmR'+suffix

;xx1=Ner11_loop_c/1.e8
;xx2=Ner21_loop_c/1.e8
;xx3=Ner31_loop_c/1.e8
;mini=0.;min(xx1)
;maxi=3.;max(xx1)                                                                                                                                                      
;histoplotNe3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ne [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',$
;tit='QS Low closed loops',filename='QS-Low-NeR'+suffix

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

;=================================================================================================================================================================
;=============== Mid Lat QS
 i = where(Abs(lat1_loop) gt 30. and Abs(lat2_loop) gt 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)
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

PlotVs,Long,Temp,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='T!Dm!N [MK]'                              ,tit='QS Mid T media vs L'   ,filename='QS-Mid-TvsL'     +suffix
PlotVs,Long,Nbas,xtit='L [Rsun]',xr=[0,2.5],yr=[0,3],ytit='N!De!N [10!U8!Ncm!U-3!N]'                 ,tit='QS Mid N basal vs L'   ,filename='QS-Mid-NvsL'     +suffix
PlotVs,Long,Phir,xtit='L [Rsun]'                    ,ytit='!9f!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Mid !9f!3!Dr!N vs L',filename='QS-Mid-PhirvsL'+suffix
PlotVs,Long,Fcb ,xtit='L [Rsun]'                    ,ytit='!9f!3!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',tit='QS Mid !9f!3!Dc!N vs L',filename='QS-Mid-FcbvsL' +suffix

xx   = Long
mini = 0
maxi = 2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Long QS Mid closed loops',filename='QS-Mid-Long-Loops'+suffix

xx   = Nbas
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N Basal [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Nbasal QS Mid closed loops',filename='QS-Mid-Nbasal'+suffix

xx   = Temp
mini = 0
maxi = 3
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS Mid closed loops',filename='QS-Mid-Tm'+suffix

xx=([NR12_loop_c,NR12_loop_c]/1.e8)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 dat [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='Qs Mid closed loops',filename='QS-Mid-Ndat'+suffix

xx=([Ner21_loop_c,Ner22_loop_c]/1.e8)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N 1.075 fit [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',tit='Qs Mid closed loops',filename='Qs-Mid-Nfit'+suffix

iin=where(TmR12_loop_c gt 0.5 and TmR12_loop_c gt 0.5) 
xx=([TmR12_loop_c(iin),TmR12_loop_c(iin)]/1.e6)>0
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N 1.075 [MK]',ytit='Frequency Histogram',tit='QS Mid closed loops',filename='QS-Mid-TmR'+suffix

;xx1=Ner11_loop_c/1.e8
;xx2=Ner21_loop_c/1.e8
;xx3=Ner31_loop_c/1.e8
;mini=0.;min(xx1)
;maxi=3.;max(xx1)                                                                                                                                                      
;histoplotNe3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ne [10!U8!N cm!U-3!N]',ytit='Frequency Histogram',$
;tit='QS Mid closed loops',filename='QS-Mid-NeR'+suffix

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

;=================================================================================================================================================================
