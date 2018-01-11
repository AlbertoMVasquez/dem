;pegatina                                               
suffix    = '-EUVI-2081-r2N075-FTm070-nuevocrit'
R2Crit_Er = 0.8
R2Crit_Ne = 0.75
R2Crit_T  = 0.5
FTscrit   = 0.70
NeCritA   = 1.4
NeCritQ   = 1.4

; i = where(Phir_loop gt 0. $
;           and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
;           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne $
;           and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit)

;filter_loop,i
;Rpoint_updown,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],[Fcb_loop_c,Fcb_loop_c],filelabel='QS-Low-CECI'+suffix,indexloop_c,rotacion

;REGIONES 
;=============== Low Lat QS
 i = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and  Phir_loop gt 0. $
           and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $ 
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne $
           and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit)
;          and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er $

filter_loop,i
nbins=80
;======================================================================= UBICACIÓN LOOPS =========================================================
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Low-CECI'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Low-CECI'+suffix,indexloop_c,rotacion
;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)

Long_filt =   Long_cm_c(iin)
Phih_filt =    Phih_L_c(iin)

xxr = Phir_filt/1.e5
xxf = -Fcb_filt/1.e5
xxh=xxr+xxf
mini=-2.;min(xx)                    
maxi=5.;max(xx)                                                                                                                                     
histoplotNe3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frecuency Histogram',$
tit='QS Low closed loops',filename='QS-Low-Phis-CECI'+suffix

;writecol,'/data/work/dem/Phi_Long_Low_2081.txt',Long_filt,Phih_L_c

;=================================================================================================================================================================
;=============== Mid Lat QS
 i = where(Abs(lat1_loop) gt 30. and Abs(lat2_loop) gt 30. and  Phir_loop gt 0. $ 
           and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne $
           and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit)
;and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)
;          and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er $

filter_loop,i
nbins=80
;======================================================================= UBICACIÓN LOOPS =========================================================
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Mid-CECI'+suffix,indexloop_c,rotacion
Rpoint_map,[Rp_lat1_loop_c,Rp_lat2_loop_c],[Rp_lon1_loop_c,Rp_lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-Mid-CECI'+suffix,indexloop_c,rotacion
;========================================================== FLUJOS ===========================================================================
iin=where(Fcb_loop_c/1.e5 gt -1.1) 
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)

Long_filt =   Long_cm_c(iin)
Phih_filt =    Phih_L_c(iin)

xxr = Phir_filt/1.e5
xxf = -Fcb_filt/1.e5
xxh=xxr+xxf
mini=-1.;min(xx)                    
maxi=3.;max(xx)                                                                                                                                     
histoplotNe3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frecuency Histogram',$
tit='QS Mid closed loops',filename='QS-Mid-Phis-CECI'+suffix

;writecol,'/data/work/dem/Phi_long_Mid_2081.txt',Long_filt,Phih_L_c

;=================================================================================================================================================================
