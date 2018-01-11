;pegatina                                                                                                                                           
suffix = '-2099-Tlinear-Ce-AIA4'
r2crit_Er = 0.9
r2crit_T  = 0.5


;Loop en lon [30.120] lat le 0
; i = where(Ner01_loop gt Ne_basal_crit AND Ner02_loop gt Ne_basal_crit and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er)
 i = where(lon1_loop le 120. AND lon1_loop ge 30. AND lon2_loop le 120. AND lon2_loop ge 30. AND lat1_loop le 0. AND lat2_loop le 0.)
filter_loop,i

nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-low-reg'+suffix,indexloop_c,rotacion

xx1=Ner11_loop_c/1.e8
xx2=Ner21_loop_c/1.e8
xx3=Ner31_loop_c/1.e8

mini=0.;min(xx1)
maxi=3.;max(xx1)                                                                                                                                                       

histoplot3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ner [B1.035,G1.075,R1.115]',ytit='Frequency Histogram',tit='QS reg low closed small loops',filename='QS-lowreg--Ner-035-075-115'+suffix

;Loop en lat ge 40
; i = where(Ner01_loop gt Ne_basal_crit AND Ner02_loop gt Ne_basal_crit and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er)
 i = where(lat1_loop ge 45. AND lat2_loop ge 45.)
filter_loop,i

nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-mid-reg'+suffix,indexloop_c,rotacion

xx1=Ner11_loop_c/1.e8
xx2=Ner21_loop_c/1.e8
xx3=Ner31_loop_c/1.e8

mini=0.;min(xx1)
maxi=3.;max(xx1)                                                                                                                                                       

histoplot3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ner [B1.035,G1.075,R1.115]',ytit='Frequency Histogram',tit='QS mid reg closed small loops',filename='QS-mid-reg-Ner-035-075-115'+suffix

Ne_basal_crit = 1.4e8

;Loop en lat [10.40] lon ge 60
 i = where(lat1_loop le 40. AND lat1_loop ge 10. AND lat2_loop le 40. AND lat2_loop ge 10. AND lon1_loop ge 60. AND lon2_loop ge 60.)
filter_loop,i

nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='AR-mid-reg'+suffix,indexloop_c,rotacion

xx1=Ner11_loop_c/1.e8
xx2=Ner21_loop_c/1.e8
xx3=Ner31_loop_c/1.e8

mini=0.;min(xx1)
maxi=3.;max(xx1)                                                                                                                                                       

histoplot3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ner [B1.035,G1.075,R1.115]',ytit='Frequency Histogram',tit='AR reg closed small loops',filename='AR-reg-Ner-035-075-115'+suffix

;QS low
 i = where(abs(lat1_loop) le 30. AND abs(lat2_loop) le 30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND $
            Phir_loop gt 0. AND r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T and Ner21_loop/1.e8 le 1.4 and Ner22_loop/1.e8 le 1.4)

filter_loop,i

nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-low'+suffix,indexloop_c,rotacion

mini=-1.;min(xx)    
maxi=3.;max(xx)  

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1                    
maxi=4                                                                                                                                     
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',$
tit='QS low closed small loops',filename='QS-low-Phis'+suffix

;QS mid
 i = where(abs(lat1_loop) ge 30. AND abs(lat2_loop) ge 30. and  r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND $
            Phir_loop gt 0. AND  r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T and  Ner21_loop/1.e8 le 1.4 and Ner22_loop/1.e8 le 1.4)

filter_loop,i

nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='QS-mid'+suffix,indexloop_c,rotacion

mini=-1.;min(xx)    
maxi=3.;max(xx)  

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1                    
maxi=4                                                                                                                                     
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',$
tit='QS mid closed small loops',filename='QS-mid-Phis'+suffix

;xx1=Ner11_loop_c/1.e8
;xx2=Ner21_loop_c/1.e8
;xx3=Ner31_loop_c/1.e8
;mini=0.;min(xx1)
;maxi=3.;max(xx1)                                                                                                                                                      
;histoplot3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ner [B1.035,G1.075,R1.115]',ytit='Frequency Histogram',tit='QS low closed small loops',filename='QS-low-Ner-035-075-115'+suffix

;AR
 i = where(Ner21_loop/1.e8 ge 1.4 and Ner22_loop/1.e8 ge 1.4)
filter_loop,i

nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='AR'+suffix,indexloop_c,rotacion

;xx1=Ner11_loop_c/1.e8
;xx2=Ner21_loop_c/1.e8
;xx3=Ner31_loop_c/1.e8
;mini=0.;min(xx1)
;maxi=3.;max(xx1)                                                                                                                                                      
;histoplot3,xx1,xx2,xx3,min=mini,max=maxi,nbins=nbins,xtit='Ner [B1.035,G1.075,R1.115]',ytit='Frequency Histogram',tit='QS low closed small loops',filename='QS-low-Ner-035-075-115'+suffix

stop

; loops chicos con pies en el rango de latitud [-25,+25,100,300]
 i=where(lat1_loop le 25. AND lat1_loop ge -25. AND lat2_loop le 25. AND lat2_loop ge -25. AND lon1_loop ge 100. AND lon1_loop le 300. AND lon2_loop ge 100. $
         AND lon2_loop le 300. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)

filter_loop,i
nbins=80

footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='low-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)    
maxi=3.;max(xx)  

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)                    
maxi=4.;max(xx)                                                                                                                                     
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',$
tit='Low latitude closed small loops',filename='low-lat-Phis'+suffix

; loops chicos con pies en el rango de [-25,-80] 
i=where(lat1_loop le -25. and lat2_loop le -25. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er $ 
        AND Phir_loop gt 0. AND r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)

filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-L-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                                            
maxi=4.;max(xx)   

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)
maxi=4.;max(xx)                                                                                                                                                       
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='mid L latitude closed small loops',filename='mid-L-lat-Phis'+suffix


; loops chicos con pies en el rango de [40,90]
i=where(lat1_loop ge 40. and lat2_loop ge 40. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er $ 
        AND Phir_loop gt 0. AND r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)

filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-H-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                                            
maxi=4.;max(xx)   

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)
maxi=4.;max(xx)                                                                                                                                                       
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!9f!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='mid H latitude closed small loops',filename='mid-H-lat-Phis'+suffix

