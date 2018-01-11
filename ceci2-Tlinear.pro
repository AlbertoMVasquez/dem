;pegatina                                                                  

suffix = '-2081-Tlinear-ceci-v2'
r2crit_Er = 0.9
r2crit_T  = 0.5

;========================================================================================================================================================
; loops chicos con pies en el rango de latitud [-30,+30]                                                                                                               
;i=where(abs(lat1_loop) le 30. AND abs(lat2_loop) le 30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0.)                                  
 i=where(abs(lat1_loop) le 30. AND abs(lat2_loop) le 30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)
filter_loop,i
nbins=80
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='low-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                                                         
maxi=3.;max(xx)                                                                                                                         
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=-1
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=-1.
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Phih'+suffix
window,0
plot,L_loop_c,xx,psym=3,th=4

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)                                                                                                                         
maxi=4.;max(xx)                                                                                                                         
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!4U!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops',filename='low-lat-small_histo_Phis'

xx=(Tmmean_loop_c)/1.e6
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Low latitude closed small loops'+suffix,filename='low-lat-small_histo_Tm'+suffix
xx=[Ner01_loop_c,Ner02_loop_c]/1.e8
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops'+suffix,filename='low-lat-small_histo_Ne_r0'+suffix
xx=(L_loop_c)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Low latitude closed small loops'+suffix,filename='low-lat-small_histo_L'+suffix

; loops chicos con pies en el rango de latitud [-90,-30]                                                                                
i=where(lat1_loop le -30. AND lat2_loop le -30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)
filter_loop,i
;nbins=50
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-S-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                                                         
maxi=4.;max(xx)                                                                                                                         
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops '+suffix,filename='mid-lat-S-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=-1.
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops '+suffix,filename='mid-lat-S-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=-1.
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops '+suffix,filename='mid-lat-S-small_histo_Phih'+suffix
window,1
plot,L_loop_c,xx,psym=3,th=4

xx=(Tmmean_loop_c)/1.e6
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops'+suffix,filename='Mid-lat-S-small_histo_Tm'+suffix
xx=[Ner01_loop_c,Ner02_loop_c]/1.e8
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops'+suffix,filename='Mid-lat-S-small_histo_Ne_r0'+suffix
xx=(L_loop_c)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops'+suffix,filename='Mid-lat-S-small_histo_L'+suffix

; loops chicos con pies en el rango de latitud [+30,+90]                                                                                
i=where(lat1_loop ge +30. AND lat2_loop ge +30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-N-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                                                         
maxi=4.;max(xx)                                                                                                                         
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops '+suffix,filename='mid-lat-N-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=-1.
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops '+suffix,filename='mid-lat-N-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=-1
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops '+suffix,filename='mid-lat-N-small_histo_Phih'+suffix
window,2
plot,L_loop_c,xx,psym=3,th=4

xx=(Tmmean_loop_c)/1.e6
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops'+suffix,filename='Mid-lat-N-small_histo_Tm'+suffix
xx=[Ner01_loop_c,Ner02_loop_c]/1.e8
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops'+suffix,filename='Mid-lat-N-small_histo_Ne_r0'+suffix
xx=(L_loop_c)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops'+suffix,filename='Mid-lat-N-small_histo_L'+suffix

; loops chicos con pies en el rango de ABS(latitud) [+30,+90]                                                                                
i=where(abs(lat1_loop) ge +30. AND abs(lat2_loop) ge +30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2T1_loop ge r2crit_T AND r2T2_loop ge r2crit_T)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                                                         
maxi=4.;max(xx)                                                                                                                         
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude closed small loops '+suffix,filename='mid-lat-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=-1.
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude closed small loops '+suffix,filename='mid-lat-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=-1.
maxi=4.
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude closed small loops '+suffix,filename='mid-lat-small_histo_Phih'+suffix
window,2
plot,L_loop_c,xx,psym=3,th=4

xxr=Phir_loop_c/1.e5
xxf=Fcb_loop_c/1.e5
xxh=xxr-xxf
mini=-1.;min(xx)                                                                                                                         
maxi=4.;max(xx)                                                                                                                         
histoplot3,xxr,xxf,xxh,min=mini,max=maxi,nbins=nbins,xtit='!4U!3 [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude closed small loops ',filename='mid-lat-small_histo_Phis'

xx=(Tmmean_loop_c)/1.e6
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Mid latitude closed small loops'+suffix,filename='Mid-lat-small_histo_Tm'+suffix
xx=[Ner01_loop_c,Ner02_loop_c]/1.e8
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Mid latitude closed small loops'+suffix,filename='Mid-lat-small_histo_Ne_r0'+suffix
xx=(L_loop_c)
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [Rsun]',ytit='Frequency Histogram',tit='Mid latitude closed small loops'+suffix,filename='Mid-lat-small_histo_L'+suffix

stop

; loops chicos con pies en el rango de latitud [+30,+90] y de longitud [100,200]                      
i=where(lat1_loop ge +30. AND lat2_loop ge +30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        lon1_loop ge 100. AND lon1_loop le +200. AND lon2_loop ge 100. AND lon2_loop le +200.)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='region1'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=-1.;min(xx)                                                                                       
maxi=4.;max(xx)                                                                                       
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Region 1 '+suffix,filename='region1_histo_Phir'+suffix
window,2
plot,L_loop_c,xx,psym=3,th=4

