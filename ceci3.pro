
;pegatina

 r2Tcuadr1_loop = r2T1_loop
 r2Tcuadr2_loop = r2T2_loop

suffix = '-2099-Tlinear-ceci-v2'
r2crit_Er = 0.9
r2crit_Tcuad = 0.5


; loops chicos con pies en el rango de latitud [-20,+20]
i=where(abs(lat1_loop) le 20. AND abs(lat2_loop) le 20. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. and lon1_loop ge 100. and lon1_loop le 300. and Tmmean_loop le 2.e6 AND r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='low-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
xx=xx>(-10.)<(10.)
mini=min(xx)*2
maxi=max(xx)*2
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
xx=xx>(-10.)<(10.)
mini=min(xx)*2
maxi=max(xx)*2
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Phih'+suffix
window,0
plot,L_loop_c,xx,psym=3,th=4


; loops chicos con pies en el rango de latitud [+20,+90]
i=where(lat1_loop gt 20. and lat2_loop gt 20. AND r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND Tmmean_loop gt 1.75e6 AND r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-N-hot-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North Hot closed small loops '+suffix,filename='mid-lat-N-hot-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
xx=xx>(-10.)<(10.)
mini=min(xx)*2
maxi=max(xx)*2
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North Hot closed small loops '+suffix,filename='mid-lat-N-hot-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
xx=xx>(-10.)<(10.)
mini=min(xx)*2
maxi=max(xx)*2
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North Hot closed small loops '+suffix,filename='mid-lat-N-hot-small_histo_Phih'+suffix

stop


xx=(abs(dTmds1_loop_c)+abs(dTmds1_loop_c))/1.e5
xx=xx>(-10.)<(10.)
mini=min(xx)*2
maxi=max(xx)*2
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North Hot closed small loops '+suffix,filename='mid-lat-N-hot-small_histo_Phih'+suffix

window,1
plot,L_loop_c,xx,psym=3,th=4

; Analisis de localizacion espacial de los loops de arriba donde ademas
; Fcb es muy grande.
i=where(lat1_loop gt 20. and lat2_loop gt 20. AND r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND Tmmean_loop gt 1.75e6 AND r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad and $
        Fcb_loop/1.e5 gt 10.)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-N-hot-small-FcLarge'+suffix,indexloop_c,rotacion

xx=Fcb_loop_c/1.e5
xx=xx>(-10.)<(10.)
mini=min(xx)*2
maxi=max(xx)*2
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North Hot closed small loops '+suffix,filename='mid-lat-N-hot-small_histo_Fc'+suffix

stop

; Todos loops chicos Tmmean gt 1.6e6 
i=where(r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND Tmmean_loop gt 1.6e6 AND r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='all-lat-hot-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='All latitude Hot closed small loops '+suffix,filename='all-lat-hot-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='All latitude Hot closed small loops '+suffix,filename='all-lat-hot-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='All latitude Hot closed small loops '+suffix,filename='all-lat-hot-small_histo_Phih'+suffix
window,2
plot,L_loop_c,xx,psym=3,th=4

; Todos loops chicos Tmmean lt 1.6e6 
i=where(r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND Tmmean_loop lt 1.6e6 AND r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='all-lat-cold-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='All latitude Cold closed small loops '+suffix,filename='all-lat-cold-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='All latitude Cold closed small loops '+suffix,filename='all-lat-cold-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='All latitude Cold closed small loops '+suffix,filename='all-lat-cold-small_histo_Phih'+suffix
window,3
plot,L_loop_c,xx,psym=3,th=4


