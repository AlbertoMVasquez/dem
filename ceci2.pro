
;pegatina

suffix = '-2081-ceci-v2'
r2crit_Er    = 0.9
r2crit_Tcuad = 0.6

; loops chicos con pies en el rango de latitud [-30,+30]
;i=where(abs(lat1_loop) le 30. AND abs(lat2_loop) le 30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0.)
i=where(abs(lat1_loop) le 30. AND abs(lat2_loop) le 30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='low-lat-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Phih'+suffix

window,0
plot,L_loop_c,xx,psym=3,th=4

; loops chicos con pies en el rango de latitud [-90,-30]
i=where(lat1_loop le -30. AND lat2_loop le -30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-S-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops '+suffix,filename='mid-lat-S-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops '+suffix,filename='mid-lat-S-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude South closed small loops '+suffix,filename='mid-lat-S-small_histo_Phih'+suffix

window,1
plot,L_loop_c,xx,psym=3,th=4

; loops chicos con pies en el rango de latitud [+30,+90]
i=where(lat1_loop ge +30. AND lat2_loop ge +30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        r2Tcuadr1_loop ge r2crit_Tcuad AND r2Tcuadr2_loop ge r2crit_Tcuad)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='mid-lat-N-small'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops '+suffix,filename='mid-lat-N-small_histo_Phir'+suffix
xx=Fcb_loop_c/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='F!Dc!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops '+suffix,filename='mid-lat-N-small_histo_Fc'+suffix
xx=(Phir_loop_c-Fcb_loop_c)/1.e5
mini=min(xx)
maxi=max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dh!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Mid latitude North closed small loops '+suffix,filename='mid-lat-N-small_histo_Phih'+suffix
window,2
plot,L_loop_c,xx,psym=3,th=4

stop

; loops chicos con pies en el rango de latitud [+30,+90] y de longitud [100,200]
i=where(lat1_loop ge +30. AND lat2_loop ge +30. and r2Er1_loop ge r2crit_Er AND r2Er2_loop ge r2crit_Er AND Phir_loop gt 0. AND $
        lon1_loop ge 100. AND lon1_loop le +200. AND lon2_loop ge 100. AND lon2_loop le +200.)
filter_loop,i
footpoint_map,[lat1_loop_c,lat2_loop_c],[lon1_loop_c,lon2_loop_c],[opclstat_loop_c,opclstat_loop_c],filelabel='region1'+suffix,indexloop_c,rotacion
xx=Phir_loop_c/1.e5
mini=0.;min(xx)
maxi=3.;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=100,xtit='!4U!3!Dr!N [10!U5!N erg cm!U-2!N sec!U-1!N]',ytit='Frequency Histogram',tit='Region 1 '+suffix,filename='region1_histo_Phir'+suffix
window,2
plot,L_loop_c,xx,psym=3,th=4
