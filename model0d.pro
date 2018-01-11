;suffix    = '-EUVI-2081'
suffix    = '-AIA-2099'
R2Crit_Er = 0.8
R2Crit_T  = 0.5
NeCritA   = 1.4
NeCritQ   = 1.4

Phihfilt = 5/1.e8
;========================LOW
 i = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and  Phir_loop gt 0. and Phih_L gt Phihfilt $
           and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)

filter_loop,i
nbins=80

iin=where(Fcb_loop_c/1.e5 gt -1.1)

Tmmean = Tmmean_loop_c(iin)/1.e6
Nemean = Nemean_loop_c(iin)/1.e8
Long_cm = Long_cm_c(iin)

xx   = Tmmean
mini = 0.
maxi = 3. ;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS Low closed loops',filename='QS-Low-Tm'+suffix

xx   = Nemean
mini = -1.
maxi = 3. ;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Ne QS Low closed loops',filename='QS-Low-Ne'+suffix

;readcol,'loops_low_2081.dat',Long_mod,Phih_mod,Tm_mod,Ne_mod,P_mod
 readcol,'loops_low_2099.dat',Long_mod,Phih_mod,Tm_mod,Ne_mod,P_mod

;match,long_cm,long_mod,suba,subb,COUNT=count
;stop
;writecol,'/data/work/dem/long_comp_Low_2081.txt',Long_cm,Long_mod 

xx   = Ne_mod/1.e8
mini = -1.
maxi = 3. ;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N mod [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Ne mod QS Low closed loops',filename='QS-Low-Ne_mod'+suffix

xx   = Tm_mod/1.e6
mini = 0.
maxi = 3. ;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N mod [MK]',ytit='Frequency Histogram',tit='Tm mod QS Low closed loops',filename='QS-Low-Tm_mod'+suffix

yy=Tmmean
zz=Tm_mod/1.e6
Tmf = yy/zz
mini = 0.
maxi = 3. ;max(yy)
histoplot,Tmf,min=mini,max=maxi,nbins=nbins,xtit='Tmmean/Tm_mod',ytit='Frequency Histogram',tit='Tmmean/Tm_mod Low',filename='QS-Low-Tm_cociente'+suffix

yy=Nemean
zz=Ne_mod/1.e8
Nef = yy/zz
mini = 0.
maxi = 10. ;max(yy)
histoplot,Nef,min=mini,max=maxi,nbins=nbins,xtit='Nemean/Ne_mod',ytit='Frequency Histogram',tit='Nemean/Ne_mod Low',filename='QS-Low-Ne_cociente'+suffix


;========================Mid
Phihfilt = 5/1.e8

 i = where(Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. and  Phir_loop gt 0. and Phih_L gt Phihfilt $
           and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T)

filter_loop,i
nbins=80

iin=where(Fcb_loop_c/1.e5 gt -1.1)

Tmmean = Tmmean_loop_c(iin)/1.e6
Nemean = Nemean_loop_c(iin)/1.e8
Long_cm = Long_cm_c(iin)

xx   = Tmmean
mini = 0.
maxi = 3. ;2
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N [MK]',ytit='Frequency Histogram',tit='Tm QS Mid closed loops',filename='QS-Mid-Tm'+suffix

xx   = Nemean
mini = -1.
maxi = 3. ;2.5
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Ne QS Mid closed loops',filename='QS-Mid-Ne'+suffix

;readcol,'loops_mid_2081.dat',Long_mod,Phih_mod,Tm_mod,Ne_mod,P_mod
 readcol,'loops_mid_2099.dat',Long_mod,Phih_mod,Tm_mod,Ne_mod,P_mod
;writecol,'/data/work/dem/long_comp_Mid_2081.txt',Long_cm,Long_mod 

xx   = Ne_mod/1.e8
mini = -1.
maxi = 3. ;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='N!De!N mod [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Ne mod QS Mid closed loops',filename='QS-Mid-Ne_mod'+suffix

xx   = Tm_mod/1.e6
mini = 0.
maxi = 3. ;max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N mod [MK]',ytit='Frequency Histogram',tit='Tm mod QS Mid closed loops',filename='QS-Mid-Tm_mod'+suffix

yy=Tmmean
zz=Tm_mod/1.e6
Tmf = yy/zz
mini = 0.
maxi = 3. ;max(yy)
histoplot,Tmf,min=mini,max=maxi,nbins=nbins,xtit='Tmmean/Tm_mod',ytit='Frequency Histogram',tit='Tmmean/Tm_mod Mid',filename='QS-Mid-Tm_cociente'+suffix

yy=Nemean
zz=Ne_mod/1.e8
Nef = yy/zz
mini = 0.
maxi = 10. ;max(yy)
histoplot,Nef,min=mini,max=maxi,nbins=nbins,xtit='Nemean/Ne_mod',ytit='Frequency Histogram',tit='Nemean/Ne_mod Mid',filename='QS-Mid-Ne_cociente'+suffix

