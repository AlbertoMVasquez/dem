;============================= MID LAT

suffix    = '-EUVI-2099-Mid-def-5e-8-lt9e10'
R2Crit_Er = 0.8
R2Crit_Ne = 0.75
R2Crit_T  = 0.5
FTscrit   = 0.70
NeCritA   = 1.4
NeCritQ   = 1.4
Phihfilt = 5/1.e8
rsun      = 6.955e10

ff=1.

 i = where(Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit $
           and    (abs(dTmds1_ts_loop)*rsun*abs(Smaxxx1_loop - Sminnn1_loop) gt ff*(2*eplegT1_loop)        $
           or      abs(dTmds2_ts_loop)*rsun*abs(Smaxxx2_loop - Sminnn2_loop) gt ff*(2*eplegT2_loop) )      $
           and ((dTmds1_ts_loop gt 0. and dTmds2_ts_loop gt 0.) or (dTmds1_ts_loop lt 0. and dTmds2_ts_loop lt 0.)) $ 
           and  Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. )

filter_loop,i
nbins=80

iin=where(Fcb_loop_c/1.e5 gt -1.1)
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)
   L_filt =    L_loop_c(iin)
   L_cm   = L_filt*rsun

   Phih_filt = Phir_filt-Fcb_filt
   Phih_L    = Phih_filt/L_cm

  Tmmean1 = Tmmean_loop_c(iin)/1.e6
  N0mean1 =  Ner0m_loop_c(iin)/1.e8

   Tmmean = Tmmean1(where(Phih_L gt 0.000002))
   N0mean = N0mean1(where(Phih_L gt 0.000002))

 readcol,'Loops_2099-mid_5e-8-lt9e10.dat',Long_mod,Phih_mod,Tm_mod,Ne_mod,P_mod

yy=N0mean
zz=Ne_mod/1.e8
Nef = yy/zz

N0_filtrada = N0mean(where(Nef lt 10))
Nm_filtrad  = Ne_mod(where(Nef lt 10))
Nm_filtrada = Nm_filtrad/1.e8

Nef2 = N0_filtrada/Nm_filtrada
mini = 0.
maxi = 20.

histoplot,Nef2,min=mini,max=maxi,nbins=nbins,xtit='N!D0!N/N!Debtel!N',ytit='Frequency Histogram',tit='N!D0!N/N!Debtel!N',filename='QS-Ne_cociente_filtrado'+suffix

yy=Tmmean
zz=Tm_mod/1.e6
Tmf = yy/zz

Tmmean_filt = yy(where(Tmf lt 2.25))
Tm_mod_filt = zz(where(Tmf lt 2.25)) 

Tmf2 = Tmmean_filt/Tm_mod_filt
mini = 0.
maxi = 3. ;max(yy)                                                                                                                                                                                                                           
histoplot,Tmf2,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N/T!Debtel!N',ytit='Frequency Histogram',tit='T!Dm!N/T!Debtel!N',filename='QS-Tm_cociente'+suffix

;============================= LOW LAT


suffix    = '-EUVI-2099-Low-def-1e-8-lt9e10'
R2Crit_Er = 0.8
R2Crit_Ne = 0.75
R2Crit_T  = 0.5
FTscrit   = 0.70
NeCritA   = 1.4
NeCritQ   = 1.4
Phihfilt = 5/1.e8
rsun      = 6.955e10

ff=1.

 i = where(Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit $
           and    (abs(dTmds1_ts_loop)*rsun*abs(Smaxxx1_loop - Sminnn1_loop) gt ff*(2*eplegT1_loop)        $
           or      abs(dTmds2_ts_loop)*rsun*abs(Smaxxx2_loop - Sminnn2_loop) gt ff*(2*eplegT2_loop) )      $
           and ((dTmds1_ts_loop gt 0. and dTmds2_ts_loop gt 0.) or (dTmds1_ts_loop lt 0. and dTmds2_ts_loop lt 0.)) $ 
           and  Abs(lat1_loop) lt 30. and Abs(lat2_loop) lt 30. )

filter_loop,i
nbins=80

iin=where(Fcb_loop_c/1.e5 gt -1.1)
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)
   L_filt =    L_loop_c(iin)
   L_cm   = L_filt*rsun

   Phih_filt = Phir_filt-Fcb_filt
   Phih_L    = Phih_filt/L_cm

  Tmmean1 = Tmmean_loop_c(iin)/1.e6
  N0mean1 =  Ner0m_loop_c(iin)/1.e8

   Tmmean = Tmmean1(where(Phih_L gt 0.000002))
   N0mean = N0mean1(where(Phih_L gt 0.000002))

 readcol,'Loops_2099-low_5e-8-lt9e10.dat',Long_mod,Phih_mod,Tm_mod,Ne_mod,P_mod

yy=N0mean
zz=Ne_mod/1.e8
Nef = yy/zz

N0_filtrada = N0mean(where(Nef lt 10))
Nm_filtrad  = Ne_mod(where(Nef lt 10))
Nm_filtrada = Nm_filtrad/1.e8

Nef2 = N0_filtrada/Nm_filtrada
mini = 0.
maxi = 20.

histoplot,Nef2,min=mini,max=maxi,nbins=nbins,xtit='N!D0!N/N!Debtel!N',ytit='Frequency Histogram',tit='N!D0!N/N!Debtel!N',filename='QS-Ne_cociente_filtrado'+suffix

yy=Tmmean
zz=Tm_mod/1.e6
Tmf = yy/zz

Tmmean_filt = yy(where(Tmf lt 2.25))
Tm_mod_filt = zz(where(Tmf lt 2.25)) 

Tmf2 = Tmmean_filt/Tm_mod_filt
mini = 0.
maxi = 3. ;max(yy)                                                                                                                                                                                                                          

histoplot,Tmf2,min=mini,max=maxi,nbins=nbins,xtit='T!Dm!N/T!Debtel!N',ytit='Frequency Histogram',tit='T!Dm!N/T!Debtel!N',filename='QS-Tm_cociente'+suffix

