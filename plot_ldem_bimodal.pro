
;Cooler/Lighter Cell
; plot_ldem_bimodal,7,30,0

;Hotter/Denser Cell
; plot_ldem_bimodal,7,47,36

pro celdas

; Radius: 1.075 Rsun
irad = 7

; Longitude: 70 deg
ilon = 35

; Latitude: [-59,-49,-39,-29,-19,-9,1] deg
indlat = [15,20,25,30,35,40,45]

filename = 'LDEM-'+['60','50','40','30','20','10','0']+'.gif'

for i=0,n_elements(indlat)-1 do plot_ldem_bimodal,irad,indlat(i),ilon,filename(i)

return
end

pro plot_ldem_bimodal,irad,ilat,ilon,output_file
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common results_tomo,tfbe,sfbe,N_e
  common demc_extra,demc2,demc3
  common fixed_width,sigma
  common each_gaussian,fdema,fdemb,demc1
  common fixed_parameter_equalizer,xmax,sigma_v,demv
; array de strings con los nombres de los archivos de AIA-4/G2  
     filesAIA4=['ldem_cr2099_AIA4_base.dat',$
                'ldem_cr2099_AIA4_hhhl.oreg.dat',$
                'ldem_cr2099_AIA4_hhhl.ureg.dat',$
                'ldem_cr2099_AIA4_hhlh.oreg.dat',$
                'ldem_cr2099_AIA4_hhlh.ureg.dat',$
                'ldem_cr2099_AIA4_hhll.oreg.dat',$
                'ldem_cr2099_AIA4_hhll.ureg.dat',$
                'ldem_cr2099_AIA4_lhhh.oreg.dat',$
                'ldem_cr2099_AIA4_lhhh.ureg.dat',$
                'ldem_cr2099_AIA4_lhhl.oreg.dat',$
                'ldem_cr2099_AIA4_lhhl.ureg.dat',$
                'ldem_cr2099_AIA4_lhlh.oreg.dat',$
                'ldem_cr2099_AIA4_lhlh.ureg.dat',$
                'ldem_cr2099_AIA4_lhll.oreg.dat',$
                'ldem_cr2099_AIA4_lhll.ureg.dat',$
                'ldem_cr2099_AIA4_hlhh.oreg.dat',$
                'ldem_cr2099_AIA4_hlhh.ureg.dat',$
                'ldem_cr2099_AIA4_hlhl.oreg.dat',$
                'ldem_cr2099_AIA4_hlhl.ureg.dat',$
                'ldem_cr2099_AIA4_hllh.oreg.dat',$
                'ldem_cr2099_AIA4_hllh.ureg.dat',$
                'ldem_cr2099_AIA4_hlll.oreg.dat',$
                'ldem_cr2099_AIA4_hlll.ureg.dat',$
                'ldem_cr2099_AIA4_llhh.oreg.dat',$
                'ldem_cr2099_AIA4_llhh.ureg.dat',$
                'ldem_cr2099_AIA4_llhl.oreg.dat',$
                'ldem_cr2099_AIA4_llhl.ureg.dat',$
                'ldem_cr2099_AIA4_lllh.oreg.dat',$
                'ldem_cr2099_AIA4_lllh.ureg.dat']
     nfiles=n_elements(filesAIA4)

     TTmin=0.5*1.e6
     TTmax=15.0*1.e6
     nTbins=825                
     DT_L = (TTmax-TTmin) / float(nTbins)
     T_L = TTmin + DT_L /2. + DT_L *findgen(nTbins)
     XX = T_L/TTmin
     t_x= TTmin*XX       

     Ne1_array=fltarr(nfiles)
     Ne2_array=fltarr(nfiles)
     Tm1_array=fltarr(nfiles)
     Tm2_array=fltarr(nfiles)
     WT1_array=fltarr(nfiles)
       R_array=fltarr(nfiles)
  lambda_array=fltarr(nfiles,5)
   demc1_array=fltarr(nfiles)
   demc2_array=fltarr(nfiles)
    ldem_array=fltarr(nfiles,nTbins)


        file =filesAIA4(0)
        read_ldem_v2,filesAIA4(0),/ldem,/dgfw
        make_Nej_v2,irad,Neg1,Neg2,Tmg1,Tmg2,WTg1,WTg2
        if Neg1(irad,ilat,ilon) ge 1.4e8 then begin
            print,'This is probably an AR. Select a more quiet voxel.'
            close,/all
            stop
        endif

     FOR i=0,nfiles-1 DO BEGIN
        print,i
        file =filesAIA4(i)   
        read_ldem_v2,file,/ldem,/dgfw
        scoreR = total(abs(1.-sfbe/tfbe)    , 4) / float(nband)
        make_Nej_v2,irad,Neg1,Neg2,Tmg1,Tmg2,WTg1,WTg2
        lambda=reform(lambda(irad,ilat,ilon,*))
        DEMC1=DEMC( irad,ilat,ilon)
        DEMC2=DEMC2(irad,ilat,ilon)
        ldem=DEMc1*fdem(xx)/Tmin

        Ne1_array(i)=Neg1(irad,ilat,ilon)
        Ne2_array(i)=Neg2(irad,ilat,ilon)
        Tm1_array(i)=Tmg1(irad,ilat,ilon)
        Tm2_array(i)=Tmg2(irad,ilat,ilon)
        WT1_array(i)=WTg1(irad,ilat,ilon)
          R_array(i)=scoreR(irad,ilat,ilon)
     lambda_array(i,*)=reform(lambda)
      demc1_array(i)  =DEMC1
      demc2_array(i)  =DEMC2
       ldem_array(i,*)=ldem

     ENDFOR

    ; Select which files to use.    
    Rcrit   = 0.10
    WT1crit = 2.e4
    p=where(R_array le Rcrit AND WT1_array gt WT1crit )
    Np=n_elements(p)
    @grafico
    @statistics

stop
     return
  end
     
 

