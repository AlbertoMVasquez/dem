pro lala
common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon


;filesT = ['traceLDEM_CR2081_euviA_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat']
filesT = ['traceLDEM_CR2099_euviA_reg1.0_safety0.5_grid0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat']
radstart = 1.075
rstart      = [1.045,1.075,1.115,1.035,1.045,1.075,1.115,1.155,1.250,1.500]
  statloop,filesT,rloopmin=1.05,/linear;,/fitcuadr

stop

return 
end


pro hacer_trace

; Ceci & Albert:
fdips_file='fdips_field_151x180x360_synop_Mr_0.polfil.2081.dat'
 fdips_file='fdips_field_150x180x360_synop_Mr_0.polfil.2081.ubdat'
ldem_file ='LDEM.v2_cr2081_l0.75_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.A_L171_gauss1_lin_Norm-median_singlStart'
   period ='2081_euviA-nodecon_reg0.75__safety1.0'
 radstart = 1.075
trace_LDEM,fdips_file=fdips_file,ldem_file=ldem_file,period=period,radstart=radstart,safety=1.0,stepmax=15000,/unifgrid

return
end

pro trace_LDEM,fdips_file=fdips_file,$
               ldem_file=ldem_file,$
               period=period,$
               outputfile=outputfile,$
               marcgrid=marcgrid,$
               unifgrid=unifgrid,$
               radstart=radstart,$
               spacing=spacing,$
               fieldtype=fieldtype,$
               safety=safety,$
               stepmax=stepmax,$
               box=box,$
               dlat=dlat,$
               dlon=dlon,$
               mhd=mhd,$
               dgfw=dgfw
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common results_tomo,tfbe,sfbe,N_e
  common loss_rate,Er
  common structure ,sph_data
  common structure2,pfss_data 
;+
; PURPOSE: 
; To trace tomographic results along magnetic field lines. 
; To sample the LDEM results that are already traced along magnetic fieldlines,
; keeping only one data point per tomographic cell (voxel), chosen as the median
; point of all the points that are located within each voxel.
;
; INPUTS:
; fdips_file = a file containing a FDIPS output.
; ldem_file  = a file containing a LDEM output.
; period = a string containing the Carrington rotation number.
; outputfile = not in use, output filename is currently created within the code.
; radstart = height [Rsun] where to start to trace, both inwards and outwards.
; spacing, fieldtype, safety = various keywords needed if /marcgrid is used, see
;                              comments in "spherical_field_start_coord.pro".
; box = [lon1,lat1,lon2,lat2] is the angular box where the starting
; points are to be located. If not set the default is full corona.
; nlat,nlon = needed if /unifgrid is used. Default values are: 90,180.
;
; KEYWORDS:
; /unifgrid = set up a uniform angular grid for the starting points.
; /marcgrid = use Marc's tools starting points routine.
; /dgfw     = set up if the double normal LDEM parametrization is used.
;
; OUTPUTS:
; A file containing the results, to be afterwards used as INPUT by the routine
; "sample_traced_ldem". Modificar

;
; FUTURE CHANGES: 
; Make it capable of using magnetic models other than FDIPS.
;
; HISTORY:
; Created by F.A. Nuevo & A.M. Vasquez.
;-

  if not keyword_set(fdips_file) then begin
     print,'set the PFSS model to trace the DEMT results'
     return
  endif
  if not keyword_set(ldem_file ) then begin
     print,'set a DEMT file to be traced'
     return
  endif
  if not keyword_set(period)     then begin
     print,'set a string with the period'
     return
  endif
; Add radstart suffix to the output filename:
  period=period+'_radstart-'+strmid(string(radstart),6,5)+'Rs' 

; Make the output filename:
  if keyword_set(marcgrid) then suffix='_marcgrid'  
  if keyword_set(unifgrid) then suffix='_unifgrid'
  output_file='traceLDEM_CR'+period+suffix+'.heating.sampled.v2.dat'

; Set parameters for Marc's line-tracing routines:
  if NOT keyword_set(radstart ) then radstart  = 1.5
  if NOT keyword_set(safety   ) then safety    = 0.2
  if NOT keyword_set(stepmax  ) then stepmax   = 30000
  if NOT keyword_set(fieldtype) then fieldtype = 5.0
  if NOT keyword_set(spacing)   then spacing   = 2.0

  print,'-------------------------------------------'
  print,'     Period: ',period
  print,'  LDEM file: ',ldem_file
  print,' FDIPS file: ',fdips_file
  print,'Output file: ',output_file
  print,'-------------------------------------------'

  print,'safety: ',safety
  print,'stepmax: ',stepmax


; Set the FDIPS filename to read:
; PFSSM_model='/data1/DATA/PFSSM/'+fdips_file
  PFSSM_model= fdips_file
; Read the FDIPS model and create a structure to serve as input to Marc's routines:
  if not keyword_set(mhd)  then create_structure    ,PFSSM_model
  if     keyword_set(mhd)  then create_structure_MHD,'/data1/DATA/MHD_SWMF/'+fdips_file
; change the name of the created structure to a new name:
  pfss_data = sph_data

; Set the uniform grid size, in case /unifgrid is used for the starting points. 
; Default size is 90x180.
  if NOT keyword_set(dlat) then dlat = 2   
  if NOT keyword_set(dlon) then dlon = 2

; If BOX was not set use the full corona:
  if NOT keyword_set(box)  then box = [0.,-90,360.,+90.] 
  box=float(box)

; Set up the starting points:
  if keyword_set(marcgrid) then spherical_field_start_coord,pfss_data,fieldtype,spacing,radstart=radstart,bbox=box
  if keyword_set(unifgrid) then sph_field_str_coord_unifang,pfss_data,dlat,dlon        ,radstart=radstart,bbox=box

; And now, do trace the field lines:
  spherical_trace_field,pfss_data,linekind=linekind,linelengths=linelengths,safety=safety,stepmax=stepmax 

; Change the coding for linekind:
  linekind=linekind-2           ; so that 0=open and 1=closed

; Number of total traced lines:
  Nlin_all   = (size(*pfss_data.ptr))(2)
; Maximum number of points along those lines:
  Nptmax     = (size(*pfss_data.ptr))(1)
; Select field lines that were identified as open or closed by
  iOC        = where ( linekind eq 0 or linekind eq 1)
; Number of selected field lines (open or closed)
  Nlin     = n_elements(iOC) 
  print,'there are '+string(Nlin)+' of '+string(Nlin_all)+' field lines that are within  the boundaries'
; only keep the values of the array for selected field lines (open or
; closed) 
;---------------------------------------------------------------
  loopL       = linelengths       (iOC) ; N_lineas
  opcls       = linekind          (iOC) ; N_lineas
  nstep       = (*pfss_data.nstep)(iOC) ; N_lineas
  str_v       = (*pfss_data.str)  (iOC) ; N_lineas
  stth_v      = (*pfss_data.stth) (iOC) ; N_lineas  
  stph_v      = (*pfss_data.stph) (iOC) ; N_lineas  
;-----------------------------------------------------------

; Read the tomographics results and set a few parameters concerning
; the tomographic grid:
  if not keyword_set(dgfw) then $
     read_ldem,ldem_file,/ldem,/gauss1
  if     keyword_set(dgfw) then $
     read_ldem,ldem_file,/ldem,/dgfw
  dr_tom = rad(1)-rad(0)        ; grid radial bin size
  Rmax_tom = rad(nr-3)          ; maximum height for which LDEM was computed

; Compute the Chisqr for quality-selection purposes:
  ratio = sfbe/tfbe
  chisqr=total( (1.-ratio)^2 , 4 ) / float(nband)


  Nptmax_v = 150                ; ESTO NO ES ROBUSTO, 
;  sin embargo, por la experiencia de haber realizado varios trazados
;  creo que va funcionar. 
;  Ningun sampleo supera este valor de puntos por linea

; Generate the tomographic grid based results
; one data point per tomographic voxel crossed by line.
  Ne_v  = fltarr(Nptmax_v,Nlin)
  Tm_v  = fltarr(Nptmax_v,Nlin)
  WT_v  = fltarr(Nptmax_v,Nlin)
  Er_v  = fltarr(Nptmax_v,Nlin)
  chisqr_v  = fltarr(Nptmax_v,Nlin) 
  rad_v  = fltarr(Nptmax_v,Nlin)
  lat_v  = fltarr(Nptmax_v,Nlin)
  lon_v  = fltarr(Nptmax_v,Nlin)
  s_v  = fltarr(Nptmax_v,Nlin)
  B_v  = fltarr(Nptmax_v,Nlin)
  Br_v  = fltarr(Nptmax_v,Nlin)
  Bth_v  = fltarr(Nptmax_v,Nlin)
  Bph_v  = fltarr(Nptmax_v,Nlin)
  midCell_v = fltarr(Nlin) - 666.
  Npts_v = fltarr(Nlin) - 666. 
  enrad_v = 0. * str_v - 555.
  enlon_v = 0. * str_v - 555.
  enlat_v = 0. * str_v - 555.
  
; The following double-loop traces the tomographic results along the
; selected field lines:

  for il = 0, Nlin-1 do begin
     print,'tracing the DEMT results along the line '+string(il+1)+'    of '+string(Nlin)
     il_all=(findgen(Nlin_all))(iOC(il))
     Np_l      = Nstep(il)      ;  Number of points along the il-line
     
; Build more arrays:  
     s_l      = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     Ne_l      = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     Tm_l      = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     WT_l      = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     Er_l      = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.  
     Chisqr_l  = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     Br_l     = fltarr(Np_l) -666.  ;fltarr(Nptmax,Nlin) -666.
     Bth_l     = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     Bph_l     = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
     B_l     = fltarr(Np_l) -666.   ;fltarr(Nptmax,Nlin) -666.
     lab_l     = fltarr(Np_l) -666. ;fltarr(Nptmax,Nlin) -666.
        
; These next five arrays are futures implementacion
  ;Happix    = fltarr(Np_l) -666.
  ;Bappix    = fltarr(Np_l) -666.
  ;Bmean     = fltarr(Np_l) -666.
  ;Bfoot1    = fltarr(Np_l) -666.
  ;Bfoot2    = fltarr(Np_p) -666. 
     
   rad_l  = reform((*pfss_data.ptr) (0:Np_l-1,il_all))
    th_l  = reform((*pfss_data.ptth)(0:Np_l-1,il_all))
   lat_l = 90. - th_l / !dtor
    ph_l  = reform((*pfss_data.ptph)(0:Np_l-1,il_all))
; Make sure ph_l is in the range [0,2pi]:
  iphneg  = where( ph_l lt 0.    ) & if iphneg(0) ne -1 then ph_l(iphneg)=ph_l(iphneg)+2.*!pi  
  iph5th  = where( ph_l gt 2.*!pi) & if iph5th(0) ne -1 then ph_l(iph5th)=ph_l(iph5th)-2.*!pi
   lon_l  = ph_l/!dtor
      xl  = rad_l * sin(th_l) * cos(ph_l)
      yl  = rad_l * sin(th_l) * sin(ph_l)
      zl  = rad_l * cos(th_l)
     ds2l = (xl(1:Np_l-1)-xl(0:Np_l-2))^2 + $
            (yl(1:Np_l-1)-yl(0:Np_l-2))^2 + $
            (zl(1:Np_l-1)-zl(0:Np_l-2))^2 
     ds2l = [0,ds2l]
;-------------AGREGADO---------------------------------------
; Store coordinates of ending point of all OPEN field lines
if opcls(il) eq 0 then begin
enrad_v(il)=rad_l(Np_l-1)
enlat_v(il)=lat_l(Np_l-1)
enlon_v(il)=lon_l(Np_l-1)
endif 
;-----------------------------------------------------------
;
  for ir = 0,Np_l-1 do begin
      s_l (ir) = total(sqrt(ds2l(0:ir)))
      r0 = rad_l (ir)
     th0 =  th_l(ir)
     ph0 =  ph_l(ir)
     ptc = [r0,th0,ph0]
     irc =get_interpolation_index(*pfss_data.rix,ptc(0))
     ithc=get_interpolation_index(*pfss_data.lat,90-ptc(1)*!radeg)
     iphc=get_interpolation_index(*pfss_data.lon,(ptc(2)*!radeg+360) mod 360)
      Brc=interpolate(*pfss_data.Br ,iphc,ithc,irc)
     Bthc=interpolate(*pfss_data.Bth,iphc,ithc,irc)
     Bphc=interpolate(*pfss_data.Bph,iphc,ithc,irc)  
     
     if r0 eq max(rad)+dr_tom/2 then r0=r0*(1.-1.e-5)
     if r0 lt max(rad)+dr_tom/2 then begin
      determindex,r0,th0,ph0,irad,ilat,ilon

      if irad ne -1 and ilon ne -1 and ilat ne -1 then begin
         lab_l(ir) = (Nth*Np)*irad+(Np)*ilat+ilon+1 ; Voxel label
         Ne_l(ir) = N_e(irad,ilat,ilon)
         Tm_l(ir) = Tm (irad,ilat,ilon)
         WT_l(ir) = WT (irad,ilat,ilon)
         Er_l(ir) = Er (irad,ilat,ilon)
         chisqr_l(ir) =chisqr (irad,ilat,ilon) 
         Br_l(ir) = Brc
         Bth_l(ir) = Bthc
         Bph_l(ir) = Bphc
         B_l(ir) = sqrt(Brc^2 + Bthc^2 + Bphc^2) 
      endif 
   endif
  endfor                        ; closes radial loop



; beginning the sampled
;--------------------------------------------------------------------------------------------------

if rad_l(0) gt 2.4 then stop

        line_end = 0
        ivox     = 0
        is       = 0 
        lab0     = lab_l(is)
        while line_end eq 0 do begin
           np=0
           while lab_l(is) eq lab0 do begin
              np=np+1        
              is=is+1
               if lab0 eq -666. or is-1 eq Np_l-1 then goto,nextvoxel
               ;if lab0 eq -666.                   then goto,nextvoxel 
;this line was modified 2013-08-16 by F.A.N. there was not robust enough
 
           endwhile
           ; "np" should be now the number of points within voxel "lab0" 
           ; "is" should be now the index of the 1st point in the NEXT voxel  
           index=is-np+indgen(np)
           ; "index" should be now the 1-D index of all points in
           ; voxel lab0, to be used in arrays *_l
           ind = (median(index))(0)
              Ne_v(ivox,il) = Ne_l(ind)
              Tm_v(ivox,il) = Tm_l(ind)
              WT_v(ivox,il) = WT_l(ind)
              Er_v(ivox,il) = Er_l(ind)
              chisqr_v(ivox,il) = chisqr_l(ind)
           if np mod 2 eq 1 then begin  ; does this if np=odd 
             rad_v(ivox,il) = rad_l(ind)
             lat_v(ivox,il) = lat_l(ind)
             lon_v(ivox,il) = lon_l(ind)
               s_v(ivox,il) =   s_l(ind)
               B_v(ivox,il) =   B_l(ind)
              Br_v(ivox,il) =   Br_l(ind) 
             Bth_v(ivox,il) =   Bth_l(ind)
             Bph_v(ivox,il) =   Bph_l(ind)     
           endif else begin             ; does this if np=even 
             rad_v(ivox,il) = 0.5*(rad_l(ind)+rad_l(ind-1))
             lat_v(ivox,il) = 0.5*(lat_l(ind)+lat_l(ind-1))
             lon_v(ivox,il) = 0.5*(lon_l(ind)+lon_l(ind-1))
               s_v(ivox,il) = 0.5*(  s_l(ind)+  s_l(ind-1))
               B_v(ivox,il) = 0.5*(  B_l(ind)+  B_l(ind-1))
              Br_v(ivox,il) = 0.5*(  Br_l(ind)+  Br_l(ind-1))
             Bth_v(ivox,il) = 0.5*(  Bth_l(ind)+  Bth_l(ind-1))
             Bph_v(ivox,il) = 0.5*(  Bph_l(ind)+  Bph_l(ind-1))   
           endelse
           ; If this voxel is the maximum of the tom grid
           ; but there are MORE points traced => it is a closed
           ; loop with appex > Rmax_tom. If so, store midCell_v.
           ; Also, do it only if midCell_v was not stored already.
           ; See in Zhenguang's Matlab code that this midCell_v 
           ; is same as his.
           ; Rmax_tom = 1.255 
           ; dr_tom = 0.01
           if (rad_l(ind) ge Rmax_tom - dr_tom/2) AND (max(index) lt Np_l-1) AND opcls(il) eq 1. then midCell_v(il) = ivox
           ivox = ivox+1 ; increase ivox 
           nextvoxel:
            if is-1 eq Np_l-1 then line_end = 1
            if is-1 lt Np_l-1 then     lab0 = lab_l(is)          

         endwhile; closes each line's while
        Npts_v(il) = ivox; For each line, record the number of points in vector
; Label small-closed-loops as "2"
   if opcls(il) eq 1. and midCell_v(il) eq -666. then begin
      midcell_v(il) = where ( rad_v(0:npts_v(il)-1,il) eq max (rad_v(0:npts_v(il)-1,il)))
          opcls(il) = 2.
       endif

;end the sampled
;-------------------------------------------------------------------------------------------
;il=il+1
;endif
endfor; closes lines loop



; Trim all unnecesary information from resulting arrays:
 Npts_max = max(Npts_v)
    Ne_v  = reform(     Ne_v(0:Npts_max-1,*) ) 
    Tm_v  = reform(     Tm_v(0:Npts_max-1,*) ) 
    WT_v  = reform(     WT_v(0:Npts_max-1,*) )
    Er_v  = reform(     Er_v(0:Npts_max-1,*) )
chisqr_v  = reform( chisqr_v(0:Npts_max-1,*) ) 
   rad_v  = reform(    rad_v(0:Npts_max-1,*) ) 
   lat_v  = reform(    lat_v(0:Npts_max-1,*) ) 
   lon_v  = reform(    lon_v(0:Npts_max-1,*) ) 
     s_v  = reform(      s_v(0:Npts_max-1,*) )
     B_v  = reform(      B_v(0:Npts_max-1,*) )
    Br_v  = reform(     Br_v(0:Npts_max-1,*) )
   Bth_v  = reform(    Bth_v(0:Npts_max-1,*) )
   Bph_v  = reform(    Bph_v(0:Npts_max-1,*) )  
; Save the sampled data:
      openw,1,output_file
   writeu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
   writeu,1,Nlin,Npts_max
   writeu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
   writeu,1,Ne_v,Tm_v,WT_v,Er_v,chisqr_v
   writeu,1,str_v,stth_v,stph_v
   writeu,1,B_v,Br_v,Bth_v,Bph_v   
;------AGREGADO--------------
   writeu,1,enrad_v,enlon_v,enlat_v
;----------------------------
  close,  1
return
end


pro determindex,r0,th0,ph0,irad,ilat,ilon
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common structure2,pfss_data
 ; Purpose: Given the position vector coordinates (r0,th0,ph0)
 ;          find the 3 1D-indexes of the tomographic grid cell
 ;          that contains that position.
 drad = rad(1) - rad (0)
 dlat = lat(1) - lat (0)
 dlon = lon(1) - lon (0)   
 rad0 = r0
 lat0 = 90 - th0/!dtor
 lon0 =      ph0/!dtor
 irad = (where(rad0 ge rad-drad/2 AND rad0 lt rad+drad/2))(0) 
 ilat = (where(lat0 ge lat-dlat/2 AND lat0 lt lat+dlat/2))(0) 
 ilon = (where(lon0 ge lon-dlon/2 AND lon0 lt lon+dlon/2))(0)                
 return
end

pro read_trace_sampled,file
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,chisqr_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   
  common B_sampled,B_v,Br_v,Bth_v,Bph_v
  common opclstatus,opcls,loopL,WTc  
;+
; PURPOSE:
; To read the output of the routine "trace_LDEM"
;-
  Nlin=0L      
  Npts_max=0.
  fieldtype=0.
  spacing =0. 
  radstart=0.
  Rmax_tom=0.
  dr_tom=0.
  WTc=0.
  dir='/data1/DATA/MLDT/'
  openr,1,dir+file
  readu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
  readu,1,Nlin,Npts_max
 
  Ne_v = fltarr(Npts_max,Nlin)
  Tm_v = fltarr(Npts_max,Nlin)
  WT_v = fltarr(Npts_max,Nlin)
  Er_v = fltarr(Npts_max,Nlin)
  chisqr_v = fltarr(Npts_max,Nlin)
  rad_v = fltarr(Npts_max,Nlin)
  lat_v = fltarr(Npts_max,Nlin)
  lon_v = fltarr(Npts_max,Nlin)
  s_v = fltarr(Npts_max,Nlin)
  B_v = fltarr(Npts_max,Nlin)
  Br_v = fltarr(Npts_max,Nlin)
  Bth_v = fltarr(Npts_max,Nlin)
  Bph_v = fltarr(Npts_max,Nlin)

  midCell_v = fltarr(Nlin) 
  Npts_v = fltarr(Nlin) 
  opcls = intarr(Nlin) 
  Loopl = fltarr(Nlin) 
  str_v   = fltarr(Nlin)   
  stth_v  = dblarr(Nlin)   
  stph_v  = dblarr(Nlin)    

  readu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
  readu,1,Ne_v,Tm_v,WT_v,Er_v,chisqr_v
  readu,1,str_v,stth_v,stph_v 
  readu,1,B_v,Br_v,Bth_v,Bph_v     
;------AGREGADO--------------
  enrad_v = fltarr(Nlin)   
  enlon_v = fltarr(Nlin)   
  enlat_v = fltarr(Nlin)  
;------AGREGADO--------------
;readu,1,enrad_v,enlon_v,enlat_v
;----------------------------
  close,1
  return
end

;make_mapoc,'traceLDEM_CR1914_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1914_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR1915_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1915_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR1919_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1919_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR2081_euviA_l1.0_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR2081_90X180blines_r_',1.075,/mdi

pro MAKE_MAPOC,file_input,filesuffix,rc,mdi=mdi,gng=gng
common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,chisqr_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

dir='/data1/DATA/PFSSM/'

 if keyword_set(mdi) then postsuffix='_MDI.dat'
 if keyword_set(gng) then postsuffix='_GNG.dat'

 stringheight=strmid(string(rc),6,5)
 file_output=filesuffix+stringheight+'_open-close-map'+postsuffix
 print,'-----> O/C: '+file_output
 
read_trace_sampled,file_input

stth = stth_v
stph = stph_v

nlat= 90
nlon=180
dlat=180./nlat
dlon=360./nlon
latmin = -90.
lonmin =   0.
lat = latmin + dlat /2. + dlat * findgen(nlat)
lon = lonmin + dlon /2. + dlon * findgen(nlon)

stlon = round(    stph/!dtor)*1.
stlat = round(90.-stth/!dtor)*1.

N = n_elements(stlon)

mapoc = fltarr(1,nlat,nlon)

for i=0,N-1 do begin
  ilat = where(lat eq stlat(i))
  ilon = where(lon eq stlon(i))
  mapoc(0,ilat,ilon) = opcls(i)
endfor

;quedo: 0=abierto o indeterminado
;       1=cerrado

;cambiar al final a:  (open = 10) y (closed = 0.1)

iopen   = where ( mapoc eq 0 )
iclosed = where ( mapoc eq 1 or mapoc eq 2)

mapoc(iopen)   = 10
mapoc(iclosed) = 0.1

 openw,1,dir+file_output
 writeu,1,mapoc
 close,1

return
end






pro statloop,file,rmin=rmin,rmax=rmax,rloopmin=rloopmin,linear=linear,fitcuadr=fitcuadr
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,chisqr_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   
  common B_sampled,B_v,Br_v,Bth_v,Bph_v
  common opclstatus,opcls,loopL,WTc  



common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon 
common angle_box,rad_ini,rad_fin,lat_ini,lat_fin,lon_ini,lon_fin

device, retain     = 2
device, true_color = 24
device, decomposed = 0

; Physical constants needed for the HS fits:
  rsun = 6.955e10    ; cm
  gsun = 2.74e4      ; cm/sec²
    kB = 1.38e-16    ; erg/K
    mH = 1.6726e-24  ; gr
     a = 0.08        ; N_He / N_H
    mu = (1.+4.*a)/(1.+2.*a)
    bb = (1.+2.*a)/(2.+3.*a)
 kappa = 9.2e-7      ; erg s ^-1 cm ^-1 K ^-7/2 

if not keyword_set(rmin) then rmin = 1.03 
if not keyword_set(rmax) then rmax = 1.20
if not keyword_set(rloopmin) then rloopmin = 1.07
rminloop=rloopmin

; Read the sampled tomographic traced data
read_trace_sampled,file

Nloop = n_elements(loopL)

index0 = where(opcls eq 0.)
index1 = where(opcls eq 1.)
index2 = where(opcls eq 2.)

Nloop0 = n_elements(index0) & if index0(0) eq -1 then Nloop0=0
Nloop1 = n_elements(index1) & if index1(0) eq -1 then Nloop1=0
Nloop2 = n_elements(index2) & if index2(0) eq -1 then Nloop2=0

if Nloop0 + Nloop1 + Nloop2 ne Nloop then stop

Nlegs = Nloop0 + 2*Nloop1 + 2*Nloop2 

; Mean and standard desviation values for each leg
 Nemean  = fltarr(Nlegs)-555.
 Tmmean  = fltarr(Nlegs)-555.
 WTmean  = fltarr(Nlegs)-555.
Nestddev = fltarr(Nlegs)-555.
Tmstddev = fltarr(Nlegs)-555.
WTstddev = fltarr(Nlegs)-555.
; HS fits results for each leg
     Ne0 = fltarr(Nlegs)-555.

    Ner0 = fltarr(Nlegs)-555.

    Ner1 = fltarr(Nlegs)-555.
    Ner2 = fltarr(Nlegs)-555.
    Ner3 = fltarr(Nlegs)-555.

lambda_N = fltarr(Nlegs)-555.
   Tefit = fltarr(Nlegs)-555.
     r2N = fltarr(Nlegs)-555.
      P0 = fltarr(Nlegs)-555.
lambda_P = fltarr(Nlegs)-555.
     r2P = fltarr(Nlegs)-555.
   gradT = fltarr(Nlegs)-555.
     Tm0 = fltarr(Nlegs)-555.  ; esto es r

    Tm0s = fltarr(Nlegs)-555.  ; en cada pierna. esto es s

;========================TS

   FTr_ts = fltarr(Nlegs)-555.
 gradT_ts = fltarr(Nlegs)-555.
   Tm0_ts = fltarr(Nlegs)-555.  ; esto es r
   FTs_ts = fltarr(Nlegs)-555.
 dTmds_ts = fltarr(Nlegs)-555.
  Tm0s_ts = fltarr(Nlegs)-555.  ; en cada pierna. esto es s

;==========================

     r2T = fltarr(Nlegs)-555.
   dTmds = fltarr(Nlegs)-555.
  Tmbase = fltarr(Nlegs)-555.

    TmR1 = fltarr(Nlegs)-555.  ;Temperatura a 1.075 ================
     NR1 = fltarr(Nlegs)-555.  ;Densidad    a 1.075 ================

    r2Ts = fltarr(Nlegs)-555.
    r2Er = fltarr(Nlegs)-555.
r2Tcuadr = fltarr(Nlegs)-555.
Acuadr_a = fltarr(Nlegs,3)-555.
  s_r0_a = fltarr(Nlegs)-555.
      Eh = fltarr(Nlegs)-555.

    Phir = fltarr(Nlegs)-963.  ; flujo radiativo
     Fcb = fltarr(Nlegs)-963.  ; Fc en la base
 Phirfit = fltarr(Nlegs)-963.  ; flujo radiativo

 deltaEh = fltarr(Nlegs)-555.
      sH = fltarr(Nlegs)-555.
    r2sH = fltarr(Nlegs)-555.
betamean = fltarr(Nlegs)-555.
betaapex = fltarr(Nlegs)-555.
   Bmean = fltarr(Nlegs)-555.
     Br0 = fltarr(Nlegs)-555. ;only applied for open lines 

; opclstat=0. if loop is open, opclstat=1. if closed large; opclstat=2 if closed small. 
opclstat  = fltarr(Nlegs)-555.
indexloop = fltarr(Nlegs)-555.
; The following arrays will contain the LOOP length (in Rsun) to which each LEG belongs: 
loop_length=fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of 
; the starting point used for the LOOP to which the LEG belongs:
strad = fltarr(Nlegs)-555.
stlat = fltarr(Nlegs)-555.
stlon = fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of 
; the initial and final points of the LOOP to which each LEG belongs:
rad_ini = fltarr(Nlegs)-555.
lat_ini = fltarr(Nlegs)-555.
lon_ini = fltarr(Nlegs)-555.
rad_fin = fltarr(Nlegs)-555.
lat_fin = fltarr(Nlegs)-555.
lon_fin = fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of 
; the FOOT-POINT of each LEG:
footrad = fltarr(Nlegs)-555.
footlat = fltarr(Nlegs)-555.
footlon = fltarr(Nlegs)-555.

;=======================================
;Rp0_rad = fltarr(Nlegs)-555.
;Rp0_lat = fltarr(Nlegs)-555.
;Rp0_lon = fltarr(Nlegs)-555.

 Rp_rad = fltarr(Nlegs)-555.
 Rp_lat = fltarr(Nlegs)-555.
 Rp_lon = fltarr(Nlegs)-555.
;=======================================

; This array will code the "STATUS of LEG" so that:
; leg_status=1 if leg contains the starting point used for the loop,
; leg_status=2 if not.
  leg_status = fltarr(Nlegs) + 1. 

; Initialize ileg index:
  ileg = 0L


; Define minimum number of data points required by leg
  Ndata=5

; Start analysis of each loop
  for il=0L,Nloop-1 do begin

;stop

; Analysis for OPEN loops:
  if opcls(il) eq 0. then begin

     Ne_l = reform ( Ne_v(0:Npts_v(il)-1,il))
     Tm_l = reform ( Tm_v(0:Npts_v(il)-1,il))
     WT_l = reform ( WT_v(0:Npts_v(il)-1,il))
     Er_l = reform ( Er_v(0:Npts_v(il)-1,il))
; scoreR_l = reform ( scoreR_v(0:Npts_v(il)-1,il)) 
    rad_l = reform (rad_v(0:Npts_v(il)-1,il))
    lat_l = reform (lat_v(0:Npts_v(il)-1,il))
    lon_l = reform (lon_v(0:Npts_v(il)-1,il))
      s_l = reform (  s_v(0:Npts_v(il)-1,il))   
      B_l = reform (  B_v(0:Npts_v(il)-1,il))   
     Br_l = reform ( Br_v(0:Npts_v(il)-1,il))   

;if lat_v(0,il) gt -75 then stop
 
    rad_ini(ileg) = rad_l(0)
    lat_ini(ileg) = lat_l(0)
    lon_ini(ileg) = lon_l(0)
        Br0(ileg) =  Br_l(0)
;----------------------------------AGREGADO----------------
    rad_fin(ileg) = enrad_v(il)
    lat_fin(ileg) = enlat_v(il) 
    lon_fin(ileg) = enlon_v(il)
;------------------------------------------------------------ 
    footrad(ileg) = rad_ini(ileg)
    footlat(ileg) = lat_ini(ileg)
    footlon(ileg) = lon_ini(ileg)
;------------------------------------------------------------   

;    rrr=findel(1.075,rad_l)
;    Rp0_rad(ileg) = rad_l(rrr)
;    Rp0_lat(ileg) = lat_l(rrr)
;    Rp0_lon(ileg) = lon_l(rrr)

;     Rp_rad(ileg) = Rp0_rad(ileg)
;     Rp_lat(ileg) = Rp0_lat(ileg)
;     Rp_lon(ileg) = Rp0_lon(ileg)

   ;Select useful data
    p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. );and scoreR_l lt 0.10)

    if p(0) eq -1 then goto,skipnextloop_open

     Ne_l =  Ne_l (p)
     Tm_l =  Tm_l (p)
     WT_l =  WT_l (p)
     Er_l =  Er_l (p) 
    rad_l = rad_l (p)
    lat_l = lat_l (p)
    lon_l = lon_l (p)
      s_l =   s_l (p)
      B_l =   B_l (p)    
    ;make tomographic pressure
    p_l   = kB/bb *Ne_l*Tm_l 
    ;make Beta plasma parameter
   beta_l = p_l/(B_l^2/(8*!pi))

;    rr1=findel(1.075,rad_l)     ;=======================================
;        TmR1(ileg) = Tm_l(rr1)  
;         NR1(ileg) = Ne_l(rr1)  

      Nemean(ileg) =   mean(Ne_l)
      Tmmean(ileg) =   mean(Tm_l)
      WTmean(ileg) =   mean(WT_l)
    Nestddev(ileg) = stddev(Ne_l)
    Tmstddev(ileg) = stddev(Tm_l)
    WTstddev(ileg) = stddev(WT_l)
    betamean(ileg) =   mean(beta_l)
       Bmean(ileg) =   mean(B_l)

    if n_elements(p) lt Ndata then goto,skipfitloop_open

    rrr1 = 1.10
    rrr2 = 1.16
    rrr3 = 1.23
    if min(rad_l) gt rrr1 OR median(rad_l) lt rrr1 OR median(rad_l) gt rrr2 OR max(rad_l) lt rrr2 then goto,skipfitloop_open


   ;Make HS-fit to Ne(r) for each open leg/loop
      xfit = rad_l
      yfit =  Ne_l
    rminhs = min(rad_l);rmin
    rmaxhs = max(rad_l);rmax 

if n_elements(xfit) ne n_elements(yfit) then stop

        if      keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
        if not  keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2
    
         Ne0(ileg) = A[0] 
    lambda_N(ileg) = 1./A[1]  ; Rsun
              Tfit = mu * mH * gsun * (lambda_N(ileg)*rsun) / kB
       Tefit(ileg) = bb*Tfit
         r2N(ileg) = corr2


    ;Make HS-fit to p(r) for each open leg/loop    
      yfit = p_l
   
        if      keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
        if not  keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2

          p0(ileg) = a[0] 
    lambda_p(ileg) = 1./A[1]  ; Rsun
         r2P(ileg) = corr2

   ;Make LINEAR-fit to T(r) for each open leg/loop
           yfit = Tm_l
             ep = 0.15
 
if n_elements(xfit) ne n_elements(yfit) then stop

    fit_ts,xfit,yfit,ep,a1,b1,F
;    fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2
;           T0r = A[0]
;      Tm0(ileg) = T0r
;    gradT(ileg) = A[1]
;      r2T(ileg) = corr2 

           T0r_ts = b1
     Tm0_ts(ileg) = T0r_ts
   gradT_ts(ileg) = a1
     FTr_ts(ileg) = F 

    ;Make LINEAR-fit to T(s) for each open leg/loop ======================
    
           xfit = s_l*rsun    
         rminhs = min(s_l*rsun);smin
         rmaxhs = max(s_l*rsun);smax 
           yfit = Tm_l
             ep = 0.15
 
    fit_ts,xfit,yfit,ep,a1,b1,F
 ;   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2
 ;           T0s = A[0] ; K
 ;    Tm0s(ileg) = T0s  
 ;   dTmds(ileg) = A[1] ; K/cm
 ;    r2Ts(ileg) = corr2 

            T0s_ts = b1 ; K
     Tm0s_ts(ileg) = T0s_ts  
    dTmds_ts(ileg) = a1 ; K/cm
      FTs_ts(ileg) = F 

;   ->STOP
;   if  r2N(ileg) lt 0.5  then  r2N(ileg) = -555.
;   if  r2P(ileg) lt 0.5  then  r2P(ileg) = -555.
    if  r2T(ileg) lt 0.5  then  r2T(ileg) = -555.
    if r2Ts(ileg) lt 0.5  then r2Ts(ileg) = -555.
    if r2Ts(ileg) lt 0.5  then goto,skiptestloopopen

      s_l = s_l * rsun ; pass s_l to cm 

; make dT/ds(s), asumiendo que ese gradiente es constante: 
  dTmds_l = dTmds (ileg)  ; DERIV(s_l,Tm_l) esto permite que el gradiente varie, pero es ruidoso.

; make conductive flux 

     ;Fc_l= -kappa*Tm_l^(5./2)*dTmds_l
      Fc_l = -kappa*(T0s +dTmds_l*s_l)^(5./2)*dTmds_l 

; make conductive loss  ;======================================================
     Ec_l = B_l * DERIV(s_l,Fc_l/B_l)   
; make heating rate  =============== 
     Eh_l = Er_l + Ec_l   
      s_l = s_l / rsun ; back s_l to Rsun 

; test diferent models of heating rate    
   Eh(ileg)= mean(Eh_l)
     index = where( Eh_l gt 0) 
   
   if n_elements(index) lt n_elements(Eh_l) then Eh(ileg) = -555.
      
   deltaEh(ileg) = stdev(Eh_l)
            smin = min(s_l)
            smax = max(s_l)
   fitEh,s_l,Eh_l,smin,smax,A,r2

       sH (ileg) = A[1]
      r2sH(ileg) = r2

;..........................................
goto,skiptestloopopen
wn=0
 window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 X=1.+.25*findgen(100)/99.

 plot,rad_l,ne_l/1.e8,psym=4,title='Ne'
 oplot,X,Ne0(ileg)* exp( -(1/lambda_N(ileg))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(Ne0(ileg)/1.e8),string(lambda_N(ileg)),string(r2n(ileg))],/normal

 plot,rad_l,p_l,psym=4,title='P'
 oplot,X,p0(ileg)* exp( -(1/lambda_p(ileg))*(1.-1./X) )
 xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.15,.2],[string(P0(ileg)),string(lambda_p(ileg)),string(r2p(ileg))],/normal

  plot,rad_l,tm_l/1.e6,psym=4,title='Tm'
  oplot,X,(T0r+gradT(ileg)*X)/1.e6
  xyouts,0.5*[1,1,1],(1./3)*1.-[.15,.2,.25],[string(T0r/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal

wn=1

 window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1

  X=1.+.25*findgen(100)/99. 
  
  plot,rad_l,tm_l/1.e6,psym=4,title='Tm(r)'
  oplot,X,(T0r+gradT(ileg)*X)/1.e6
  xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(T0r/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal

  x = min(s_l) + (max(s_l)-min(s_l))*findgen(100)/99
  
  plot,s_l,tm_l/1.e6,psym=4,title='Tm(s)'
  oplot,X,(T0s+dTmds(ileg)*rsun*X)/1.e6
  xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.15,.2],[string(T0s/1.e6),string(dTmds(ileg)/1.e6*rsun),string(r2Ts(ileg))],/normal
  
  plot,s_l,Fc_l,psym=4,title='Fc'

wn=2

 window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=3
 !p.charthick=1
 
 plot,rad_l,Er_l,psym=4,title='radiative loss'
 plot,rad_l,Ec_l,psym=4,title='conductive loss'
 plot,rad_l,Eh_l,psym=4,title='heating rate'
 
stop
skiptestloopopen:
;...........................................

   skipfitloop_open:
       opclstat(ileg) = opcls(il)   
    loop_length(ileg) = loopL(il)
          strad(ileg) = str_v(il)
          stlat(ileg) = (90-stth_v(il)/!dtor)
          stlon(ileg) = stph_v(il)/!dtor
   skipnextloop_open:
   indexloop(ileg) = il 
              ileg = ileg+1

 endif else begin 

; Analysis closed loops:
  if max(rad_v(0:Npts_v(il)-1,il)) lt rminloop then goto,skipnextloop

;<---------------------------------------                     
  if opcls(il) eq 2 then begin
      ifirs_1 = 0
      ilast_1 = midcell_v(il)
      ifirs_2 = midcell_v(il)
      ilast_2 = Npts_v(il)-1
  endif
  if opcls(il) eq 1 then begin
      ifirs_1 = 0
      ilast_1 = midcell_v(il)-1
      while Ne_v(ilast_1,il) eq -666. do ilast_1=ilast_1-1
      ifirs_2 = midcell_v(il)
      ilast_2 = Npts_v(il)-1
  endif
 ;<-----------------------------------------
 
  Ne_l1 = reform ( Ne_v(ifirs_1:ilast_1,il))
  Ne_l2 = reform ( Ne_v(ifirs_2:ilast_2,il))
  Tm_l1 = reform ( Tm_v(ifirs_1:ilast_1,il))
  Tm_l2 = reform ( Tm_v(ifirs_2:ilast_2,il))
  WT_l1 = reform ( WT_v(ifirs_1:ilast_1,il))
  WT_l2 = reform ( WT_v(ifirs_2:ilast_2,il))
  Er_l1 = reform ( Er_v(ifirs_1:ilast_1,il))
  Er_l2 = reform ( Er_v(ifirs_2:ilast_2,il))
;  scoreR_l1 = reform ( scoreR_v(ifirs_1:ilast_1,il))
;  scoreR_l2 = reform ( scoreR_v(ifirs_2:ilast_2,il))
  rad_l1 = reform( rad_v(ifirs_1:ilast_1,il))
  rad_l2 = reform( rad_v(ifirs_2:ilast_2,il))
  lat_l1 = reform( lat_v(ifirs_1:ilast_1,il))
  lat_l2 = reform( lat_v(ifirs_2:ilast_2,il))
  lon_l1 = reform( lon_v(ifirs_1:ilast_1,il))
  lon_l2 = reform( lon_v(ifirs_2:ilast_2,il))
    s_l1 = reform(   s_v(ifirs_1:ilast_1,il))
    s_l2 = loopL(il) - reform(   s_v(ifirs_2:ilast_2,il))
    B_l1 = reform(   B_v(ifirs_1:ilast_1,il))
    B_l2 = reform(   B_v(ifirs_2:ilast_2,il))

  switching = 'no'
; Make "ileg"   the LEG that contains the starting point, and "ileg+1" the other one:
 ;switch_legs,rad_l1,rad_l2,lat_l1,lat_l2,lon_l1,lon_l2,Ne_l1,Ne_l2,Tm_l1,Tm_l2,WT_l1,WT_l2,scoreR_l1,scoreR_l2,il,switching 
; Make leg_status=2 for ileg+1 (for ileg we already set leg_status=1, by default)
  leg_status(ileg+1) = 2.
; IMPORTANT NOTE: leg_status contains now the correct information ONLY
; if switch_legs was used. If not, this information may be incorrect
; but it will NOT be used anyhow.
; ALBERT'S ADVICE: please *ALWAYS* USE SWITCHING!! It is what you want anyway.

; According to the switching status, associate to each LEG the 
; INITIAL and FINAL coordinates of the LOOP wo which BOTH belong:
  if switching eq 'no' then begin
  rad_ini(ileg)   = rad_l1(0)
  rad_ini(ileg+1) = rad_l1(0)
  rad_fin(ileg)   = rad_l2(n_elements(rad_l2)-1)
  rad_fin(ileg+1) = rad_l2(n_elements(rad_l2)-1)
  lat_ini(ileg)   = lat_l1(0)
  lat_ini(ileg+1) = lat_l1(0)
  lat_fin(ileg)   = lat_l2(n_elements(lat_l2)-1)
  lat_fin(ileg+1) = lat_l2(n_elements(lat_l2)-1)
  lon_ini(ileg)   = lon_l1(0)
  lon_ini(ileg+1) = lon_l1(0)
  lon_fin(ileg)   = lon_l2(n_elements(lon_l2)-1)
  lon_fin(ileg+1) = lon_l2(n_elements(lon_l2)-1)
  endif
  if switching eq 'yes' then begin
  rad_ini(ileg)     = rad_l1(n_elements(rad_l1)-1)
  rad_iOAni(ileg+1) = rad_l1(n_elements(rad_l1)-1)
  rad_fin(ileg)   = rad_l2(0)
  rad_fin(ileg+1) = rad_l2(0)
  lat_ini(ileg)   = lat_l1(n_elements(lat_l1)-1)
  lat_ini(ileg+1) = lat_l1(n_elements(lat_l1)-1)
  lat_fin(ileg)   = lat_l2(0)
  lat_fin(ileg+1) = lat_l2(0)
  lon_ini(ileg)   = lon_l1(n_elements(lon_l1)-1)
  lon_ini(ileg+1) = lon_l1(n_elements(lon_l1)-1)
  lon_fin(ileg)   = lon_l2(0)
  lon_fin(ileg+1) = lon_l2(0)
  endif

; These lines store the FOOT-POINT coordinates for each LEG,
; and they do it correctly independently of the switching status:
  Footrad(ileg)   = rad_ini(ileg)    
  Footrad(ileg+1) = rad_fin(ileg+1)  
  Footlat(ileg)   = lat_ini(ileg)    
  Footlat(ileg+1) = lat_fin(ileg+1)  
  Footlon(ileg)   = lon_ini(ileg)    
  Footlon(ileg+1) = lon_fin(ileg+1)  

;   rrr1=findel(1.075,rad_l1)
;   rrr2=findel(1.075,rad_l2)
;  Rp0_rad(ileg)   = rad_l1(rrr1)
;  Rp0_rad(ileg+1) = rad_l2(rrr2)
;  Rp0_lat(ileg)   = lat_l1(rrr1)
;  Rp0_lat(ileg+1) = lat_l2(rrr2)
;  Rp0_lon(ileg)   = lon_l1(rrr1)
;  Rp0_lon(ileg+1) = lon_l2(rrr2)
 
;   Rp_rad(ileg)   = Rp0_rad(ileg)
;   Rp_rad(ileg+1) = Rp0_rad(ileg+1)
;   Rp_lat(ileg)   = Rp0_lat(ileg)
;   Rp_lat(ileg+1) = Rp0_lat(ileg+1)
;   Rp_lon(ileg)   = Rp0_lon(ileg)
;   Rp_lon(ileg+1) = Rp0_lon(ileg+1)

  p1 = where ( rad_l1 ge rmin and rad_l1 le rmax and Ne_l1 ne -999. and  WT_l1 ge WTc*1.e6); scoreR_l1 lt 0.1 and

  p2 = where ( rad_l2 ge rmin and rad_l2 le rmax and Ne_l2 ne -999. and  WT_l2 ge WTc*1.e6) ;scoreR_l2 lt 0.1 and

   if  p1(0) eq -1 or p2(0) eq -1 then goto,skipnextloop

;   rr11=findel(1.075,rad_l1)
;   rr12=findel(1.075,rad_l2)
 ;    TmR1(ileg)   = Tm_l1(rr11)
 ;    TmR1(ileg+1) = Tm_l2(rr12)

 ;     NR1(ileg)   = Ne_l1(rr11)
 ;     NR1(ileg+1) = Ne_l2(rr12)

;----------------------------------------------------------------------------------------------------------------
  r0 = 1.025
  s_l1_max = max(s_l1) ; Rsun
  s_l2_max = max(s_l2) ; Rsun
  r_l1_max = max(rad_l1) ; Rsun
  r_l2_max = max(rad_l2) ; Rsun

  Nl1 = n_elements(B_l1)
  Nl2 = n_elements(B_l2)
  B_l1_max = B_l1(Nl1-1)
  B_l2_max = B_l2(0    )
  B_l1_r0  = interpol(B_l1,rad_l1,r0)
  B_l2_r0  = interpol(B_l2,rad_l2,r0)

  Ner0_l1  = interpol(Ne_l1,rad_l1,r0)
  Ner0_l2  = interpol(Ne_l2,rad_l2,r0)

  Ner0(ileg)   = Ner0_l1
  Ner0(ileg+1) = Ner0_l2

;======================================================================================================
  Ner1_l1  = -555
  Ner1_l2  = -555
  Ner1(ileg)   = Ner1_l1
  Ner1(ileg+1) = Ner1_l2

  Ner2_l1  = -555
  Ner2_l2  = -555
  Ner2(ileg)   = Ner2_l1
  Ner2(ileg+1) = Ner2_l2

  Ner3_l1  = -555
  Ner3_l2  = -555
  Ner3(ileg)   = Ner3_l1
  Ner3(ileg+1) = Ner3_l2
;======================================================================================================

 ; Fit lineal s_l1 = m1 rad_l1 + b1, usar primeros 5 datos.
 ; Fit lineal s_l2 = m2 rad_l2 + b2
   Npoints=Ndata
  if n_elements(rad_l1) ge Npoints AND n_elements(rad_l2) ge Npoints then begin 

   rad_l1_base = rad_l1(0:Npoints-1) 
   rad_l2_rev  = reverse(rad_l2)  ; da vuelta rad_l2 
   rad_l2_base = rad_l2_rev (0:Npoints-1)

     s_l1_base =   s_l1(0:Npoints-1) 
     s_l2_rev  =   reverse(s_l2)     ; también necesito los últimos s_l2
     s_l2_base =   s_l2_rev(0:Npoints-1) 

;fit s_l1_base = m1 * rad_l1_base + s0r1 
  xfit = rad_l1_base
  yfit =   s_l1_base
  fitr0,xfit,yfit,A,corr2
         S0r1 = A[0]
         m1   = A[1]
         r2s1 = corr2

;fit s_l2_base = m2 * rad_l2_base + s0r2
  xfit = rad_l2_base
  yfit =   s_l2_base
  fitr0,xfit,yfit,A,corr2
         S0r2 = A[0]
         m2   = A[1]
         r2s2 = corr2

    s_l1_r0 = m1 * r0 + s0r1 
    s_l2_r0 = m2 * r0 + s0r2

  s_r0_a(ileg  ) = s_l1_r0 
  s_r0_a(ileg+1) = s_l2_r0 

  endif

;----------------------------------------------------------------------------------------------------------------
 ;STOP
  
   Ne_l1 =  Ne_l1 (p1)
   Ne_l2 =  Ne_l2 (p2)
   Tm_l1 =  Tm_l1 (p1)
   Tm_l2 =  Tm_l2 (p2)
   WT_l1 =  WT_l1 (p1)
   WT_l2 =  WT_l2 (p2)
   Er_l1 =  Er_l1 (p1)
   Er_l2 =  Er_l2 (p2)
  rad_l1 = rad_l1 (p1)
  rad_l2 = rad_l2 (p2)
    s_l1 =   s_l1 (p1)
    s_l2 =   s_l2 (p2)
    B_l1 =   B_l1 (p1)
    B_l2 =   B_l2 (p2)
; tomographic pressure      
  p_l1 = kB/bb *Ne_l1*Tm_l1   
  p_l2 = kB/bb *Ne_l2*Tm_l2   
;make Beta plasma parameter
 beta_l1 = p_l1/(B_l1^2/(8*!pi))
 beta_l2 = p_l2/(B_l2^2/(8*!pi))
 
    Nemean(ileg)   = mean(Ne_l1)
    Nemean(ileg+1) = mean(Ne_l2)
    Tmmean(ileg)   = mean(Tm_l1)
    Tmmean(ileg+1) = mean(Tm_l2)
    WTmean(ileg)   = mean(WT_l1)
    WTmean(ileg+1) = mean(WT_l2)
  Nestddev(ileg)   = stddev(Ne_l1)
  Nestddev(ileg+1) = stddev(Ne_l2)
  Tmstddev(ileg)   = stddev(Tm_l1)
  Tmstddev(ileg+1) = stddev(Tm_l2)
  WTstddev(ileg)   = stddev(WT_l1)
  WTstddev(ileg+1) = stddev(WT_l2)
  betamean(ileg)   = mean(beta_l1)
  betamean(ileg+1) = mean(beta_l2)
  betaapex(ileg)   = beta_l1(n_elements(rad_l1(p1))-1)
  betaapex(ileg+1) = beta_l2(0)
     Bmean(ileg)   = mean(B_l1)
     Bmean(ileg+1) = mean(B_l2)
; if you want to use beta at some height instead <beta> uncomment the
; following lines
;...........................................................
rad0= 1.035
index_l1 = where (abs(rad_l1-rad0) eq min(abs(rad_l1-rad0)))
index_l2 = where (abs(rad_l2-rad0) eq min(abs(rad_l2-rad0)))
;betamean(ileg)   = beta_l1(index_l1(0))
;betamean(ileg+1) = beta_l2(index_l2(0))
;.......................................................... 

if n_elements(p1) lt Ndata  or n_elements(p2) lt Ndata then goto,skipfitloop

rrr1 = 1.10
if min(rad_l1) gt rrr1 OR min(rad_l2) gt rrr1 then goto,skipfitloop
;fits for leg1-------------------------------------- 
  rminhs = min(rad_l1) 
  rmaxhs = max(rad_l1)

if opcls(il) eq 1 and max(rad_v(*,il)) lt 1.24 then print,il

;HS-fit to Ne(r)
  xfit = rad_l1
  yfit =  Ne_l1
if n_elements(xfit) ne n_elements(yfit) then stop

    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2

       Ne0(ileg) = a[0] 
  lambda_N(ileg) = 1./A[1]  ; Rsun
            Tfit = mu * mH * gsun * (lambda_N(ileg)*rsun) / kB
     Tefit(ileg) = bb*Tfit
     r2N  (ileg) = corr2

  ;HS-fit to P(r)
  yfit = p_l1
  
    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2

        P0(ileg) = a[0] 
  lambda_P(ileg) = 1./A[1]  ; Rsun
       r2P(ileg) = corr2

  ;linear-fit to T(r)
  yfit = Tm_l1
    ep = 0.15
if n_elements(xfit) ne n_elements(yfit) then stop

    fit_ts,xfit,yfit,ep,a1,b1,F
;  fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

          T0r1_ts = b1
     Tm0_ts(ileg) = T0r1_ts
   gradT_ts(ileg) = a1
     FTr_ts(ileg) = F 

;         T0r1 = A[0]
;    Tm0(ileg) = T0r1
;  gradT(ileg) = A[1]
;  r2T  (ileg) = corr2

 ; linear-fit to T(s) ==================================
      xfit = s_l1 *rsun    
    rminhs = min(s_l1*rsun);smin
    rmaxhs = max(s_l1*rsun);smax 
      yfit = Tm_l1
        ep = 0.15

    fit_ts,xfit,yfit,ep,a1,b1,F
;   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

          T0s1_ts = b1
    Tm0s_ts(ileg) = T0s1_ts
   dTmds_ts(ileg) = a1
     FTs_ts(ileg) = F 

;            T0s1 = A[0]
;      Tm0s(ileg) = T0s1
;    dTmds (ileg) = A[1]
;    r2Ts  (ileg) = corr2

   if keyword_set(fitcuadr) then begin
  ; cuadratic-fit to T(s) 
      xfit = s_l1 ;*rsun    
    rminhs = min(s_l1);*rsun);smin
    rmaxhs = max(s_l1);*rsun);smax 
    yfit   = Tm_l1
   fitcuadrTemp,xfit,yfit,rminhs,rmaxhs,A,corr2,T0s1,dTmds(ileg)*rsun
        Acuadr1 = A
 r2Tcuadr(ileg) = corr2
 Acuadr_a(ileg,*) = Acuadr1
  endif

;fits for leg2-------------------------------------- 
  rminhs = min(rad_l2) 
  rmaxhs = max(rad_l2)

  ;HS-fit to Ne(r)
  xfit = rad_l2
  yfit =  Ne_l2

if n_elements(xfit) ne n_elements(yfit) then stop

    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2  

       Ne0(ileg+1) = a[0] 
  lambda_N(ileg+1) = 1./A[1]  ; Rsun
              Tfit = mu * mH * gsun * (lambda_N(ileg+1)*rsun) / kB
     Tefit(ileg+1) = bb*Tfit
     r2N  (ileg+1) = corr2  

  ;HS-fit to P(r)
  yfit = p_l2
    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2  
        P0(ileg+1) = a[0] 
  lambda_P(ileg+1) = 1./A[1]  ; Rsun
     r2P  (ileg+1) = corr2

  ;HS-fit to T(r)  
  yfit = Tm_l2
    ep = 0.15
if n_elements(xfit) ne n_elements(yfit) then stop

    fit_ts,xfit,yfit,ep,a1,b1,F
;   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

            T0r2_ts = b1
     Tm0_ts(ileg+1) = T0r2_ts
   gradT_ts(ileg+1) = a1
     FTr_ts(ileg+1) = F 
 
;           T0r2 = A[0]
;    Tm0(ileg+1) = T0r2
;  gradT(ileg+1) = A[1]
;  r2T  (ileg+1) = corr2

  ;linear-fit to T(s) ==========================================
      xfit = s_l2 *rsun   
    rminhs = min(s_l2*rsun);smin
    rmaxhs = max(s_l2*rsun);smax 
      yfit = Tm_l2
        ep = 0.15

    fit_ts,xfit,yfit,ep,a1,b1,F
   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

            T0s2_ts = b1
    Tm0s_ts(ileg+1) = T0s2_ts
   dTmds_ts(ileg+1) = a1
     FTs_ts(ileg+1) = F 

;              T0s2 = A[0]
;      Tm0s(ileg+1) = T0s2  ;<---
;    dTmds (ileg+1) = A[1]
;    r2Ts  (ileg+1) = corr2  

 if keyword_set(fitcuadr) then begin
   ;cuadratic-fit to T(s) 
      xfit = s_l2 ;*rsun    
    rminhs = min(s_l2);*rsun;smin
    rmaxhs = max(s_l2);*rsun;smax 
    yfit   = Tm_l2
    fitcuadrTemp,xfit,yfit,rminhs,rmaxhs,A,corr2,T0s2,dTmds(ileg+1)
         Acuadr2 = A
r2Tcuadr(ileg+1) = corr2
Acuadr_a(ileg+1,*) = Acuadr2
 endif
 
;STOP

goto,nocambiarlosr2
   if r2T(ileg) lt 0.5 or r2T(ileg+1) lt 0.5 then begin
      r2T(ileg)   = -555.   
      r2T(ileg+1) = -555.       
   endif

   if r2Ts(ileg) lt 0.5 or r2Ts(ileg+1) lt 0.5 then begin
      r2Ts(ileg)   = -555.   
      r2Ts(ileg+1) = -555.       
    goto,skiptestloopclosed
   endif
nocambiarlosr2:
;stop
;make dT/ds 
  dTmds_l1 = dTmds_ts(ileg);DERIV(s_l1,Tm_l1)
  dTmds_l2 = dTmds_ts(ileg+1);DERIV(s_l2,Tm_l2)     

if keyword_set(fitcuadr) then  dTmds_l1= (2*Acuadr1[0]*s_l1 + Acuadr1[1])/rsun
if keyword_set(fitcuadr) then  dTmds_l2= (2*Acuadr2[0]*s_l2 + Acuadr2[1])/rsun

;  make conductive flux 
  Fc_l1= -kappa*Tm_l1^(5./2)*dTmds_l1
  Fc_l2= -kappa*Tm_l2^(5./2)*dTmds_l2   
  Fc_l1 = -2*kappa*DERIV(S_l1,Tm_l1^(7./2))/7
  Fc_l2 = -2*kappa*DERIV(S_l2,Tm_l2^(7./2))/7


 ; NOTA ALBERT
 ; AQUI HAY UNA INCONSISTENCIA: [s]=Rsun y [dTmds]=K/cm
                                ; con lo cual [Fc_l1,2] está MAL
                                ; CALCULADO AQUI, pues lo que está
                                ; entre () NO DA EN K!!
;   Fc_l1= -kappa*(T0s1 +dTmds_l1*s_l1)^(5./2)*dTmds_l1
;   Fc_l2= -kappa*(T0s2 +dTmds_l2*s_l2)^(5./2)*dTmds_l2   

; NOTA ALBERT
; ESTO EN CAMBIO SI ES CONSISTE pues el ajuste cuadrático ASUME
; [s]=Rsun, como se usa en esta fórmula. Lo que está entre ()
; da en K, como corresponde, y luego [dTmds] = K/cm, con lo cual 
; [Fc_l1,2] aqui está en CGS: erg / cm² / sec.
if keyword_set(fitcuadr) then   Fc_l1= -kappa*(Acuadr1[0]*s_l1^2 + Acuadr1[1]*s_l1+ Acuadr1[2])^(5./2)*dTmds_l1
if keyword_set(fitcuadr) then   Fc_l2= -kappa*(Acuadr2[0]*s_l2^2 + Acuadr2[1]*s_l2+ Acuadr2[2])^(5./2)*dTmds_l2   

;stop

; NOTA ALBERT
; TODO ES CONSISTENTE AQUI ABAJO
; [Fc2_l1,2] aqui está en CGS: erg/cm²/sec,
; tanto en el ajuste lineal como en el cuadrático.
;
; Fcb con el ajuste lineal


     Tmbase_l1=Tm0_ts(ileg)  +gradT_ts(ileg)  *r0
     Tmbase_l2=Tm0_ts(ileg+1)+gradT_ts(ileg+1)*r0
     Fc2_l1 = -kappa*Tmbase_l1^(5./2)*dTmds_ts(ileg) 
     Fc2_l2 = -kappa*Tmbase_l2^(5./2)*dTmds_ts(ileg+1)
     Fcb(ileg  ) = Fc2_l1 * B_l2_r0/(B_l1_r0+B_l2_r0)
     Fcb(ileg+1) = Fc2_l2 * B_l1_r0/(B_l1_r0+B_l2_r0)

; Fcb con el ajuste cuadrático
if keyword_set(fitcuadr) then begin
     Tmbase_l1=Acuadr1[0]*s_l1_r0^2 + Acuadr1[1]*S_l1_r0+ Acuadr1[2]
     Tmbase_l2=Acuadr2[0]*s_l2_r0^2 + Acuadr2[1]*S_l2_r0+ Acuadr2[2]
     dTmds_l1_base = (2*Acuadr1[0]*s_l1_r0 + Acuadr1[1])/rsun
     dTmds_l2_base = (2*Acuadr2[0]*s_l2_r0 + Acuadr2[1])/rsun
     Fc2_l1 = -kappa*Tmbase_l1^(5./2)*dTmds_l1_base 
     Fc2_l2 = -kappa*Tmbase_l2^(5./2)*dTmds_l2_base
     Fcb(ileg  ) = Fc2_l1 * B_l2_r0/(B_l1_r0+B_l2_r0)
     Fcb(ileg+1) = Fc2_l2 * B_l1_r0/(B_l1_r0+B_l2_r0)
   dTmds(ileg)   = dTmds_l1_base
   dTmds(ileg+1) = dTmds_l2_base
  Tmbase(ileg)   = Tmbase_l1
  Tmbase(ileg+1) = Tmbase_l2
endif

;========================================================================;<---
;nuevo ajuste de Temperatura. Necesito T(s(r0))=T(s_li_r0) y dT/ds
;stop

; COMMENT DE ALBERT
; BAD IDEA
s_l1 = s_l1 * rsun ; pass s_l1 to cm 
s_l2 = s_l2 * rsun ; pass s_l2 to cm 

ss1=n_elements(s_l1)
ss2=n_elements(s_l2)

Numpts=7

if ss1 gt Numpts then begin
s_ref_l1 = s_l1(0:Numpts-1)
endif else begin
if ss1 le Numpts then s_ref_l1=s_l1
endelse

if ss2 gt Numpts then begin
s_ref_l2 = s_l2((ss2-Numpts):ss2-1)
endif else begin
if ss2 le Numpts then s_ref_l2=s_l2
endelse

Tm_ref_l1 = Tm_l1(0:n_elements(s_ref_l1)-1)
Tm_ref_l2 = Tm_l2((n_elements(Tm_l2)-n_elements(s_ref_l2)):n_elements(Tm_l2)-1)

;<---l1
   xfit = s_ref_l1
   yfit = Tm_ref_l1
   fitr0,xfit,yfit,A,r2
       T0_fit1 = A[0]
     dTds_fit1 = A[1]
     r2Ts_fit1 = r2  
      T0_s0_l1 = T0_fit1+dTds_fit1*s_l1_r0*rsun 

;<---l2
   xfit = s_ref_l2
   yfit = Tm_ref_l2
   fitr0,xfit,yfit,A,r2
       T0_fit2 = A[0]
     dTds_fit2 = A[1]
     r2Ts_fit2 = r2
      T0_s0_l2 = T0_fit2+dTds_fit2*s_l2_r0*rsun

;-----------------------Fc2 con el ajuste de Numpts puntos ;<---------------------------------
 
     FcN2_l1 = -kappa*T0_s0_l1^(5./2)*dTds_fit1 
     FcN2_l2 = -kappa*T0_s0_l2^(5./2)*dTds_fit2 
;Aún no lo grabo, vemos cual decidimos usar ;<---


;stop

; make conductive loss
    Ec_l1 = B_l1 * DERIV(s_l1,Fc_l1/B_l1)
    Ec_l2 = B_l2 * DERIV(s_l2,Fc_l2/B_l2)    
; make heating rate
    Eh_l1 = Er_l1 + Ec_l1
    Eh_l2 = Er_l2 + Ec_l2

;  extrapolate Er_l down to r0

Er_l1_base  =  Er_l1
Er_l2_base  =  Er_l2

rr_l1 = rad_l1
rr_l2 = rad_l2

;----l1
xfit = rr_l1
yfit = Er_l1_base

fitEr,xfit,yfit,A,r2
Er_l1_r0  = A[0] * exp( A[1]*(r0) )
Er_l1_max = A[0] * exp( A[1]*(r_l1_max) )
r2Er(ileg) = r2
if max(s_l1) lt s_l1_max * rsun then begin   
    s_l1_e = [ s_l1_r0  * rsun, s_l1, s_l1_max * rsun]
    r_l1_e = [r0,rad_l1,r_l1_max]
    Er_l1_e = [Er_l1_r0,Er_l1,Er_l1_max]
     B_l1_e = [ B_l1_r0, B_l1, B_l1_max]
    Er_l1_e_fit = A[0] * exp( A[1] * r_l1_e )
   ;s1fine=s_l1_r0*rsun+(s_l1_max * rsun-s_l1_r0*rsun)*findgen(100)/99.
   ;r1fine=r0+(r_l1_max-r0)*findgen(100)/99.
   ;Er_l1_e_fit = A[0] * exp( A[1] * r1fine )
endif
if max(s_l1) eq s_l1_max * rsun then begin   
    s_l1_e = [ s_l1_r0  * rsun, s_l1]
    r_l1_e =  [r0,rad_l1]
    Er_l1_e = [Er_l1_r0,Er_l1]
     B_l1_e = [ B_l1_r0, B_l1]
    Er_l1_e_fit = A[0] * exp( A[1] * r_l1_e )
   ;s1fine=s_l1_r0*rsun+(s_l1(n_elements(s_l1)-1)-s_l1_r0*rsun)*findgen(100)/99.
   ;r1fine=r0+(r_l1(n_elements(r_l1)-1)-r0)*findgen(100)/99.
   ;Er_l1_e_fit = A[0] * exp( A[1] * r1fine )
endif

;if opcls(il) eq 2 and footlat(ileg)*footlat(ileg+1) lt 0. AND abs(footlat(ileg)) le 30. AND abs(footlat(ileg+1)) le 30. AND $
;r2T(ileg) ge 0.6 and r2T(ileg+1) ge 0.6 AND r2N(ileg) ge 0.95 AND r2N(ileg+1) ge 0.95 then stop;goto,grafica

;----l2
xfit = rr_l2
yfit = Er_l2_base
fitEr,xfit,yfit,A,r2
Er_l2_r0  = A[0] * exp( A[1]*(r0) )
Er_l2_max = A[0] * exp( A[1]*(r_l2_max) )
r2Er(ileg+1) = r2
if max(s_l2) lt s_l2_max * rsun then begin   
    s_l2_e = [ s_l2_max * rsun, s_l2, s_l2_r0  * rsun]     ;<--- lo defino acá
    r_l2_e = [r_l2_max,rad_l2,r0]
    Er_l2_e = [Er_l2_max,Er_l2,Er_l2_r0] ;<---
     B_l2_e = [ B_l2_max, B_l2, B_l2_r0] ;<---
    Er_l2_e_fit = A[0] * exp( A[1] * r_l2_e )
   ;s2fine = s_l2_max+(s_l2_r0  * rsun-s_l2_max)*findgen(100)/99.
   ;r2fine=r_l2_max+(r0-r_l2_max)*findgen(100)/99.   
   ;Er_l2_e_fit = A[0] * exp( A[1] * r2fine )
endif 
if max(s_l2) eq s_l2_max * rsun then begin   
    s_l2_e = [ s_l2, s_l2_r0  * rsun]     ;<--- lo defino acá
    r_l2_e =  [rad_l2,r0]
    Er_l2_e = [Er_l2,Er_l2_r0] ;<---
     B_l2_e = [ B_l2, B_l2_r0] ;<---
    Er_l2_e_fit = A[0] * exp( A[1] * r_l2_e )
   ;s2fine = s_l2(0)+(s_l2_r0  * rsun-s_l2(0))*findgen(100)/99.
   ;r2fine=r_l2(0)+(r0-r_l2(0))*findgen(100)/99.   
   ;Er_l2_e_fit = A[0] * exp( A[1] * r2fine )
endif 

    B_eq = (B_l1_r0*B_l2_r0) / (B_l1_r0+B_l2_r0)

     xtmp=s_l1_e
     ytmp=Er_l1_e *(B_eq/B_l1_e)
     phir_l1 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<---
  phir(ileg) = phir_l1
     xtmp=s_l2_e                                    ;<--
     ytmp=Er_l2_e *(B_eq/B_l2_e)
     phir_l2 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<--- di vuelta los datos. si doy vuelta s_l1 sale un - que debería poner?
  phir(ileg+1) = phir_l2                                    ;<---     aunque tmb haya dado vuelta er? (lo pongo) 

     xtmp=s_l1_e
     ytmp=Er_l1_e_fit *(B_eq/B_l1_e)
     phir_l1 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<---
  phirfit(ileg) = phir_l1                                   ;<--
     xtmp=s_l2_e                                    ;<--
     ytmp=Er_l2_e_fit *(B_eq/B_l2_e)
     phir_l2 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<--- di vuelta los datos. si doy vuelta s_l1 sale un - que debería poner?
  phirfit(ileg+1) = phir_l2                                    ;<---     aunque tmb haya dado vuelta er? (lo pongo) 

;stop
;if opcls(il) eq 2 then stop

s_l1 = s_l1 / rsun ; back s_l1 to Rsun 
s_l2 = s_l2 / rsun ; back s_l2 to Rsun

; test diferent models of the heating rate

   Eh(ileg)   = mean (Eh_l1)
   Eh(ileg+1) = mean (Eh_l2)

       index1 = where( Eh_l1 gt 0) 
       index2 = where( Eh_l2 gt 0) 

if n_elements(index1) lt n_elements(Eh_l1) then Eh(ileg)   = -555.
if n_elements(index2) lt n_elements(Eh_l2) then Eh(ileg+1) = -555.

  deltaEh (ileg)   = stdev (Eh_l1)
  deltaEh (ileg+1) = stdev (Eh_l2)
              smin = min(s_l1)
              smax = max(s_l1)
              xfit = s_l1
              yfit = Eh_l1

  fitEh,xfit,yfit,smin,smax,A,r2
              Eh01 = A[0]
          sH(ileg) = A[1]
        r2sH(ileg) = r2
              smin = min(s_l2)
              smax = max(s_l2)
              xfit = s_l2
              yfit = Eh_l2

  fitEh,xfit,yfit,smin,smax,A,r2
              Eh02 = A[0]
        sH(ileg+1) = A[1]
      r2sH(ileg+1) = r2

skiptestloopclosed:

  skipfitloop:
     opclstat(ileg)   =      opcls(il)
     opclstat(ileg+1) =      opcls(il)
  loop_length(ileg)   =      loopL(il) 
  loop_length(ileg+1) =      loopL(il)
        strad(ileg)   =      str_v(il)
        stlat(ileg)   = (90-stth_v(il)/!dtor)
        stlon(ileg)   =     stph_v(il)/!dtor  
        strad(ileg+1) =      str_v(il)
        stlat(ileg+1) = (90-stth_v(il)/!dtor)
        stlon(ileg+1) =     stph_v(il)/!dtor
  skipnextloop:
  indexloop(ileg)  = il
  indexloop(ileg+1)= il  
 ;if gradT(ileg)*gradT(ileg+1) lt 0. then begin
 ;indexloop(ileg)   = -678.   
 ;indexloop(ileg+1) = -678.
 ;endif 

  ileg = ileg+2

endelse
endfor

return
end



pro fitcuadrtemp,rr,yy,Rmin,Rmax,A,r2,T0,gradT
p=where(rr ge Rmin and rr le Rmax)
rr=rr(p)
yy=yy(p)
A2 = T0
A1 = gradT
A0 = 0.0
A=[A0,A1,A2]
ww=yy*0.+1. ;1./yy
fit=curvefit(rr,yy,ww,A,Sigma,function_name='function_cuadr')
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot
return
end

PRO function_cuadr, X, A, F, pder   
   F = A[0]*X^2 + A[1]*X + A[2]
   IF N_PARAMS() GE 4 then pder=[ [X^2], [X], [replicate(1.0, N_ELEMENTS(X))]]
END



pro fithslinear,rr,yy,Rmin,Rmax,A,r2
p=where(rr ge Rmin and rr le Rmax)
;ep=0.01
rr=rr(p)
yy=yy(p)
xx= 1/rr
zz= alog(yy)
A = [0.,0.]
;fit_ts,xx,zz,ep,a1,b1,F
;Alin=[b1,a1]
;A[0] = exp( a1 + b1)
;A[1] = a1
;fit_ts =  A[0] * exp( -A[1]*(1.-1./rr) )

Alin = linfit(xx,zz)
A[0] = exp( Alin[0]+ Alin[1])
A[1] = Alin[1]
fit =  A[0] * exp( -A[1]*(1.-1./rr) ) 

meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit)^2 )
r2    = 1.-SSerr/SStot 

return
end

pro fitr0,rr,yy,A,r2   ;<---
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr 
meanyy = mean(yy)
SStot  = total( (yy-meanyy)^2 )
SSerr  = total( (yy-fit    )^2 )
r2     = 1.-SSerr/SStot 
return 
end


pro fithsEr,rr,yy,A,r2
xx= 1/rr
zz= alog(yy)
A = [0.,0.]
Alin = linfit(xx,zz)
A[0] = exp( Alin[0]+ Alin[1])
A[1] = Alin[1]
fit =  A[0] * exp( -A[1]*(1.-1./rr) ) 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
return
end

pro fitEr,rr,yy,A,r2  ;<--- ajusto exponencial común
ep=0.01
zz= alog(yy)
A = [0.,0.]
;fit_ts,rr,zz,ep,a1,b1,F
;Alin=[b1,a1]
;A[0] = exp(b1)
;A[1] = a1
Alin = linfit(rr,zz)
A[0] = exp( Alin[0])
A[1] = Alin[1]
fit =  A[0] * exp( A[1]*(rr) ) 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
return
end


pro fitTemp,rr,yy,Rmin,Rmax,A,r2
p=where(rr ge Rmin and rr le Rmax)
rr=rr(p)
yy=yy(p)
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
return 
end


pro fit_ts,x,y,ep,a1,b1,F
                                                  
n=n_elements(x)
nn=n*(n-1)*0.5

xx1=x#replicate(1,n)
yy1=y#replicate(1,n)

xx=xx1
yy=yy1

yb=fltarr(n,n)-100.
xb=fltarr(n,n)-100.
ts=fltarr(n,n)-100.
bb=fltarr(n)-100.


for j=0,n-1 do begin
   for i=0,n-1 do begin
      if j gt i then begin
      xb(j,i)=xx(j,i)-x(i)
      yb(j,i)=yy(j,i)-y(i)
      ts(j,i)=yb(j,i)/xb(j,i)
   endif else begin
         if j lt i then ts(j,i)=0
      endelse
   endfor
endfor

for j=0,n-1 do begin
   for i=0,n-1 do begin
      if j gt i then begin
         ts(j,i)=ts(j,i)
      endif else begin
         if j le i then ts(j,i)=0
      endelse
   endfor
endfor
ts1=ts[where(ts ne 0)]
a1=median(ts1)

for i=0,n-1 do begin
   bb(i)=y(i)-a1*x(i)
endfor
b1=median(bb)

yfit = b1 + a1*x

;Calidad de ajuste                                  
                
nfit = n_elements(yfit)
eps = ep * median(yfit)
   yru = yfit + eps
   yrd = yfit - eps

 Yin = fltarr(n)-100.
for i=0,n-1 do begin
   if y(i) ge yrd(i) and y(i) le yru(i) then  Yin(i) = 1. 
   if y(i) lt yrd(i) or  y(i) gt yru(i) then  Yin(i) = 0.
endfor
nok=where(Yin eq 1.)

F=float(n_elements(Nok))/float(nfit)

return 
end

pro fitEh,ss,yy,smin,smax,A,r2

A= [-555.,-555]
r2 = -555.
p=where(ss ge smin and ss le smax and yy gt 0.)

if n_elements(p) ge 5 and 2*n_elements(p) gt n_elements(yy) then begin
ss=ss(p)
yy=yy(p)
zz = alog(yy) 
Alin = linfit(ss,zz)
A[0] = exp (Alin[0])
A[1] = 1./Alin[1]
fit =  A[0] * exp(ss/A[1]) 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
endif

return
end

