pro hacer_trace

; Ceci & Albert:
;fdips_file='fdips_field_151x180x360_synop_Mr_0.polfil.2081.dat'
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

