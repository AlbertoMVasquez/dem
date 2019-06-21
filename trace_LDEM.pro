;trace_LDEM,field_awsom='/data1/work/MHD/sph_data_awsom_2082_1.85.sav',awsom_file='awsom_2082_1.85',period=period,safety=.5,stepmax=8000,/unifgrid_v2,dlat=dlat,dlon=dlon,radstart=radstart
;trace_LDEM,pfss_data_file='pfss_data_cr2082_trazado5alturas.sav',ldem_file='LDEM.v3_CR2082_l.25.75.5_fd_Rmin1.00_Rmax1.30_Nr26_InstRmax1.26_bf4_r3d_B_vfullcadence_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.B_L171_DECON_gauss1_lin_Norm-median_singlStart',period=period,safety=.5,stepmax=8000,/unifgrid_v2,dlat=dlat,dlon=dlon,radstart=radstart
;trace_LDEM,field_awsom='sph_data_awsom_2208_1.85.sav',ldem_file='asd',period='probando_nuevamente_',safety=.5,stepmax=7500,/unifgrid_v2,radstart=1.025+0.04*findgen(6),awsom_file='awsom_2208_1.85_new'


;trace_LDEM,ldem_file='LDEM.v3_CR2208_l.50.20.20_h1_Rmin1.00_Rmax1.26_Nr26_InstRmax1.26_bf4_r3d_B_chianti.ioneq_sun_coronal_1992_feldman_ext.abundaia3_171_gauss1_lin_Norm-median_singlStart',pfss_data_file='pfss_data_awsom_2208_1.85_newprobando_nuevamente__radstart-1.025-1.225Rs.sav',radstart=1.025 + 0.04 *findgen(6),period='probando_cr2208_con_demt_',/unifgrid_v2
;trace_LDEM,pfss_data_file='pfss_data_awsom_2208_1.85_newprobando_nuevamente__radstart-1.025-1.225Rs.sav',awsom_file='awsom_2208_1.85',radstart=1.025 + 0.04 *findgen(6),period='2208_con_awsomdata_',/unifgrid_v2
;trace_LDEM,pfss_data_file='pfss_data_cr2082_trazado5alturas.sav',awsom_file='awsom_2082_1.85_short',period='2082_con_awsomdata_',/unifgrid_v2,radstart=1.025 + 0.04 *findgen(6),safety=.5,stepmax=10000
;trace_LDEM,pfss_data_file='pfss_data_cr2082_trazado5alturas.sav',ldem_file='LDEM.v3_CR2082_l.35.2.3_h1_Rmin1.00_Rmax1.30_Nr26_InstRmax1.26_bf4_r3d_B_vfullcadence_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.B_L171_DECON_gauss1_lin_Norm-median_singlStart',period='2082_hollow_demt_',safety=.5,stepmax=7500,/unifgrid_v2,radstart=1.025 + 0.04 *findgen(6)
pro trace_LDEM,fdips_file=fdips_file,$
               ldem_file=ldem_file,$
               period=period,$
               outputfile=outputfile,$
               marcgrid=marcgrid,$
               aunifgrid=aunifgrid,$
               radstart=radstart,$
               spacing=spacing,$
               fieldtype=fieldtype,$
               safety=safety,$
               stepmax=stepmax,$
               Box=box,$
               dlat=dlat,$
               dlon=dlon,$
               mhd=mhd,$
               dgfw=dgfw,$
               expand=expand,$
               unifgrid_v2=unifgrid_v2,$
               field_awsom=field_awsom,$
               awsom_file=awsom_file,$
               pfss_data_file=pfss_data_file
  
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common results_tomo,tfbe,sfbe,N_e
  common loss_rate,Er
  common results_images,ima,sima,ra,pa,ya,za,em,npx
  common fixed_width,sigma
  common fixed_parameter_equalizer,xmax,sigma_v,demv
;los commons pertenecen al read_ldem


;test
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
; /awsom    = use SWMF magnetic field.
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
;
;
; 12/5/2016
; Modified by A. Vasquez & D. Lloveras
;    - Two bugs fixed during tracing loops.
;
; 29/05/2019
; Modified by D. Lloveras
;  - Atomisacion del codigo, se guarda el trazado magnetico primero en
;    un output.sav para futuro uso.
;  - Agregado de pfss_data_file para trazar a partir de un .sav
;    previamente guardado.  
;  - Optimizacion del uso de memoria ram
;  - Incorporacion de resultados termodinamicos dados por SWMF
;  - Incorporacion de campo magnetico dado por SWMF y trazado a
;    travez de èl.  
;
!EXCEPT=2

if keyword_set(ldem_file)  then file_flag = 0
if keyword_set(awsom_file) then file_flag = 1

  if not keyword_set(fdips_file) and not keyword_set(field_awsom) and not keyword_set(pfss_data_file) then begin
     print,'set the PFSS model to trace the DEMT results'
     return
  endif
  if not keyword_set(ldem_file ) and not keyword_set(awsom_file) then begin
     print,'set a DEMT or AWSOM file to be traced'
     return
  endif     
  if not keyword_set(period)     then begin
     print,'set a string with the period'
     return
  endif

; Add radstart suffix to the output filename:
  if n_elements(radstart) eq 1 then $
  period=period+'_radstart-'+strmid(string(radstart),6,5)+'Rs'
  if n_elements(radstart) gt 1 then $
  period=period+'_radstart-'+strmid(string(radstart(0)),6,5)+'-'+strmid(string(radstart(n_elements(radstart)-1)),6,5)+'Rs'



; Make the output filename:
  if keyword_set(marcgrid) then suffix='_marcgrid'  
  if keyword_set(aunifgrid) then suffix='_unifgrid'
  if keyword_set(unifgrid_v2) then suffix='_unifgrid_v2'
  output_file='traceLDEM_CR'+period+suffix+'.heating.sampled.v2.DIEGO.dat'

; Set parameters for Marc's line-tracing routines:
  if NOT keyword_set(radstart ) then radstart  = 1.5
  if NOT keyword_set(safety   ) then safety    = 0.2
  if NOT keyword_set(stepmax  ) then stepmax   = 30000
  if NOT keyword_set(fieldtype) then fieldtype = 5.0
  if NOT keyword_set(spacing)   then spacing   = 2.0

  print,'-------------------------------------------'
  print,'     Period: ',period
  case file_flag of 
     0: print,'  LDEM file: ',ldem_file
     1: print,'  awsom file: ',awsom_file
  endcase
  if keyword_set(fdips_file) then print,' FDIPS file: ',fdips_file
  if keyword_set(awsom_file) then print,' AWSOM suffix: ',awsom_file
  print,'Output file: ',output_file
  print,'-------------------------------------------'

  print,'safety: ',safety
  print,'stepmax: ',stepmax

  if keyword_set(pfss_data_file) then goto,salto_creacion_pfss
; Set the FDIPS filename to read:
; PFSSM_model='/data1/DATA/PFSSM/'+fdips_file
if keyword_set(fdips_file) then PFSSM_model= fdips_file
; Read the FDIPS model and create a structure to serve as input to Marc's routines:
  if not keyword_set(mhd) and not keyword_set(field_awsom)   then create_structure    ,    PFSSM_model
  if     keyword_set(mhd)                                    then create_structure_MHD,    '/data1/DATA/MHD_SWMF/'+fdips_file
;  if     keyword_set(field_awsom)                            then create_structure_MHD_new,'/data1/DATA/MHD_SWMF/'+fdips_file
  if     keyword_set(field_awsom)                            then read_structure_MHD,'/data1/work/MHD/'+field_awsom,sph_data ;esto e sun simple restore!!!!

; change the name of the created structure to a new name:
  pfss_data = sph_data
  undefine,sph_data             ;liberando espacio 300Mb  
; Set the uniform grid size, in case /unifgrid is used for the starting points. 
; Default size is 90x180.
  if NOT keyword_set(dlat) then dlat = 2. + fltarr(n_elements(radstart))  
  if NOT keyword_set(dlon) then dlon = 2. + fltarr(n_elements(radstart))

; If BOX was not set use the full corona:
  if NOT keyword_set(box)  then box = [0.,-90,360.,+90.] 
  box=float(box)

; Set up the starting points:
  if keyword_set(marcgrid) then spherical_field_start_coord,pfss_data,fieldtype,spacing,radstart=radstart,bbox=box
  if keyword_set(aunifgrid) then sph_field_str_coord_unifang,pfss_data,dlat,dlon        ,radstart=radstart,bbox=box
  if keyword_set(unifgrid_v2) then sph_field_str_coord_unifang_v2,pfss_data,dlatv=dlat,dlonv=dlon,radstartv=radstart,bbox=box
;guardar el pfss_data si se acaba de calcular.
; And now, do trace the field lines:
stop
if not keyword_set (pfss_data_file) then  spherical_trace_field,pfss_data,linekind=linekind,linelengths=linelengths,safety=safety,stepmax=stepmax 
stop
if not keyword_set (pfss_data_file) then  save,pfss_data,linekind,linelengths,FILENAME = 'pfss_data_'+awsom_file+period+'.sav'
salto_creacion_pfss:
if keyword_set (pfss_data_file) then restore,pfss_data_file

; Change the coding for linekind:
  linekind=linekind-2           ; so that 0=open and 1=closed

; Number of total traced lines:
  Nlin_all   = (size(*pfss_data.ptr))(2)
; Maximum number of points along those lines:
  Nptmax     = (size(*pfss_data.ptr))(1)
; Select field lines that were identified as open or closed by
  iOC        = where ( linekind eq 0 or linekind eq 1)
; Number of selected field lines (open or closed)
  Nlin     = 0L
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
  if not keyword_set(dgfw) and not keyword_set(awsom_file) and keyword_set(ldem_file) then      read_ldem,ldem_file,/ldem,/gauss1
  if     keyword_set(dgfw) and not keyword_set(awsom_file) and keyword_set(ldem_file) then      read_ldem,ldem_file,/ldem,/dgfw
;  if     keyword_set(awsom)then      read_awsom,awsom_file

  if     keyword_set(awsom_file) then  begin
     read_awsom_matrix,suff_file=awsom_file,nr=26,nt=90,np=180,/te_out,te,/ne_out,n_e,/qrad_out,qrad;,/nelasco_out,ne_lasco,/qheat_out,qheat,/qebyq_out,qebyq
;OBS: las salidas te,n_e,qrad etc tienene que estar en el mismo orden
;que figuran en el read_awsom_matrix sino aca cam,bian de nombre y s
;epudre el rancho, viejo
;     Nrad=500
;     nr=500
;las matrices fueron previamente interpoladas
     Nrad=26
     nr=26
     Nlat=90
     nth=90
     Nlon=180
     np =180
     dt = 2.
     dr = 0.01
     rad =   1. + dr/2. + dr * findgen(Nrad)
     lon =   0. + dt/2. + dt * findgen(Nlon)
     lat = -90. + dt/2. + dt * findgen(Nlat)
     ;DEMc= N_e * 0. - 666.
     ;ScoreR=N_e * 0. - 666.
     ;Wt = N_e * 0. - 666.
     ;lambda= fltarr(nr,nth,np,3) - 666.
     WTc = -666.
     Tmin=500000.
     Tmax=3.50000e+06
     Er = qrad                  ;solo un cambio de nombre
     Tm = Te
  endif
stop 
  dr_tom = rad(1)-rad(0)        ; grid radial bin size
  if     keyword_set(awsom_file) then Rmax_tom = rad(nr-3)
  if not keyword_set(awsom_file) then Rmax_tom = rad(nr-3) ; maximum height for which LDEM was computed
  

  if not keyword_set(awsom_file) then begin
; Compute the scoreR for quality-selection purposes:
     ratio = sfbe/tfbe
 ;scoreR=total(    (1.-ratio)^2 , 4 ) / float(nband)
     scoreR=total( abs(1.-ratio)   , 4 ) / float(nband)
  endif
  Nptmax_v = 150                ; ESTO NO ES ROBUSTO, 
  if keyword_set(awsom_file)  then Nptmax_v = 150
;  sin embargo, por la experiencia de haber realizado varios trazados
;  creo que va funcionar. 
;  Ningun sampleo supera este valor de puntos por linea

; Generate the tomographic grid based results
; one data point per tomographic voxel crossed by line.
      Ne_v = fltarr(Nptmax_v,Nlin)
      Tm_v = fltarr(Nptmax_v,Nlin)
      Er_v = fltarr(Nptmax_v,Nlin)
      if keyword_set(awsom_file)  then begin
;         qheat_v = fltarr(Nptmax_v,Nlin)
;         qebyq_v = fltarr(Nptmax_v,Nlin)
;         ne_lasco_v = fltarr(Nptmax_v,Nlin) 
      endif
      
      if keyword_set(ldem_file) then begin
         npar = (size(lambda))(4)         
         WT_v = fltarr(Nptmax_v,Nlin)
         lambda_v = fltarr(Nptmax_v,Nlin,npar)
         DEMc_v = fltarr(Nptmax_v,Nlin)     
         scoreR_v = fltarr(Nptmax_v,Nlin) 
      endif
  rad_v = fltarr(Nptmax_v,Nlin)
     lat_v = fltarr(Nptmax_v,Nlin)
     lon_v = fltarr(Nptmax_v,Nlin)
       s_v = fltarr(Nptmax_v,Nlin)
       B_v = fltarr(Nptmax_v,Nlin)
      Br_v = fltarr(Nptmax_v,Nlin)
     Bth_v = fltarr(Nptmax_v,Nlin)
     Bph_v = fltarr(Nptmax_v,Nlin)
 midCell_v = fltarr(Nlin) - 666.
    Npts_v = fltarr(Nlin) - 666. 
   enrad_v = 0. * str_v - 555.
   enlon_v = 0. * str_v - 555.
   enlat_v = 0. * str_v - 555.

; The following double-loop traces the tomographic results along the
; selected field lines:
xxx=0
  for il = 0L, Nlin-1 do begin
     ;stop  ;<--
     print,'tracing the DEMT results along the line '+string(il+1)+'    of '+string(Nlin)
     il_all=(findgen(Nlin_all))(iOC(il))
     Np_l      = Nstep(il)      ;  Number of points along the il-line
     
; Build more arrays:   
           s_l = fltarr(Np_l)      -666. 
          Ne_l = fltarr(Np_l)      -666. 
          Tm_l = fltarr(Np_l)      -666. 
          Er_l = fltarr(Np_l)      -666. 
          if keyword_set(ldem_file) then begin
             WT_l = fltarr(Np_l)      -666. 
             lambda_l = fltarr(Np_l,npar) -666. 
             DEMc_l = fltarr(Np_l)      -666. 
             scoreR_l = fltarr(Np_l)      -666. 
          endif
          Br_l = fltarr(Np_l)      -666. 
          Bth_l = fltarr(Np_l)      -666. 
          Bph_l = fltarr(Np_l)      -666. 
          B_l = fltarr(Np_l)      -666. 
          lab_l = fltarr(Np_l)      -666. 
          if keyword_set(awsom_file)  then begin
;             Ne_lasco_l = fltarr(Np_l)      -666.
 ;            qheat_l = fltarr(Np_l)      -666.
 ;            qebyq_l = fltarr(Np_l)      -666.
          endif
; These next five arrays are futures implementacion
  ;Happix    = fltarr(Np_l) -666.
  ;Bappix    = fltarr(Np_l) -666.
  ;Bmean     = fltarr(Np_l) -666.
  ;Bfoot1    = fltarr(Np_l) -666.
  ;Bfoot2    = fltarr(Np_p) -666. 
     
   rad_l  = reform((*pfss_data.ptr) (0:Np_l-1,il_all))
    th_l  = reform((*pfss_data.ptth)(0:Np_l-1,il_all))
   lat_l  = 90. - th_l / !dtor
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
xxx=0L
  for ir = 0,Np_l-1 do begin
      s_l (ir) = total(sqrt(ds2l(0:ir)))
      r0 = rad_l (ir)
     th0 =  th_l(ir)
     ph0 =  ph_l(ir)
     ptc = [r0,th0,ph0]
     irc = get_interpolation_index(*pfss_data.rix,ptc(0))
     ithc= get_interpolation_index(*pfss_data.lat,90-ptc(1)*!radeg)
     iphc= get_interpolation_index(*pfss_data.lon,(ptc(2)*!radeg+360) mod 360)
      Brc= interpolate(*pfss_data.Br ,iphc,ithc,irc)
     Bthc= interpolate(*pfss_data.Bth,iphc,ithc,irc)
     Bphc= interpolate(*pfss_data.Bph,iphc,ithc,irc)  
     
     if r0 eq max(rad)+dr_tom/2 then r0=r0*(1.-1.e-5)
     if r0 lt max(rad)+dr_tom/2 then begin
      determindex,r0,th0,ph0,irad,ilat,ilon
     if irad ne -1 and ilon ne -1 and ilat ne -1 then begin
        lab_l(ir) = (Nth*Np)*irad+(Np)*ilat+ilon+1    ; Voxel label
        if  rad_l(ir) le Rmax_tom+dr_tom/2 then begin ;<--
           Ne_l(ir)   = N_e(irad,ilat,ilon)
           Tm_l(ir)   = Tm (irad,ilat,ilon)
           Er_l(ir)   = Er (irad,ilat,ilon)
           if keyword_set(awsom_file) then begin
  ;            Ne_lasco_l(ir) = ne_lasco (irad,ilat,ilon)
  ;            qheat_l(ir) = qheat (irad,ilat,ilon)
  ;            qebyq_l(ir) = qebyq (irad,ilat,ilon)
           endif
           if keyword_set(ldem_file) then begin           
              WT_l(ir)   = WT (irad,ilat,ilon)
              lambda_l(ir,*) = lambda(irad,ilat,ilon,*) 
              DEMc_l  (ir)   = DEMc  (irad,ilat,ilon)   
              scoreR_l(ir)   = scoreR (irad,ilat,ilon) 
           endif
        endif                
          Br_l(ir) = Brc
         Bth_l(ir) = Bthc
         Bph_l(ir) = Bphc
           B_l(ir) = sqrt(Brc^2 + Bthc^2 + Bphc^2) 
      endif 
   endif

  endfor                        ; closes radial loop

; beginning the sampled
;--------------------------------------------------------------------------------------------------

;if rad_l(0) gt 2.4 then stop ;ESTO DEBE CAMBIARSE AHORA QUE TRAZAMOS HASTA 6RSUN

        line_end = 0
        ivox     = 0
        is       = 0 
        lab0     = lab_l(is)
        while line_end eq 0 do begin
           npp=0
           while lab_l(is) eq lab0 do begin
              npp=npp+1        
              is=is+1
               if lab0 eq -666. or is-1 eq Np_l-1 then goto,nextvoxel
               ;if lab0 eq -666.                   then goto,nextvoxel 
;this line was modified 2013-08-16 by F.A.N. there was not robust enough
 
           endwhile
           ; "npp" should be now the number of points within voxel "lab0" 
           ; "is" should be now the index of the 1st point in the NEXT voxel  
           index=is-npp+indgen(npp)
           ; "index" should be now the 1-D index of all points in
           ; voxel lab0, to be used in arrays *_l
           ind = (median(index))(0)
                Ne_v(ivox,il) =     Ne_l(ind)
                Tm_v(ivox,il) =     Tm_l(ind)
                Er_v(ivox,il) =     Er_l(ind)
                if keyword_set(awsom_file) then begin
   ;                Ne_lasco_v(ivox,il) = ne_lasco_l (ind)
   ;                qheat_v(ivox,il) = qheat_l (ind)
   ;                qebyq_v(ivox,il) = qebyq_l (ind)
                endif
                if keyword_set(ldem_file) then begin
                   WT_v(ivox,il) =     WT_l(ind)
                   lambda_v(ivox,il,*)=lambda_l(ind,*)
                   DEMc_v(ivox,il) =   DEMc_l(ind)  
                   scoreR_v(ivox,il) = scoreR_l(ind)
                endif
           if npp mod 2 eq 1 then begin  ; does this if np=odd 
               rad_v(ivox,il) =    rad_l(ind)
               lat_v(ivox,il) =    lat_l(ind)
               lon_v(ivox,il) =    lon_l(ind)
                 s_v(ivox,il) =      s_l(ind)
                 B_v(ivox,il) =      B_l(ind)
                Br_v(ivox,il) =     Br_l(ind) 
               Bth_v(ivox,il) =    Bth_l(ind)
               Bph_v(ivox,il) =    Bph_l(ind)     
           endif else begin             ; does this if np=even 
             rad_v(ivox,il) = 0.5*(rad_l(ind)+rad_l(ind-1))
             lat_v(ivox,il) = 0.5*(lat_l(ind)+lat_l(ind-1))
             lon_v(ivox,il) = 0.5*(lon_l(ind)+lon_l(ind-1))
               s_v(ivox,il) = 0.5*(  s_l(ind)+  s_l(ind-1))
               B_v(ivox,il) = 0.5*(  B_l(ind)+  B_l(ind-1))
              Br_v(ivox,il) = 0.5*( Br_l(ind)+ Br_l(ind-1))
             Bth_v(ivox,il) = 0.5*(Bth_l(ind)+Bth_l(ind-1))
             Bph_v(ivox,il) = 0.5*(Bph_l(ind)+Bph_l(ind-1))   
           endelse
           ; If this voxel is the maximum of the tom grid
           ; but there are MORE points traced => it is a closed
           ; loop with appex > Rmax_tom. If so, store midCell_v.
           ; Also, do it only if midCell_v was not stored already.
           ; See in Zhenguang's Matlab code that this midCell_v 
           ; is same as his.
           ; Rmax_tom = 1.255 ---> 1.24 
           ; dr_tom = 0.01
goto,skip_print
          print,'vox',ivox
          print,'ind',ind
          print,'med ind',index
          print,'label',lab_l(index)
          print,rad_l(ind), Rmax_tom - dr_tom/2
          print,max(index),Np_l-1
          print,opcls(il)
          print,'midcel',midCell_v(il)
          print, 'is',is,'pts',Np_l
skip_print:
          if rad_l(ind) ge Rmax_tom - dr_tom/2 AND max(index) lt Np_l-1 AND opcls(il) eq 1. then midCell_v(il) = ivox
          ivox = ivox+1         ; increase ivox 
           
           nextvoxel:
            if is-1 eq Np_l-1 then line_end = 1
            if is-1 lt Np_l-1 then     lab0 = lab_l(is)          

;    print, 'line end',line_end
;    print, 'lab0', lab0

         endwhile; closes each line's while
        Npts_v(il) = ivox; For each line, record the number of points in vector
; Label small-closed-loops as "2"
        if opcls(il) eq 1. and midCell_v(il) eq -666. then begin
                                ;midcell_v(il) = where ( rad_v(0:npts_v(il)-1,il) eq max (rad_v(0:npts_v(il)-1,il)))
           opcls(il) = 2.
           escalar = where ( rad_v(0:npts_v(il)-1,il) eq max (rad_v(0:npts_v(il)-1,il)))
           midcell_v(il)= escalar(0)
                                ;Mofied 12/05/2016 by D.G.L, evading a
                                ;vector prevents an erroneous loop definition
           if n_elements(escalar) gt 1 then xxx=xxx+1 ; xxx should be eq 0 at the end of the trace, otherwise the other line is not working properly
        endif
;   if opcls(il) eq 0. and midcell_v(il) gt -666. then stop
;-------------------------------------------------------------------------------------------
endfor                          ; closes lines loop
  print, xxx

; Trim all unnecesary information from resulting arrays:
  Npts_max = max(Npts_v)
  print, 'IMPORTANTE: puntos maximos de todas las lineas --> Npts_max'+Npts_max
  
    Ne_v  = reform(     Ne_v(0:Npts_max-1,*) ) 
    Tm_v  = reform(     Tm_v(0:Npts_max-1,*) ) 
    Er_v  = reform(     Er_v(0:Npts_max-1,*) )
    if keyword_set(awsom_file) then begin
    ;   Ne_lasco_v = reform( ne_lasco_v (0:Npts_max-1,*) )
    ;   qheat_v = reform( qheat_v (0:Npts_max-1,*) )
    ;   qebyq_v = reform( qebyq_v (0:Npts_max-1,*) )
    endif
    if keyword_set(ldem_file) then begin
       WT_v  = reform(     WT_v(0:Npts_max-1,*) )
       lambda_v  = reform( lambda_v(0:Npts_max-1,*,*) )
       DEMc_v  = reform(   DEMc_v(0:Npts_max-1,*) )  
       scoreR_v  = reform( scoreR_v(0:Npts_max-1,*) )
    endif
   rad_v  = reform(    rad_v(0:Npts_max-1,*) ) 
   lat_v  = reform(    lat_v(0:Npts_max-1,*) ) 
   lon_v  = reform(    lon_v(0:Npts_max-1,*) ) 
     s_v  = reform(      s_v(0:Npts_max-1,*) )
     B_v  = reform(      B_v(0:Npts_max-1,*) )
    Br_v  = reform(     Br_v(0:Npts_max-1,*) )
   Bth_v  = reform(    Bth_v(0:Npts_max-1,*) )
   Bph_v  = reform(    Bph_v(0:Npts_max-1,*) )  
; Save the sampled data:

   if keyword_set(ldem_file) then save,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc,Nlin,Npts_max,rad_v,lat_v,lon_v,$
                                       s_v,npts_v,midcell_v,loopL,opcls,Ne_v,Tm_v,WT_v,scoreR_v,str_v,stth_v,stph_v,$
                                       B_v,Br_v,Bth_v,Bph_v,Tmin,Tmax,npar,DEMc_v,lambda_v,enrad_v,enlat_v,enlon_v,Er_v,FILENAME = output_file+'.sav'

   if keyword_set(awsom_file) then save,fieldtype,spacing,radstart,Rmax_tom,dr_tom,Nlin,Npts_max,rad_v,lat_v,lon_v,$
                                        s_v,npts_v,midcell_v,loopL,opcls,Ne_v,Tm_v,str_v,stth_v,stph_v,$
                                        B_v,Br_v,Bth_v,Bph_v,enrad_v,enlat_v,enlon_v,Tmin,Tmax,Er_v,FILENAME = output_file+'.sav'

   stop
   goto,final
   openw,1,output_file
      if keyword_set(ldem_file) then  writeu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
      if keyword_set(awsom_file) then  writeu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom
   writeu,1,Nlin,Npts_max
   writeu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
   if keyword_set(ldem_file) then writeu,1,Ne_v,Tm_v,WT_v,scoreR_v
   if keyword_set(awsom_file) then writeu,1,Ne_v,Tm_v,Er_v
   writeu,1,str_v,stth_v,stph_v
   writeu,1,B_v,Br_v,Bth_v,Bph_v   
;------AGREGADO--------------
   writeu,1,enrad_v,enlon_v,enlat_v
   if keyword_set(awsom_file) then    writeu,1,ne_lasco_v,qheat_v,qebyq_v
;<---------------
   L=0
   if Tmax gt 3.4e6 and Tmax lt 3.6e6 then L=171
   if Tmax gt 3.9e6 and Tmax lt 4.1e6 then L=192
   writeu,1,Tmin,Tmax,L
   if keyword_set(ldem_file) then begin
      writeu,1,npar,DEMc_v
      writeu,1,lambda_v
   endif
;<---------------
   close,  1
   final:
   return
end
