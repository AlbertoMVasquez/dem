pro test,ind=ind,filelabel=filelabel

common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb
common starttrace,strad,stlat,stlon,footrad,footlat,footlon 


filesT = ['traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.035Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.045Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.075Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.115Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.155Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.250Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_expand_radstart-1.500Rs_unifgrid.heating.sampled.v2.CECI.dat']

NfilesT     = n_elements(filesT)
rstart      = [1.035,1.045,1.075,1.115,1.155,1.250,1.500]
Nlineas     = findgen(NfilesT)*0

Nsmallclose = findgen(NfilesT)*0
Nlargeclose = findgen(NfilesT)*0
Nopen       = findgen(NfilesT)*0

Nsmall_leg  = findgen(NfilesT)*0
Nlarge_leg  = findgen(NfilesT)*0
 Nopen_leg  = findgen(NfilesT)*0

Nof1 = findgen(NfilesT)*0
Nof2 = findgen(NfilesT)*0
Nof  = findgen(NfilesT)*0

Nlf1 = findgen(NfilesT)*0
Nlf2 = findgen(NfilesT)*0
Nlf3 = findgen(NfilesT)*0
Nlf4 = findgen(NfilesT)*0
Nlf  = findgen(NfilesT)*0

Nsf1 = findgen(NfilesT)*0
Nsf2 = findgen(NfilesT)*0
Nsf3 = findgen(NfilesT)*0
Nsf4 = findgen(NfilesT)*0
Nsf  = findgen(NfilesT)*0

marker_ind = intarr(NfilesT)
marker_ind(ind)=1

for  i=0,NfilesT-1 do begin
if marker_ind(i) eq 0 then goto,lainsoportablelevedad

   read_trace_sampled,filesT(i)

   Nlineas(i)=n_elements(rad_v(1,*))

   small_close = where(opcls eq 2) & Nsmallclose(i) = n_elements(small_close)
   large_close = where(opcls eq 1) & Nlargeclose(i) = n_elements(large_close)
   open        = where(opcls eq 0) &      Nopen (i) = n_elements(open)

goto,ejemplo_grafico
window,i,XSIZE=800, YSIZE=900, title=rstart(i)
!p.multi=[0,2,3]

plot,rad_v(0:Npts_v(5000)-1,5000), Ne_v(0:Npts_v(5000)-1,5000),yrange=[-0.3,1.5]*1.e8,xstyle=1,ystyle=1,title='Density vs rad' ,charsize=2
plot,lat_v(0:Npts_v(5000)-1,5000),rad_v(0:Npts_v(5000)-1,5000),                       xstyle=1,ystyle=1,title='loop rad vs lat',charsize=2

plot,rad_v(0:Npts_v(10000)-1,10000), Ne_v(0:Npts_v(10000)-1,10000),yrange=[-0.3,1.5]*1.e8,xstyle=1,ystyle=1,charsize=2
plot,lat_v(0:Npts_v(10000)-1,10000),rad_v(0:Npts_v(10000)-1,10000),                       xstyle=1,ystyle=1,charsize=2

plot,rad_v(0:Npts_v(14000)-1,14000), Ne_v(0:Npts_v(14000)-1,14000),yrange=[-0.3,1.5]*1.e8,xstyle=1,ystyle=1,charsize=2
plot,lat_v(0:Npts_v(14000)-1,14000),rad_v(0:Npts_v(14000)-1,14000),                       xstyle=1,ystyle=1,charsize=2

!p.multi=0
ejemplo_grafico:

;record_gif,'./','cambio_Ne_rstart_distintos.gif','X'

  statloop,filesT(i),rloopmin=1.025,/linear

  ; select legs with enough data points
  iok = where(gradT ne -555.)
  footlat_s   = footlat(iok)
  footlon_s   = footlon(iok)
  opclstat_s  = opclstat(iok) 
  indexloop_s = indexloop(iok)
  
  ; create concatened data arrays for the footpoint maps
  if i eq ind(0) then begin
  print,'defino _c'
  footlat_c   = footlat_s
  footlon_c   = footlon_s  
  opclstat_c  = opclstat_s
  indexloop_c = indexloop_s
  help,footlat_c
  endif
  if i gt ind(0) AND marker_ind(i) eq 1 then begin
  print,'concateno'
  footlat_c =[footlat_c,footlat_s]
  footlon_c =[footlon_c,footlon_s]
  opclstat_c=[opclstat_c,opclstat_s]
  indexloop_c=[indexloop_c,indexloop_s]
  help,footlat_c
  endif

;;=====================================Energy=========================================================================
goto,cookbooks

window,6,XSIZE=800, YSIZE=900, title=rstart(i)
!p.multi=[0,1,3]

plot,footlat,Phir,psym=3,charsize=3,xrange=[-90,90],ystyle=1,ytit='Phir'

plot,footlat,Fcb,psym=3,charsize=3,xrange=[-90,90],ystyle=1,ytit='Fcbb'

;plot,latt,Phihh,charsize=3,xrange=[-90,90],ystyle=1,ytit='Phih'

!p.multi=0
cookbooks:
;===========================Análisis de piernas=======================================================================

;===========open

goto,lainsoportablelevedad
   open_leg=where(opclstat eq 0)            
   Nopen_leg(i)=n_elements(open_leg)
   flopen=footlat(where(opclstat eq 0))

        open_foot1 = where(flopen le  80. and flopen ge  50.) & Nof1(i)=n_elements(open_foot1) ;[50,80]
        open_foot2 = where(flopen le -50. and flopen ge -80.) & Nof2(i)=n_elements(open_foot2) ;[-80,-50]
      
        Nof(i)    = Nof1(i) + Nof2(i)

ofoot=findgen(Nof(i))*0
ofoot(0:Nof1(i)-1)=flopen(open_foot1)
ofoot(Nof1(i):Nof(i)-1)=flopen(open_foot2)

;=========small

   if rstart(i) lt 1.200 then begin ; lo separo porque para rstart altos, esto es -1 y ya no puede hacer Nleg(-1)

      small_leg=where(opclstat eq 2) 
      Nsmall_leg(i)=n_elements(small_leg)

      flsmall=footlat(where(opclstat eq 2))
  
        small_foot1 = where(flsmall lt  30. and flsmall ge   0.) & Nsf1(i)=n_elements(small_foot1) ;[0,30)
        small_foot2 = where(flsmall le  70. and flsmall ge  30.) & Nsf2(i)=n_elements(small_foot2) ;[30,70]
        small_foot3 = where(flsmall lt   0. and flsmall ge -30.) & Nsf3(i)=n_elements(small_foot3) ;[-30,0)
        small_foot4 = where(flsmall lt -30. and flsmall ge -70.) & Nsf4(i)=n_elements(small_foot4) ;[-70,-30)

        Nsf(i)     = Nsf1(i) + Nsf2(i) + Nsf3(i) + Nsf4(i)       
 
sfoot=findgen(Nsf(i))*0
sfoot(0:Nsf1(i)-1)=flsmall(small_foot1)
sfoot(Nsf1(i):Nsf1(i)+Nsf2(i)-1)=flsmall(small_foot2)
sfoot(Nsf1(i)+Nsf2(i):Nsf1(i)+Nsf2(i)+Nsf3(i)-1)=flsmall(small_foot3)
sfoot(Nsf1(i)+Nsf2(i)+Nsf3(i):Nsf(i)-1)=flsmall(small_foot4)

endif else begin

if rstart(i) gt 1.200 then sfoot=findgen(7000)*0

endelse

;=========large
      
   large_leg=(where(opclstat eq 1))            
   Nlarge_leg(i)=n_elements(large_leg)
   fllarge=footlat(where(opclstat eq 1))

        large_foot1 = where(fllarge lt  30. and fllarge ge   0.) & Nlf1(i)=n_elements(large_foot1) ;[0.30)
        large_foot2 = where(fllarge le  70. and fllarge ge  30.) & Nlf2(i)=n_elements(large_foot2) ;[30,70]
        large_foot3 = where(fllarge lt   0. and fllarge ge -30.) & Nlf3(i)=n_elements(large_foot3) ;[-30,0)
        large_foot4 = where(fllarge lt -30. and fllarge ge -70.) & Nlf4(i)=n_elements(large_foot4) ;[-70.-30)

        Nlf(i)     = Nlf1(i) + Nlf2(i) + Nlf3(i) + Nlf4(i)            

lfoot=findgen(Nlf(i))*0
lfoot(0:Nlf1(i)-1)=fllarge(large_foot1)
lfoot(Nlf1(i):Nlf1(i)+Nlf2(i)-1)=fllarge(large_foot2)
lfoot(Nlf1(i)+Nlf2(i):Nlf1(i)+Nlf2(i)+Nlf3(i)-1)=fllarge(large_foot3)
lfoot(Nlf1(i)+Nlf2(i)+Nlf3(i):Nlf(i)-1)=fllarge(large_foot4)

;=============Histograma
;goto,chauhisto

   xmin=-100
   xmax= 100
   nb  = 100 
   No1 = histogram(ofoot,min=xmin,max=xmax,nbins=nb,locations=xrat)
   Ns1 = histogram(sfoot,min=xmin,max=xmax,nbins=nb,locations=xrat) 
   Nl1 = histogram(lfoot,min=xmin,max=xmax,nbins=nb,locations=xrat)

window,i,XSIZE=800, YSIZE=900, title=rstart(i)

!p.multi=[0,1,3]

  plot,xrat,No1,charsize=3,yrange=[0,1000],ytit='open_loops',/nodata
    oplot,xrat,No1,psym=10

  plot,xrat,Nl1,charsize=3,yrange=[0,1000],ytit='large_loops',/nodata
     oplot,xrat,Nl1,psym=10

  plot,xrat,Ns1,charsize=3,yrange=[0,1000],xtit='footlat',ytit='small_loops',/nodata
    oplot,xrat,Ns1,psym=10

!p.multi=0

chauhisto:
lainsoportablelevedad:

endfor

 footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel=filelabel,indexloop_c,rotacion

;====================================================================================================================

;goto,tografic

  print,'rstart        =',rstart
  print,'Nlineas       =',Nlineas
  print,'Nsmallclose   =',Nsmallclose
  print,'Nlargeclose   =',Nlargeclose
  print,'Nopen         =',Nopen
  print,''
  print,'Análisis de piernas'

  print,'Nsmall_leg    =',Nsmall_leg
  print,'Nlarge_leg    =',Nlarge_leg
  print,'Nopen_leg     =',Nopen_leg
  print,''
  print,'open'
  print,'rsatrt          =',rstart
  print,'footlat[50,80]  =',Nof1
  print,'footlat[-80,-50]=',Nof2
  print,''
  print,'large_close'
  print,'rsatrt          =',rstart
  print,'footlat[-70,-30)=',Nlf4
  print,'footlat[-30,0)  =',Nlf3
  print,'footlat[0,30)   =',Nlf1
  print,'footlat[30,70]  =',Nlf2
  print,''
  print,'small_close'
  print,'rsatrt          =',rstart
  print,'footlat[-70,-30)=',Nsf4
  print,'footlat[-30,0)  =',Nsf3
  print,'footlat[0,30)   =',Nsf1
  print,'footlat[30,70]  =',Nsf2

tografic:

stop

return
end

pro hacer_trace

; Ceci & Albert:
;fdips_file='fdips_field_151x180x360_synop_Mr_0.polfil.2081.dat'

;rad0=[1.045, 1.075, 1.115, 1.155, 1.250, 1.500]
rad0=[1.045,1.075,1.115]
for i=0,2 do begin
 stop
 fdips_file='fdips_field_150x180x360_synop_Mr_0.polfil.2081.ubdat'
ldem_file ='LDEM.v3_cr2081_l0.75_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.A_L171_DECON_gauss1_lin_Norm-median_singlStart'
   period ='2081_euviA-DECON_reg0.75_safety1.0_grid1'
trace_LDEM,fdips_file=fdips_file,ldem_file=ldem_file,period=period,radstart=rad0(i),safety=1.0,stepmax=15000,/unifgrid,dlat=1,dlon=1,/expand
;trace_LDEM,fdips_file=fdips_file,ldem_file=ldem_file,period=period,radstart=rad0,safety=1.0,stepmax=15000,/unifgrid,dlat=2,dlon=2,/expand
endfor

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
               Box=box,$
               dlat=dlat,$
               dlon=dlon,$
               mhd=mhd,$
               dgfw=dgfw,$
               expand=expand
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
;
; 7/24/15
; Modified by C. Mac Cormack & A.M. Vasquez.
;    - Trazado de los parámetros de la LDEM
;    - Expansión del trazado geométrico hasta 2.5 Rsun
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

if keyword_set(expand) then period=period+'_expand'

; Add radstart suffix to the output filename:
  period=period+'_radstart-'+strmid(string(radstart),6,5)+'Rs' 

; Make the output filename:
  if keyword_set(marcgrid) then suffix='_marcgrid'  
  if keyword_set(unifgrid) then suffix='_unifgrid'
  output_file='traceLDEM_CR'+period+suffix+'.heating.sampled.v2.CECI.dat' ;<-- le agrego CECI para que salga con otro nombre

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

;<--
if keyword_set(expand) then begin
nr=150
rad=1.+dr_tom/2+dr_tom*findgen(Nr)
endif
;<--

;stop  ;<--

; Compute the scoreR for quality-selection purposes:
  ratio = sfbe/tfbe
 ;scoreR=total(    (1.-ratio)^2 , 4 ) / float(nband)
  scoreR=total( abs(1.-ratio)   , 4 ) / float(nband)

  Nptmax_v = 1500                ; ESTO NO ES ROBUSTO, 
;  sin embargo, por la experiencia de haber realizado varios trazados
;  creo que va funcionar. 
;  Ningun sampleo supera este valor de puntos por linea

; Generate the tomographic grid based results
; one data point per tomographic voxel crossed by line.
      Ne_v = fltarr(Nptmax_v,Nlin)
      Tm_v = fltarr(Nptmax_v,Nlin)
      WT_v = fltarr(Nptmax_v,Nlin)
      Er_v = fltarr(Nptmax_v,Nlin)
   
      npar = (size(lambda))(4)        ;<--
  
  lambda_v = fltarr(Nptmax_v,Nlin,npar);<--
    DEMc_v = fltarr(Nptmax_v,Nlin)     ;<--
  scoreR_v = fltarr(Nptmax_v,Nlin) 
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
  

;stop ;<--


; The following double-loop traces the tomographic results along the
; selected field lines:

  for il = 0L, Nlin-1 do begin

;     il=100  ;<--
     ;stop    ;<--

     print,'tracing the DEMT results along the line '+string(il+1)+'    of '+string(Nlin)
     il_all=(findgen(Nlin_all))(iOC(il))
     Np_l      = Nstep(il)      ;  Number of points along the il-line
     
; Build more arrays:   
           s_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Ne_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Tm_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          WT_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Er_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.  
      lambda_l = fltarr(Np_l,npar) -666. ;fltarr(Nptmax,Nlin) -666.  ;<-- Intento grabar los tres valores de lambda 
        DEMc_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.  ;<--
      scoreR_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Br_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
         Bth_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
         Bph_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
           B_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
         lab_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
        
     ;stop ;<--

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
         lab_l(ir) = (Nth*Np)*irad+(Np)*ilat+ilon+1 ; Voxel label
        if  rad_l(ir) le Rmax_tom+dr_tom/2 then begin ;<--
             Ne_l(ir)   = N_e(irad,ilat,ilon)
             Tm_l(ir)   = Tm (irad,ilat,ilon)
             WT_l(ir)   = WT (irad,ilat,ilon)
             Er_l(ir)   = Er (irad,ilat,ilon)
         lambda_l(ir,*) = lambda(irad,ilat,ilon,*)  ;<-- grabo cada componente
         DEMc_l  (ir)   = DEMc  (irad,ilat,ilon)  ;<--
         scoreR_l(ir)   = scoreR (irad,ilat,ilon) 
        endif                    ;<--
          Br_l(ir) = Brc
         Bth_l(ir) = Bthc
         Bph_l(ir) = Bphc
           B_l(ir) = sqrt(Brc^2 + Bthc^2 + Bphc^2) 
      endif 
   endif
;stop  ;<--
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
                Ne_v(ivox,il) =     Ne_l(ind)
                Tm_v(ivox,il) =     Tm_l(ind)
                WT_v(ivox,il) =     WT_l(ind)
                Er_v(ivox,il) =     Er_l(ind)
            lambda_v(ivox,il,*)=lambda_l(ind,*)  ;<--
              DEMc_v(ivox,il) =   DEMc_l(ind)    ;<--
            scoreR_v(ivox,il) = scoreR_l(ind)
           if np mod 2 eq 1 then begin  ; does this if np=odd 
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
lambda_v  = reform( lambda_v(0:Npts_max-1,*,*) )    ;<--
  DEMc_v  = reform(   DEMc_v(0:Npts_max-1,*) )      ;<--
scoreR_v  = reform( scoreR_v(0:Npts_max-1,*) ) 
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
   writeu,1,Ne_v,Tm_v,WT_v,Er_v,scoreR_v
   writeu,1,str_v,stth_v,stph_v
   writeu,1,B_v,Br_v,Bth_v,Bph_v   
;------AGREGADO--------------
   writeu,1,enrad_v,enlon_v,enlat_v
;<---------------
   L=0
   if Tmax gt 3.4e6 and Tmax lt 3.6e6 then L=171
   if Tmax gt 3.9e6 and Tmax lt 4.1e6 then L=192
   writeu,1,Tmin,Tmax,L
   writeu,1,npar,DEMc_v
   writeu,1,lambda_v   
;<---------------
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
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
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
  dir='/data1/DATA/MLDT/' ;<--
; dir='/data/Trabajo/dem/' ;<--
  openr,1,dir+file
  readu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
  readu,1,Nlin,Npts_max
 
      Ne_v = fltarr(Npts_max,Nlin)
      Tm_v = fltarr(Npts_max,Nlin)
      WT_v = fltarr(Npts_max,Nlin)
      Er_v = fltarr(Npts_max,Nlin)
  scoreR_v = fltarr(Npts_max,Nlin)
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
     str_v = fltarr(Nlin)   
    stth_v = dblarr(Nlin)   
    stph_v = dblarr(Nlin)    
  readu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
  readu,1,Ne_v,Tm_v,WT_v,Er_v,scoreR_v
  readu,1,str_v,stth_v,stph_v 
  readu,1,B_v,Br_v,Bth_v,Bph_v     
;------AGREGADO--------------
   enrad_v = fltarr(Nlin)   
   enlon_v = fltarr(Nlin)   
   enlat_v = fltarr(Nlin)  
;------AGREGADO--------------
 readu,1,enrad_v,enlon_v,enlat_v
;----------------------------
    Tmin=0.
    Tmax=0.
    L=0
 readu,1,Tmin,Tmax,L
    npar   = 0                      ;<--
    DEMc_v = fltarr(Npts_max,Nlin)  
 readu,1,npar,DEMc_v
  lambda_v = fltarr(Npts_max,Nlin,npar) 
 readu,1,lambda_v
  close,1
  return
end

;make_mapoc,'traceLDEM_CR1914_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1914_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR1915_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1915_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR1919_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1919_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR2081_euviA_l1.0_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR2081_90X180blines_r_',1.075,/mdi
pro MAKE_MAPOC,file_input,filesuffix,rc,mdi=mdi,gng=gng
common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
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
common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  
common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb
common starttrace,strad,stlat,stlon,footrad,footlat,footlon 
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
 Nemean = fltarr(Nlegs)-555.
 Tmmean = fltarr(Nlegs)-555.
 WTmean = fltarr(Nlegs)-555.
Nestddev= fltarr(Nlegs)-555.
Tmstddev= fltarr(Nlegs)-555.
WTstddev= fltarr(Nlegs)-555.
; HS fits results for each leg
     Ne0 = fltarr(Nlegs)-555.
lambda_N = fltarr(Nlegs)-555.
   Tefit = fltarr(Nlegs)-555.
     r2N = fltarr(Nlegs)-555.
      P0 = fltarr(Nlegs)-555.
lambda_P = fltarr(Nlegs)-555.
     r2P = fltarr(Nlegs)-555.
   gradT = fltarr(Nlegs)-555.
     Tm0 = fltarr(Nlegs)-555.  ; esto es r

    Tm0s = fltarr(Nlegs)-555.  ;<--- en cada pierna. esto es s

     r2T = fltarr(Nlegs)-555.
   dTmds = fltarr(Nlegs)-555.
    r2Ts = fltarr(Nlegs)-555.
r2Tcuadr = fltarr(Nlegs)-555.
      Eh = fltarr(Nlegs)-555.

    Phir = fltarr(Nlegs)-963.  ;<--- flujo radiativo
     Fcb = fltarr(Nlegs)-963.  ;<--- Fc en la base

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

; Analysis for OPEN loops:
  if opcls(il) eq 0. then begin

     Ne_l = reform ( Ne_v(0:Npts_v(il)-1,il))
     Tm_l = reform ( Tm_v(0:Npts_v(il)-1,il))
     WT_l = reform ( WT_v(0:Npts_v(il)-1,il))
     Er_l = reform ( Er_v(0:Npts_v(il)-1,il))
 scoreR_l = reform ( scoreR_v(0:Npts_v(il)-1,il)) 
    rad_l = reform (rad_v(0:Npts_v(il)-1,il))
    lat_l = reform (lat_v(0:Npts_v(il)-1,il))
    lon_l = reform (lon_v(0:Npts_v(il)-1,il))
      s_l = reform (  s_v(0:Npts_v(il)-1,il))   
      B_l = reform (  B_v(0:Npts_v(il)-1,il))   
     Br_l = reform ( Br_v(0:Npts_v(il)-1,il))   
 
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

   ;Select useful data

    p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. and scoreR_l lt 0.10)

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

      Nemean(ileg) =   mean(Ne_l)
      Tmmean(ileg) =   mean(Tm_l)
      WTmean(ileg) =   mean(WT_l)
    Nestddev(ileg) = stddev(Ne_l)
    Tmstddev(ileg) = stddev(Tm_l)
    WTstddev(ileg) = stddev(WT_l)
    betamean(ileg) =   mean(beta_l)
       Bmean(ileg) =   mean(B_l)

    if n_elements(p) lt Ndata then goto,skipfitloop_open

   ;Make HS-fit to Ne(r) for each open leg/loop
      xfit = rad_l
      yfit =  Ne_l
    rminhs = min(rad_l);rmin
    rmaxhs = max(rad_l);rmax 

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
  
    fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2
            T0r = A[0]
      Tm0(ileg) = T0r
    gradT(ileg) = A[1]
      r2T(ileg) = corr2 

    ;Make LINEAR-fit to T(s) for each open leg/loop ======================
    
           xfit = s_l*rsun    
         rminhs = min(s_l*rsun);smin
         rmaxhs = max(s_l*rsun);smax 
           yfit = Tm_l
    fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2
            T0s = A[0] ; K

     Tm0s(ileg) = T0s  ;<---

   dTmds (ileg) = A[1] ; K/cm
   r2Ts  (ileg) = corr2 

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
  scoreR_l1 = reform ( scoreR_v(ifirs_1:ilast_1,il))
  scoreR_l2 = reform ( scoreR_v(ifirs_2:ilast_2,il))
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

  p1 = where ( rad_l1 ge rmin and rad_l1 le rmax and Ne_l1 ne -999. and scoreR_l1 lt 0.1 and WT_l1 ge WTc*1.e6)
  p2 = where ( rad_l2 ge rmin and rad_l2 le rmax and Ne_l2 ne -999. and scoreR_l2 lt 0.1 and WT_l2 ge WTc*1.e6)
  
  if  p1(0) eq -1 or p2(0) eq -1 then goto,skipnextloop


;----------------------------------------------------------------------------------------------------------------
  r0 = 1.025
  s_l1_max = max(s_l1) ; Rsun
  s_l2_max = max(s_l2) ; Rsun
  r_l1_max = max(rad_l1) ; Rsun
  r_l2_max = max(rad_l2) ; Rsun

 ; Fit lineal s_l1 = m1 rad_l1 + b1, usar primeros 5 datos.
 ; Fit lineal s_l2 = m2 rad_l2 + b2
   Npoints=Ndata
   if n_elements(rad_l1) ge Npoints AND n_elements(rad_l2) ge Npoints then begin 

   rad_l1_base = rad_l1(0:Npoints-1) 
;  rad_l2_base = rad_l2(0:Npoints-1) ;esot no toma los últimos puntos sino los primeros. Lo cambio porquedebería tomar llos últimosradios 
   rad_l2_rev  = reverse(rad_l2)  ;<--- da vuelta rad_l2 
   rad_l2_base = rad_l2_rev (0:Npoints-1)

     s_l1_base =   s_l1(0:Npoints-1) 
     s_l2_rev  =   reverse(s_l2)     ;<--- también necesito los últimos s_l2
     s_l2_base =   s_l2_rev(0:Npoints-1) 

   ; fit s_l1_base = m1 * rad_l1_base + s0r1 ; <----!!! HACER ESTO
  xfit = rad_l1_base
  yfit =   s_l1_base
  fitr0,xfit,yfit,A,corr2
         S0r1 = A[0]
         m1   = A[1]
         r2s1 = corr2

   ; fit s_l2_base = m2 * rad_l2_base + s0r2
  xfit = rad_l2_base
  yfit =   s_l2_base
  fitr0,xfit,yfit,A,corr2
         S0r2 = A[0]
         m2   = A[1]
         r2s2 = corr2

    s_l1_r0 = m1 * r0 + s0r1 
    s_l2_r0 = m2 * r0 + s0r2

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

;fits for leg1-------------------------------------- 
  rminhs = min(rad_l1) 
  rmaxhs = max(rad_l1)
  
;HS-fit to Ne(r)
  xfit = rad_l1
  yfit =  Ne_l1

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

  fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

         T0r1 = A[0]
    Tm0(ileg) = T0r1
  gradT(ileg) = A[1]
  r2T  (ileg) = corr2

 ; linear-fit to T(s) ==================================
      xfit = s_l1 *rsun    
    rminhs = min(s_l1*rsun);smin
    rmaxhs = max(s_l1*rsun);smax 
      yfit = Tm_l1
    fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2

            T0s1 = A[0]
      Tm0s(ileg) = T0s1  ;<---
    dTmds (ileg) = A[1]
    r2Ts  (ileg) = corr2

   if keyword_set(fit_cuadr) then begin
  ; cuadratic-fit to T(s) 
      xfit = s_l1 ;*rsun    
    rminhs = min(s_l1);*rsun);smin
    rmaxhs = max(s_l1);*rsun);smax 
    yfit   = Tm_l1
 
  fitcuadrTemp,xfit,yfit,rminhs,rmaxhs,A,corr2,T0s1,dTmds(ileg)*rsun
        Acuadr1 = A
 r2Tcuadr(ileg) = corr2
  endif

;fits for leg2-------------------------------------- 
  rminhs = min(rad_l2) 
  rmaxhs = max(rad_l2)

  ;HS-fit to Ne(r)
  xfit = rad_l2
  yfit =  Ne_l2

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

  fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

           T0r2 = A[0]
    Tm0(ileg+1) = T0r2
  gradT(ileg+1) = A[1]
  r2T  (ileg+1) = corr2

  ;linear-fit to T(s) ==========================================
      xfit = s_l2 *rsun   
    rminhs = min(s_l2*rsun);smin
    rmaxhs = max(s_l2*rsun);smax 
      yfit = Tm_l2
 
   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2

              T0s2 = A[0]
      Tm0s(ileg+1) = T0s2  ;<---
    dTmds (ileg+1) = A[1]
    r2Ts  (ileg+1) = corr2  

 if keyword_set(fit_cuadr) then begin
   ;cuadratic-fit to T(s) 
      xfit = s_l2 ;*rsun    
    rminhs = min(s_l2);*rsun;smin
    rmaxhs = max(s_l2);*rsun;smax 
    yfit   = Tm_l2
    fitcuadrTemp,xfit,yfit,rminhs,rmaxhs,A,corr2,T0s2,dTmds(ileg+1)
         Acuadr2 = A
r2Tcuadr(ileg+1) = corr2
 endif

;->STOP

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
   
s_l1 = s_l1 * rsun ; pass s_l1 to cm 
s_l2 = s_l2 * rsun ; pass s_l2 to cm 

;make dT/ds 
  dTmds_l1 = dTmds(ileg);DERIV(s_l1,Tm_l1)
  dTmds_l2 = dTmds(ileg+1);DERIV(s_l2,Tm_l2)     

if keyword_set(fit_cuadr) then  dTmds_l1= (2*Acuadr1[0]*(s_l1/rsun) + Acuadr1[1])/rsun
if keyword_set(fit_cuadr) then  dTmds_l2= (2*Acuadr2[0]*(s_l2/rsun) + Acuadr2[1])/rsun

;  make conductive flux 
;  Fc_l1= -kappa*Tm_l1^(5./2)*dTmds_l1
;  Fc_l2= -kappa*Tm_l2^(5./2)*dTmds_l2   
;  Fc_l1 = -2*kappa*DERIV(S_l1,Tm_l1^(7./2))/7
;  Fc_l2 = -2*kappa*DERIV(S_l2,Tm_l2^(7./2))/7

   Fc_l1= -kappa*(T0s1 +dTmds_l1*s_l1)^(5./2)*dTmds_l1
   Fc_l2= -kappa*(T0s2 +dTmds_l2*s_l2)^(5./2)*dTmds_l2   

if keyword_set(fit_cuadr) then   Fc_l1= -kappa*(Acuadr1[0]*(s_l1/rsun)^2 + Acuadr1[1]*(S_l1/rsun)+ Acuadr1[2])^(5./2)*dTmds_l1
if keyword_set(fit_cuadr) then   Fc_l2= -kappa*(Acuadr2[0]*(s_l2/rsun)^2 + Acuadr2[1]*(S_l2/rsun)+ Acuadr2[2])^(5./2)*dTmds_l2   

;stop

     Tmbase_l1=Tm0(ileg)  +gradT(ileg)  *r0
     Tmbase_l2=Tm0(ileg+1)+gradT(ileg+1)*r0
     Fc2_l1 = -kappa*Tmbase_l1^(5./2)*dTmds_l1 ;función en la base  ;<---
     Fc2_l2 = -kappa*Tmbase_l2^(5./2)*dTmds_l2 ;función en la base  ;<---
     Fcb(ileg  ) = Fc2_l1 
     Fcb(ileg+1) = Fc2_l2 
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

;----l2
xfit = rr_l2
yfit = Er_l2_base
fitEr,xfit,yfit,A,r2
Er_l2_r0  = A[0] * exp( A[1]*(r0) )
Er_l2_max = A[0] * exp( A[1]*(r_l2_max) )

;stop
    s_l1_e = [ s_l1_r0  * rsun, s_l1, s_l1_max * rsun]
    s_l2_e = [ s_l2_max * rsun, s_l2, s_l2_r0  * rsun]     ;<--- lo defino acá
    Er_l1_e = [Er_l1_r0,Er_l1,Er_l1_max]
    Er_l2_e = [Er_l2_max,Er_l2,Er_l2_r0] ;<---

;stop
     phir_l1 = int_tabulated(s_l1_e,Er_l1_e,/sort) ; flujo radiaivo ;<---
  phir(ileg) = phir_l1                                    ;<--
     phir_l2 = int_tabulated(s_l2_e,Er_l2_e,/sort) ; flujo radiaivo ;<--- di vuelta los datos. si doy vuelta s_l1 sale un - que debería poner?
phir(ileg+1) = phir_l2                                    ;<---     aunque tmb haya dado vuelta er? (lo pongo) 


if opcls(il) eq 2 then stop

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

;...........................................
;if Eh(ileg) lt 0 or Eh(ileg+1) lt 0 then  goto,skiptestloopclosed
;if r2sh(ileg) lt 0.75  or r2sh(ileg+1) lt 0.75  then
goto,skipmordor
if  r2t(ileg) gt 0.98 and r2t(ileg+1) gt 0.5  and opcls(il) eq 2 and gradt(ileg) gt 0 and gradt(ileg+1) gt 0 then begin
  wn=0
  window,wn,xs=400,ys=800
 !p.multi=[0,1,2]
 !p.charsize=2
 !p.charthick=1
 X=1.+.25*findgen(100)/99.
plot,rad_l1,ne_l1/1.e8,psym=4,title='Ne'
oplot,X,Ne0(ileg)* exp( -(1/lambda_N(ileg))*(1.-1./X) ) /1.e8
;oplot,rad_l2,ne_l2/1.e8,psym=5
;oplot,X,Ne0(ileg+1)* exp( -(1/lambda_N(ileg+1))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(Ne0(ileg)/1.e8),string(lambda_N(ileg)),string(r2n(ileg))],/normal
 plot,rad_l1,tm_l1/1.e6,psym=4,title='Tm'
  oplot,X,(T0r1+gradT(ileg)*X)/1.e6
; oplot,rad_l2,tm_l2/1.e6,psym=5
;oplot,X,(T0r2+gradT(ileg+1)*X)/1.e6
  xyouts,0.5*[1,1,1],(1./3)*1.-[.15,.2,.25],[string(T0r1/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal
stop
endif
skipmordor:
goto,skiptestloopclosed
; diferents plots for leg1
 
wn=0
;window,wn,xs=400,ys=800
  ps1,'./newfigs/'+'loop_example.eps',0
  DEVICE,/INCHES,YSIZE=10.,XSIZE=5.,SCALE_FACTOR=2
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 X=1.+.25*findgen(100)/99.

 plot,rad_l1,ne_l1/1.e8,psym=4,title='Ne(r)-leg1'
 oplot,X,Ne0(ileg)* exp( -(1/lambda_N(ileg))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(Ne0(ileg)/1.e8),string(lambda_N(ileg)),string(r2n(ileg))],/normal

 plot,rad_l1,p_l1,psym=4,title='P(r)-leg1'
 oplot,X,p0(ileg)* exp( -(1/lambda_p(ileg))*(1.-1./X) )
 xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.15,.2],[string(P0(ileg)),string(lambda_p(ileg)),string(r2p(ileg))],/normal

  plot,rad_l1,tm_l1/1.e6,psym=4,title='Tm(r)-leg1'
  oplot,X,(T0r1+gradT(ileg)*X)/1.e6
  xyouts,0.5*[1,1,1],(1./3)*1.-[.15,.2,.25],[string(T0r1/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal
 
 wn=1
 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 
  x = min(s_l1) + (max(s_l1)-min(s_l1))*findgen(100)/99
  
  plot,s_l1,tm_l1/1.e6,psym=4,title='Tm(s)-leg1 linear'
  oplot,X,(T0s1+dTmds(ileg)*rsun*X)/1.e6
  xyouts,0.5*[1,1,1],(1.)*1.-[.1,.15,.2],[string(T0s1/1.e6),string(dTmds(ileg)/1.e6*rsun),string(r2Ts(ileg))],/normal

  plot,s_l1,tm_l1/1.e6,psym=4,title='Tm(s)-leg1 cuadr'
  oplot,X,(Acuadr1[0]*(X)^2 + Acuadr1[1]*(X) + Acuadr1[2])/1.e6
  xyouts,0.5*[1],(2./3)*1.-[.2],[string(r2Tcuadr(ileg))],/normal
  
  plot,s_l1,Fc_l1,psym=4,title='Fc-leg1'
  
 wn=2
 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 
 plot,rad_l1,Er_l1,psym=4,title='radiative loss-leg1'

 plot,rad_l1,Ec_l1,psym=4,title='conductive loss-leg1'
 
 ;plot,rad_l1,Eh_l1,psym=4,title='heating rate-leg1'
 plot,s_l1,Eh_l1,psym=4,title='heating rate-leg1'
oplot,x,Eh01*exp(x/Sh(ileg))
xyouts,0.5*[1,1,1],(1./3)*1.-[.1,.15,.20],[string(Eh01),string(sH(ileg)),string(r2sh(ileg))],/normal
;stop

; diferents plots for leg2
 
 wn=0
 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 X=1.+.25*findgen(100)/99.

 plot,rad_l2,ne_l2/1.e8,psym=4,title='Ne-leg2'
 oplot,X,Ne0(ileg+1)* exp( -(1/lambda_N(ileg+1))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(Ne0(ileg+1)/1.e8),string(lambda_N(ileg+1)),string(r2n(ileg+1))],/normal

 plot,rad_l2,p_l2,psym=4,title='P-leg2'
 oplot,X,p0(ileg+1)* exp( -(1/lambda_p(ileg+1))*(1.-1./X) )
 xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.15,.2],[string(P0(ileg+1)),string(lambda_p(ileg+1)),string(r2p(ileg+1))],/normal

  plot,rad_l2,tm_l2/1.e6,psym=4,title='Tm-leg2'
  oplot,X,(T0r2+gradT(ileg+1)*X)/1.e6
  xyouts,0.5*[1,1,1],(1./3)*1.-[.15,.2,.25],[string(T0r2/1.e6),string(gradT(ileg+1)/1.e6),string(r2t(ileg+1))],/normal

wn=1
 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 
 x = min(s_l2) + (max(s_l2)-min(s_l2))*findgen(100)/99
  
  plot,s_l2,tm_l2/1.e6,psym=4,title='Tm(s)-leg2 linear'
  oplot,X,(T0s2+dTmds(ileg+1)*rsun*X)/1.e6
  xyouts,0.5*[1,1,1],(1.)*1.-[.1,.15,.2],[string(T0s2/1.e6),string(dTmds(ileg+1)/1.e6*rsun),string(r2Ts(ileg+1))],/normal

  plot,s_l2,tm_l2/1.e6,psym=4,title='Tm(s)-leg2 cuadr'
  oplot,X,(Acuadr2[0]*(X)^2 + Acuadr2[1]*(X) + Acuadr2[2])/1.e6
  xyouts,0.5*[1],(2./3)*1.-[.2],[string(r2Tcuadr(ileg+1))],/normal
  
  plot,s_l2,Fc_l2,psym=4,title='Fc-leg2'
 
wn=2

 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 
 plot,rad_l2,Er_l2,psym=4,title='radiative loss-leg2'

 plot,rad_l2,Ec_l2,psym=4,title='conductive loss-leg2'
 
 ;plot,rad_l2,Eh_l2,psym=4,title='heating rate-leg2'
  plot,s_l2,Eh_l2,psym=4,title='heating rate-leg2'
 oplot,x,Eh02*exp(x/Sh(ileg+1))
 xyouts,0.5*[1,1,1],(1./3)*1.-[.1,.15,.20],[string(Eh02),string(sH(ileg+1)),string(r2sh(ileg+1))],/normal
ps2
stop


skiptestloopclosed:
;...........................................


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
  if gradT(ileg)*gradT(ileg+1) lt 0. then begin
  indexloop(ileg)   = -678.   
  indexloop(ileg+1) = -678.
  endif 

  ileg = ileg+2

endelse
endfor

return
end

  pro switch_legs,rad_l1,rad_l2,lat_l1,lat_l2,lon_l1,lon_l2,Ne_l1,Ne_l2,Tm_l1,Tm_l2,WT_l1,WT_l2,scoreR_l1,scoreR_l2,il,switching
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   

  str = str_v(il)  &  stth = stth_v(il)  &  stph = stph_v(il)
  xst = str * sin(stth) * cos(stph) 
  yst = str * sin(stth) * sin(stph) 
  zst = str * cos(stth)

  fr1 = abs(rad_l1-radstart) & i1 = (where(fr1 eq min(fr1)))(0)
  fr2 = abs(rad_l2-radstart) & i2 = (where(fr2 eq min(fr2)))(0)

  r1 = rad_l1(i1)  &  th1 = (90.-lat_l1(i1))*!dtor  &  ph1 = lon_l1(i1)*!dtor
  r2 = rad_l2(i2)  &  th2 = (90.-lat_l2(i2))*!dtor  &  ph2 = lon_l2(i2)*!dtor

  x1 = r1 * sin(th1) * cos(ph1)  &  y1 = r1 * sin(th1) * sin(ph1)  &  z1 = r1 * cos(th1)
  x2 = r2 * sin(th2) * cos(ph2)  &  y2 = r2 * sin(th2) * sin(ph2)  &  z2 = r2 * cos(th2)

  d1 = sqrt((x1-xst)^2+(y1-yst)^2+(z1-zst)^2)
  d2 = sqrt((x2-xst)^2+(y2-yst)^2+(z2-zst)^2)

  goto,skiptestswitch
  print,'-----------------------------------------------------------'
  print,'line #:',il
  print,'-----------------------------------------------------------'
  print,'                   r (Rsun)        th (deg)        ph (deg)'
  print,'-----------------------------------------------------------'
  print,'Line  start:',str/1.d,stth/!dtor,stph/!dtor
  print,'Leg-1 start:',r1/1.d,th1/!dtor/1.d,ph1/!dtor/1.d
  print,'Leg-2 start:',r2/1.d,th2/!dtor/1.d,ph2/!dtor/1.d
  print,'Dist. Leg-1:',d1/1.d
  print,'Dist. Leg-2:',d2/1.d
  if d1 le d2 then switching='no'
  if d1 gt d2 then switching='yes'
  print,'Switch legs?: '+switching
  print,'-----------------------------------------------------------'
  ;if switching eq 'YES' then stop ; Descomentar para ver todo switch
  if d1 gt .5 then STOP           ; Descomentar para ver un switch
                                  ; para el cual la Leg1 empezaba
                                  ; a más de 1/2 Rsun del start point.
  skiptestswitch:

  ; SOLO si d1>d2 INTERCAMBIAR las piernas, sino dejarlas como están.
  ; Así, al salir de esta rutina leg1 será SIEMPRE la pierna del START POINT.
  if d1 gt d2 then begin

  rad_tm = rad_l1
  lat_tm = lat_l1
  lon_tm = lon_l1
   Ne_tm =  Ne_l1  
   Tm_tm =  Tm_l1
   WT_tm =  WT_l1
   scoreR_tm = scoreR_l1

  rad_l1 = rad_l2
  lat_l1 = lat_l2
  lon_l1 = lon_l2
   Ne_l1 =  Ne_l2  
   Tm_l1 =  Tm_l2
   WT_l1 =  WT_l2
   scoreR_l1 = scoreR_l2

  rad_l2 = rad_tm
  lat_l2 = lat_tm
  lon_l2 = lon_tm
   Ne_l2 =  Ne_tm  
   Tm_l2 =  Tm_tm
   WT_l2 =  WT_tm
   scoreR_l2 = scoreR_tm
   switching ='yes' ;FEDE
  
  endif

  return
  end

pro fithslinear,rr,yy,Rmin,Rmax,A,r2
p=where(rr ge Rmin and rr le Rmax)
rr=rr(p)
yy=yy(p)
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

pro fitr0,rr,yy,A,r2   ;<---
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr 
meanyy = mean(yy)
SStot  = total( (yy-meanyy)^2 )
SSerr  = total( (yy-fit    )^2 )
r2     = 1.-SSerr/SStot 
return 
end


pro fithsEr,rr,yy,A,r2  ;<--- no me sale. A termina teniendo una componente infinita
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
zz= alog(yy)
A = [0.,0.]
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


pro footpoint_map,box=box,footlat_c,footlon_c,opclstat_c,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=10.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

iopen    = where(opclstat_c eq 0.)
ilarge   = where(opclstat_c eq 1.)
ismall   = where(opclstat_c eq 2.)

ps1,'./newfigs/'+filelabel+'_footpoint-map.eps',0
  DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1
  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
       title='Physical location of the footpoints',xtitle='Lon [deg]',ytitle='Lat [deg]',/nodata,xstyle=1,ystyle=1
loadct,coltb
 if iopen (0) ne -1 then oplot,footlon_c(iopen ),footlat_c(iopen ),color=blue ,th=2,psym=8
 if ilarge(0) ne -1 then oplot,footlon_c(ilarge),footlat_c(ilarge),color=green,th=2,psym=8
 if ismall(0) ne -1 then oplot,footlon_c(ismall),footlat_c(ismall),color=red  ,th=2,psym=8

loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end

pro trace_openclose,string,ir,height,gng=gng,mdi=mdi,lonvA,latvA
common mapopcl,mapoc
if keyword_set(gng) then load_mapoc,string,height,/gng
if keyword_set(mdi) then load_mapoc,string,height,/mdi
map2d=reform(mapoc(ir,*,*))
map2d=rotate(map2d,4)
Nlon=(size(map2d))(1)
Nlat=(size(map2d))(2)
dt = 2.
lon =   0. + dt/2. + dt * findgen(Nlon)
lat = -90. + dt/2. + dt * findgen(Nlat)
flag=0
for ilon=0,nlon-1 do begin
for ilat=1,nlat-1 do begin
if map2d(ilon,ilat) ne map2d(ilon,ilat-1) then begin
 lonv = lon(ilon)
 latv = mean([lat(ilat-1),lat(ilat)])
 if flag eq 0 then begin
 lonvA = [lonv]
 latvA = [latv]
 flag=1
 endif else begin
 lonvA = [lonvA,lonv]
 latvA = [latvA,latv]
 endelse
endif
endfor
endfor
return
end
