
pro hacer_graficos
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
  common mapopcl,mapoc
  common results_tomo,tfbe,sfbe,N_e
  common devi,dev
  common laconchadetumadre,Ne_aia3

  ldemfile='LDEM.v2_cr2081_l1.0_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.A_L171_gauss1_lin_Norm-median_singlStart'
  read_ldem,ldemfile,/ldem,/gauss1
  scalefactor=3                 ; es un factor de escala respecto al mapa original 180x90 pixeles
  load_mapoc,'CR2081_90X180blines_r_',1.075,/mdi ; lee el mapa O/C
  graficos,7,scalefactor,'cr2081l1.0',Nemax=1.5,Tmmax=2.,/xtit,/ytit,/Bfield ;1.075 Rsun
  return
end


; Si usan las keyword Nemax y Tmmax pueden controlar el valor máximo
; en los mapas de Ne y Tm.
pro graficos,ir,scalefactor,suffix,instrument=instrument,zbuff=zbuff,Bfield=Bfield,xtit=xtit,ytit=ytit,Nemax=Nemax,Tmmax=Tmmax
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common results_tomo,tfbe,sfbe,N_e
  common devi,dev
  common Bstat,Bfield_stat
  common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
  common mapopcl,mapoc
  common titles,xtitle_status,ytitle_status
  common demc_extra,demc2,demc3
  common loss_rate,Er
  common fixed_width,sigma
  common AIA3,Ne_aia3,Tm_aia3,WT_aia3,scoreR_AIA3,demc_AIA3
  xtitle_status=0
  ytitle_status=0
  if keyword_set(xtit) then xtitle_status=1
  if keyword_set(ytit) then ytitle_status=1
  Bfield_stat=0
  if keyword_set(Bfield) then Bfield_stat=1
  
  height_string=strmid(string(rad(ir)),6,5)
  suffix=height_string+'_'+suffix
  
  if not keyword_set(instrument) then instrument='EUVI'
  if     keyword_set(zbuff) then SET_PLOT,'Z'
  dev='X'
  if     keyword_set(zbuff) then dev='Z'
  if not keyword_set(zbuff) then begin
     device, retain     = 2
     device, true_color = 24
     device, decomposed = 0
  endif

  
; calculate chi^2 and R
  ratio=sfbe/tfbe
  chisqr=total( (1.-ratio)^2 , 4 ) / float(nband)
  R=total( abs(1.-ratio), 4 ) / float(nband) 

  ZDA = where(demc eq -999.)
  CNS = where(demc ne -999. AND (chisqr gt 5.e-2 OR WT lt WTc*1.e6) )
  CNS = where(demc ne -999. AND (R      gt 0.25  OR WT lt WTc*1.e6) )
  
  superhigh=0.25                ;1.e+1
  superlow=0.01                 ;1.e-3
  reallylow=1.1*superlow        ; FEDE: preguntame sobre esto.
  p=where(demc ne -999. AND R le reallylow) & if p(0) ne -1 then R(p)=reallylow
  p=where(demc ne -999. AND R ge superhigh) & if p(0) ne -1 then R(p)=superhigh
  if ZDA(0) ne -1 then R(ZDA)=superlow
  display_carrmap,R,ir,2*Nband,scalefactor,1.,superlow,superhigh,12,'R @ '+strmid(suffix,0,5)+' Rsun',instrument ;,/log
  record_gif,'~/','R_'+suffix+'.gif'

  ;grilla,nr,nth,np,radA,latA,lonA

 if not keyword_set(Nemax) then Nemax=2.0
 if not keyword_set(Tmmax) then Tmmax=2.5
 Nesat = N_e
 Tmsat = Tm



 if ZDA(0) ne -1 then begin
    Nesat(ZDA)=0.
    Tmsat(ZDA)=0.
 endif


 if CNS(0) ne -1 then begin
    Nesat(CNS)=0.
    Tmsat(CNS)=Tmmax*1.e6
 endif


 display_carrmap,Nesat/1.e8,ir,2*Nband+1,scalefactor,1.,0.,Nemax,4,'Ne [10!U8!Ncm!U-3!N] @ '+strmid(suffix,0,5)+' Rsun',instrument
 record_gif,'~/','Ne_'+suffix+'.gif'

 display_carrmap,Tmsat/1.e6,ir,2*Nband+2,scalefactor,1.,0.,Tmmax,5,'Tm [MK] @'+strmid(suffix,0,5)+' Rsun',instrument
 record_gif,'~/','Tm_'+suffix+'.gif'

 

   return
end


pro display_carrmap,map,ir,win,scalefactor,units,mini,maxi,cltb,titulo,instrument,log=log,mark=mark;,xtit=xtit,ytit=ytit
common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
common devi,dev
common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
common mapopcl,mapoc
common Bstat,Bfield_stat
common titles,xtitle_status,ytitle_status

;change units
map=map/units

;mark ZDAs
if keyword_set(mark) then begin
p=where(demc eq -999.)
map(p)=-999.
endif

;----create 2D map, scale up, orientate------------------------
if (size(map))(0) eq 3 then begin
nr=fix((size(map))(1))
nt=fix((size(map))(2))
np=fix((size(map))(3))
tmp=reform(map(ir,*,*))
endif
ThMIN= -90.
ThMAX= +90.
PhMIN=   0.
PhMAX= 360.
Th=ThMIN+(ThMAX-ThMIN)*FINDGEN(NT)/FLOAT(NT-1)
Ph=PhMIN+(PhMAX-PhMIN)*FINDGEN(NP)/FLOAT(NP-1)
map2d=tmp
nt2=nt*scalefactor
np2=np*scalefactor
t2=thmin+(thmax-thmin)*findgen(nt2)/float(nt2-1)
p2=phmin+(phmax-phmin)*findgen(np2)/float(np2-1)
map2=fltarr(nt2,np2)
for ip2=0,np2-1 do begin
p0=p2(ip2)
fp=abs(ph-p0)
ip=fix( (where(fp eq min(fp)))(0) )
for it2=0,nt2-1 do begin
t0=t2(it2)
ft=abs(th-t0)
it=fix( (where(ft eq min(ft)))(0) )
map2(it2,ip2)=map2d(it,ip)
endfor
endfor
map_2d_rot_scl=rotate(map2,4)
;-----------------------------------------------------

;---threshold bottom and top---------------------
map_2d_rot_scl(0,0)=mini
map_2d_rot_scl(0,1)=maxi
map_2d_rot_scl=map_2d_rot_scl>mini<maxi
;------------------------------------------------

if keyword_set(log) then begin
   map_2d_rot_scl=alog10(map_2d_rot_scl)
   mini=min(map_2d_rot_scl)
   maxi=max(map_2d_rot_scl)
endif

;---create over-sized window with white background----------------
x0=80
y0=60
DX=x0+90
DY=y0+40
if dev eq 'X' then $
window,win,xs=np*scalefactor+DX,ys=nt*scalefactor+DY
loadct,27
tvscl,fltarr(2*np*scalefactor+DX,nt*scalefactor+DY)
;-------------------------------------------------------

;----display map-------------------------
if cltb le 40 then loadct,cltb
if cltb gt 40 and instrument ne 'aia' then eit_colors,cltb
if cltb gt 40 and instrument eq 'aia' then AIA_LCT,wave=cltb,/load
tvscl,map_2d_rot_scl,x0,y0
;-----------------------------------------

;----put axes, titles--------------------------------------------------------
nlon=np*scalefactor
nlat=nt*scalefactor
LAT=ThMIN+(ThMAX-ThMIN)*FINDGEN(Nlat)/FLOAT(Nlat-1)
LON=PhMIN+(PhMAX-PhMIN)*FINDGEN(Nlon)/FLOAT(Nlon-1)
loadct,40
xtitle=''
ytitle=''
if xtitle_status eq 1 then xtitle='Carrington Longitude [deg]'
if ytitle_status eq 1 then ytitle='Latitude [deg]'
contour,map_2d_rot_scl,lon,lat,pos=[X0,Y0,X0+nlon,Y0+nlat],/noerase,/nodata,$
        /device,color=255,xstyle=1,ystyle=1,charsize=2.,$
        xtitle=xtitle,$
        ytitle=ytitle,$
        title=titulo,$
        yticklen=.02,xticklen=.03,ythick=2,xthick=2,charthick=2
;-----------------------------------------------------------------------------

;---OVERPLOT MAGNETIC STRENGTH CONTOUR LEVELS----------------------------------
if Bfield_stat eq 1 then begin
goto,skipBlevels
mapB=rotate(rebin(reform(Bint(ir,*,*)),nt*scalefactor,np*scalefactor),4)
maxabsB=5.*median(abs(Bint(ir,*,*)))

nl=5

lev=-maxabsB+2.*maxabsB*findgen(nl)/float(nl-1)
dlev=float((lev(1)-lev(0))(0))

lstyle=intarr(nl)
;p = where(lev gt 0.) & if p(0) ne -1 then lstyle(p)=0
;p = where(lev lt 0.) & if p(0) ne -1 then lstyle(p)=0

cth=intarr(nl)+1
;p = where(lev gt 0.)  & if p(0) ne -1 then cth(p)=1
 p = where(lev gt (-1.*dlev) AND lev lt dlev); & if p(0) ne -1 then cth(p)=1
 cth(p)=2

ccol=intarr(nl)
p = where(lev gt 0.) & if p(0) ne -1 then ccol(p)=255
p = where(lev lt 0.) & if p(0) ne -1 then ccol(p)=0

loadct,0

;comentando este goto se hacen los contornos de nivel de B.

contour,mapB,lon,lat,/noerase,color=0,xstyle=1,ystyle=1,$
                         /device,pos=[x0,y0,x0+nlon,y0+nlat],$
                         xtitle='',$
                         ytitle='',$
                         title=titulo,levels=lev,$
                         yticklen=.02,xticklen=0.03,ythick=2,xthick=2,charthick=2,$
                         c_color=ccol,c_thick=cth,$;,c_labels=intarr(n_elements(nl))+1
                         c_linestyle=lstyle,charsize=2
skipBlevels:
ThMIN= -90.
ThMAX= +90.
PhMIN=   0.
PhMAX= 360.
T=ThMIN+(ThMAX-ThMIN)*FINDGEN(NT)/FLOAT(NT-1)
P=PhMIN+(PhMAX-PhMIN)*FINDGEN(NP)/FLOAT(NP-1)
nt2=nt*scalefactor
np2=np*scalefactor
t2=thmin+(thmax-thmin)*findgen(nt2)/float(nt2-1)
p2=phmin+(phmax-phmin)*findgen(np2)/float(np2-1)


map2d=reform(mapoc(ir,*,*))
if ir eq 4 then begin
   map2d=reform(mapoc(ir-1,*,*)) ; Force to use 1.035 Rsun OC-map as if it was 1.045 Rsun
   print,'Used 1.035 O/C map! for 1.045 LDEM'
endif
map2 =fltarr(nt2,np2)
for ip2=0,np2-1 do begin
p0=p2(ip2)
fp=abs(p-p0)
ip=fix( (where(fp eq min(fp)))(0) )
for it2=0,nt2-1 do begin
t0=t2(it2)
ft=abs(t-t0)
it=fix( (where(ft eq min(ft)))(0) )
map2(it2,ip2)=map2d(it,ip)
endfor
endfor
mapoc_2d_rot_scl=rotate(map2,4)

contour,mapoc_2d_rot_scl,lon,lat,/noerase,color=0,xstyle=1,ystyle=1,charsize=2,$
                         /device,pos=[x0,y0,x0+nlon,y0+nlat],$
                         xtitle='',$
                         ytitle='',$
                         title='',$
                         yticklen=.02,xticklen=0.03,ythick=2,xthick=2,charthick=2,$
                         c_color=0,c_thick=5,$;,c_labels=intarr(n_elements(nl))+1
                         c_linestyle=lstyle
;stop
endif


;---PUT COLOR SCALE BAR---------------------------------------------------------
ny=nt*scalefactor
nx= 30
scale=fltarr(nx,ny)
x0=55+x0+np*scalefactor
y0=y0
for ix=0,nx-1 do scale(ix,*)=mini+(maxi-mini)*findgen(ny)/float(ny-1)

if cltb le 40 then loadct,cltb
if cltb gt 40 and instrument ne 'aia' then eit_colors,cltb
if cltb gt 40 and instrument eq 'aia' then AIA_LCT,wave=cltb,/load

tvscl,scale,x0,y0
loadct,0
contour,scale,findgen(nx),scale,$
              pos=[x0,y0,x0+nx,y0+ny],/device,color=0,/noerase,$
              yticklen=.2,/nodata,ythick=2,xthick=2,charthick=2,$
              xstyle=5,ystyle=1,charsize=2

return
end

pro hacer_visualizacion,create=create
common structure,sph_data
common flags,flag_fan,flag_spine,flag_onebyone,flag_create,flag_extra,flag_vnormal,flag_cube,flag_interpol


radstart= 1.035 + 0.08 *findgen(3)
Nrad = n_elements(radstart)
dlat = 11. + fltarr(Nrad)
dlon = 11. + fltarr(Nrad)

flag_create = 0
if keyword_set(create) then flag_create=1
flag_interpol=0 

;visual3D,'fdips_field_150x180x360_synop_Mr_0.polfil.2081.ubdat',$
 visual3D,'fdips_field_150x180x360_synop_Mr_0.polfil.2099.ubdat',$
          'CR2099.gif',$
;         'CR2081.gif',$
          lat0=0,lon0=0,dlat=dlat,dlon=dlon,radstart=radstart,/unifgrid_v2;/marcgrid

return
end

; open field lines:
; verde:  polaridad positiva
; magenta:polaridad negativa
pro visual3D,input_dat,output_gif,lat0=lat0,lon0=lon0,image_max=image_max,win=win,spacing=spacing,radstart=radstart,npx=npx,box=box,create=create,dlat=dlat,dlon=dlon,unifgrid=unifgrid,marcgrid=marcgrid,tony=tony,cubegrid=cubegrid,null=null,cubesize=cubesize,cubedensity=cubedensity,sizef=sizef,densityf=densityf,MHD=MHD,interpol=interpol,sinlabel=sinlabel,addlines=addlines,unifgrid_v2=unifgrid_v2
  common structure,sph_data
  common flags,flag_fan,flag_spine,flag_onebyone,flag_create,flag_extra,flag_vnormal,flag_cube,flag_interpol
;+ 
; PURPOSE:
; Make a 3D still view of a FDIPS model + a central image (it can be 
; either a magnetogram, the Br field at any given radius, or a
; tomographically reconstructed quantity at a given radius.
; Also, make a 3D interactive visualization
;
; INPUT:
; box=[lon1,lat1,lon2,lat2], bounding coordinates to be used if /bbox is set
; radstart = height [Rsun] where to start to trace, both inwards and outwards.
; spacing, fieldtype, safety = various keywords needed if /marc is used, see
;                              comments in "spherical_field_start_coord.pro".
;
; KEYWORDS:
; /unifgrid = set up a uniform angular grid for the starting points.
; /marcgrid = use Marc's tools starting points routine.
;
;-

; if keyword_set(create) eq 0 then flag_create = 0
; if keyword_set(create) eq 1 then flag_create = 1
; if keyword_set(interpol) eq 0 then flag_interpol=0

  if flag_create eq 1 then begin
     if not keyword_set(tony) AND not keyword_set(MHD) then begin
        PFSSM_model=input_dat
        create_structure,PFSSM_model
     endif 
     if keyword_set(tony) then  begin
        model='../Anthony/'+input_dat
        create_structure_tony,model
     endif
     if keyword_set(MHD)  then  begin
        model='/data1/DATA/MHD_SWMF/'+input_dat
        create_structure_MHD,model
     endif
  endif

  ;if flag_interpol eq 1 then interpolate_structure ; interpol to grid LINEAR in LATITUDE.

  pfss_data = sph_data

  fieldtype=5
  if NOT keyword_set(spacing)  then spacing  = 10
  if NOT keyword_set(radstart) then radstart = 1.5

; Set the starting points density in case /unifgrid is used.
; Default density is one line every 2 degrees.
  if not keyword_set(dlat) then dlat = 2   
  if not keyword_set(dlon) then dlon = 2

; If BOX was not set use the full corona:
  if NOT keyword_set(box)  then box = [0.,-90,360.,+90.] 
  box=float(box)

  ; Set up the starting points:
  if keyword_set(marcgrid) then spherical_field_start_coord,pfss_data,fieldtype,spacing,radstart=radstart,bbox=box
  if keyword_set(unifgrid) then sph_field_str_coord_unifang,pfss_data,dlat,dlon,radstart=radstart,bbox=box

  if keyword_set(unifgrid_v2) then sph_field_str_coord_unifang_v2,pfss_data,dlatv=dlat,dlonv=dlon,radstartv=radstart,bbox=box


  ; Trace the fieldlines:
  if not keyword_set(safety) then  safety=0.5
  spherical_trace_field,pfss_data,linekind=linekind,safety=safety,stepmax=30000,outfield=outfield ;,/oneway



;-----------------------------------------------------------
; Seleccionar un INDICE de altura (0 es 1 Rsun)
irad       = 0
; Hacer que la imagen de BASE de la visualizacion 3D sea B_r(irad)
base_image = (*pfss_data.br)(*,*,irad)
; Impose min and max
if NOT keyword_set(image_max) then image_max=30
;impose_min_max,image_max,base_image
; Hacer que RADIUS sea el radio de la grilla en el indice irad
radius     = (*pfss_data.rix)(irad)
; Crear imagen base
image_data=spherical_image_create(base_image,*pfss_data.lon,*pfss_data.lat,radius)

if not keyword_set(npx) then npx = 1000       ; numero de pixels de la ventana del gráfico
npy=npx


; PARA IR HACIENDO VISTAS 3D DESDE DISTINTOS ANGULOS DE VISION CAMBIAR
; LO DE ABAJO. NO RE-HACER LO DE ARRIBA, YA QUE ESTA TODO EN MEMORIA.

if NOT keyword_set(lat0) then lat0=0 ; Latitud  centro del disco
if NOT keyword_set(lon0) then lon0=0 ; Longitud centro del disco
if NOT keyword_set(win ) then win =0 ; Window number

; Crear imagen
spherical_draw_field,pfss_data,outim=outim,bcent=lat0,lcent=lon0,$
xsize=npx,ysize=npy,im_data=image_data,imsc=image_max,/for_ps;,width=0.55
window,win,xs=npx,ys=npx
tv,outim,/true
if not keyword_set(sinlabel) then xyouts,0.02,0.02,'Lat='+strcompress(lat0)+'  Long='+strcompress(lon0),charsize=2,charthick=2.5,color=0;,/norm
  record_gif,'~/Downloads/',output_gif
  nname=strlen(output_gif)
  record_jpg,'~/Downloads/',strmid(output_gif,0,nname-4)+'.jpg'

; using the spherical_trackball_widget.pro from solar soft
; spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max

; using the spherical_trackball_widget.pro in /data1/work/dem/
; you need to compilate before
  spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max,/for_ps

;stop
return
end

pro load_mapoc,filesuffix,rc,mdi=mdi,gng=gng
common mapopcl,mapoc
 dir='/data1/DATA/PFSSM/'
 if keyword_set(mdi) then postsuffix='_MDI.dat'
 if keyword_set(gng) then postsuffix='_GNG.dat'
 nr = 26
 nth= 90
 np =180
 mapoc=fltarr(nr,nth,np)
 tmp=fltarr(1,nth,np)

;for ir=0,nr-1 do begin
;if ir eq 3 or ir eq 7 or ir eq 13 or ir eq 19 then begin
;if            ir eq 7                         then begin

 stringheight=strmid(string(rc),6,5)
 openclosefile=filesuffix+stringheight+'_open-close-map'+postsuffix
 print,'-----> O/C: '+openclosefile
 openr,1,dir+openclosefile
 readu,1,tmp
 close,1

 dr=0.01
 radv=1.+dr/2.+dr*findgen(nr)
 ir = where(radv eq rc)
 mapoc(ir,*,*)=tmp(0,*,*)
;endif
;endfor 

return
end
