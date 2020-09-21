;hacer_visualizacion,/create
pro hacer_visualizacion,create=create
common structure,sph_data
common flags,flag_fan,flag_spine,flag_onebyone,flag_create,flag_extra,flag_vnormal,flag_cube,flag_interpol

flag_create = 0
if keyword_set(create) then flag_create=1
flag_interpol=0 

; AWSOM vs PFSSS: CR-2082,  08/05/2019

;visual3D,'fdips_field_150x180x360_synop_Mr_0.polfil.2082.ubdat','CR2082_PFSS.gif',/marcgrid,radstart=1.5,spacing=10,lon0=80
;visual3D,'sph_data_awsom_2082_1.85_extended.sav','CR2082_awsom.jpg',/marcgrid,radstart=1.065,spacing=5,/field_awsom,lon0=80

;visual3D,'fdips_field_150x180x360_GONG_CR2219_mrnqs190713t0141c2219_rmax18.out','CR2223_PFSS_GONG_mrnqs_18.gif',/marcgrid,radstart=1.025,spacing=5,lon0=1,dir='/data1/DATA/PFSSM/'
;visual3D,'fdips_field_150x180x360_GONG_CR2219_GONG_mrnqs190713t0141c2219.out','CR2219_PFSS_GONG_mrnqs_25.gif',/marcgrid,radstart=1.025,spacing=5,lon0=1,dir='/data1/DATA/PFSSM/'
;visual3D,'fdips_field_150x180x360_GONG_CR2219_mrnqs190713t0141c2219_rmax215.out','CR2223_PFSS_GONG_mrnqs_215.gif',/marcgrid,radstart=1.025,spacing=5,lon0=1,dir='/data1/DATA/PFSSM/'
;visual3D,'fdips_field_150x180x360_GONG_CR2219_mrnqs190713t0141c2219_rmax275.out','CR2223_PFSS_GONG_mrnqs_275.gif',/marcgrid,radstart=1.025,spacing=5,lon0=1,dir='/data1/DATA/PFSSM/'
visual3D,'fdips_field_150x180x360_GONG_CR2219_shifted_mrzqs190716t2204c2219_069.out','CR2219_PFSS_GONG_mrzqs_25_shifted.gif',/marcgrid,radstart=1.025,spacing=5,lon0=1,dir='/data1/DATA/PFSSM/'
return
end

pro visual3D,input_dat,output_gif,$
             lat0=lat0,lon0=lon0,image_max=image_max,win=win,spacing=spacing,radstart=radstart,$
             npx=npx,box=box,create=create,dlat=dlat,dlon=dlon,unifgrid=unifgrid,marcgrid=marcgrid,$
             interpol=interpol,sinlabel=sinlabel,field_awsom=field_awsom,dir=dir
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
  if not keyword_set(dir) then dir=''
  
  if flag_create eq 1 then begin
     if not keyword_set(field_awsom) then begin
        PFSSM_model=input_dat
        create_structure,dir+PFSSM_model
   ;create_structure_v2,PFSSM_model
     endif else begin
        ;CAMBIAR =========================
        model='/data1/work/MHD/'+input_dat
        read_structure_MHD,model,sph_data
     endelse
  endif

  pfss_data = sph_data
  
  fieldtype=5
  if NOT keyword_set(spacing)  then spacing  = 10.
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
;...................................................................................................  
; store the starting-point in an ASCII file
  outfile=''
  read,outfile,PROMPT='Filename to store the start points data? [Default: startpoints.out] '
  if outfile eq '' then outfile='startpoints.out'
  openw,1,outfile
  printf,1,n_elements(*pfss_data.str)
  printf,1,'starting points:'
  printf,1,'    Rad     Lat      Lon     '
  printf,1,'   [Rsun]  [deg]    [deg]    '
  for i=0,n_elements(*pfss_data.str)-1 do begin
     printf,1,(*pfss_data.str)(i),90.-(*pfss_data.stth)(i)/!dtor,(*pfss_data.stph)(i)/!dtor        
  endfor
  close,1
;-------------------------------------------------------------------------------------------------  

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
  impose_min_max,image_max,base_image
; Hacer que RADIUS sea el radio de la grilla en el indice irad
  radius     = (*pfss_data.rix)(irad)
; Crear imagen base
  image_data=spherical_image_create(base_image,*pfss_data.lon,*pfss_data.lat,radius)
;--------------------------------------------------------------------------------

  if not keyword_set(npx) then npx = 1000 ; numero de pixels de la ventana del gráfico
  npy=npx


; PARA IR HACIENDO VISTAS 3D DESDE DISTINTOS ANGULOS DE VISION CAMBIAR
; LO DE ABAJO. NO RE-HACER LO DE ARRIBA, YA QUE ESTA TODO EN MEMORIA.

  if NOT keyword_set(lat0) then lat0=0 ; Latitud  centro del disco
  if NOT keyword_set(lon0) then lon0=0 ; Longitud centro del disco
  if NOT keyword_set(win ) then win =0 ; Window number

; Crear imagen
  spherical_draw_field,pfss_data,outim=outim,bcent=lat0,lcent=lon0,$
                       xsize=npx,ysize=npy,im_data=image_data,imsc=image_max,/for_ps ;,width=0.55
  window,win,xs=npx,ys=npx
  tv,outim,/true
  if not keyword_set(sinlabel) then xyouts,0.02,0.02,'Lat='+strcompress(lat0)+'  Long='+strcompress(lon0),charsize=2,charthick=2.5,color=0 ;,/norm
;  record_gif,'~/Downloads/',output_gif
  nname=strlen(output_gif)
  record_jpg,'/data1/work/dem/github_dem/dem/newfigs/',strmid(output_gif,0,nname-4)+'.jpg'
stop
; using the spherical_trackball_widget.pro from solar soft
  spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max
  stop
; using the spherical_trackball_widget.pro in /data1/work/dem/
; you need to compilate before
; spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max,/for_ps

  return
end


pro read_structure_MHD,file,sph_data
;file must be a string in .sav format
  restore,file
  return
end

pro sph_field_str_coord_unifang,pfss_data,dlat,dlon,radstart=radstart,bbox=bbox
;+
; PURPOSE: 
; Create the starting points in a uniform angular grid.
;
; INPUT:
; bbox= [lon1,lat1,lon2,lat2], if not set the default is full corona.
;
; HISTORY:
; Created by F.A. Nuevo, based on M. DeRosa's PFSS package codes.
; Updated by A.M. Vasquez to use a bbox other than the full corona.
;-

if NOT keyword_set(bbox)     then bbox=[0.,-90.,360.,+90.]
if NOT keyword_set(radstart) then radstart=rad(0)

bbox = float(bbox)

nrad = 26
drad = 0.01
if NOT keyword_set(dlat) then dlat = 2.
if NOT keyword_set(dlon) then dlon = 2.

dlat=float(dlat)
dlon=float(dlon)

nlat = fix((bbox[3]-bbox[1])/dlat)
nlon = fix((bbox[2]-bbox[0])/dlon)

rad = 1.      + drad /2. + drad * findgen(nrad)
lat = bbox[1] + dlat /2. + dlat * findgen(nlat)
lon = bbox[0] + dlon /2. + dlon * findgen(nlon)

if not keyword_set(radstart) then radstart=rad(0)

goto,albert
latbin = reverse(!dtor*(90-lat)) ; calculo la co-latitud en radianes
nlonbin = lonarr(nlat)+nlon
dlonbin = 2*!dpi/nlonbin
nloncum = lonarr(nlat)
for i=0l,nlat-1 do nloncum(i)=total(nlonbin(0:i))
npt=round(total(nlonbin)) ;  total number of points

;  set stth,stph
stth=dblarr(npt,/noz)
stph=dblarr(npt,/noz)
    for i=0l,npt-1 do begin
      lonbinix=(where(i lt nloncum))(0)
      stth(i)=latbin(lonbinix)
      stph(i)=(i-(nloncum(lonbinix)-nlonbin(lonbinix)))*dlonbin(lonbinix) + dlon/2*!dtor
   endfor

stph=(stph+2*!dpi) mod (2*!dpi)
if n_elements(bbox) eq 4 then begin  ;  remove all points outside box
;  filter in latitude                                                                                                                                  
      whlat=where((stth ge (90-bbox(3))*!dpi/180) $
        and (stth le (90-bbox(1))*!dpi/180),nwh)
      if nwh gt 0 then begin
        stth=stth(whlat)
        stph=stph(whlat)
     endif      
;  filter in longitude                                                                                                                                 
      lon1=((bbox(0)+360) mod 360)*!dpi/180
      lon2=((bbox(2)+360) mod 360)*!dpi/180
      if (lon2 gt lon1) then begin
        whlon=where((stph ge lon1) and (stph le lon2),nwh)
     endif else begin
        whlon=where((stph le lon2) or (stph ge lon1),nwh)
     endelse
      if nwh gt 0 then begin
        stth=stth(whlon)
        stph=stph(whlon)
     endif
   endif

if n_elements(bbox) eq 0 then begin

lat1 = min(*pfss_data.theta)
lat2 = max(*pfss_data.theta)
lon1 = min(*pfss_data.phi)
lon2 = max(*pfss_data.phi)

whlat= where((stth gt lat1) and (stth lt lat2),nwh)
if nwh gt 0 then begin
        stth=stth(whlat)
        stph=stph(whlat)
     endif    
whlon= where((stph gt lon1) and (stph lt lon2),nwh)
if nwh gt 0 then begin
        stth=stth(whlon)
        stph=stph(whlon)
     endif       
endif
str=replicate(radstart,n_elements(stth))
goto,skipalbert

albert:
; Colatitude and longitude in radians:
theta=(90.-lat)*!dtor
phi  =     lon *!dtor
stth=dblarr(nlat*nlon,/noz)
stph=dblarr(nlat*nlon,/noz)
i=0
for ilat=0,nlat-1 do begin
for ilon=0,nlon-1 do begin
    stth[i]=theta[ilat]
    stph[i]=  phi[ilon]
    i=i+1
endfor
endfor
str=replicate(radstart,n_elements(stth))
skipalbert:

pfss_data.str  = ptr_new(str)
pfss_data.stth = ptr_new(stth)
pfss_data.stph = ptr_new(stph)

return
end
