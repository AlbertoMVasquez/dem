; visual3D,'CR2106_PFSSM_GNG_151X180X361_FDIPS.dat','cr2106-FDIPS.gif',lat0=0,lon0=0,image_max=30,win=0,radstart=1.5,/unifgrid,dlat=10,dlon=5.,/create,/FDIPS

; visual3D,'3d__var_1_n00000005.out__chip0_xxxiii.txt_gridded_1.00-2.50.dat','Jimmy_MHD_1.00-2.50Rsun_radstart-1.25.gif',lat0=0,lon0=0,image_max=10,win=0,radstart=1.25,/unifgrid,dlat=10,dlon=10,npx=800,/MHD,/create
; visual3D,'3d__var_1_n00000005.out__chip0_xxxiii.txt_gridded_1.02-2.50.dat','Jimmy_MHD_1.00-2.50Rsun_radstart-1.25.gif',lat0=0,lon0=0,image_max=10,win=0,radstart=1.25,/unifgrid,dlat=10,dlon=10,npx=800,/MHD,/create
; visual3D,'3d__var_1_n00000005.out__chip0_xxxiii.txt_gridded_1.02-9.00.dat','Jimmy_MHD_1.00-9.00Rsun_radstart-1.50.gif',lat0=0,lon0=0,image_max=10,win=0,radstart=1.5,/unifgrid,dlat=10,dlon=10,npx=800,/MHD ;,/create
; visual3D,'3d__var_1_n00000005.out__chip0_xxxiii.txt_gridded_1.02-9.00.dat','Jimmy_MHD_1.00-9.00Rsun_radstart-2.50.gif',lat0=0,lon0=0,image_max=10,win=0,radstart=2.5,/unifgrid,dlat=10,dlon=10,npx=800,/MHD ;,/create
; visual3D,'3d__var_1_n00000005.out__chip0_xxxiii.txt_gridded_10.00-11.00.dat','Jimmy_MHD_10.00-11.00Rsun_radstart-10.50.gif',lat0=0,lon0=0,image_max=10,win=0,radstart=10.5,/unifgrid,dlat=10,dlon=10,npx=800,/MHD,/create

pro visual3D,input_dat,output_gif=output_gif,lat0=lat0,lon0=lon0,image_max=image_max,win=win,spacing=spacing,radstart=radstart,npx=npx,box=box,create=create,dlat=dlat,dlon=dlon,unifgrid=unifgrid,marcgrid=marcgrid,tony=tony,cubegrid=cubegrid,null=null,cubesize=cubesize,cubedensity=cubedensity,sizef=sizef,densityf=densityf,FDIPS=FDIPS,MHD=MHD,interpol=interpol,sinlabel=sinlabel,addlines=addlines,input_dir=input_dir
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
; box=[lon1,lat1,lon2,lat2], bounding coordinates to be used, default is full corona.
; radstart = height [Rsun] where to start to trace, both inwards and outwards.
; spacing, fieldtype, safety = various keywords needed, see
;                              comments in "spherical_field_start_coord.pro".
;
; KEYWORDS:
; /unifgrid = set up a uniform angular grid for the starting points.
; /marcgrid = use Marc's tools starting points routine.
;
; Developed by A.M.Vásquez.
; Latest version August 21, 2017
;-

; if keyword_set(create) eq 0 then flag_create = 0
  if keyword_set(create) eq 1 then flag_create = 1
; if keyword_set(interpol) eq 0 then flag_interpol=0

;flag_create = 1

if not keyword_set(output_gif) then output_gif='output.gif'
if not keyword_set(input_dir) then input_dir='/data1/DATA/MHD_SWMF/'

if flag_create eq 1 then begin
 if keyword_set(FDIPS) then begin
    PFSSM_model=input_dir+input_dat
    create_structure,PFSSM_model
 endif 
 if keyword_set(MHD)   then begin
    model=input_dir+input_dat
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

; ...................................................................................................
; Custom-made starting points for a "local" project, may serve as an
; example for Jimmy/Sergio's purposes.
if NOT keyword_set(unifgrid) AND NOT keyword_set(marcgrid) then begin
     if flag_cube eq 1 then sph_field_str_coord_cube,pfss_data,null=null,cubesize=cubesize,cubedensity=cubedensity,sizef=sizef,densityf=densityf
     if flag_fan eq 1 or flag_spine eq 1 or flag_onebyone eq 1 then begin

; OJO! el nulo 2 es el nulo 4 del paper, y viceversa

;    Null point coordinates:  rad [Rsun], lat [deg], lon [deg] (MDI 150x180x360)
     null1 = [1.156475295, +32.9768004, 236.049883] * 1.d0 
     null2 = [1.03635923 , -32.7259142, 198.978578] * 1.d0 
     null3 = [1.03933061 , -09.8097359, 202.179838] * 1.d0 
     null4 = [1.03960353 , +36.3954270, 237.064914] * 1.d0 
;    Null point coordinates:  lon [deg] , lat [deg], rad [Rsun] (GONG 150x180x360)
     null1 = [1.111855749, +36.60374080, 237.741214] * 1.d0
     null2 = [1.018127192, -33.00072160, 199.440530] * 1.d0
     null3 = [1.027859066, -09.50812648, 202.532914] * 1.d0
     null4 = [1.042027548, +22.92076070, 241.867875] * 1.d0
;    Null point coordinates:  lon [deg] , lat [deg], rad [Rsun] (MDI 300x360x720)
     null1 = [1.155848444, +32.97441900, 235.951043] * 1.d0 
     null2 = [1.036293503, -32.78973170, 198.946431] * 1.d0 
     null3 = [1.038393788, -09.91363164, 202.185066] * 1.d0 
     null4 = [1.037343651, +36.49008490, 237.113882] * 1.d0

;    extra Nulls   
     null5 = [1.068      , +08.45     , 154.0     ] * 1.d0 ;NR1
     null6 = [1.0409     , +05.74     , 165.0     ] * 1.d0 ;NR2

;    Null1 de CR-2065
;    null1 = [1.174603691, - 7.105321720, 206.151104] * 1.d0 ; MDI
;    null1 = [1.165438828, - 7.707798110, 206.101730] * 1.d0 ; GONG
;    null1 = [1.187294462, -11.445379900, 205.012486] * 1.d0 ; Marc-model

     nullpoint = dblarr(3)
     null=0
     read,null,PROMPT='Select the Null (1-6): '
     case null of
        1: nullpoint = null1
        2: nullpoint = null2
        3: nullpoint = null3
        4: nullpoint = null4
        5: nullpoint = null5 ;NR1
        6: nullpoint = null6 ;NR2
     endcase

     if flag_spine eq 1 then begin
       ns         = 0  & read,ns,            PROMPT='Enter             ns '
       fact_spine = 0. & read,fact_spine    ,PROMPT='Enter     fact_spine '
     endif
     if flag_fan   eq 1 then begin
       nf1        = 0  & read,nf1,           PROMPT='Enter           nf1 '
       nf2        = 0  & read,nf2,           PROMPT='Enter           nf2 '
       fact_fan1  = 0. & read,fact_fan1     ,PROMPT='Enter      fact_fan1 '
       fact_fan2  = 0. & read,fact_fan2     ,PROMPT='Enter      fact_fan2 '
   factor_height  = 0. & read,factor_height ,PROMPT='Enter  factor_height '
     endif

     if flag_fan eq 1 or flag_spine eq 1 then $
     sph_field_str_coord_fan_spine,nf1,nf2,ns,factor_height,fact_spine,fact_fan1,fact_fan2,pfss_data,nullpoint=nullpoint

     if flag_onebyone eq 1 then $
     sph_field_str_coord_one_by_one,nf1,nf2,ns,fact_spine,fact_fan1,fact_fan2,pfss_data,nullpoint=nullpoint

     endif
endif

;.....................................................................................................................

  goto,skip_map_starting_points
  ps1,'~/Downloads/map.eps',0
  plot,*pfss_data.stph/!dtor,90.-*pfss_data.stth/!dtor,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=3,$
       title='Physical location map of the starting points',xtitle='Lon [deg]',ytitle='Lat [deg]'
  ps2
  stop
  skip_map_starting_points:

;stop


if keyword_set(addlines) then begin
str=*pfss_data.str
stth=*pfss_data.stth
stph=*pfss_data.stph
Nlines=n_elements(str)
outfile=''
read,outfile,PROMPT='Filename to read the data? [Default: startpoint.onebyone.out]  '
if outfile eq '' then outfile='startpoints.out'
print,outfile
 openr,1,outfile
N=0
x=''
readf,1,N
readf,1,x
readf,1,x
readf,1,x
 str_extra=fltarr(N+Nlines)
stth_extra=fltarr(N+Nlines)
stph_extra=fltarr(N+Nlines)

 str_extra(0:Nlines-1)=str
stth_extra(0:Nlines-1)=stth
stph_extra(0:Nlines-1)=stph

for i=Nlines,N+Nlines-1 do begin
readf,1,strad,stlat,stlon
 str_extra(i)=strad
stth_extra(i)=(90.-stlat)*!dtor
stph_extra(i)= stlon*!dtor       
endfor
close,1

pfss_data.str  = ptr_new(str_extra)
pfss_data.stth = ptr_new(stth_extra)
pfss_data.stph = ptr_new(stph_extra)

str = *pfss_data.str
stth= *pfss_data.stth
stph= *pfss_data.stph
endif


goto,skip_this
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
skip_this:

;-----------------------------------------------------------
; Trace the fieldlines with Marc's SolarSoft pfss package
if not keyword_set(safety) then  safety=0.5
spherical_trace_field,pfss_data,linekind=linekind,safety=safety,stepmax=30000,outfield=outfield;,/oneway


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

;---------BASE IMAGE MAGNETOGRAMA ALTA RESOLUCION------------------------------
goto,skip
 dir ='../Magnetograms/'               
 archivo1='synop_Mr_0.polfil.2009.fits'
 archivo1='synop_Mr_0.polfil.2065.fits'
 archivo1='mrmqs110203t0120c2106_000.fits'
 mreadfits,dir+archivo1,hdr1,data1     
 ; crear la grilla en la alta resolución del magnetograma MDI:
 magnetogram_grid,1080,3600,Lat,Lon
 ; crear la grilla en la alta resolución del magnetograma MDI:
 magnetogram_grid,180,360,Lat,Lon
                                
 ; Saturar en un par de valores predeterminados para que se vea algo
 ; de la zona tranquila
 Bmax=image_max
 data1=data1>(-Bmax)<Bmax
 image_data=spherical_image_create(data1,Lon,Lat,radius)
skip:
;--------------------------------------------------------------------------------

if not keyword_set(npx) then npx = 800       ; numero de pixels de la ventana del gráfico
npy=npx

; PARA IR HACIENDO VISTAS 3D DESDE DISTINTOS ANGULOS DE VISION CAMBIAR
; LO DE ABAJO. NO RE-HACER LO DE ARRIBA, YA QUE ESTA TODO EN MEMORIA.

if NOT keyword_set(lat0) then lat0=0 ; Latitud  centro del disco
if NOT keyword_set(lon0) then lon0=0 ; Longitud centro del disco
if NOT keyword_set(win ) then win =0 ; Window number

; Crear imagen
spherical_draw_field,pfss_data,outim=outim,bcent=lat0,lcent=lon0,$
xsize=npx,ysize=npy,im_data=image_data,imsc=image_max,/for_ps,/noimage;,width=0.55
window,win,xs=npx,ys=npx
tv,outim,/true
if not keyword_set(sinlabel) then xyouts,0.02,0.02,'Lat='+strcompress(lat0)+'  Long='+strcompress(lon0),charsize=2,charthick=2.5,color=0;,/norm
  record_gif,'~/Downloads/',output_gif
nname=strlen(output_gif)

;  record_gif,'~/',strmid(output_gif,0,nname-4)+'.gif'

; using the spherical_trackball_widget.pro from solar soft
; spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max

; using the spherical_trackball_widget.pro in /data1/work/dem/
; you need to compile before <-- Fede: COMPILATE? :)

 spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max,/for_ps,/noimage


stop
return
end



; create_structure,'fdips_field_150x180x360_synop_Mr_0.polfil.1974.out',/writefile
; create_structure,'fdips_field_151x180x361_mrmqs110425t2355c2109_000.out',/writefile
; create_structure,'CR2106_PFSSM_GNG_151X180X361_FDIPS.dat'
pro create_structure,inputfile,writefile=writefile
common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
common structure,sph_data

; Read PFSS model into RAM
print,'Reading model...'
read_PFSSM,inputfile
print,'Done.'

; Take out last LON point
   NphB=NphB-1
   phB = phB(*,*,0:nphB-1)

print,'Creating Structure...'
; ------------------------------
; Re-order the dimensions of Br, Bth, and Bph, so that for each array: 
; the 3rd dimension (index=2) becomes 1st,
; the 2nd dimension (index=1) stays   2nd,
; the 1st dimension (index=0) becomes 3rd. 
  Br_new = TRANSPOSE(  Br(*,*,0:nphB-1) , [2, 1, 0] ) 
 Bth_new = TRANSPOSE( Bth(*,*,0:nphB-1) , [2, 1, 0] ) 
 Bph_new = TRANSPOSE( Bph(*,*,0:nphB-1) , [2, 1, 0] ) 

; Create Structure needed by Marc's SolarSoft PFSS package tools.
sph_data = {        BR: ptr_new( Br_new)                            ,$
                   BTH: ptr_new(Bth_new)                            ,$
                   BPH: ptr_new(Bph_new)                            ,$
               BDERIVS: ptr_new()                                   ,$
                    NR: NrB*1L                                      ,$
                  NLAT: nthB*1L                                     ,$
                  NLON: nphB*1L                                     ,$
                   RIX: ptr_new( reform(rB(*,0,0))*1.d )            ,$
                 THETA: ptr_new( reform(90.-thB(0,*,0))*!pi/180.d ) ,$
                   PHI: ptr_new( reform(phB(0,0,*))*!pi/180.d )     ,$
                   LAT: ptr_new( reform(thB(0,*,0))*1.d )           ,$
                   LON: ptr_new( reform(phB(0,0,*))*1.d )           ,$
             LONBOUNDS: dblarr(2)-1.                                ,$
                   STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new() ,$
                   PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new() ,$
                 NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()          }
print,'Done.'

return
end

;-------------------------------------------------------
pro read_PFSSM,inputfile
common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
x=''
nrB=0
nthB=0
nphB=0
openr,1,inputfile
for i=1,2 do readf,1,x
readf,1,nrB,nphB,nthB
for i=1,2 do readf,1,x
rB  =fltarr(nrB,nthB,nphB)
thB =fltarr(nrB,nthB,nphB)
phB =fltarr(nrB,nthB,nphB)
Br  =fltarr(nrB,nthB,nphB)
Bth =fltarr(nrB,nthB,nphB)
Bph =fltarr(nrB,nthB,nphB)
B   =fltarr(nrB,nthB,nphB)
  rB_0=0.
 thB_0=0.
 phB_0=0.
  Br_0=0.
 Bth_0=0.
 Bph_0=0.
for ith=0,nthB-1 do begin
for iph=0,nphB-1 do begin
for ir =0, nrB-1 do begin
  readf,1, rB_0, phB_0, thB_0, Br_0, Bph_0, Bth_0
    rB(ir,ith,iph) =  rB_0
   thB(ir,ith,iph) = thB_0 *180./!pi
   phB(ir,ith,iph) = phB_0 *180./!pi
    Br(ir,ith,iph) =  Br_0
   Bth(ir,ith,iph) = Bth_0
   Bph(ir,ith,iph) = Bph_0
endfor
endfor
endfor
close,1
p    = where(Br ne 0.)
B(p) = sqrt(Br(p)^2+Bth(p)^2+Bph(p)^2) * Br(p)/abs(Br(p))
Bunit=1. 
B=B/Bunit
print,'-----> Bunit =',Bunit
return
end


pro create_structure_MHD,model
common structure,sph_data
print,'Reading model...'
 openr,1,model
 nMHD =0L
 readu,1,  nMHD
  x=fltarr(nMHD)
  y=fltarr(nMHD)
  z=fltarr(nMHD)
 Bx=fltarr(nMHD)
 By=fltarr(nMHD)
 Bz=fltarr(nMHD)
  r=fltarr(nMHD)
 th=fltarr(nMHD)
 ph=fltarr(nMHD)
 Br=fltarr(nMHD)
Bth=fltarr(nMHD)
Bph=fltarr(nMHD)
 readu,1,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
 Nr   = 0L
 Nth  = 0L
 Nph  = 0L
 readu,1,Nr,Nth,Nph
    rg = fltarr(Nr ) 
   thg = fltarr(Nth)
   phg = fltarr(Nph)
   Brg = fltarr(Nr,Nth,Nph)
  Bthg = fltarr(Nr,Nth,Nph)
  Bphg = fltarr(Nr,Nth,Nph)
 readu,1,rg,thg,phg,Brg,Bthg,Bphg
 close,1
print,'Done.'
print,'Creating structure...'
; Re-order the dimensions of Br, Bth, and Bph, so that for each array: 
; the 3rd dimension (index=2) becomes 1st,
; the 2nd dimension (index=1) stays   2nd,
; the 1st dimension (index=0) becomes 3rd. 
  Br_new = TRANSPOSE(  Brg , [2, 1, 0] ) 
 Bth_new = TRANSPOSE( Bthg , [2, 1, 0] ) 
 Bph_new = TRANSPOSE( Bphg , [2, 1, 0] ) 

sph_data = {        BR: ptr_new( Br_new)                            ,$
                   BTH: ptr_new(Bth_new)                            ,$
                   BPH: ptr_new(Bph_new)                            ,$
               BDERIVS: ptr_new()                                   ,$
                    NR: long(Nr)                                    ,$
                  NLAT: long(Nth)                                   ,$
                  NLON: long(Nph)                                   ,$
                   RIX: ptr_new(  rg * 1.d )                        ,$
                 THETA: ptr_new( thg * 1.d )                        ,$
                   PHI: ptr_new( phg * 1.d )                        ,$
                   LAT: ptr_new( 90.d0-thg/!dtor )                  ,$
                   LON: ptr_new(  1.d0*phg/!dtor )                  ,$
             LONBOUNDS: dblarr(2)-1.                                ,$
                   STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new() ,$
                   PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new() ,$
                 NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()          }
print,'Done.'
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

pro impose_min_max,datamax,data
pmin=(where(data eq min(data)))(0)
pmax=(where(data eq max(data)))(0)
data(pmin)=-datamax
data(pmax)=+datamax
return
end

