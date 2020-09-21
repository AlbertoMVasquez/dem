pro perfil_magnetograma,file,adapt_gong=adapt_gong,gong=gong,hmi=hmi

;file es un archivo adapt-gong 90x180x#realizations
;file='/data1/Descargas/adapt40311_03i012_201910231600_i00005600n0.fts'
;  perfil_magnetograma,'mrbqs190702t2014c2219_255.fits',/gong
;  perfil_magnetograma,'mrmqs190713t0141c2219_000.fits',/gong
;  perfil_magnetograma,'adapt40311_03i012_201907021600_i00005600n0.fts',/adapt_gong
;  perfil_magnetograma,'adapt40311_03i012_201907090000_i00005600n0.fts',/adapt_gong
;  perfil_magnetograma,'hmi.Synoptic_Mr_small.2219.fits',/hmi

  
  mreadfits,'/data1/DATA/Magnetograma/'+file,hdr,ima
Data = readfits('/data1/DATA/Magnetograma/'+file, ImHeader)
if keyword_set(adapt_gong) then  n_realizations=(size(ima))(3)
if not keyword_set(adapt_gong) then  n_realizations=1

  Nlat = 90
  Nlon = nlat*2
  min_lat = -90.
  max_lat = +90.
  dlat = (max_lat - min_lat) / Nlat
  lat = min_lat + dlat/2. + dlat * findgen(nlat)
  lon = findgen(Nlon)*2.


;latitud
  stop
s=size(Data)
Nx=s(1)
Ny=s(2)
UseCosTheta='T'
for i = 0L, Ny-1 do begin
 ;  if (UseCosTheta eq 'T' ) then $
      Latitude1 = acos(1 - (i + 0.5)*dY)*180.0/!pi - 90 
;   else $
      Latitude2 = (i+0.5)*180.0/Ny - 90

endfor
  stop
;  lat3D = fltarr(Nr,Nlat,Nlon)
;  rad3d = fltarr(Nr,Nlat,Nlon)

  prom_lat = fltarr(180)*0.
  prom_mat = MAKE_ARRAY((size(ima))(3), 180, /double, VALUE = 0)
;  latitud=(findgen(90)-44.5)*2
  latitud=(findgen(180)-89)

 if keyword_set(adapt_gong) then begin 
    for j=0,n_realizations-1 do begin
       for lat=0,180-1 do begin
          prom_lat(lat) = mean(ima(*,lat,j))
       endfor
       prom_mat(j,*) = prom_lat
    endfor
 endif
 
 stop
 prom_mat_init= prom_mat(0,*)
 prom_mat_init(0)=min(prom_mat)
 prom_mat_init(1)=max(prom_mat)

 loadct,12
 
 plot,latitud,prom_mat_init ,/nodata,xtit='Latitude [deg]',xstyle=1,ystyle=1,Font=0
oplot,latitud,prom_mat(0,*),color=20
oplot,latitud,prom_mat(1,*),color=40
oplot,latitud,prom_mat(2,*),color=80
oplot,latitud,prom_mat(3,*),color=100
oplot,latitud,prom_mat(4,*),color=140
oplot,latitud,prom_mat(5,*),color=180
oplot,latitud,prom_mat(6,*),color=200
oplot,latitud,prom_mat(7,*),color=220
oplot,latitud,prom_mat(8,*),color=240
oplot,latitud,prom_mat(9,*),color=250
oplot,latitud,prom_mat(10,*),color=220
oplot,latitud,prom_mat(11,*),color=60

stop

 
return
end
