;Hecho por Fede Nuevo 2019.

pro hacer_make_mapoc
  file_input='traceLDEM_CR2223_ldem__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025+ 0.04 *findgen(6)
  file_suffix='CR2223_90X180_fdips'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

stop
  file_input='traceLDEM_CR2082_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025+ 0.04 *findgen(6)
  file_suffix='CR2082_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/mhd
stop
  file_input= 'traceLDEM_CR2208_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025+ 0.04 *findgen(6)
  file_suffix='CR2208_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/mhd
  stop
  return
end



pro MAKE_MAPOC,file_input,file_suffix,alturas,mdi=mdi,gng=gng,mhd=mhd,dir_input=dir_input,dir_output=dir_output

  if not keyword_set(dir_input) then dir_input='/data1/work/dem/'
  if not keyword_set(dir_output) then dir_output='/data1/work/MHD/'

  print,'calcula mapa open-close en:',alturas

  print,'leyendo trazado: ',file_input
  restore,dir_input+file_input

  if keyword_set(mdi) then postsuffix='_MDI.dat'
  if keyword_set(gng) then postsuffix='_GNG.dat'
  if keyword_set(mhd) then postsuffix='_MHD.dat'

  nr=n_elements(alturas)
  for ir=0,nr-1 do begin
     altura=alturas(ir)
     stringheight=strmid(string(altura),6,5)
     file_output=file_suffix+stringheight+'_open-close-map'+postsuffix
     print,'-----> O/C: '+file_output
     index=where(str_v eq altura)
     stth = stth_v(index)
     stph = stph_v(index) 
     
     nlat=90
     nlon=180
     dlat=2.
     dlon=2.
     lat = -90. + dlat /2. + dlat * findgen(nlat)
     lon =   0. + dlon /2. + dlon * findgen(nlon)

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
stop
     openw,1,dir_output+file_output
     writeu,1,mapoc
     close,1
  endfor

  return
end
