pro testeo_fbes

dir = '/data1/tomography/bindata/'
nr =26
nt =90
np = 180

file = 'x_AIA_CR2208_171b_h1_Rmin1.00_Rmax1.26_Nr26_InstRmax1.26_bf4_r3d_B_L0.50'
xread,dir=dir,file=file,nr=nr,nt=nt,np=np,map=map
map171 = map

file = 'x_AIA_CR2208_193b_h1_Rmin1.00_Rmax1.26_Nr26_InstRmax1.26_bf4_r3d_B_L0.20'
xread,dir=dir,file=file,nr=nr,nt=nt,np=np,map=map
map193 = map

file = 'x_AIA_CR2208_211b_h1_Rmin1.00_Rmax1.26_Nr26_InstRmax1.26_bf4_r3d_B_L0.20'
xread,dir=dir,file=file,nr=nr,nt=nt,np=np,map=map
map211 = map


file = 'x_euvi.A.171.cr2081.26x90_bf4_ri.00_ro1.02_l.3_DECON_Oldset_H1_r3d'
xread,dir=dir,file=file,nr=nr,nt=nt,np=np,map=map
mape171 = map

file = 'x_euvi.A.195.cr2081.26x90_bf4_ri.00_ro1.02_l.2_DECON_Oldset_H1_r3d'
xread,dir=dir,file=file,nr=nr,nt=nt,np=np,map=map
mape195 = map

file = 'x_euvi.A.284.cr2081.26x90_bf4_ri.00_ro1.02_l.3_DECON_Oldset_H1_r3d'
xread,dir=dir,file=file,nr=nr,nt=nt,np=np,map=map
mape284 = map


  Nlat = 90
  Nlon = nlat*2
  min_lat = -90.
  max_lat = +90.
  dlat = (max_lat - min_lat) / Nlat
  lat = min_lat + dlat/2. + dlat * findgen(nlat)
  lon = findgen(Nlon)*2.
  Nrad  = 26
  rmax         = 1.26
  rmin         = 1.
  dr = (rmax - rmin)/nrad
  rad =   rmin + dr/2. + dr * findgen(Nrad)
  Nr=nrad

  lat3D = fltarr(Nr,Nlat,Nlon)
  rad3d = fltarr(Nr,Nlat,Nlon)
  lon3d = fltarr(Nr,Nlat,Nlon)

  for irad=0,Nrad-1 do begin
     rad3d(irad,*,*) = rad(irad)
     for ilon=0,Nlon-1 do begin
        lat3D(irad,*,ilon) = lat
     endfor
     for ilat=0,Nlat-1 do begin
        lon3d(irad,ilat,*) = lon
     endfor
  endfor

stop
;mapoc
load_mapoc,"CR2208_90X180blines_r_1.025_open-close-map_MHD.dat",1.025,/mhd,mapoc_1025,file="CR2208_90X180blines_r_1.025_open-close-map_MHD.dat"
load_mapoc,"CR2208_90X180blines_r_1.065_open-close-map_MHD.dat",1.065,/mhd,mapoc_1065,file="CR2208_90X180blines_r_1.065_open-close-map_MHD.dat"
load_mapoc,"CR2208_90X180blines_r_1.105_open-close-map_MHD.dat",1.105,/mhd,mapoc_1105,file="CR2208_90X180blines_r_1.105_open-close-map_MHD.dat"
stop
load_mapoc,"CR2082_90X180blines_r_1.025_open-close-map_MHD.dat",1.025,/mhd,mapoce_1025,file="CR2082_90X180blines_r_1.025_open-close-map_MHD.dat"
load_mapoc,"CR2082_90X180blines_r_1.065_open-close-map_MHD.dat",1.065,/mhd,mapoce_1065,file="CR2082_90X180blines_r_1.065_open-close-map_MHD.dat"
load_mapoc,"CR2082_90X180blines_r_1.105_open-close-map_MHD.dat",1.105,/mhd,mapoce_1105,file="CR2082_90X180blines_r_1.105_open-close-map_MHD.dat"


altura = 2
lat3=reform(lat3d(altura,*,*))
;aia

p1025_o = where (map171(altura,*,*) gt 0. and map193(altura,*,*) gt 0. and map211(altura,*,*) gt 0. and mapoc_1025(2,*,*) eq 10. )
p1025_c = where (map171(altura,*,*) gt 0. and map193(altura,*,*) gt 0. and map211(altura,*,*) gt 0. and mapoc_1025(2,*,*) eq 0.1 )

a171=reform(map171(altura,*,*))
a193=reform(map193(altura,*,*))
a211=reform(map211(altura,*,*))

histoplot,a171(p1025_o),win=0,max=15
histoplot,a171(p1025_c),win=1,max=15
histoplot,a193(p1025_o),win=2,max=25
histoplot,a193(p1025_c),win=3,max=25
histoplot,a211(p1025_o),win=4,max=15
histoplot,a211(p1025_c),win=5,max=15




;euvi
p1025_o = where (mape171(altura,*,*) gt 0. and mape195(altura,*,*) gt 0. and mape284(altura,*,*) gt 0. and mapoce_1025(2,*,*) eq 10. )
p1025_c = where (mape171(altura,*,*) gt 0. and mape195(altura,*,*) gt 0. and mape284(altura,*,*) gt 0. and mapoce_1025(2,*,*) eq 0.1 )

a171e=reform(mape171(altura,*,*))
a195e=reform(mape195(altura,*,*))
a284e=reform(mape284(altura,*,*))

histoplot,a171e(p1025_o),win=0,max=50
histoplot,a171e(p1025_c),win=1,max=50
histoplot,a195e(p1025_o),win=2,max=32
histoplot,a195e(p1025_c),win=3,max=32
histoplot,a284e(p1025_o),win=4,max=12
histoplot,a284e(p1025_c),win=5,max=12








stop
histoplot,map171(altura,*,*)
histoplot,lat3(where(map171(altura,*,*) le 3. and map171(altura,*,*) ge 0.)),win=1

histoplot,map193(altura,*,*),win=2,max=30
histoplot,lat3(where(map193(altura,*,*) le 3. and map193(altura,*,*) ge 0.)),win=3

histoplot,map211(altura,*,*),win=4,max=15
histoplot,lat3(where(map211(altura,*,*) le 2. and map211(altura,*,*) ge 0.)),win=5

histoplot,mape171(altura,*,*)
histoplot,lat3(where(mape171(altura,*,*) le 3. and mape171(altura,*,*) ge 0.)),win=1

histoplot,mape195(altura,*,*),win=2,max=30
histoplot,lat3(where(mape195(altura,*,*) le 3. and mape195(altura,*,*) ge 0.)),win=3

histoplot,mape211(altura,*,*),win=4,max=15
histoplot,lat3(where(mape211(altura,*,*) le 2. and mape211(altura,*,*) ge 0.)),win=5


return
end





