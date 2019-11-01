
pro wrapper
  trace_openclose,'CR2082_90X180blines_r_',10,1.105,/mhd,lonvA,latvA
  stop
  return 
end

pro trace_openclose,string,ir,height,gng=gng,mdi=mdi,mhd=mhd,lonvA,latvA

if keyword_set(gng) then load_mapoc,string,height,/gng,mapoc
if keyword_set(mdi) then load_mapoc,string,height,/mdi,mapoc
if keyword_set(mhd) then load_mapoc,string,height,/mhd,mapoc
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
