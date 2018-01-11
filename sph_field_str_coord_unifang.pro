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

  bbox = float(bbox)
  nrad = 26
  drad = 0.01
  if NOT keyword_set(dlat) then dlat = 2.
  if NOT keyword_set(dlon) then dlon = 2.

  dlat=float(dlat)
  dlon=float(dlon)
  ;<----
  nlat = long(fix((bbox[3]-bbox[1])/dlat))
  nlon = long(fix((bbox[2]-bbox[0])/dlon))

  rad = 1.      + drad /2. + drad * findgen(nrad)
  lat = bbox[1] + dlat /2. + dlat * findgen(nlat)
  lon = bbox[0] + dlon /2. + dlon * findgen(nlon)

  if not keyword_set(radstart) then radstart=rad(0)

; Colatitude and longitude in radians:
  theta=(90.-lat)*!dtor
  phi  =     lon *!dtor
  stth=dblarr(nlat*nlon,/noz)
  stph=dblarr(nlat*nlon,/noz)
  i=0.
  for ilat=0,nlat-1 do begin
     for ilon=0,nlon-1 do begin
        
        stth[i]=theta[ilat]
        stph[i]=  phi[ilon]
        i=i+1
     endfor
  endfor
  str=replicate(radstart,n_elements(stth))
  
  pfss_data.str  = ptr_new(str)
  pfss_data.stth = ptr_new(stth)
  pfss_data.stph = ptr_new(stph)

  return
end
