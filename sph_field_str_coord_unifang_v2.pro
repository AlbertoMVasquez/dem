pro sph_field_str_coord_unifang_v2,pfss_data,dlatv=dlatv,dlonv=dlonv,radstartv=radstartv,bbox=bbox
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
; Updated by A.M. Vasquez & Diego Lloveras to concatenate starting
; points with different heights.
;-

  if NOT keyword_set(bbox)     then bbox=[0.,-90.,360.,+90.]
  bbox = float(bbox)

 for v = 0,n_elements(radstartv)-1 do begin

  radstart = radstartv(v)
  dlat     = dlatv    (v)   
  dlon     = dlonv    (v)

  dlat=float(dlat)
  dlon=float(dlon)

  nlat = long(fix((bbox[3]-bbox[1])/dlat))
  nlon = long(fix((bbox[2]-bbox[0])/dlon))

  lat = bbox[1] + dlat /2. + dlat * findgen(nlat)
  lon = bbox[0] + dlon /2. + dlon * findgen(nlon)

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

  ; DEFINE AND BUILD the CONCATENATED ARRAYS
  if v eq 0 then begin
     str_c = str
    stth_c = stth
    stph_c = stph 
  endif else begin
     str_c = [ str_c,str ]
    stth_c = [stth_c,stth]
    stph_c = [stph_c,stph] 
  endelse

endfor

  pfss_data.str  = ptr_new( str_c)
  pfss_data.stth = ptr_new(stth_c)
  pfss_data.stph = ptr_new(stph_c)

  return
end
