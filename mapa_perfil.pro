;fileB='Te_awsom_2082_1.85_short'
;fileB='Ne_awsom_2082_1.85_short'
;fileB='Te_awsom_2208_1.85_short'
;fileB='Ne_awsom_2208_1.85_short'

;fileA='Tm_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
;fileA='Ne_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
;fileA='Tm_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'
;fileA='Ne_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'

;fileC='R_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
;filec='R_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'
;mapa_perfil,fileA,fileB=fileB,fileC=fileC,win=1,unit=1.e8,rads=[1.025,1.065,1.105,1.155,1.205]
;----------------------------------------
;experimentacion
;fileA='Vr_awsom_2082_1.85_extend'
;--------------------------------------------
;mapa_perfil,fileA,rmax=6,nr=500,win=1,rads=[5.],dirA='/data1/work/MHD/'

;Este codigo agarra un mapa (matriz de datos) y hace perfiles
;latitudinales a altura fija. En caso de awsom, usar solo una
;entrada. En caso de demt usar ademas fileB como la matris R,
;en el caso de querer overplot, usar las 3 entradas con A y B demt y
;la otra awsom.
pro mapa_perfil,fileA,fileB=fileB,fileC=fileC,filename=filename,tit=tit,lats=lats,lons=lons,rads=rads,nrad=nrad,rmin=rmin,rmax=rmax,win=win,unit=unit,ytit=ytit,dirA=dirA,dirB=dirB
  if not keyword_set(dirB)        then dirB         = '/data1/work/MHD/'
  if not keyword_set(dirA)        then dirA         = '/data1/work/dem/';'/data1/DATA/ldem_files/'
  if not keyword_set(lats)        then lats         =[-90,90]
  if not keyword_set(lons)        then lons         =[0,360]
  if not keyword_set(rads)        then rads         =[1.105]
  if not keyword_set(win )        then win          = 1
  if not keyword_set(unit)        then unit         = 1.
  if not keyword_set(tit )        then tit          = 'Latitudinal Profile'
  if not keyword_set(nrad)        then nrad         = 26
  if not keyword_set(rmin)        then rmin         = 1.
  if not keyword_set(rmax)        then rmax         = 1.26
  
;fileA es demt ya sea Ne o tm
; fileB es awsom ya sea ne o te
;fileC es R de demt
  Nlat = 90
  Nlon = nlat*2
  min_lat = -90.
  max_lat = +90.
  dlat = (max_lat - min_lat) / Nlat
  lat = min_lat + dlat/2. + dlat * findgen(nlat)
  lon = findgen(Nlon)*2.
  
;  Nrad  = 26
;  dr = 0.01
  dr = (rmax - rmin)/nrad
  rad =   rmin + dr/2. + dr * findgen(Nrad)

  Nr=nrad

  lat3D = fltarr(Nr,Nlat,Nlon)
  rad3d = fltarr(Nr,Nlat,Nlon)
  lon3d = fltarr(Nr,Nlat,Nlon)
  
  xread,dir=dirA,file=fileA,nr=nr,nt=nlat,np=nlon,map=map1
  if keyword_set(fileB) then xread,dir=dirB,file=fileB,nr=26,nt=90,np=180,map=map2
  if keyword_set(fileC) then xread,dir=dirA,file=fileC,nr=26,nt=90,np=180,map=mapC

  for irad=0,Nrad-1 do begin
     rad3d(irad,*,*) = rad(irad)
     for ilon=0,Nlon-1 do begin
        lat3D(irad,*,ilon) = lat
     endfor
     for ilat=0,Nlat-1 do begin
        lon3d(irad,ilat,*) = lon
     endfor
  endfor


;comienza el loop sobre alturas
;  for ir=0,n_elements(rads)-1 do begin
;     p = where(lat3d ge lats(0) and lat3d le lats(1) and lon3d ge lons(0) and lon3d le lons(1) AND quality eq 1 and rad3d eq rads(ir) )
;     if p(0) le 0 then goto,skip1
jj = 0 ;es un contador para las ventanas
  for irr=0,n_elements(rads)-1 do begin
     ir = where(rad eq rads(irr))
     if rads(irr) eq 1.065 then ir = 6;ME CAGO EN LA PUTA MADRE double(rad(6)))-double(1.065) NO DA CERO XQ A IDL SE LE CANTA EL ORTO.
     map1_ir = reform(map1(ir,*,*))
     if keyword_set(map2) then map2_ir = reform(map2(ir,*,*)) 
     if keyword_set(mapC) then mapC_ir = reform(mapC(ir,*,*)) 
     rad2d = reform(rad3d(ir,*,*))
     lat2d = reform(lat3d(ir,*,*))
     lon2d = reform(lon3d(ir,*,*))

     lati=0.
     long=0.
     v_prom_map1 = fltarr(90)*0.
     v_prom_map2 = fltarr(90)*0.

     for lati= 0,89 do begin
        if keyword_set(mapC)     then  ok = where (map1_ir(lati,*) ne -999. and mapC_ir(lati,*) le 0.25 and lon2d(lati,*) ge lons(0) and lon2d(lati,*) le lons(1) )
        if not keyword_set(mapC) then  ok = where (map1_ir(lati,*) ne -999. and lon2d(lati,*) ge lons(0) and lon2d(lati,*) le lons(1) )
        if  ok(0) eq -1 then v_prom_map1(lati) = 0
        if  ok(0) ne -1 then v_prom_map1(lati) = median(map1_ir(lati,ok))        

        if keyword_set(map2) then begin
;           if keyword_set(mapC)     then  ok = where (map1_ir(lati,*) ne -999. and mapC_ir(ir,*,*) le 0.25 and lon2d(lati,*) ge lons(0) and lon2d(lati,*) le lons(1) )
            ok = where (map2_ir(lati,*) ne -999. and lon2d(lati,*) ge lons(0) and lon2d(lati,*) le lons(1) )
           v_prom_map2(lati) = median(map2_ir(lati,ok))           
        endif       
     endfor

     latitud=(findgen(90)-44.5)*2
     
     if keyword_set(filename) then begin
        ps1,'./newfigs/Perfil_'+filename+'.eps',0
        !p.charsize=1
        DEVICE,/INCH,YSIZE=5,XSIZE=10,SCALE_FACTOR=3
     endif
     if not keyword_set(filename) then window,win+jj
     thick=3
     loadct,12
     verde = 25
     azul  =100
     rojo  =200
     negro =0
     if keyword_set(map2) then begin
        maxx = max([max(v_prom_map1),max(v_prom_map2)])
        minn = min([min(v_prom_map1),min(v_prom_map2)])
        v_prom = v_prom_map1
        v_prom (0) = maxx
        v_prom (1) = minn
     endif
     if keyword_set(map2) then $
        plot,latitud,v_prom/unit ,psym=1,xtit='Latitude [deg]',thick=3,xstyle=1,ystyle=1,/nodata,charthick=2.25,Font=0,charsize=2.5,title=tit+' at'+strmid(rads(irr),5,6)+' Rsun'
     if not keyword_set(map2) then $
        plot,latitud,v_prom_map1/unit ,psym=1,xtit='Latitude [deg]',thick=3,xstyle=1,ystyle=1,/nodata,charthick=2.25,Font=0,charsize=2.5,title=tit+' at'+strmid(rads(irr),5,6)+' Rsun'
     oplot,latitud,v_prom_map1/unit,psym=10,color=azul ,LINESTYLE=0,th=5 
     if keyword_set(map2) then oplot,latitud,v_prom_map2/unit,psym=10,color=rojo ,LINESTYLE=0,th=5
     if not keyword_set(map2) then  xyouts,.95-[.18],0.83*[1],['demt'],/normal,color=[azul] ,charthick=3,charsize=2.3,Font=0
     if     keyword_set(map2) then  xyouts,.95-[.2,.2],0.83*[1,.85],['demt','awsom'],/normal,color=[azul,rojo] ,charthick=3,charsize=2.3,Font=0
   
     if keyword_set(filename) then ps2
     jj = jj+1
  endfor
  
  return
end
