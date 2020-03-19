pro graficos_paper
;Este e sun wrapper de los graficos del paper  
fileA='Ne_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
fileB='Ne_awsom_2082_1.85_short' 
fileC='R_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
mapa_perfil,fileA,fileB=fileB,fileC=fileC,win=1,unit=1.e8,rads=[1.105],lons=[100,300],filename='Ne_demt_awsom_2082',/mapoc,/cr2082,ytitle='Ne [10!U8!Ncm!U-3!N]',linestyle=[0,2],color=[0,0]

fileA='Ne_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'
fileB='Ne_awsom_2208_1.85_short'
filec='R_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'
mapa_perfil,fileA,fileB=fileB,fileC=fileC,win=1,unit=1.e8,rads=[1.105],lons=[0,150],filename='Ne_demt_awsom_2208',/mapoc,/cr2208,ytitle='Ne [10!U8!Ncm!U-3!N]',linestyle=[0,2],color=[1,1] 

fileA='Tm_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d' 
fileB='Te_awsom_2082_1.85_short'
fileC='R_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
mapa_perfil,fileA,fileB=fileB,fileC=fileC,win=1,unit=1.e6,rads=[1.105],lons=[100,300],filename='Te_demt_awsom_2082',/mapoc,/cr2082,ytitle='Te [MK]',linestyle=[0,2],color=[0,0]

fileA='Tm_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'
fileB='Te_awsom_2208_1.85_short' 
filec='R_CR2208_DEMT-AIA_H1_L.5.2.2_r3d'
mapa_perfil,fileA,fileB=fileB,fileC=fileC,win=1,unit=1.e6,rads=[1.105],lons=[0,150],filename='Te_demt_awsom_2208',/mapoc,/cr2208,ytitle='Te [MK]',linestyle=[0,2],color=[1,1]
return
end



;mapa_perfil,fileA,fileC=fileC,win=1,unit=1.e8,rads=[1.025,1.065,1.105,1.155,1.205],filename='Ne_demt_2082'
;mapa_perfil,fileA,fileC=fileC,win=1,unit=1.e8,rads=[1.025,1.065,1.105,1.155,1.205],filename='Ne_demt_2208'
;----------------------------------------
;experimentacion
;fileA='Vr_awsom_2082_1.85_extend'
;fileA='Vr_awsom_2208_1.85_extend'
;--------------------------------------------
;mapa_perfil,fileA,rmax=6,nr=500,win=1,rads=[3.005,4.005,5.005,5.995],dirA='/data1/work/MHD/',filename='Vr_2082',ytitle='Vr [m/s]',/cr2082,linestyle=[2],color=[0]
;mapa_perfil,fileA,rmax=6,nr=500,win=1,rads=[3.005,4.005,5.005,5.995],dirA='/data1/work/MHD/',filename='Vr_2208',ytitle='Vr [m/s]',/cr2208,linestyle=[2],color=[1]
;--------------------------------------------
;Campo Br_interpolado
;
;fileA=''
;
;
;Este codigo agarra un mapa (matriz de datos) y hace perfiles
;latitudinales a altura fija. En caso de awsom, usar solo una
;entrada. En caso de demt usar ademas fileB como la matris R,
;en el caso de querer overplot, usar las 3 entradas con A y B demt y
;la otra awsom.
pro mapa_perfil,fileA,fileB=fileB,fileC=fileC,filename=filename,tit=tit,lats=lats,lons=lons,rads=rads,nrad=nrad,rmin=rmin,rmax=rmax,win=win,unit=unit,ytit=ytit,dirA=dirA,dirB=dirB,$
                cr2082=cr2082,cr2208=cr2208,mapoc=mapoc,ytitle=ytitle,linestyle=linestyle,color=color
  if not keyword_set(dirB)        then dirB         = '/data1/work/MHD/'
  if not keyword_set(dirA)        then dirA         = '/data1/work/dem/';'/data1/DATA/ldem_files/'
  if not keyword_set(lats)        then lats         =[-90,90]
  if not keyword_set(lons)        then lons         =[0,360]
  if not keyword_set(rads)        then rads         =[1.105]
  if not keyword_set(win )        then win          = 1
  if not keyword_set(unit)        then unit         = 1.
  if     keyword_set(cr2082)      then aux          = 'CR2082 '
  if     keyword_set(cr2208)      then aux          = 'CR2208 '
  if not keyword_set(cr2082) and not keyword_set(cr2208) then aux          = ''
  if not keyword_set(tit )        then tit          = aux+'Long Average'
  if not keyword_set(nrad)        then nrad         = 26
  if not keyword_set(rmin)        then rmin         = 1.
  if not keyword_set(rmax)        then rmax         = 1.26
  if not keyword_set(ytitle)        then ytitle         = ''
  if keyword_set (fileB) and not keyword_set(linestyle) then begin
     linestyle =[0,0]
     color = [0,1]
  endif
  if not keyword_set (fileB) and not keyword_set(linestyle) then begin
     linestyle =[0]
     color = [0]
  endif
  
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
     if rads(irr) eq 1.205 then ir = 20
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
        ps1,'./newfigs/Perfil_'+filename+'_'+strmid(rads(irr),6,5)+'.eps',0
;        !p.charsize=1
;        DEVICE,/INCH,YSIZE=5,XSIZE=10,SCALE_FACTOR=3
        device,/inches,xsize=10,ysize=5, /helvetica,SCALE_FACTOR=1
     endif
     if not keyword_set(filename) then window,win+jj
     loadct,12
     azul = 100
     rojo = 200
     verde= 20
     negro =0
     cyan = 80
     violeta = 110
     !P.CHARTHICK=6
     !p.charsize=2.5
     thick=3

     if keyword_set(map2) then begin
        maxx = max([max(v_prom_map1),max(v_prom_map2)])
        minn = min([min(v_prom_map1),min(v_prom_map2)])
        v_prom = v_prom_map1
        v_prom (0) = maxx
        v_prom (1) = minn
     endif
     if keyword_set(map2) then $
        plot,latitud,v_prom/unit ,psym=10,xtit='Latitude [deg]',thick=thick,ythick=thick,xstyle=1,ystyle=1,/nodata,Font=0,title=tit+' at'+strmid(rads(irr),5,6)+' Rsun',ytit=ytitle
     if not keyword_set(map2) then $
        plot,latitud,v_prom_map1/unit ,psym=10,xtit='Latitude [deg]',thick=thick,ythick=thick,xstyle=1,ystyle=1,/nodata,Font=0,title=tit+' at'+strmid(rads(irr),5,6)+' Rsun',ytit=ytitle
     oplot,latitud,v_prom_map1/unit,psym=10 ,LINESTYLE=linestyle(0),th=8,color=fun(color(0))
;     oplot,latitud,v_prom_map1/unit,psym=10,color=rojo ,LINESTYLE=0,th=5 
     if keyword_set(map2) then oplot,latitud,v_prom_map2/unit,psym=10 ,LINESTYLE=linestyle(1),th=8,color=fun(color(1))
;     if not keyword_set(map2) then  xyouts,.95-[.18],0.83*[1],['demt'],/normal,color=[azul] ,charthick=3,charsize=2.3,Font=0
;     if not keyword_set(map2) then  xyouts,.95-[.18],0.83*[1],['awsom'],/normal,color=[rojo] ,charthick=3,charsize=2.3,Font=0

     if     keyword_set(map2) then begin
        dx=(180)/20.
        t1=  -90 + (180.)/2 *1.1
        t2 = t1+dx/2. *2
        y0=maxx /unit
        drel=((maxx - minn)/unit )/10.
        stop
        oplot,[t1,t2],y0*[1,1]-drel*7.5,linestyle=linestyle(0),th=8,color=fun(color(0))
        oplot,[t1,t2],y0*[1,1]-drel*9.2,linestyle=linestyle(1),th=8,color=fun(color(1))
        xyouts,.8-[.15,.15],1.-[.6,.7],['DEMT','AWSoM'],/normal,color=[fun(color(0)),fun(color(1))] ,charthick=2,charsize=2.,Font=0
     endif
     
;     mapoc  = 1
;     cr2082 = 1
     ;cr2208 = 1
     if keyword_set(mapoc) then begin
        if keyword_set(cr2082) then begin
           restore,'/data1/work/dem/github_dem/dem/vec_mapoc_2082.sav'
           lat_superior = median(latva(where(latva gt  30 and lonva ge lons(0) and lonva le lons(1))))
           lat_inferior = median(latva(where(latva lt -30 and lonva ge lons(0) and lonva le lons(1))))
        endif
        if keyword_set(cr2208) then begin
           restore,'/data1/work/dem/github_dem/dem/vec_mapoc_2208.sav'
           lat_superior = median(latva(where(latva gt  30 and lonva ge lons(0) and lonva le lons(1))))
           lat_inferior = median(latva(where(latva lt -30 and lonva ge lons(0) and lonva le lons(1))))
        endif
        oplot,[lat_superior,lat_superior],[minn,maxx]/unit,color=0,LINESTYLE=0,th=8
        oplot,[lat_inferior,lat_inferior],[minn,maxx]/unit,color=0,LINESTYLE=0,th=8
     endif
     
     if keyword_set(filename) then ps2

     jj = jj+1
  endfor
  
  return
end
