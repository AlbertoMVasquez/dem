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

pro mapa_perfil,fileA,fileB,fileC,filename=filename,tit=tit,lats=lats,lons=lons,rads=rads
  if not keyword_set(dirB)        then dirB         = '/data1/work/MHD/'
  if not keyword_set(dirA)        then dirA         = '/data1/work/dem/';'/data1/DATA/ldem_files/'
  if not keyword_set(lats)        then lats         =[-90,90]
  if not keyword_set(lons)        then lons         =[0,360]
  if not keyword_set(rads)        then rads         =[1.105]
  
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
  
  Nrad  = 26
  dr = 0.01
  rad =   1. + dr/2. + dr * findgen(Nrad)

  Nr=26

  lat3D = fltarr(Nr,Nlat,Nlon)
  rad3d = fltarr(Nr,Nlat,Nlon)
  lon3d = fltarr(Nr,Nlat,Nlon)
  
  xread,dir=dirA,file=fileA,nr=26,nt=90,np=180,map=map1
  xread,dir=dirB,file=fileB,nr=26,nt=90,np=180,map=map2
  xread,dir=dirA,file=fileC,nr=26,nt=90,np=180,map=mapC

  quality = indgen(Nrad,Nlat,Nlon)*0
  p= where(mapC le  0.25 and mapC ge 0.)
  quality(p) = 1
  

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
  
     ir = where(rad eq altura)
     map1_ir = reform(map1(ir,*,*))
     if keyword_set(map2) then map2_ir = reform(map2(ir,*,*)) 

     lati=0.
     long=0.
     v_prom_map1 = fltarr(90)
     v_prom_map2 = fltarr(90)

     for lati= 0,89 do begin
        ok = where (map1_ir(lati,lonfi:longf) ne -999.)
        v_prom_map1(lati) = mean(map1_ir(lati,ok))        

        if keyword_set(map2) then begin
           ok = where (map2_ir(lati,lonfi:longf) ne -999.)
           v_prom_map2(lati) = mean(map2_ir(lati,ok))           
        endif       
     endfor
     latitud=(findgen(90)-44.5)*2
filelabel='euvi_ts_R1215'
ps1,'./newfigs/Perfil_Ne_Te_'+filelabel+'.eps',0
!p.charsize=1
 DEVICE,/INCH,YSIZE=5,XSIZE=10,SCALE_FACTOR=3

plot,latitud,v_prom_ne/1.e8,psym=10,yr=[0.,.6] ,ytit='Ne [1.e8]'
plot,latitud,v_prom_tm/1.e6,psym=10,yr=[1.,1.4],ytit='Te [MK]' ,xtit='Lat'
ps2
!P.MULTI =0

  
goto,estono  
  ps1,'./newfigs/Standev_'+filename+'.eps',0
  DEVICE,/INCHES,YSIZE=7,XSIZE=10,SCALE_FACTOR=1
  DEVICE,/TIMES, FONT_INDEX=4
  thick=3
  loadct,12
  verde = 25
  azul  =100
  rojo  =200
  negro =0
  
  plot,rad,maxx ,psym=1,ytit='stdev',xtit='Height [Rsun]',thick=3,xstyle=1,ystyle=1,/nodata,charthick=2.25,Font=0,charsize=2.5,title=tit
  oplot,rad,StdvAbsRelDiff_Streamer,psym=1,color=azul ,LINESTYLE=0,th=5
  oplot,rad,StdvAbsRelDiff_Streamer       ,color=azul ,LINESTYLE=0,th=5
  oplot,rad,StdvAbsRelDiff_NHole   ,psym=2,color=verde,LINESTYLE=0,th=5
  oplot,rad,StdvAbsRelDiff_NHole          ,color=verde,LINESTYLE=0,th=5
  oplot,rad,StdvAbsRelDiff_SHole   ,psym=2,color=rojo ,LINESTYLE=0,th=5
  oplot,rad,StdvAbsRelDiff_SHole          ,color=rojo ,LINESTYLE=0,th=5
  xyouts,.95-[.18],0.83*[1],['Streamer'],/normal,color=[azul] ,charthick=3,charsize=2.3,Font=0
  xyouts,.95-[.18],0.78*[1],['NHole']   ,/normal,color=[verde],charthick=3,charsize=2.3,Font=0
  xyouts,.95-[.18],0.73*[1],['SHole']   ,/normal,color=[rojo] ,charthick=3,charsize=2.3,Font=0
  ps2
  estono:
  
  ps1,'./newfigs/Median_'+filename+'.eps',0
  DEVICE,/INCHES,YSIZE=7,XSIZE=10,SCALE_FACTOR=1
  DEVICE,/TIMES, FONT_INDEX=4
  thick=3
  loadct,12
  verde = 25
  azul  =100
  rojo  =200
  negro =0

  ok = (where(rad ge 1.055 and rad lt 1.2))
  rad=rad(ok)
  referencia =MedianAbsRelDiff_SHole(ok)
  referencia(0)=-0.1
  plot,rad,referencia ,psym=1,ytit='Median',xtit='Height [Rsun]',thick=3,xstyle=1,ystyle=1,/nodata,charthick=2.25,Font=0,charsize=2.5,title=tit
  oplot,rad,MedianAbsRelDiff_Streamer(ok),psym=1,color=azul ,LINESTYLE=0,th=5
  oplot,rad,MedianAbsRelDiff_Streamer(ok)       ,color=azul ,LINESTYLE=0,th=5
  oplot,rad,MedianAbsRelDiff_NHole(ok)   ,psym=2,color=verde,LINESTYLE=0,th=5
  oplot,rad,MedianAbsRelDiff_NHole(ok)          ,color=verde,LINESTYLE=0,th=5
  oplot,rad,MedianAbsRelDiff_SHole(ok)   ,psym=2,color=rojo ,LINESTYLE=0,th=5
  oplot,rad,MedianAbsRelDiff_SHole(ok)          ,color=rojo ,LINESTYLE=0,th=5
  
  xyouts,.95-[.18],0.83*[1],['Streamer'],/normal,color=[azul] ,charthick=3,charsize=2.3,Font=0
  xyouts,.95-[.18],0.78*[1],['NHole']   ,/normal,color=[verde],charthick=3,charsize=2.3,Font=0
  xyouts,.95-[.18],0.73*[1],['SHole']   ,/normal,color=[rojo] ,charthick=3,charsize=2.3,Font=0
  ps2




  return
end
