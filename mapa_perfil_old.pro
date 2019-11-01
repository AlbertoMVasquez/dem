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

pro mapa_perfil,fileA,fileB,fileC,filename=filename,tit=tit
  if not keyword_set(dirB)        then dirB         = '/data1/work/MHD/'
  if not keyword_set(dirA)        then dirA         = '/data1/work/dem/';'/data1/DATA/ldem_files/'
;fileA es demt ya sea Ne o tm
; fileB es awsom ya sea ne o te
;fileC es R de demt
  Nlat = 90
  min_lat = -90.
  max_lat = +90.
  dlat = (max_lat - min_lat) / Nlat
  lat = min_lat + dlat/2. + dlat * findgen(90.)

  Nrad  = 26
  dr = 0.01
  rad =   1. + dr/2. + dr * findgen(Nrad)

  Nr=26
  Nlon  = 2*Nlat
  lat3D = fltarr(Nr,Nlat,Nlon)
  rad3d = fltarr(Nr,Nlat,Nlon)
 
  xread,dir=dirA,file=fileA,nr=26,nt=90,np=180,map=map1
  xread,dir=dirB,file=fileB,nr=26,nt=90,np=180,map=map2
  xread,dir=dirA,file=fileC,nr=26,nt=90,np=180,map=mapC

  quality = indgen(Nrad,Nlat,Nlon)*0
  p= where(mapC le  0.25 and mapC ge 0.)
  quality(p) = 1
  

  for irad=0,Nrad-1 do begin
     for ilon=0,Nlon-1 do begin
        lat3D(irad,*,ilon) = lat
     endfor
  endfor
  
  for irad=0,Nrad-1 do begin
     rad3d(irad,*,*) = rad(irad)
  endfor
  
  
  Nr=26
  StdvAbsRelDiff_Streamer   = fltarr(Nr)
  StdvAbsRelDiff_SHole      = fltarr(Nr)
  StdvAbsRelDiff_NHole      = fltarr(Nr)
  MedianAbsRelDiff_Streamer = fltarr(Nr)
  MedianAbsRelDiff_SHole    = fltarr(Nr)
  MedianAbsRelDiff_NHole    = fltarr(Nr)
  
  for ir=0,Nr-1 do begin
     p = where(abs(lat3d) le 50. AND quality eq 1 and rad3d eq rad(ir) and rad3d ge 1.055 and rad3d le 1.2)
     if p(0) le 0 then goto,skip1
     AbsRelDiffMap = ((map1-map2)/map2)(p)
     StdvAbsRelDiff_Streamer(ir) = stdev(AbsRelDiffMap)
     MedianAbsRelDiff_Streamer(ir) = median(AbsRelDiffMap)
     skip1:
     ; NHole                                                                                                                                                                                                                                     
     p = where (lat3D ge -1.*80. and lat3D le -1.*70 and quality eq 1 and rad3d eq rad(ir) and rad3d ge 1.055 and rad3d le 1.2)
     if p(0) le 0 then goto,skip2
     AbsRelDiffMap = (abs((map1-map2)/0.5/(map1+map2)))(p)
     StdvAbsRelDiff_SHole(ir)   = stdev(AbsRelDiffMap)
     MedianAbsRelDiff_SHole(ir) = median(AbsRelDiffMap)
     skip2:

     p = where (lat3D le 1.*80. and lat3D ge 1.*70 and quality eq 1 and rad3d eq rad(ir) and rad3d ge 1.055 and rad3d le 1.2)
     if p(0) le 0 then goto,skip3
     AbsRelDiffMap = (abs((map1-map2)/0.5/(map1+map2)))(p)
     StdvAbsRelDiff_NHole(ir) = stdev(AbsRelDiffMap)
     MedianAbsRelDiff_NHole(ir) = median(AbsRelDiffMap)
     skip3:
  endfor
  
  if max(StdvAbsRelDiff_Streamer) ge max(StdvAbsRelDiff_NHole) and max(StdvAbsRelDiff_Streamer) ge max(StdvAbsRelDiff_SHole) then maxx = StdvAbsRelDiff_Streamer
  if max(StdvAbsRelDiff_NHole) ge max(StdvAbsRelDiff_Streamer) and max(StdvAbsRelDiff_NHole) ge max(StdvAbsRelDiff_SHole) then maxx = StdvAbsRelDiff_NHole
  if max(StdvAbsRelDiff_SHole) ge max(StdvAbsRelDiff_Streamer) and max(StdvAbsRelDiff_SHole) ge max(StdvAbsRelDiff_NHole) then maxx = StdvAbsRelDiff_SHole
  
  if max(MedianAbsRelDiff_Streamer) ge max(MedianAbsRelDiff_NHole) and max(MedianAbsRelDiff_Streamer) ge max(MedianAbsRelDiff_SHole) then maxx2 = MedianAbsRelDiff_Streamer
  if max(MedianAbsRelDiff_NHole) ge max(MedianAbsRelDiff_Streamer) and max(MedianAbsRelDiff_NHole) ge max(MedianAbsRelDiff_SHole) then maxx2 = MedianAbsRelDiff_NHole
  if max(MedianAbsRelDiff_SHole) ge max(MedianAbsRelDiff_Streamer) and max(MedianAbsRelDiff_SHole) ge max(MedianAbsRelDiff_NHole) then maxx2 = MedianAbsRelDiff_SHole
  
  
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
