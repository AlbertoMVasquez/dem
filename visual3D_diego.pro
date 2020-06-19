
;hacer_visualizacion,/create
pro visual3D_diego,output_gif,$
                   lat0=lat0,lon0=lon0,image_max=image_max,$
                   npx=npx,altura=altura,zbuff=zbuff
  common structure,sph_data


if     keyword_set(zbuff) then SET_PLOT,'Z'
if     keyword_set(zbuff) then dev=     'Z'

if not keyword_set(npx) then npx = 1024 ; numero de pixels de la ventana del gráfico
npy=npx
Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[npx,npy]
;  stop
pfss_data=sph_data

;-----------------------------------------------------------
; Seleccionar un INDICE de altura (0 es 1 Rsun)
  irad       = 0
; Hacer que la imagen de BASE de la visualizacion 3D sea B_r(irad)
  base_image = (*pfss_data.br)(*,*,irad)
; Impose min and max
  if NOT keyword_set(image_max) then image_max=30
  impose_min_max,image_max,base_image
; Hacer que RADIUS sea el radio de la grilla en el indice irad
  radius     = (*pfss_data.rix)(irad)
; Crear imagen base
  image_data=spherical_image_create(base_image,*pfss_data.lon,*pfss_data.lat,radius)
;--------------------------------------------------------------------------------

  if not keyword_set(npx) then npx = 1000 ; numero de pixels de la ventana del gráfico
  npy=npx


; PARA IR HACIENDO VISTAS 3D DESDE DISTINTOS ANGULOS DE VISION CAMBIAR
; LO DE ABAJO. NO RE-HACER LO DE ARRIBA, YA QUE ESTA TODO EN MEMORIA.

  if NOT keyword_set(lat0) then lat0=0 ; Latitud  centro del disco
  if NOT keyword_set(lon0) then lon0=90 ; Longitud centro del disco


; Crear imagen
;Este codigo va a dibujar todas las lineas, entonces vamos a recortar
;seleccionando las lineas que salen de starting point a una sola altura
  
  if not keyword_set(altura) then altura=1.105
  new_pfss_data=pfss_data
  ind = where(*pfss_data.str eq altura);elijo todo lo que sale de esta altura de starting point
  *new_pfss_data.str  = (*pfss_data.str)(ind)
  *new_pfss_data.stth = (*pfss_data.stth)(ind)
  *new_pfss_data.stph = (*pfss_data.stph)(ind)
  *new_pfss_data.ptr  = (*pfss_data.ptr )(*,ind)
  *new_pfss_data.ptth = (*pfss_data.ptth)(*,ind)
  *new_pfss_data.ptph = (*pfss_data.ptph)(*,ind)
  *new_pfss_data.nstep= (*pfss_data.nstep)(ind)  
;OBS: si aun asi hay muchos puntos, quizas se podria reducir
;quitandopuntos en stth y stph
  spherical_draw_field,new_pfss_data,outim=outim,bcent=lat0,lcent=lon0,$
                       xsize=npx,ysize=npy,im_data=image_data,imsc=image_max,/for_ps ;,width=0.55
;  window,win,xs=npx,ys=npx
  tv,outim,/true
  if not keyword_set(sinlabel) then xyouts,0.02,0.02,'Lat='+strcompress(lat0)+'  Long='+strcompress(lon0),charsize=2,charthick=2.5,color=0 ;,/norm
;  record_gif,'~/Downloads/',output_gif
  nname=strlen(output_gif)
  stop
  record_gif,'/data1/work/dem/',strmid(output_gif,0,nname-0)+'.gif','Z'
  print,'Bola de pelos -> '+strmid(output_gif,0,nname-0)+'.gif'+' generado en -> /data1/work/dem/'

;  record_jpg,'/data1/work/dem/',strmid(output_gif,0,nname-4)+'.jpg'

; using the spherical_trackball_widget.pro from solar soft
;  spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max
  
; using the spherical_trackball_widget.pro in /data1/work/dem/
; you need to compilate before
; spherical_trackball_widget,pfss_data,im_data=image_data,imsc=image_max,/for_ps

  return
end

pro impose_min_max,datamax,data
pmin=(where(data eq min(data)))(0)
pmax=(where(data eq max(data)))(0)
data(pmin)=-datamax
data(pmax)=+datamax
return
end
