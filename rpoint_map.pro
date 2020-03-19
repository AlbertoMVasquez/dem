pro rpoint_map,data1,rlon,rlat,vec_color=vec_color,data2=data2,data3=data3,data4=data4,data5=data5,data6=data6,data7=data7,data8=data8,data9=data9,filename=filename,title=title,box=box,thick=thick,win=win,gris=gris
  common colores,colgris
  if not keyword_set(gris) then colgris =0
  if keyword_set(gris) then colgris =1
;data1,,,data9 son vectores con el where que seleccione lo que se
;quiere plotear
;vector_color es un vector con numeros 0:4 que indica el color a utilizar  
;rlon y rlat podrian ser footlat y footlon o bien rmidpoints
  if not keyword_set(title) then title = 'Physical location of legs at R=1.025'
  if not keyword_set(box) then box = [0.,360.,-90.,+90.]
  if not keyword_set(thick) then thick = 3
  if n_elements(vec_color) eq 0. then vec_color = [0,1,1,2,2,3,3,4,4]
  if not keyword_set(win) then win = 0
;el vector va desde el streamer ecuatoria hacia afuera  
  !P.CHARTHICK=6
  !p.charsize=2.5


  N=25
  A = FINDGEN(N) * (!PI*2/float(N-1))
  f=0.4
  USERSYM, f*COS(A), f*SIN(A),/FILL
  cant_elementos = n_elements(vec_color)

  if keyword_set(filename) then begin
     ps1,'./newfigs/'+filename+'_Rpoint-map.eps',0
     DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1,/helvetica
  endif
  if not keyword_set(filename) then window,win
  loadct,39 ;es necesario para que ponga los titulos en negro
  plot,rlon,rlat,xr=[box[0],box[1]],yr=[box[2],box[3]],psym=8,$
       title=title,xtitle='Longitude [deg]',ytitle='Latitude [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
;  loadct,39
;  loadct,12
  loadct,33

  if keyword_set(gris) then loadct,0
  SWITCH cant_elementos OF
     9: oplot,rlon(data9),rlat(data9),color=fun(vec_color(8)),th=2,psym=8
     8: oplot,rlon(data8),rlat(data8),color=fun(vec_color(7)),th=2,psym=8
     7: oplot,rlon(data7),rlat(data7),color=fun(vec_color(6)),th=2,psym=8
     6: oplot,rlon(data6),rlat(data6),color=fun(vec_color(5)),th=2,psym=8
     5: oplot,rlon(data5),rlat(data5),color=fun(vec_color(4)),th=2,psym=8
     4: oplot,rlon(data4),rlat(data4),color=fun(vec_color(3)),th=2,psym=8
     3: oplot,rlon(data3),rlat(data3),color=fun(vec_color(2)),th=2,psym=8
     2: oplot,rlon(data2),rlat(data2),color=fun(vec_color(1)),th=2,psym=8
     1: begin
        color1 = fun(vec_color(0))
        oplot,rlon(data1),rlat(data1),color=color1,th=2,psym=8
        break
        end
     ELSE: BEGIN
         PRINT, 'algo no va bien'
         END
   ENDSWITCH

  if keyword_set(filename) then ps2
  !p.multi = 0
  !P.CHARTHICK=0

  return
end

FUNCTION FUN,x
common colores,colgris
;al haber seleccionado los valores de colores para saca area segun el
;paper entonces los valores 0,1,2,3,4 se reflejan en los valores
;correspondientes al colo table 39

if colgris eq 0 then begin
   case x of
;     0: y = 80
;     1: y = 245
;     2: y = 150
;     3: y = 110
;     4: y = 90   
      0: y = 50 ;100                ;azul
      1: y = 190;200                ;rojo
      2: y = 120                ;violeta
      3: y =  20                ;verde
      4: y =  90                ;cyan
      5: y = 220;130                ;fuccia

;      blue=fix(256/3.3)
;      dblue=10
;      red=200
;      dred=250
   endcase
endif

if  colgris eq 1 then begin
   case x of
      0: y = 0                  ;negro
      1: y = 150                ;gris 
   endcase
endif
   return,y
END
