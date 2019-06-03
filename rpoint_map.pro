pro roint_map,data1,vec_color,rlon,rlat,data2=data2,data3=data3,data4=data4,data5=data5,data6=data6,data7=data7,data8=data8,data9=data9,data5=data5,filelabel=filelabel,title=title,box=box,thick=thick
;data1,,,data9 son vectores con el where que seleccione lo que se
;quiere plotear
;vector_color es un vector con numeros 0:4 que indica el color a utilizar  
;rlon y rlat podrian ser footlat y footlon o bien rmidpoints
  if not keyword_set(title) then title = 'Physical location of loop at R=1.025'
  if not keyword_set(box) then box = [0.,360.,-90.,+90.]
  if not keyword_set(thick) then thick = 3
  if vec_color(0) le 0. then vec_color = [0,1,1,2,2,3,3,4,4]
;el vector va desde el streamer ecuatoria hacia afuera  
  !P.CHARTHICK=6
  !p.charsize=2.5


  N=25
  A = FINDGEN(N) * (!PI*2/float(N-1))
  f=15.
  USERSYM, COS(A)/f, SIN(A)/f,/FILL
  cant_elementos = n_elements(vec_color)

  ps1,'./newfigs/'+filelabel+'_Rpoint-map.eps',0
  DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1
  plot,footlon,footlat,xr=[box[0],box[1]],yr=[box[2],box[3]],psym=8,$
       title=title,xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
  loadct,39
  
  SWITCH cant_elementos OF
     9: oplot,rlon(data9),rlat(data9),color=fun(color(8))
     8: oplot,rlon(data8),rlat(data8),color=fun(color(7))
     7: oplot,rlon(data7),rlat(data7),color=fun(color(6))
     6: oplot,rlon(data6),rlat(data6),color=fun(color(5))
     5: oplot,rlon(data5),rlat(data5),color=fun(color(4))
     4: oplot,rlon(data4),rlat(data4),color=fun(color(3))
     3: oplot,rlon(data3),rlat(data3),color=fun(color(2))
     2: oplot,rlon(data2),rlat(data2),color=fun(color(1))
     1: begin
        oplot,footlon(data1),footlat(data1),color=fun(color(0))
        break
        end
     ELSE: BEGIN
         PRINT, 'algo no va bien'
         END
   ENDSWITCH

  ps2
  !p.multi = 0
  !P.CHARTHICK=0

  return
end

function fun,x
;al haber seleccionado los valores de colores para saca area segun el
;paper entonces los valores 0,1,2,3,4 se reflejan en los valores
;correspondientes al colo table 39
  case x of
     0: y = 200
     1: y = 28
     2: y = 245
     3: y = 150
     4: y = 90
  return,y
end
