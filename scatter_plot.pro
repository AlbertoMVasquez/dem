

pro scatter_plot,x,y,data_aux=data_aux,filename=filename,pearson=pearson,table=table,title=title,xtitle=xtitle,ytitle=ytitle,thick=thick,$
                 min_color=min_color,max_color=max_color,min_y=min_y,max_y=max_y,inverse=inverse,win=win
;la idea es tener un codigo que haga scater plot y que tenga como entrada
;un vector que pueda darle distintos colores segun lat o long o lo que
;sea. Tambien funciona sin darle ese vector adicional.
;reverse invierte los colores y sirve para distinguir minimos
  if n_elements(x) ne n_elements(y) then stop
  if keyword_set(data_aux) then begin
     if n_elements(data_aux) ne n_elements(x) then stop
  endif

  if not keyword_set(win) then win=2
  
  ;seteando valores maximos y minimos de y
  if keyword_set(max_y) then begin
     ok_y = where(y le max_y)
     y=y(ok_y)
     x=x(ok_y)
     data_aux=data_aux(ok_y)
  endif
  if keyword_set(min_y) then begin
     ok_y = where(y ge min_y)
     y=y(ok_y)
     x=x(ok_y)
     data_aux=data_aux(ok_y)
  endif

  
  device, retain     = 2
  device, true_color = 24
  device, decomposed = 0
  
  !P.CHARTHICK=6
  !p.charsize=3.

  TVLCT, 255, 255, 255, 254 
  TVLCT, 0, 0, 0, 253
  !P.Color = 253
  !P.Background = 254
  
  if not keyword_set(table) then table=39 ;color table
;  if keyword_set(filename) then begin
;     ps1,'./newfigs/'+filename+'_scater_plot.eps',0
;     DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1,/helvetica
;  endif
  if not keyword_set(title)  then  title= ''
  if not keyword_set(xtitle) then xtitle= 'Longitude [deg]'
  if not keyword_set(ytitle) then ytitle= 'Latitude [deg]'
  if keyword_set(pearson) then coef = correlate(x,y)
  if not keyword_set(thick) then thick = 3
  if not keyword_set(min_color) then min_color = 0
  if not keyword_set(max_color) then max_color = 255
  
  N=25
  A = FINDGEN(N) * (!PI*2/float(N-1))
  f=0.4*2
  USERSYM, f*COS(A), f*SIN(A),/FILL

  if keyword_set(data_aux) then  begin
     if not keyword_set(inverse) then color_list,min_color,max_color,   data_aux,vec_color 
     if     keyword_set(inverse) then color_list,min_color,max_color,-1*data_aux,vec_color 
  endif
   
  if keyword_set(data_aux) then  begin

   TVLCT, 255, 255, 255, 254 ; White color
   TVLCT, 0, 0, 0, 253       ; Black color
   !P.Color = 253
   !P.Background = 254
     
     x0=80
     y0=60
     DX=x0+90
     DY=y0+40
     
     window,win,xs=900+DX,ys=500+DY
;     loadct,27
;     tvscl,fltarr(2*900,2*500);pone window blanca
     x_nodata = x
     y_nodata = y
     ;asegura que al setear min_y tmb se fijen los valores del eje y
     if keyword_set(min_y) then begin
        x_nodata = [0,x_nodata]
        y_nodata = [min_y,y] 
     endif
     if keyword_set(max_y) then begin
        x_nodata = [0,x_nodata]
        y_nodata = [max_y,y] 
     endif
        plot,x_nodata,y_nodata,psym=8,title=title,xtitle=xtitle,ytitle=ytitle,xthick=thick,ythick=thick,xstyle=1,ystyle=1,$
          yticklen=.02,xticklen=.03,charthick=2,font=1,pos=[0.11,0.2,0.85,0.85],/nodata

     for j=0,n_elements(data_aux)-1 do begin
        loadct,table
        oplot,[x(j),0],[y(j),0],color=vec_color(j),th=2,psym=8
     endfor
     if keyword_set(pearson) then xyouts,.7-[.2],0.85-[.05],'Pearson='+strmid( string(coef),4,5),/normal,charthick=2
     
      nsy = 400
      nsx = 50

      frame=1
      black = fltarr(Nsx+frame,Nsy+frame)
      xs0 = 1000
      ys0 = 115
      loadct,0
      tvscl,black,xs0-frame/2,ys0-frame/2

      scale       = fltarr(nsx,nsy)
      scale_color = fltarr(nsx,nsy)
;      mini=0
;      maxi=255
      for ix=0,nsx-1 do scale(ix,*)      = min(data_aux)+(max(data_aux)-min(data_aux))*findgen(nsy)/float(nsy-1) ;mini+(maxi-mini)*findgen(nsy)/float(nsy-1)
      if not keyword_set(inverse) then for ix=0,nsx-1 do scale_color(ix,*)= min_color+(max_color-min_color)*findgen(nsy)/float(nsy-1)
      if     keyword_set(inverse) then for ix=0,nsx-1 do scale_color(ix,*)= reverse( min_color+(max_color-min_color)*findgen(nsy)/float(nsy-1) )
;stop
      loadct,table
      scale_color(0,0)=0
      scale_color(0,1)=255
      tvscl,scale_color,xs0,ys0

      loadct,0
      contour,scale,findgen(nsx),reform(scale(0,*)),$
              pos=[xs0,ys0,xs0+nsx,ys0+nsy],/device,color=0,/noerase,$
              yticklen=.2,/nodata,ythick=2,xthick=2,charthick=3,$
              xstyle=5,ystyle=1,charsize=3,font=1

      if keyword_set(filename) then begin
         ;str='map_'+file+'_'+height_string+'_Rsun'
         ;record_gif,'../newfigs/',(STRJOIN(STRSPLIT(str, /EXTRACT,'.'), ''))+'.gif','X'
         record_gif,'newfigs/','scatter_plot'+filename+'.gif','X'
      endif
      
   endif



  if not keyword_set(data_aux) then begin
;scatter comun y silvestre estalón
     loadct,table
     oplot,x,y,color=150,th=2,psym=8
  endif

;colorbar

  
;  if keyword_set(filename) then ps2
  !p.multi = 0
  !P.CHARTHICK=0
;  !P.Color = 240 ;?
;  !P.Background = 0
  
  return
end


pro color_list,c0,c1,data,salida
;c0 va a ser el valor minimo de la escala de colores
;c1 va a ser el valor maximo de la escala de colores
;ambos pueden tomar valores de 0 a 255 con c0<c1  .

;data es un vector, c0 y c1 son constantes a determinar
;salida es un vector ENTERO que en principio va entre 0 y 255
;pero la idea de c1 y c2 es poder acotarlo de ser necesario

;esta salida se pretende utilizar para dar vector de colores
;en los ploteos.  
  salida = round( c0 + (c1 - c0) * ( (data -min(data)) / (max(data) - min(data)) ) )
  return
END



  
