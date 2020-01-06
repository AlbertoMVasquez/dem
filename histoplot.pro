pro histoplot,data1,data2=data2,min=min,max=max,label1=label1,label2=label2,label3=label3,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename,win=win,data3=data3,xsize=xsize
;Histograma convencional, se le da vectores de entrada en data1 y/o data2
  if not keyword_set(nbins) then nbins = 50
  if not keyword_set(ytit)  then ytit = 'Freq. Histogram' 
  if n_elements(min) eq 0   then min = min(data1)
  if not keyword_set(max)   then max = max(data1)
  if not keyword_set(xtit)  then xtit = ''
  if not keyword_set( tit)  then  tit = ''
  if not keyword_set(label1)then label1 = 'data1'
  if not keyword_set(label2)then label2 = ''
  if not keyword_set(label3)then label3 = ''
  if not keyword_set(win)   then win = 0
  if not keyword_set(xsize) then xsize=9 
  avg1        =   mean(data1) 
  med1        = median(data1,/even)
  stdev1 =  stdev(data1)/abs(avg1)
  cant1       = long(n_elements(data1))

  if keyword_set(data2) then begin
     avg2        =   mean(data2)
     med2        = median(data2,/even)
     stdev2 =  stdev(data2)/abs(avg2)
     cant2       = long(n_elements(data2))
     if not keyword_set(min)   then min = min([data1,data2])
     if not keyword_set(max)   then max = max([data1,data2])
     f2 = histogram(data2,min=min,max=max,nbins=nbins,locations=vbin2) & f2 = float(f2) / float(total(f2))
  endif
;f1 se define aca por si min y max cambian segun data2 exista o no. 
  f1 = histogram(data1,min=min,max=max,nbins=nbins,locations=vbin1) & f1 = float(f1) / float(total(f1))

     if keyword_set(data3) then begin
        f3 = histogram(data3,min=min,max=max,nbins=nbins,locations=vbin3) & f3 = float(f3) / float(total(f3))
        med3        = median(data3,/even)
     endif


aspect_ratio=0.75
ysize=xsize*aspect_ratio
  
;se activa si quiero guardar algo, sino solo muestra en pantalla
  if keyword_set(filename) then begin
     ps1,'./newfigs/'+filename+'.eps',0
;     device,/inches,xsize=12,ysize=8;5
device,/inches,xsize=xsize, ysize=ysize, /helvetica,SCALE_FACTOR=1

  endif
  if not keyword_set(filename) then window,win
  loadct,12
  azul = 100
  rojo = 200
;  rojo = 170
  verde= 20
  negro =0
  cyan = 80
  violeta = 110
  !P.CHARTHICK=6
  !p.charsize=2.5
  thick=3

     if not keyword_set(data2) then  plot,vbin1,f1,psym=10,xtitle=xtit,ytitle=ytit,title=tit,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,xstyle=1
     if keyword_set(data2) and not keyword_set(data3) then begin
        if max(abs(f1)) gt max(abs(f2)) then  plot,vbin1,f1,xtitle=xtit,ytitle=ytit,title=tit,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,xstyle=1
        if max(abs(f2)) gt max(abs(f1)) then  plot,vbin2,f2,xtitle=xtit,ytitle=ytit,title=tit,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,xstyle=1
     endif

;stop
     if keyword_set(data3) then begin
        if max(abs(f3)) gt max(abs(f1)) and max(abs(f3)) gt max(abs(f2)) then  begin
           plot,vbin3,f3,xtitle=xtit,ytitle=ytit,title=tit,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,xstyle=1
        endif else begin
           if max(abs(f1)) gt max(abs(f2)) then  plot,vbin1,f1,xtitle=xtit,ytitle=ytit,title=tit,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,xstyle=1
           if max(abs(f2)) gt max(abs(f1)) then  plot,vbin2,f2,xtitle=xtit,ytitle=ytit,title=tit,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,xstyle=1
        endelse
     endif
     oplot,vbin1,f1,thick=5,color=azul,psym=10
     if keyword_set(data2) then oplot,vbin2,f2,thick=5,color=rojo,psym=10

     if keyword_set(data3) then begin
        oplot,vbin3,f3,thick=5,color=verde,psym=10
     endif
     
;outputs

  if not keyword_set(data2) then begin
     ;xyouts,0.8*[1,1,1,1],1-[0.18,0.25,0.32,0.38],['m='+strmid(string(med1),4,6),'!9m!3='+strmid(string(avg1),4,6),'!9s!3/!9m!3='+strmid(string(stdev1),4,6),'N='+strmid(string(cant1),6,7)],/normal,charthick=1,Font=0,charsize=2.2
     xyouts,0.8*[1,1,1],1-[0.18,0.25,0.32],['m='+strmid(string(med1),4,6),'!9m!3='+strmid(string(avg1),4,6),'!9s!3/!9m!3='+strmid(string(stdev1),4,6)],/normal,charthick=2;,Font=0,charsize=2.2
     xyouts,0.8*[1],1.-[0.1],[label1],/normal,color=[azul],charthick=2
  endif

  if keyword_set(data2) and not keyword_set(data3) then begin
     ;xyouts,0.78*[1,1,1,1],0.9-[0.18,0.25,0.32,0.38],['m='+strmid(string(med1),4,6)+' & '+strmid(string(med2),4,6),'!9m!3='+strmid(string(avg1),4,6)+' & '+strmid(string(avg2),4,6),'!9s!3/!9m!3='+strmid(string(stdev1),4,6)+' & '+strmid(string(stdev2),4,6),'N='+strmid(string(cant1),6,7)+' & '+strmid(string(cant2),6,7)],/normal,charthick=3,Font=0,charsize=1.2
     ;xyouts,0.7*[1],0.9-[0.18],['m='+strmid(string(med1),4,6)+' & '+strmid(string(med2),4,6)],/normal,charthick=2.5,Font=0,charsize=2
     ;xyouts,[.72,.83],.9-[0.1,0.1],[label1,label2],/normal,color=[azul,rojo],charthick=3,Font=0,charsize=2.5

;     xyouts,[.69,.66],.9-[0.1,0.2],[label1,label2],/normal,color=[azul,rojo],charthick=3,Font=0,charsize=2.5
;     xyouts,[.66,.66],.9-[0.1,0.2],[label1,label2],/normal,color=[azul,rojo],charthick=3,Font=0,charsize=2.5
if med1 le 0.4 then xyouts,0.7 *[1,1],0.9-[0.1,0.2],['m='+strmid(string(med1),4,6),'m='+strmid(string(med2),4,6)],/normal,charthick=2,color=[azul,rojo]
if med1 gt 0.4 then xyouts,0.74*[1,1],0.9-[0.1,0.2],['m='+strmid(string(med1),6,4),'m='+strmid(string(med2),6,4)],color=[azul,rojo],/normal,charthick=2.

  endif

  if keyword_set(data3) then begin ;esto funciona para los graficos energeticos del paper
;     Letter_phi = "146B
;     label1 = '!9' + String(Letter_phi) + '!X'+'!Dr!N'
;     label2 = '!9' + String(Letter_phi) + '!X'+'!Dh!N'
;     label3 = '!9' + String(Letter_phi) + '!X'+'!Dc!N'

     vec_aux =[med1,med3,med2]
     aux1=0
     for i= 0,2 do begin
        aux_aux = aux1
        if vec_aux(i) ge 1. then aux1 = strmid(string(vec_aux(i)),5,5)
        if vec_aux(i) le 1. and vec_aux(i) gt 0 then  aux1 = strmid(string(vec_aux(i)),4,5)
        if vec_aux(i) le 0. then aux1 = strmid(string(vec_aux(i)),4,5)

        if i gt 0 then aux1=[aux_aux,aux1]
     endfor

     xyouts,0.71*[1,1,1],0.9-[0.1,0.2,0.3],['m='+aux1(0),'m='+aux1(1),'m='+aux1(2)],/normal,charthick=2.5    
;     xyouts,0.71*[1,1,1],0.9-[0.1,0.2,0.3],['m='+strmid(string(med1),4,6),'m='+strmid(string(med3),4,5),'m='+strmid(string(med2),4,5)],/normal,charthick=2;.,Font=0,charsize=2.5
     xyouts,0.66*[1,1,1],.9-[0.1,0.2,0.3],[label1,label3,label2],/normal,color=[azul,verde,rojo],charthick=2.5;,Font=0,charsize=2.5
  endif
  
  if keyword_set(filename) then ps2
  !p.multi = 0
  return
end
