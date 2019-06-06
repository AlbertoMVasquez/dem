pro histoplot,data1,data2=data2,min=min,max=max,label1=label1,label2=label2,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename
;Histograma convencional, se le da vectores de entrada en data1 y/o data2
  if not keyword_set(nbins) then nbins = 50
  if not keyword_set(ytit)  then ytit = 'Freq. Histogram' 
  if n_elements(min) eq 0   then min = min(data1)
  if not keyword_set(max)   then max = max(data1)
  if not keyword_set(xtit)  then xtit = ''
  if not keyword_set( tit)  then  tit = ''
  if not keyword_set(label1)then label1 = 'data1'

  
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
     if not keyword_set(label2)then label2 = 'data2'
     f2 = histogram(data1,min=min,max=max,nbins=nbins,locations=vbin2) & f2 = float(f2) / float(total(f2))
  endif
;f1 se define aca por si min y max cambian segun data2 exista o no. 
  f1 = histogram(data1,min=min,max=max,nbins=nbins,locations=vbin1) & f1 = float(f1) / float(total(f1))

;se activa si quiero guardar algo, sino solo muestra en pantalla
  if keyword_set(filename) then begin
     ps1,'./newfigs/'+filename+sufijo+'.eps',0
     device,/inches,xsize=12,ysize=5
  endif
     loadct,12

     azul = 100
     rojo = 200


  plot,vbin1,f1,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,thick=5,charthick=2,Font=0,color=azul
  if keyword_set(data2) then oplot,vbin2,f2,psym=10,th=5,color=rojo

;outputs
  if not keyword_set(data2) then begin
     xyouts,0.8*[1,1,1,1],1-[0.18,0.25,0.32,0.38],['m='+strmid(string(med1),4,6),'!9m!3='+strmid(string(avg1),4,6),'!9s!3/!9m!3='+strmid(string(stdev1),4,6),'N='+strmid(string(cant1),6,7)],/normal,charthick=1,Font=0,charsize=2.2
     xyouts,0.8*[1],1.-[0.1],[label1],/normal,color=[azul],charthick=3
  endif

  if keyword_set(data2) then begin
     xyouts,0.8*[1,1,1,1],1-[0.18,0.25,0.32,0.38],['m='+strmid(string(med1),4,6)+'&'+strmid(string(med2),4,6),'!9m!3='+strmid(string(avg1),4,6)+'&'+strmid(string(avg2),4,6),'!9s!3/!9m!3='+strmid(string(stdev1),4,6)+'&'+strmid(string(stdev2),4,6),'N='+strmid(string(cant1),6,7)+'&'+strmid(string(cant2),6,7)],/normal,charthick=1,Font=0,charsize=2.2
     xyouts,0.8*[1,1.05],1.-[0.1,0.1],[label1,label2],/normal,color=[azul,rojo],charthick=3
  endif

  if keyword_set(filename) then ps2
  !p.multi = 0
  return
end
