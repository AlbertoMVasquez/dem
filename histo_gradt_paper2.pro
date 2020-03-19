pro histo_gradt_paper2,v1=v1,v2=v2,v3=v3,v4=v4,v5=v5,v6=v6,min=min,max=max,label1=label1,label2=label2,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename,win=win,units=units,normalizado=normalizado
;Histograma convencional, se le da vectores de entrada en v1 y/o v2
  if not keyword_set(nbins) then nbins = 50
  if not keyword_set(ytit)  then ytit = 'Freq. Histogram' 
  if n_elements(min) eq 0   then min = min(v1)
  if not keyword_set(max)   then max = max(v1)
  if not keyword_set(xtit)  then xtit = ''
  if not keyword_set( tit)  then  tit = ''
  if not keyword_set(label1)then label1 = 'DEMT'
  if not keyword_set(label2)then label2 = 'AWSoM'
  if not keyword_set(win)   then win = 1
  if not keyword_set(units) then units = 1.e6
;usar /normalizado hara los histos normalizados al numero total
  !P.CHARTHICK=6
  !p.charsize=2.5
  thick=3
  th = 8
  
  if keyword_set(v6) then begin
     if not keyword_set(min)   then min = min([v1,v2,v3,v4,v5,v6])
     if not keyword_set(max)   then max = max([v1,v2,v3,v4,v5,v6])
  endif
  if not keyword_set(v6) then begin
     if not keyword_set(min)   then min = min([v1,v2,v3,v4])
     if not keyword_set(max)   then max = max([v1,v2,v3,v4])
  endif

     f1 = histogram(v1/units,min=min,max=max,nbins=nbins,locations=vbin1) & f1 = float(f1) / float(total(f1))
     f2 = histogram(v2/units,min=min,max=max,nbins=nbins,locations=vbin2) & f2 = float(f2) / float(total(f2))
     f3 = histogram(v3/units,min=min,max=max,nbins=nbins,locations=vbin3) & f3 = float(f3) / float(total(f3))
     f4 = histogram(v4/units,min=min,max=max,nbins=nbins,locations=vbin4) & f4 = float(f4) / float(total(f4))

     if keyword_set(normalizado) then begin
        tot = n_elements(v1) + n_elements(v2) + n_elements(v3) + n_elements(v4)
        f1 = histogram(v1/units,min=min,max=max,nbins=nbins,locations=vbin1) & f1 = float(f1) / float(tot)
        f2 = histogram(v2/units,min=min,max=max,nbins=nbins,locations=vbin2) & f2 = float(f2) / float(tot)
        f3 = histogram(v3/units,min=min,max=max,nbins=nbins,locations=vbin3) & f3 = float(f3) / float(tot)
        f4 = histogram(v4/units,min=min,max=max,nbins=nbins,locations=vbin4) & f4 = float(f4) / float(tot)
     endif

     
if keyword_set(v6) then begin
   f5 = histogram(v5/units,min=min,max=max,nbins=nbins,locations=vbin5) & f5 = float(f5) / float(total(f5))
   f6 = histogram(v6/units,min=min,max=max,nbins=nbins,locations=vbin6) & f6 = float(f6) / float(total(f6))
endif
;se activa si quiero guardar algo, sino solo muestra en pantalla
  if keyword_set(filename) then begin
     ps1,'./newfigs/'+filename+'.eps',0
     device,/inches,xsize=10,ysize=5,/helvetica,SCALE_FACTOR=1
  endif
  if not keyword_set(filename) then window,win
  loadct,12
  azul = 100
  rojo = 200;170                    ;200
  verde= 20
  negro =0
  violeta = 120
  fucsia = 130
  cyan = 90
  if keyword_set(v6) then begin
     min2 = min([f1,f2,f3,f4,f5,f6])
     max2 = max([f1,f2,f3,f4,f5,f6])
  endif
  if not keyword_set(v6) then begin
     min2 = min([f1,f2,f3,f4])
     max2 = max([f1,f2,f3,f4])
  endif

  faux=f1
  faux(0)=min2
  faux(1)=max2
  plot,vbin1,faux,psym=10,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,/nodata,font=0,xthick=thick,ythick=thick,ystyle=1,yTICKINTERVAL=0.01,yr=[0,0.05];yr=[0,0.05]
  if keyword_set(v6) then xyouts,0.7*[1,1],.97-[0.15,0.22],[label1,label2],/normal,color=[negro,negro],charthick=2.5,font=0

  loadct,33
  azul = 50
  rojo = 190
;  verde= 20
;  negro =0
;  violeta = 120
  fucsia = 220
  cyan = 90

  
  if keyword_set(v6) then begin
     oplot,vbin1,f1,psym=10,thick=th,color=azul,linestyle=0
     oplot,vbin2,f2,psym=10,thick=th,color=azul,linestyle=2
     oplot,vbin3,f3,psym=10,thick=th,color=rojo,linestyle=0
     oplot,vbin4,f4,psym=10,thick=th,color=rojo,linestyle=2
     if keyword_set(v5) then oplot,vbin5,f5,psym=10,thick=th,color=verde,linestyle=0
     if keyword_set(v6) then oplot,vbin6,f6,psym=10,thick=th,color=verde,linestyle=2
  endif

  if not keyword_set(v6) then begin
     oplot,vbin1,f1,psym=10,thick=9,color=azul,linestyle=0
     oplot,vbin2,f2,psym=10,thick=9,color=rojo,linestyle=0
     oplot,vbin3,f3,psym=10,thick=9,color=fucsia,linestyle=0
     oplot,vbin4,f4,psym=10,thick=9,color=cyan,linestyle=0
;el orden deberia ser cerrado down, cerrado up, cerrado grande, abierto

  endif

;outputs
;     xyouts,[.71,.68],.9-[0.1,0.2],[label1,label2],/normal,color=[azul,rojo],charthick=3,Font=0,charsize=2.5

;if not keyword_set(v6) then xyouts,0.7*[1,1],.97-[0.15,0.22],[label1,label2],/normal,color=[negro,negro],charthick=2.5,font=0

  if keyword_set(filename) then ps2
  !p.multi = 0
  return
end
