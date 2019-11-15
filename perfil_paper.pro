pro perfil_paper,v0,rad,v1=v1,v2=v2,v3=v3,v4=v4,v5=v5,v6=v6,mi=mi,ma=ma,label1=label1,label2=label2,filename=filename,sufijo=sufijo,xtit=xtit,ytit=ytit,tit=tit,win=win
  if not keyword_set(label1) then label1 = 'data1'
  if not keyword_set(label2) then label2 = 'data2'
  if not keyword_set(sufijo) then sufijo = 'perfil_'
  if not keyword_set(tit) then tit = 'Radial profile'
  if not keyword_set(xtit) then xtit = 'solar radii'
  if keyword_set(filename) then  begin
     ps1,'./'+sufijo+filename+'.eps',0
     device,/inches,xsize=12,ysize=5
  endif
  if not keyword_set(filename) then window=win
  loadct,12
  azul = 100
  rojo = 20
  verde=150
  if not keyword_set(mi) then mi=min[min(v1),min(v2),min(v3),min(v4),min(v5)]
  if not keyword_set(ma) then ma=max[max(v1),max(v2),max(v3),max(v4),max(v5)]
  vaux=v1
  vaux(0)=mi
  vaux(1)=ma
  plot,v0,vaux,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,thick=5,charthick=2,Font=0,/nodata
  ;cerrados chicos
  oplot,rad,v0,thick=5,color=azul,linestyle=0
  oplot,rad,v1,thick=5,color=azul,linestyle=2
  ;cerrados grandes
  oplot,rad,v2,thick=5,color=rojo,linestyle=0
  oplot,rad,v3,thick=5,color=rojo,linestyle=2
  ;abiertos
  oplot,rad,v4,thick=5,color=verde,linestyle=0
  oplot,rad,v5,thick=5,color=verde,linestyle=2
  stop
  xyouts,0.8*[1,1],1.-[0.15,0.2],[label1,label2],/normal,color=[azul,rojo],charthick=3
  if keyword_set(filename) then ps2

stop




  return
end


