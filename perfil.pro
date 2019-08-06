pro perfil,vec0,vec1,vec2=vec2,label1=label1,label2=label2,filename=filename,sufijo=sufijo,xtit=xtit,ytit=ytit,tit=tit
  if not keyword_set(label1) then label1 = 'data1'
  if not keyword_set(label2) then label2 = 'data2'
  if not keyword_set(sufijo) then sufijo = 'perfil_'
  if not keyword_set(tit) then tit = 'Radial profile'
  if not keyword_set(xtit) then xtit = 'solar radii'
  if keyword_set(filename) then  begin
     ps1,'./'+sufijo+filename+'.eps',0
     device,/inches,xsize=12,ysize=5
  endif
  loadct,12
  azul = 100
  rojo = 20
  plot,vec0,vec1,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,thick=5,charthick=2,Font=0,/nodata
  oplot,vec0,vec1,color=azul
  oplot,vec0,vec2,psym=4,thick=5,color=rojo
  xyouts,0.8*[1,1],1.-[0.15,0.2],[label1,label2],/normal,color=[azul,rojo],charthick=3
  if keyword_set(filename) then ps2
  return  
end

