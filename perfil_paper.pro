pro perfil_paper,v0,rad,v1=v1,v2=v2,v3=v3,v4=v4,v5=v5,v6=v6,mi=mi,ma=ma,label1=label1,label2=label2,filename=filename,sufijo=sufijo,xtit=xtit,ytit=ytit,tit=tit,win=win,units=units
  if not keyword_set(label1) then label1 = 'DEMT'
  if not keyword_set(label2) then label2 = 'AWSoM'
  if not keyword_set(sufijo) then sufijo = 'perfil_paper'
  if not keyword_set(tit) then tit = 'Radial profile'
  if not keyword_set(xtit) then xtit = 'solar radii'
  if keyword_set(filename) then  begin
     ps1,'./newfigs/'+sufijo+filename+'.eps',0
     device,/inches,xsize=12,ysize=5
  endif
  if not keyword_set(filename) then window=win
  loadct,12
  azul = 100
  rojo = 170
  verde= 20
  negro =0
  if keyword_set (units) then begin
     v00 = v0/units
     v11 = v1/units
     v22 = v2/units
     v33 = v3/units
     v44 = v4/units
     v55 = v5/units
  endif
  
  if not keyword_set(mi) then mi=min([min(v00),min(v11),min(v22),min(v33),min(v44),min(v55)])
  if not keyword_set(ma) then ma=max([max(v00),max(v11),max(v22),max(v33),max(v44),max(v55)])
  vaux=v1
  vaux(0)=mi
  vaux(1)=ma
  vaux = [ma,vaux]
  rad_aux = [1.01,rad]
  plot,rad_aux,vaux,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,thick=5,charthick=2,Font=0,/nodata
  ;cerrados chicos
  oplot,rad,v0,thick=5,color=azul,linestyle=0
  oplot,rad,v1,thick=5,color=azul,linestyle=2
  ;cerrados grandes
  oplot,rad,v2,thick=5,color=rojo,linestyle=0
  oplot,rad,v3,thick=5,color=rojo,linestyle=2
  ;abiertos
  oplot,rad,v4,thick=5,color=verde,linestyle=0
  oplot,rad,v5,thick=5,color=verde,linestyle=2
  xyouts,0.7*[1,1],.97-[0.15,0.22],[label1,label2],/normal,color=[negro,negro],charthick=2.5,font=0
;  xyouts,0.9*[1,1],1.-[0.15,0.2],['----','----'],/normal,color=[negro,negro],charthick=3,font=0
  if keyword_set(filename) then ps2

  return
end


