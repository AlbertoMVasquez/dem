pro histoplot2,data1,data2,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename,ndig=ndig

!p.charsize=2.5

ps1,'./newfigs/'+filename+'.eps',0

DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1
DEVICE,/TIMES, FONT_INDEX=4

f1 = histogram(data1,min=min,max=max,nbins=nbins,locations=vbin1) & f1 = f1 / total(f1)
f2 = histogram(data2,min=min,max=max,nbins=nbins,locations=vbin2) & f2 = f2 / total(f2)

maxy=max([f1,f2])

 plot,vbin1,f1,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,yr=[0,maxy],xr=[min,max],xstyle=1,/nodata,xthick=5,ythick=5,th=5,Font=0

loadct,12

verde= 20

azul =100
rojo =200
negro=  0
oplot,vbin1,f1,psym=10,th=5,color=rojo
oplot,vbin2,f2,psym=10,th=5,color=azul
loadct,0

avg1        =  mean(data1)
avg2        =  mean(data2)
stdev1      = stdev(data1)/avg1
stdev2      = stdev(data2)/avg2

xyouts,.92-[.18,.1],0.8*[1,1],['!9m!3','!9s!3/!9m!3'],/normal,Font=0

loadct,12

xyouts,.9-[.22,.1],0.7*[1,1],[strmid(string(avg1),4,ndig),strmid(string(stdev1),4,5)],/normal,color=[rojo,rojo],charthick=2,Font=0
xyouts,.9-[.22,.1],0.6*[1,1],[strmid(string(avg2),4,ndig),strmid(string(stdev2),4,5)],/normal,color=[azul,azul],charthick=2,Font=0

xyouts,0.2*[1,1],[0.7,0.6],['1996','2009'],/normal,color=[rojo,azul],charthick=2,Font=0

loadct,0
ps2

return
end


