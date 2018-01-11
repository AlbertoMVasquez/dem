
pro histoplot3,datar,dataf,datah,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename

ps1,'./newfigs/'+filename+'.eps',0

fr = histogram(datar,min=min,max=max,nbins=nbins,locations=vbinr) & fr = fr / total(fr)
fh = histogram(datah,min=min,max=max,nbins=nbins,locations=vbinh) & fh = fh / total(fh)
ff = histogram(dataf,min=min,max=max,nbins=nbins,locations=vbinf) & ff = ff / total(ff)

maxy=max([fr,fh,ff])

 plot,vbinr,fr,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,yr=[0,maxy],xr=[min,max],xstyle=1,/nodata,thick=3,charthick=2
loadct,12

verde= 20
azul=100
rojo=200
oplot,vbinr,fr,psym=10,th=5,color=azul
oplot,vbinh,fh,psym=10,th=5,color=rojo
oplot,vbinf,ff,psym=10,th=5,color=verde
loadct,0

avgr        =  mean(datar)
avgh        =  mean(datah)
avgf        =  mean(dataf)
stdevr      = stdev(datar);/avgr
stdevh      = stdev(datah);/avgh
stdevf      = stdev(dataf);/avgf

xyouts,.95-[.2,.1],0.8*[1,1],['!4l!3','!4r!3'],/normal

loadct,12
negro=0
xyouts,.9-[.25,.2,.1],0.7*[1,1,1],['!4U!3!Dr!N',strmid(string(avgr),4,6),strmid(string(stdevr),4,6)],/normal,color=[azul ,negro,negro],charthick=2
xyouts,.9-[.25,.2,.1],0.6*[1,1,1],['!4U!3!Dc!N',strmid(string(avgf),4,6),strmid(string(stdevf),4,6)],/normal,color=[verde,negro,negro],charthick=2
xyouts,.9-[.25,.2,.1],0.5*[1,1,1],['!4U!3!Dh!N',strmid(string(avgh),4,6),strmid(string(stdevh),4,6)],/normal,color=[rojo ,negro,negro],charthick=2

loadct,0
ps2

return
end


