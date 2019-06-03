pro xhistoplot,data,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename
;sumarle data2 como se hizo en histoplot2B  
  if not keyword_set(nbins) then nbins = 50
  if not keyword_set(ytit)  then ytit = 'Freq. Histogram' 
  if not keyword_set(min)   then min = min(data)
  if not keyword_set(max)   then max = max(data)
  avg        =   mean(data) 
  med        = median(data,/even)
  stdev_frac =  stdev(data)/abs(avg)
  cant       = long(n_elements(data))
  
  ps1,'./newfigs/'+filename+sufijo+'.eps',0
  device,/inches,xsize=12,ysize=5
  f = histogram(data,min=min,max=max,nbins=nbins,locations=vbin) & float(f) = float(f) / float(total(f))
  plot,vbin,f,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,thick=3,charthick=2,Font=0
  xyouts,0.8*[1,1,1,1],1-[0.18,0.25,0.32,0.38],['m='+strmid(string(med),4,6),'!9m!3='+strmid(string(avg),4,6),'!9s!3/!9m!3='+strmid(string(stdev_frac),4,6),'N='+strmid(string(cant),7,7)],/normal,charthick=1,Font=0,charsize=2.2
  ps2
  !p.multi = 0
  return
end
