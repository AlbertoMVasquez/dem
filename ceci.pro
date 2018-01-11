
r2Tcuadrcrit = 0.9

i = where(gradT ne -555. AND opclstat eq 2. and r2Tcuadr gt r2Tcuadrcrit)
filter,i

footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='small_'+strmid(string(r2Tcuadrcrit),5,3)+'footpoints',indexloop_c,rotacion
;=============

xmin=0.
xmax=100000
nbins=100
ps1,'./newfigs/phir_histo'+strmid(string(r2Tcuadrcrit),5,3)+'.eps',0
f = histogram(Phir_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f = f / total(f)
plot,xrat,f,psym=10,charsize=2,th=2,xtitle='Phir',ytitle='Frequency Histogram',title='Phir_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Phir_c)
stdev_frac = stdev(Phir_c)/avg
xyouts,0.7*[1,1],1-[0.2,0.3],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal
ps2

xmin=-100000
xmax=100000
nbins=100
ps1,'./newfigs/Fcb_histo'+strmid(string(r2Tcuadrcrit),5,3)+'.eps',0
f = histogram(Fcb_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f = f / total(f)
plot,xrat,f,psym=10,charsize=2,th=2,xtitle='Fcb',ytitle='Frequency Histogram',title='Fcb_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Fcb_c)
stdev_frac = stdev(Fcb_c)/avg
xyouts,0.7*[1,1],1-[0.2,0.3],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal
ps2

xmin=0.
xmax=2.
nbins=100
ps1,'./newfigs/Loop_length_histo'+strmid(string(r2Tcuadrcrit),5,3)+'.eps',0
f = histogram(Loop_length_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f = f / total(f)
plot,xrat,f,psym=10,charsize=2,th=2,xtitle='Loop_length',ytitle='Frequency Histogram',title='Loop_length_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Fcb_c)
stdev_frac = stdev(Fcb_c)/avg
xyouts,0.7*[1,1],1-[0.2,0.3],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal
ps2

;===================================================================

box=[0.,-90.,360.,+90.]
!P.CHARTHICK=6
!p.charsize=2
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200
; Create custom made symbol (psym=8) for scatter plots                                                                                                                 
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=5.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL
;======

iphir1    = where(phir_c lt  40000)
iphir2    = where(phir_c gt  40000 and phir_c lt 70000)
iphir3    = where(phir_c gt  70000)

ps1,'./newfigs/intensidad_phir'+strmid(string(r2Tcuadrcrit),5,3)+'-map.eps',0
  DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1
  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
       title='Phir_r:lt40000_g:[40000,70000]_b:gt70000',xtitle='Lon [deg]',ytitle='Lat [deg]',/nodata,xstyle=1,ystyle=1
loadct,coltb
 if iphir1 (0) ne -1 then oplot,footlon_c(iphir1 ),footlat_c(iphir1 ),color=red   ,th=2,psym=8
 if iphir2 (0) ne -1 then oplot,footlon_c(iphir2 ),footlat_c(iphir2 ),color=green ,th=2,psym=8
 if iphir3 (0) ne -1 then oplot,footlon_c(iphir3 ),footlat_c(iphir3 ),color=blue  ,th=2,psym=8
loadct,0
!p.multi = 0
!P.CHARTHICK=0
ps2
;======
goto,noFcb

iFcb1    = where(Fcb_c lt   -20000)
iFcb2    = where(Fcb_c gt   -20000 and Fcb_c lt 120000)
iFcb3    = where(Fcb_c gt   120000)

ps1,'./newfigs/intensidad_Fcb'+strmid(string(r2Tcuadrcrit),5,3)+'-map.eps',0
  DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1
  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
       title='Fcb_r:lt-20000_g:[-20000,120000]_b:gt120000',xtitle='Lon [deg]',ytitle='Lat [deg]',/nodata,xstyle=1,ystyle=1
loadct,coltb
 if iFcb1 (0) ne -1 then oplot,footlon_c(iFcb1 ),footlat_c(iFcb1 ),color=red   ,th=2,psym=8
 if iFcb2 (0) ne -1 then oplot,footlon_c(iFcb2 ),footlat_c(iFcb2 ),color=green ,th=2,psym=8
 if iFcb3 (0) ne -1 then oplot,footlon_c(iFcb3 ),footlat_c(iFcb3 ),color=blue  ,th=2,psym=8
loadct,0
!p.multi = 0
!P.CHARTHICK=0
ps2
noFcb:
;======

ilength1    = where(loop_length_c lt   0.4)
ilength2    = where(loop_length_c gt   0.4 and loop_length_c lt 0.8)
ilength3    = where(loop_length_c gt   0.8)

ps1,'./newfigs/loop_length'+strmid(string(r2Tcuadrcrit),5,3)+'-map.eps',0
  DEVICE,/INCHES,YSIZE=5,XSIZE=10,SCALE_FACTOR=1
  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
       title='loop_length_r:lt-0.4_g:[0.4,1.1]_b:gt1.1',xtitle='Lon [deg]',ytitle='Lat [deg]',/nodata,xstyle=1,ystyle=1
loadct,coltb
 if ilength1 (0) ne -1 then oplot,footlon_c(ilength1 ),footlat_c(ilength1 ),color=red   ,th=2,psym=8
 if ilength2 (0) ne -1 then oplot,footlon_c(ilength2 ),footlat_c(ilength2 ),color=green ,th=2,psym=8
 if ilength3 (0) ne -1 then oplot,footlon_c(ilength3 ),footlat_c(ilength3 ),color=blue  ,th=2,psym=8
loadct,0
!p.multi = 0
!P.CHARTHICK=0
ps2

;======================================================================================================================
;======================================================================================================================
;======================================================================================================================
;sería mejor verlo con leg_length, no con el largo del loop entero


i=where(gradT ne -555. and opclstat eq 2. and r2Tcuadr gt r2tcuadrcrit and loop_length lt 0.4)
filter,i

ps1,'./newfigs/loop_length_lt0.4_phir_histo'+strmid(string(r2Tcuadrcrit),5,3)+'.eps',0

xmin=0.
xmax=100000
nbins=100
f1 = histogram(Phir_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f1 = f1 / total(f1)

xmin=0.
xmax=2.
nbins=100
f2 = histogram(Loop_length_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f2 = f2 / total(f2)

!p.multi=[0,1,2]
plot,xrat,f1,psym=10,charsize=1,th=2,xtitle='Phir',ytitle='Frequency Histogram',title='Phir_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Phir_c)
stdev_frac = stdev(Phir_c)/avg
xyouts,0.7*[1,1],1-[0.25,0.35],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal

plot,xrat,f2,psym=10,charsize=1,th=2,xtitle='Loop_length',ytitle='Frequency Histogram',title='Loop_length_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Fcb_c)
stdev_frac = stdev(Fcb_c)/avg
xyouts,0.7*[1,1],0.5-[0.25,0.35],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal

ps2
!p.multi=0

i=where(gradT ne -555. and opclstat eq 2. and r2Tcuadr gt r2tcuadrcrit and loop_length gt 0.4 and loop_length lt 0.8)
filter,i

ps1,'./newfigs/loop_length_0.4:0.8_phir_histo'+strmid(string(r2Tcuadrcrit),5,3)+'.eps',0

xmin=0.
xmax=100000
nbins=100
f1 = histogram(Phir_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f1 = f1 / total(f1)

xmin=0.
xmax=2.
nbins=100
f2 = histogram(Loop_length_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f2 = f2 / total(f2)

!p.multi=[0,1,2]
plot,xrat,f1,psym=10,charsize=1,th=2,xtitle='Phir',ytitle='Frequency Histogram',title='Phir_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Phir_c)
stdev_frac = stdev(Phir_c)/avg
xyouts,0.7*[1,1],1-[0.25,0.35],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal

plot,xrat,f2,psym=10,charsize=1,th=2,xtitle='Loop_length',ytitle='Frequency Histogram',title='Loop_length_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Fcb_c)
stdev_frac = stdev(Fcb_c)/avg
xyouts,0.7*[1,1],0.5-[0.25,0.35],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal

ps2
!p.multi=0

i=where(gradT ne -555. and opclstat eq 2. and r2Tcuadr gt r2tcuadrcrit and loop_length gt 0.8)
filter,i

ps1,'./newfigs/loop_length_gt0.8_phir_histo'+strmid(string(r2Tcuadrcrit),5,3)+'.eps',0

xmin=0.
xmax=100000
nbins=100
f1 = histogram(Phir_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f1 = f1 / total(f1)

xmin=0.
xmax=2.
nbins=100
f2 = histogram(Loop_length_c,min=xmin,max=xmax,nbins=nbins,locations=xrat) & f2 = f2 / total(f2)

!p.multi=[0,1,2]
plot,xrat,f1,psym=10,charsize=1,th=2,xtitle='Phir',ytitle='Frequency Histogram',title='Phir_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Phir_c)
stdev_frac = stdev(Phir_c)/avg
xyouts,0.7*[1,1],1-[0.25,0.35],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal

plot,xrat,f2,psym=10,charsize=1,th=2,xtitle='Loop_length',ytitle='Frequency Histogram',title='Loop_length_'+strmid(string(r2Tcuadrcrit),5,3)+'_small_histo'
avg        =  mean(Fcb_c)
stdev_frac = stdev(Fcb_c)/avg
xyouts,0.7*[1,1],0.5-[0.25,0.35],['!4l!3   = '+strmid(string(avg),4,6),'!4r!3/!4l!3 ='+strmid(string(stdev_frac),4,6)],/normal

ps2
!p.multi=0
