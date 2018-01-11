;Este archivo de codigo realiza comparaciones de 2 o 3 fbes. Utilizado
;para ver diferencias entre mitados de hollow (hollow 1 y hollow2) y
;las disk y full hollow. Hace mapas de carrington, histogramas y
;plotea diferencias en latitudes fijas ademas de hacer los showshell
;de las fbes.


pro wrapper,irad1,irad2,cr1915=cr1915,cr2081=cr2081

if keyword_set(cr2081) then begin
goto,skip3     
; Compare TESIS (que es DISK) versus NEW-HOLLOW for CR-2081:
 compare_fbes,'x_euvi.A.171.cr2081.base.G','x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON',171,7,win=0,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.195.cr2081.base.G','x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON',195,7,win=4,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.284.cr2081.base.G','x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON',284,7,win=8,/cr2081,irad1=irad1,irad2=irad2
stop
skip1:
; Compare TESIS HOLLOW y half-HOLLOW1 for CR-2081:
 compare_fbes,'x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON','x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1',171,7,win=0,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON','x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1',195,7,win=4,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON','x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1',284,7,win=8,/cr2081,irad1=irad1,irad2=irad2
stop
skip2:
; Compare TESIS HOLLOW y half-HOLLOW2 for CR-2081:
 compare_fbes,'x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON','x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2',171,7,win=0,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON','x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2',195,7,win=4,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON','x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2',284,7,win=8,/cr2081,irad1=irad1,irad2=irad2
stop
skip3:
; Compare half-HOLLOW1 y half-HOLLOW2 for CR-2081:
 compare_fbes,'x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1','x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2',171,7,win=0,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1','x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2',195,7,win=4,/cr2081,irad1=irad1,irad2=irad2
 compare_fbes,'x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1','x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2',284,7,win=8,/cr2081,irad1=irad1,irad2=irad2
stop

;Compare TESIS HOLLOW  y 1/2*(half-Hollow1 + half-hollow2) for CR-2081
fc='x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2'
compare_fbes,'x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON' ,'x_euvi.A.171.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1' ,171,7,win=0,/cr1915,irad1=irad1,irad2=irad2,fc=fc,/promedio
fc='x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2'
compare_fbes,'x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON' ,'x_euvi.A.195.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1' ,195,7,win=0,/cr1915,irad1=irad1,irad2=irad2,fc=fc,/promedio
fc='x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow2'
compare_fbes,'x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON' ,'x_euvi.A.284.cr2081.26x90_bf4_ri.000_ro1.025_l0.75_NODECON.halfhollow1' ,284,7,win=0,/cr1915,irad1=irad1,irad2=irad2,fc=fc,/promedio


endif

if keyword_set(cr1915) then begin
; Compare NEW-DISK versus NEW-HOLLOW for CR-1915:
 compare_fbes,'x_eit.171.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75_TEST','x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75',171,7,win=0,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.195.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75_TEST','x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75',195,7,win=4,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.284.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75_TEST','x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75',284,7,win=8,/cr1915,irad1=irad1,irad2=irad2

stop
; Compare TESIS HOLLOW y half-HOLLOW1 for CR-1915:
 compare_fbes,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75' ,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1' ,171,7,win=0,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75' ,'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1' ,195,7,win=4,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75' ,'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1' ,284,7,win=8,/cr1915,irad1=irad1,irad2=irad2

stop
; Compare TESIS HOLLOW y half-HOLLOW2 for CR-1915:
 compare_fbes,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75'  ,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2',171,7,win=0,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75'  ,'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2',195,7,win=4,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75'  ,'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2',284,7,win=8,/cr1915,irad1=irad1,irad2=irad2

stop
; Compare half-HOLLOW1 y half-HOLLOW2 for CR-1915:
 compare_fbes,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1', 'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2'  ,171,7,win=0,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1', 'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2'  ,195,7,win=4,/cr1915,irad1=irad1,irad2=irad2
 compare_fbes,'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1', 'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2'  ,284,7,win=8,/cr1915,irad1=irad1,irad2=irad2

stop
 fc='x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2'
 compare_fbes,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75' ,'x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1' ,171,7,win=0,/cr1915,irad1=irad1,irad2=irad2,fc=fc,/promedio
 fc='x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2'
 compare_fbes,'x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75','x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1'  ,195,7,win=0,/cr1915,irad1=irad1,irad2=irad2,fc=fc,/promedio
 fc='x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow2'
 compare_fbes,'x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75','x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75.halfhollow1'  ,284,7,win=0,/cr1915,irad1=irad1,irad2=irad2,fc=fc,/promedio
endif

return
; Compare TESIS versus NEW-DISK for CR-2081:
; compare_fbes,'x_euvi.A.284.cr2081.base.G','x_euvi.A.284.cr2081.26x90_bf4_ri.98_ro1.025_l0.75_NODECON_TEST',284,7


; Compare TESIS versus NEW-HOLLOW for CR-1915:
 compare_fbes,'x_eit.171.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75075.G','x_eit.171.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75',171,7
 compare_fbes,'x_eit.195.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75075.G','x_eit.195.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75',195,7
 compare_fbes,'x_eit.284.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75075.G','x_eit.284.cr1915.26x90_bf4_ri.000_ro1.025_b4_l0.75',284,7
return
end

pro compare_fbes,fa,fb,band,ir,full=full,win=win,cr1915=cr1915,cr2081=cr2081,irad1=irad1,irad2=irad2,fc=fc,promedio=promedio

 device, retain     = 2
 device, true_color = 24
 device, decomposed = 0

 if not keyword_set(win) then win=0

 readtom_sph,'/data1/tomography/bindata/',fa,26,90,1.00,1.26,xa
 readtom_sph,'/data1/tomography/bindata/',fb,26,90,1.00,1.26,xb
 if keyword_set(promedio) then readtom_sph,'/data1/tomography/bindata/',fc,26,90,1.00,1.26,xc
 factor=2
 xsize=180*factor

;if not keyword_set(irad1) then irad1=3
;if not keyword_set(irad2) then irad1=20
if not keyword_set(irad1) then irad1=7
if not keyword_set(irad2) then irad2=7
 
; Mean rad
 meanir=mean([irad1,irad2])

; Spherical shells of FBE at mean rad.
 arra=reform(xa(meanir,*,*))
 arrb=reform(xb(meanir,*,*))
 pa=where(arra gt 0.)
 pb=where(arrb gt 0.)

 if keyword_set(promedio) then begin
    goto,skip5
    arrc=reform(xc(meanir,*,*))
    pc=where(arrc gt 0.)

    minim = min([min(arra(pa)),min(arrb(pb)), min(arrc(pc))])
    maxim = max([max(arra(pa)),max(arrb(pb)), max(arrc(pc))])
    showshell,xa,meanir,win  ,factor,band,minim,maxim,/log
    minim = min([min(arra(pa)),min(arrb(pb)), min(arrc(pc))])
    maxim = max([max(arra(pa)),max(arrb(pb)), max(arrc(pc))])
    showshell,xb,meanir,win+1,factor,band,minim,maxim,/log
    minim = min([min(arra(pa)),min(arrb(pb)), min(arrc(pc))])
    maxim = max([max(arra(pa)),max(arrb(pb)), max(arrc(pc))])
    showshell,xc,meanir,win+2,factor,band,minim,maxim,/log
    xprom=(xc + xb)/2.
    arrprom=reform(xprom(meanir,*,*))
    pprom=where(xc ge 0. and xb ge 0.)
                                ;al ser un promedio el minimo y el
                                ;maximo tiene que estar contenido en
                                ;el dominio de min, max de los
                                ;anteriores
    minim = min([min(arra(pa)),min(arrb(pb)), min(arrc(pc))])
    maxim = max([max(arra(pa)),max(arrb(pb)), max(arrc(pc))])
    showshell,xprom,meanir,win+3,factor,band,minim,maxim,/log
    skip5:
 endif
 
 if not keyword_set(promedio) then begin
    goto,skip1
    minim = min([min(arra(pa)),min(arrb(pb))])
    maxim = max([max(arra(pa)),max(arrb(pb))])
    showshell,xa,meanir,win  ,factor,band,minim,maxim,/log
    minim = min([min(arra(pa)),min(arrb(pb))])
    maxim = max([max(arra(pa)),max(arrb(pb))])
    showshell,xb,meanir,win+1,factor,band,minim,maxim,/log
    skip1:
 ; Equatorial Cut at mean height and histograms gathering all heights 
 ; Select Mainly Streamer
    ilat1=10                    ; -69 deg
    ilat2=79                    ; +69 deg 

    if keyword_set(full) then begin
                                ; Select Full Corona
       ilat1=0
       ilat2=89
    endif

    ilon1=0
    ilon2=179
                                ;Avoid ARs:
    if keyword_set(cr2081) then ilon1=60 ; 120 deg
    if keyword_set(cr1915) then ilon2=90 ; 180 deg
    radv=1.005+.01*findgen(26)
   goto,skip2
    sa=reform(xa(irad1:irad2,ilat1:ilat2,ilon1:ilon2)) & pa=where(sa gt 0.) & sa=sa(pa)
    sb=reform(xb(irad1:irad2,ilat1:ilat2,ilon1:ilon2)) & pb=where(sb gt 0.) & sb=sb(pb)
    
    !p.charsize=1.25
    window,win+4,ys=xsize*1.5,xs=xsize/1.2
    !p.multi=[0,1,2]
    plot,xa(meanir,44,*),xstyle=1,title='Equat. FBE-'+strmid(string(band),5,3)+' at '+strmid(string(radv(meanir)),6,5)
    oplot,xb(meanir,44,*),linestyle=2,th=2
    
    nb=25
    maxi=max([median(sa),median(sb)])
    maxi=maxi*(1.+2.*max([stdev(sa),stdev(sb)])/maxi)
    ha=histogram(sa,nbins=nb,min=0,max=maxi,LOCATIONS=xa2)
    hb=histogram(sb,nbins=nb,min=0,max=maxi,LOCATIONS=xb2)
    maxi=max([max((ha)),max(hb)])  
    plot ,xa2,ha,yr=[0,maxi],title='Hist. of FBE-'+strmid(string(band),5,3)+' at '+strmid(string(radv(meanir)),6,5)
    oplot,xb2,hb,linestyle=2,th=2
    
    print,median(sa),mean(sa)
    print,median(sb),mean(sb)
    print,(median(sa)-median(sb))/median(sb),(mean(sa)-mean(sb))/mean(sb)

    skip2:

;    arra=reform(xa(irad1:irad2,ilat1:ilat2,ilon1:ilon2))
;    arrb=reform(xb(irad1:irad2,ilat1:ilat2,ilon1:ilon2))

    arra=reform(xa(irad1:irad2,*,*))
    arrb=reform(xb(irad1:irad2,*,*))
    
    diff=arra-arrb
    avg =(arra+arrb)/2.
    p=where(arra gt 0. AND arrb gt 0.)
;    absfracdiff=abs(diff)/mean(avg(p)) ;con esto fueron creadas las
;    primeras figuras. la idea era dividir por un valor fijo medio a
;    cada altura. En los mapas de carrington me sirve para ver como se
;    comportan estas diferencias relativas. En particular se observa
;    que son mayyores cerca del streamer y aumentan con la
;    altura. Aunque el error total (sin dividir) es mayor a alturas
;    bajas ya que los valores de las fbes son mas grandes.
    absfracdiff=abs(diff)/avg ;diferencia absoluta en cada pixel
    data = absfracdiff(p)
                                ;estimador de la diferencia promedio
    diff2 = diff(p)
    est = total(abs(diff2));/n_elements(diff2)

    nb=25
    nsig=3.
    maxx=median(data)+nsig*stdev(data)
                                ;minx=median(data)-nsig*stdev(data)
    minx=0.
    hdata=histogram(data,nbins=nb,min=minx,max=maxx,LOCATIONS=La)
    maxy=max(hdata)
    !p.multi=0
    window,win+5,xs=500,ys=300
    plot,La,hdata,xr=[minx,maxx],yr=[0,maxy],title='Hist. of A.F.D. of FBE-'+strmid(string(band),5,3)+' @ ['+strmid(string(radv(irad1)),6,5)+','+strmid(string(radv(irad2)),6,5)+'] Rs',xstyle=1,psym=10
    xyouts,0.2*[1.,1.,1.,1.],1.-[.2,.3,.4,.5],strmid([string(median(data)),string(mean(data)),string(stdev(data)),string(mean(avg(p))) ],4,8),/normal
    xyouts,0.15*[1.,1.,1.,1.],1.-[.2,.3,.4,.5],['med','mean','stdv','<fbe>'],/normal
    xyouts,0.85*[1.,1.,1.],1.-[.2,.3,.4],strmid([string(est),string(n_elements(diff2)),string(est/mean(avg(p)))],5,8),/normal
    xyouts,0.75*[1.,1.,1.],1.-[.2,.3,.4],['diff','n_elem','diff/<fbe>'],/normal
    
;--------------------------------------
    ;mapas de carrington de la diferencia a alturas fijas.     
scalefactor=3
superhigh=1.
superlow=-1.
instrument='euvi'
stop
win=5
ir=7
display_carrmap,xa,xb,ir,win+1,scalefactor,1.,superlow,superhigh,12,'Carrmap diff(fbes) @ '+strmid(string(radv(ir)),6,5)+' Rs - '+strmid(string(band),5,3),instrument
    
;--------------------------------------    
    
    
 endif
  
;stop

 if keyword_set(promedio) then begin

    ilat1=10 ; -69 deg
    ilat2=79

    ilon1=60
    ilon2=179

    if keyword_set(cr2081) then ilon1=60 
    if keyword_set(cr1915) then ilon2=90
    
    sa=reform(xa(irad1:irad2,ilat1:ilat2,ilon1:ilon2)) & pa = where (sa gt 0.) & sa = sa(pa)
    xd= (xc + xb)/2.
    sd=reform(xd(irad1:irad2,ilat1:ilat2,ilon1:ilon2)) & pd = where (sd gt 0.) & sd = sd(pd)
    radv=1.005+.01*findgen(26)
;    sb1=reform(xb(irad1:irad2,ilat1:ilat2,ilon1:ilon2)) & pb=where(sb gt 0.) & sb1=sb1(pb)
;    sc1=reform(xc(irad1:irad2,ilat1:ilat2,ilon1:ilon2)) & pc=where(sc gt 0.) & sc1=sc1(pc)
    goto,skip6
    !p.charsize=1.25
    window,win+4,ys=xsize*1.5,xs=xsize/1.2
    !p.multi=[0,1,2]
    plot ,xa(meanir,44,*),xstyle=1,title='Equat. FBE-'+strmid(string(band),5,3)+' at '+strmid(string(radv(meanir)),6,5)
    oplot,xd(meanir,44,*),linestyle=2,th=2
    ;oplot,xc(meanir,44,*),th=2,SYMBOL='*'
  
    nb=25
    maxi=max([median(sa),median(sd)])
    maxi=maxi*(1.+2.*max([stdev(sa),stdev(sd)])/maxi)
    ha=histogram(sa,nbins=nb,min=0,max=maxi,LOCATIONS=xaa)
    hd=histogram(sd,nbins=nb,min=0,max=maxi,LOCATIONS=xdd)
    
    maxi=max([max(ha),max(hd)])
    plot ,xaa,ha,yr=[0,maxi],title='Hist. of FBE-'+strmid(string(band),5,3)+' at '+strmid(string(radv(meanir)),6,5)
    oplot,xdd,hd,linestyle=2,th=2
    skip6:

                                ;chequear este estadistico para distintas lat y long por separado
    
    arra=reform(xa(irad1:irad2,*,*))
    arrd=reform(xd(irad1:irad2,*,*))

    diff=arra-arrd
    avg =(arra+arrd)/2.
    p=where(arra gt 0. AND arrd gt 0.)
    absfracdiff=abs(diff)/avg
    data = absfracdiff(p)
                                ;estimador de la diferencia promedio
    diff2 = diff(p)
    est = total(abs(diff2)) ;/n_elements(diff2)
    nb=25
    nsig=3.
    maxx=median(data)+nsig*stdev(data)
    minx=0.
    hdata=histogram(data,nbins=nb,min=minx,max=maxx,LOCATIONS=La)
    maxy=max(hdata)
    !p.multi=0
    window,win+5,xs=500,ys=300
    plot,La,hdata,xr=[minx,maxx],yr=[0,maxy],title='Hist. of A.F.D. of FBE-'+strmid(string(band),5,3)+' @ ['+strmid(string(radv(irad1)),6,5)+','+strmid(string(radv(irad2)),6,5)+'] Rs',xstyle=1,psym=10
    xyouts,0.2* [1.,1.,1.,1.],1.-[.2,.3,.4,.5],strmid([string(median(data)),string(mean(data)),string(stdev(data)),string(mean(avg(p))) ],4,8),/normal
    xyouts,0.15*[1.,1.,1.,1.],1.-[.2,.3,.4,.5],['med','mean','stdv','<fbe>'],/normal
    xyouts,0.85*[1.,1.,1.],1.-[.2,.3,.4],strmid([string(est),string(n_elements(diff2)),string(est/mean(avg(p)))],5,8),/normal
    xyouts,0.75*[1.,1.,1.],1.-[.2,.3,.4],['diff','n_elem','diff/<fbe>'],/normal
     ;--------------------------------------
                                ;mapas de carrington de la diferencia
                                ;a alturas fijas.
    scalefactor=3
    superhigh=1.
    superlow=-1.
    instrument='euvi'
    stop
    win=5
    ir=7
    display_carrmap,xa,xd,ir,win+1,scalefactor,1.,superlow,superhigh,12,'Carrmap diff(fbes) @ '+strmid(string(radv(ir)),6,5)+' Rs - '+strmid(string(band),5,3),instrument
    ;--------------------------------------                                                                                                                    
 endif
 

 
return
end

;display_carrmap,R,ir,2*Nband,scalefactor,1.,superlow,superhigh,12,'R @ '+strmid(suffix,0,5)+' Rsun',instrument
pro display_carrmap,xa,xb,ir,win,scalefactor,units,mini,maxi,cltb,titulo,instrument,log=log

  diff3=xa-xb
;  sxa=reform(xa(ir,*,*))
;  sxb=reform(xb(ir,*,*))
;  p=where(sxa ge 0. and sxb ge 0.) ;& prom=(sxa(p)+sxb(p))/2. & valmed=mean(prom)
  
  prom=(xa+xb)/2.
  ;abs3=abs(diff3)/prom          ;valmed
  abs3=diff3/prom
  scalefactor=3
  superhigh=1.
  superlow=0.01*0.
  superlow2=-1.
  p=where(xa lt 0. AND xb lt 0.) & if p(0) ne -1 then abs3(p)=superlow
  p=where(abs3 ge superhigh)     & if p(0) ne -1 then abs3(p)=superhigh
  p=where(abs3 le superlow2 )     & if p(0) ne -1 then abs3(p)=superlow2
  map=abs3
  
;change units
  map=map/units
  dev='X'  
  xtitle_status=1
  ytitle_status=1
  Bfield_stat=0
;----create 2D map, scale up,

  if (size(map))(0) eq 3 then begin
     nr=fix((size(map))(1))
     nt=fix((size(map))(2))
     np=fix((size(map))(3))
     tmp=reform(map(ir,*,*))
  endif
  ThMIN= -90.
  ThMAX= +90.
  PhMIN=   0.
  PhMAX= 360.
  Th=ThMIN+(ThMAX-ThMIN)*FINDGEN(NT)/FLOAT(NT-1)
  Ph=PhMIN+(PhMAX-PhMIN)*FINDGEN(NP)/FLOAT(NP-1)
  map2d=tmp
  nt2=nt*scalefactor
  np2=np*scalefactor
  t2=thmin+(thmax-thmin)*findgen(nt2)/float(nt2-1)
  p2=phmin+(phmax-phmin)*findgen(np2)/float(np2-1)
  map2=fltarr(nt2,np2)
  for ip2=0,np2-1 do begin
     p0=p2(ip2)
     fp=abs(ph-p0)
     ip=fix( (where(fp eq min(fp)))(0) )
     for it2=0,nt2-1 do begin
        t0=t2(it2)
        ft=abs(th-t0)
      it=fix( (where(ft eq min(ft)))(0) )
      map2(it2,ip2)=map2d(it,ip)
   endfor
  endfor
  map_2d_rot_scl=rotate(map2,4)
;-----------------------------------------------------

;---threshold bottom and top---------------------
map_2d_rot_scl(0,0)=mini
map_2d_rot_scl(0,1)=maxi
map_2d_rot_scl=map_2d_rot_scl>mini<maxi
;------------------------------------------------

if keyword_set(log) then begin
   map_2d_rot_scl=alog10(map_2d_rot_scl)
   mini=min(map_2d_rot_scl)
   maxi=max(map_2d_rot_scl)
endif

;---create over-sized window with white background----------------
x0=80
y0=60
DX=x0+90
DY=y0+40
if dev eq 'X' then $
   window,win,xs=np*scalefactor+DX,ys=nt*scalefactor+DY
loadct,27
tvscl,fltarr(2*np*scalefactor+DX,nt*scalefactor+DY)
;-------------------------------------------------------

;----display map-------------------------
if cltb le 40 then loadct,cltb
if cltb gt 40 and instrument ne 'aia' then eit_colors,cltb
if cltb gt 40 and instrument eq 'aia' then AIA_LCT,wave=cltb,/load
tvscl,map_2d_rot_scl,x0,y0


;----put axes,
;titles--------------------------------------------------------
nlon=np*scalefactor
nlat=nt*scalefactor
LAT=ThMIN+(ThMAX-ThMIN)*FINDGEN(Nlat)/FLOAT(Nlat-1)
LON=PhMIN+(PhMAX-PhMIN)*FINDGEN(Nlon)/FLOAT(Nlon-1)
loadct,40
xtitle=''
ytitle=''
if xtitle_status eq 1 then xtitle='Carrington Longitude [deg]'
if ytitle_status eq 1 then ytitle='Latitude [deg]'
contour,map_2d_rot_scl,lon,lat,pos=[X0,Y0,X0+nlon,Y0+nlat],/noerase,/nodata,$
        /device,color=255,xstyle=1,ystyle=1,charsize=2.,$
        xtitle=xtitle,$
        ytitle=ytitle,$
        title=titulo,$
        yticklen=.02,xticklen=.03,ythick=2,xthick=2,charthick=2
;-----------------------------------------------------------------------------

ThMIN= -90.
ThMAX= +90.
PhMIN=   0.
PhMAX= 360.
T=ThMIN+(ThMAX-ThMIN)*FINDGEN(NT)/FLOAT(NT-1)
P=PhMIN+(PhMAX-PhMIN)*FINDGEN(NP)/FLOAT(NP-1)
nt2=nt*scalefactor
np2=np*scalefactor
t2=thmin+(thmax-thmin)*findgen(nt2)/float(nt2-1)
p2=phmin+(phmax-phmin)*findgen(np2)/float(np2-1)

;if ir eq 2 then  begin
;   map2d=reform(mapoc(3,*,*)) ;---> Esto va xq el mapoc mas bajo que tengo es en 1.035
;endif else begin
;;   map2d=reform(mapoc(ir,*,*))
;endelse
;if ir eq 4 then begin
;   map2d=reform(mapoc(ir-1,*,*)) ; Force to use 1.035 Rsun OC-map as if it was 1.045 Rsun
;   print,'Used 1.035 O/C map! for 1.045 LDEM'
;endif
map2 =fltarr(nt2,np2)
for ip2=0,np2-1 do begin
   p0=p2(ip2)
   fp=abs(p-p0)
   ip=fix( (where(fp eq min(fp)))(0) )
   for it2=0,nt2-1 do begin
      t0=t2(it2)
      ft=abs(t-t0)
      it=fix( (where(ft eq min(ft)))(0) )
      map2(it2,ip2)=map2d(it,ip)
   endfor
endfor
mapoc_2d_rot_scl=rotate(map2,4)
goto,no

contour,mapoc_2d_rot_scl,lon,lat,/noerase,color=0,xstyle=1,ystyle=1,charsize=2,$
        /device,pos=[x0,y0,x0+nlon,y0+nlat],$
        xtitle='',$
        ytitle='',$
        title='',$
        yticklen=.02,xticklen=0.03,ythick=2,xthick=2,charthick=2,$
        c_color=0,c_thick=5,$ ;,c_labels=intarr(n_elements(nl))+1
        c_linestyle=lstyle


no:


;---PUT COLOR SCALE
;BAR---------------------------------------------------------
ny=nt*scalefactor
nx= 30
scale=fltarr(nx,ny)
x0=55+x0+np*scalefactor
y0=y0
for ix=0,nx-1 do scale(ix,*)=mini+(maxi-mini)*findgen(ny)/float(ny-1)

if cltb le 40 then loadct,cltb
if cltb gt 40 and instrument ne 'aia' then eit_colors,cltb
if cltb gt 40 and instrument eq 'aia' then AIA_LCT,wave=cltb,/load

tvscl,scale,x0,y0
loadct,0
contour,scale,findgen(nx),scale,$
        pos=[x0,y0,x0+nx,y0+ny],/device,color=0,/noerase,$
        yticklen=.2,/nodata,ythick=2,xthick=2,charthick=2,$
        xstyle=5,ystyle=1,charsize=2

return
end
