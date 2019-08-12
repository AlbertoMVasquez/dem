pro images_euvi
goto,salt
;euvi_deconvolve_test,directory='/data1/tomography/DATA/euvi/CR2081/A171/',listfile='list.A171',nfiles=618,binfactor=2,/decon,/dnsec,/rebin,/rescale,/normalize,/despike
euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes_2209.txt',nfiles=3,binfactor=2,/dnsec,/rebin
euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes_2209.txt',nfiles=3,binfactor=2,/dnsec,/rebin,/decon,/despike
euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes_2208.txt',nfiles=3,binfactor=2,/dnsec,/rebin
euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes_2208.txt',nfiles=3,binfactor=2,/dnsec,/rebin,/decon,/despike

euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes_2209.txt',nfiles=3,binfactor=2,/rebin
;.r /data1/tomography/SolarTom_idl/compute_image_grid.pro

salt:
datadir='/data1/work/dem/github_dem/dem/nishtha/obsdata/'
;openr,1,datadir+'list_imagenes_proc_2209.txt'
;openr,2,datadir+'list_imagenes_proc_decon_2209.txt'
openr,1,datadir+'list_imagenes_proc_2208.txt'
;openr,2,datadir+'list_imagenes_proc_decon_2208.txt'
openr,2,datadir+'list_imagenes_photo.txt'
;sufijo = '2209'
;sufijo = '2208'
sufijo='photons'
n=3
filenames  = strarr(n)
filenames2 = strarr(n)
x=''
y=''
for i=0,n-1 do begin
   readf,1,x & filenames(i) = x
   readf,2,y & filenames2(i)= y
endfor
close,1
close,2
stop
;   device, retain     = 2
;   device, true_color = 24
;   device, decomposed = 0
   xs=1024
   ys=1024
   !p.multi=0  
mreadfits,datadir+filenames(0),hdr,ima
hdr_171 = hdr
ima_171 = ima
mreadfits,datadir+filenames(1),hdr,ima
hdr_195 = hdr
ima_195 = ima
mreadfits,datadir+filenames(2),hdr,ima
hdr_284 = hdr
ima_284 = ima

mreadfits,datadir+filenames2(0),hdr,ima
hdr2_171 = hdr
ima2_171 = ima
mreadfits,datadir+filenames2(1),hdr,ima
hdr2_195 = hdr
ima2_195 = ima
mreadfits,datadir+filenames2(2),hdr,ima
hdr2_284 = hdr
ima2_284 = ima

minval1 = 0.001
maxval1 = 600
eit_colors,171
imag_171 = ima_171
imag2_171 = ima2_171
imag_171(0,0)=maxval1
imag_171(0,1)=minval1
imag2_171(0,0)=maxval1
imag2_171(0,1)=minval1
imag_171=imag_171>minval1<maxval1
imag2_171=imag2_171>minval1<maxval1
compute_image_grid,hdr=hdr_171,instrument='euvi',ra=ra,pa=pa,x=x,y=y
rad = where(ra le 1.405 and ra ge 1.395)
imag_171(rad) = maxval1
imag2_171(rad) = maxval1
stop
ps1,'image_171_nodecon_'+sufijo+'.eps'
;window,1,xs=xs,ys=ys
tvscl,alog10(imag_171)
ps2
ps1,'image_171_decon_'+sufijo+'.eps'
;window,2,xs=xs,ys=ys
tvscl,alog10(imag2_171)
ps2
;stop
vec171_x = ima_171(round(hdr_171.crpix1),*)
vec171_y = ima_171(*,round(hdr_171.crpix2))
vec171_x2 = ima2_171(round(hdr2_171.crpix1),*)
vec171_y2 = ima2_171(*,round(hdr2_171.crpix2))
ps1,'image_171_profiles_'+sufijo+'.eps',12
!p.multi=[0,1,2]  
;window,3,xs=xs,ys=ys
plot, x,alog10(vec171_x),ystyle=1,xrange=[-1.4,1.4],/nodata
oplot, x,alog10(vec171_x),color=110
oplot, x ,alog10(vec171_x2>0.01),color=200
plot,x, alog10(vec171_y),ystyle=1,xrange=[-1.4,1.4],/nodata
oplot,x, alog10(vec171_y),color=110
oplot,x, alog10(vec171_y2>0.01),color=200
!p.multi=0 
ps2

;stop
eit_colors,195
maxval2=800
imag_195 = ima_195
imag2_195 = ima2_195
imag_195(0,0)=maxval2
imag_195(0,1)=minval1
imag2_195(0,0)=maxval2
imag2_195(0,1)=minval1
imag_195=imag_195>minval1<maxval2
imag2_195=imag2_195>minval1<maxval2
compute_image_grid,hdr=hdr_195,instrument='euvi',ra=ra,pa=pa,x=x,y=y
rad = where(ra le 1.405 and ra ge 1.395)
imag_195(rad) = maxval2
imag2_195(rad) = maxval2
ps1,'image_195_nodecon_'+sufijo+'.eps'
;window,1,xs=xs,ys=ys
tvscl,alog10(imag_195)
ps2
ps1,'image_195_decon_'+sufijo+'.eps'
;window,2,xs=xs,ys=ys
tvscl,alog10(imag2_195)
ps2
;stop
ps1,'image_195_profiles_'+sufijo+'.eps',12
!p.multi=[0,1,2]
;window,3,xs=xs,ys=ys
vec195_x = ima_195(round(hdr_195.crpix1),*)
vec195_y = ima_195(*,round(hdr_195.crpix2))
vec195_x2 = ima2_195(round(hdr2_195.crpix1),*)
vec195_y2 = ima2_195(*,round(hdr2_195.crpix2))
plot, x,alog10(vec195_x),ystyle=1,xrange=[-1.4,1.4],/nodata
oplot,x, alog10(vec195_x),color=110
oplot,x, alog10(vec195_x2>0.01),color=200
plot, x,alog10(vec195_y),ystyle=1,xrange=[-1.4,1.4],/nodata
oplot, x,alog10(vec195_y),color=110
oplot,x, alog10(vec195_y2>0.01),color=200
!p.multi=0
ps2

eit_colors,284
maxval3=300
imag_284 = ima_284
imag2_284 = ima2_284
imag_284(0,0)=maxval3
imag_284(0,1)=minval1
imag2_284(0,0)=maxval3
imag2_284(0,1)=minval1
imag_284=imag_284>minval1<maxval3
imag2_284=imag2_284>minval1<maxval3
compute_image_grid,hdr=hdr_284,instrument='euvi',ra=ra,pa=pa,x=x,y=y
rad = where(ra le 1.405 and ra ge 1.395)
imag_284(rad) = maxval3
imag2_284(rad) = maxval3
ps1,'image_284_nodecon_'+sufijo+'.eps'
;window,1,xs=xs,ys=ys
tvscl,alog10(imag_284)
ps2
ps1,'image_284_decon_'+sufijo+'.eps'
;window,2,xs=xs,ys=ys
tvscl,alog10(imag2_284)
ps2
;stop
ps1,'image_284_profiles_'+sufijo+'.eps',12
!p.multi=[0,1,2]
;window,3,xs=xs,ys=ys
vec284_x = ima_284(round(hdr_284.crpix1),*)
vec284_y = ima_284(*,round(hdr_284.crpix2))
vec284_x2 = ima2_284(round(hdr2_284.crpix1),*)
vec284_y2 = ima2_284(*,round(hdr2_284.crpix2))
plot, x,alog10(vec284_x),ystyle=1,xrange=[-1.4,1.4],/nodata
oplot,x, alog10(vec284_x),color=110
oplot,x, alog10(vec284_x2>0.01),color=200
plot, x,alog10(vec284_y),ystyle=1,xrange=[-1.4,1.4],/nodata
oplot, x,alog10(vec284_y),color=110
oplot,x, alog10(vec284_y2>0.01),color=200
!p.multi=0
ps2

  return
end
