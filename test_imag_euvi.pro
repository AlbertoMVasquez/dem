pro images_euvi
goto,salt
;euvi_deconvolve_test,directory='/data1/tomography/DATA/euvi/CR2081/A171/',listfile='list.A171',nfiles=618,binfactor=2,/decon,/dnsec,/rebin,/rescale,/normalize,/despike
euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes.txt',nfiles=3,binfactor=2,/dnsec,/rebin
euvi_deconvolve_test,directory='/data1/work/dem/github_dem/dem/nishtha/obsdata/',listfile='list_imagenes.txt',nfiles=3,binfactor=2,/dnsec,/rebin,/decon,/despike

salt:
datadir='/data1/work/dem/github_dem/dem/nishtha/obsdata/'
openr,1,datadir+'list_imagenes_proc.txt'
openr,2,datadir+'list_imagenes_proc_decon.txt'

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

   device, retain     = 2
   device, true_color = 24
   device, decomposed = 0
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
hdr2_211 = hdr
ima2_211 = ima

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
stop
window,1,xs=1024,ys=1024
tvscl,alog10(imag_171)

window,2,xs=1024,ys=1024
tvscl,alog10(imag2_171)
!p.multi=[0,1,2]  
vec171_x = ima_171(round(hdr_171.crpix1),*)
vec171_y = ima_171(*,round(hdr_171.crpix2))
vec171_x2 = ima2_171(round(hdr2_171.crpix1),*)
vec171_y2 = ima2_171(*,round(hdr2_171.crpix2))

plot, alog10(vec171_x)
oplot, alog10(vec171_x2>0.01)

plot, alog10(vec171_y)
oplot, alog10(vec171_y2>0.01)

!p.multi=0 

eit_colors,195
imag_195 = ima_195
imag2_195 = ima2_195
imag_195(0,0)=maxval2
imag_195(0,1)=minval2
imag2_195(0,0)=maxval2
imag2_195(0,1)=minval2
imag_195=imag_195>minval2<maxval2
imag2_195=imag2_195>minval2<maxval2
window,1,xs=1024,ys=1024
tvscl,alog10(imag_195)
window,2,xs=1024,ys=1024
tvscl,alog10(imag2_195)
!p.multi=[0,1,2]
vec195_x = ima_195(round(hdr_195.crpix1),*)
vec195_y = ima_195(*,round(hdr_195.crpix2))
vec195_x2 = ima2_195(round(hdr2_195.crpix1),*)
vec195_y2 = ima2_195(*,round(hdr2_195.crpix2))
plot, alog10(vec195_x)
oplot, alog10(vec195_x2>0.01)
plot, alog10(vec195_y)
oplot, alog10(vec195_y2>0.01)
!p.multi=0


eit_colors,284
imag_284 = ima_284
imag2_284 = ima2_284
imag_284(0,0)=maxval3
imag_284(0,1)=minval3
imag2_284(0,0)=maxval3
imag2_284(0,1)=minval3
imag_284=imag_284>minval3<maxval3
imag2_284=imag2_284>minval3<maxval3
window,1,xs=1024,ys=1024
tvscl,alog10(imag_284)
window,2,xs=1024,ys=1024
tvscl,alog10(imag2_284)
!p.multi=[0,1,2]
vec284_x = ima_284(round(hdr_284.crpix1),*)
vec284_y = ima_284(*,round(hdr_284.crpix2))
vec284_x = ima_284(round(hdr_284.crpix1),*)
vec284_y = ima_284(*,round(hdr_284.crpix2))
plot, alog10(vec284_x)
oplot, alog10(vec284_x2>0.01)
plot, alog10(vec284_y)
oplot, alog10(vec284_y2>0.01)
!p.multi=0


  return
end
