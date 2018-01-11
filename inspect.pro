; inspect,dir='/data1/tomography/DATA/euvi/CR2081/B171/',file='list.B171.nodecon.b4'

pro inspect,dir=dir,file=file

goto,skip
dir       = '/data1/tomography/bindata/'
file = 'yeuviB.171.cr2081.ri.98-ro1.025.NODECON'

nyn = 2915873L

yn = fltarr(nyn)

openr,1,dir+file
readu,1,yn
close,1

ineg = where(yn lt 0.)
ipos = where(yn gt 0.)
skip:


nf=0
x=''
openr,1,dir+file
readf,1,nf
imgarr=fltarr(1024,1024,nf)
for i=0,nf-1 do begin
readf,1,x
print,i,x
mreadfits,dir+x,hdr,img
imgarr(*,*,i)=img
endfor
close,1

yd = reform(imgarr,1024.^2*106L)
inegd=where(yd lt 0.)
iposd=where(yd gt 0.)

stop
return
end
