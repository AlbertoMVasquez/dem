
pro wrapper_correct_data

correct_data,'/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.nodecon.b4'
correct_data,'/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.nodecon.b4'
correct_data,'/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.nodecon.b4'

correct_data,'/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.b4'
correct_data,'/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.b4'
correct_data,'/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.b4'

end


pro correct_data,dir,file
nf=0
x=''
openr,1,dir+file
readf,1,nf
for i=0,nf-1 do begin
readf,1,x
mreadfits,dir+x,hdr,img
p=where(img lt -100.)
if p(0) ne -1 then img(p)=-999.
mwritefits,hdr,img,outfile=dir+x
print,i,x,min(img)
endfor
close,1
return

stop

dir       = '/data1/tomography/bindata/'
file_ynew = 'yeuviB.171.cr2081.ri.98-ro1.025.NODECON'

nyn = 2915873L

yn = fltarr(nyn)

openr,1,dir+file_ynew
readu,1,yn
close,1

ineg = where(yn lt 0.)
ipos = where(yn gt 0.)

return
end
