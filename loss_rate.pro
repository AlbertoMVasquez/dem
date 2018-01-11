pro loss_rate,outputfile=outputfile,density=density

; Chianti Ritual steps:
basedir='/usr/local/ssw/packages/chianti/'
!path = '+'+basedir+'idl:'+!path
!path=expand_path(!path)
use_chianti,basedir+'dbase/'


if not keyword_set(density) then density=1.e10

rad_loss,t,lr,density=density

;outputfile='lr_3e10.out'
;outputfile='loss_func_chianti.ioneq_sun_coronal.abund.new.out'

goto,caca
openw,1,outputfile
printf,1,'density'
printf,1,3.e10
printf,1,'temp ',' Loss Rate'
n=n_elements(t)
for i=0,n-1 do begin
printf,1,t(i),lr(i)
endfor
close,1
print,t
caca:

N_Temp=n_elements(t)

openw,2,outputfile
printf,2,'N_temp'
printf,2,N_temp
printf,2,'temp','Loss Function'
for i=0,N_temp-1 do begin
printf,2,t(i),lr(i)
endfor
close,2

return
end
