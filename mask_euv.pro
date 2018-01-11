

; APLICACION DE MASCARA:
; mask_euv,'/data1/tomography/DATA/eit/CR1914/284/','list.284.b',instrument='eit'
; mask_euv,'/data1/tomography/DATA/eit/CR1915/284/','list.284.b',instrument='eit'
; mask_euv,'/data1/tomography/DATA/eit/CR1919/284/','list.284.b',instrument='eit'

;---------------------------------------------------------------------------------------------------------------

; SOLO PARA VER EL ANILLO:

; mask_euv,'/data1/tomography/DATA/eit/CR1914/171/','list.171.b',instrument='eit'
; mask_euv,'/data1/tomography/DATA/eit/CR1914/195/','list.195.b',instrument='eit'

; mask_euv,'/data1/tomography/DATA/euvi/CR2081/A171/','list.A171.b4',instrument='euvi'
; mask_euv,'/data1/tomography/DATA/euvi/CR2081/A195/','list.A195.b4',instrument='euvi'
; mask_euv,'/data1/tomography/DATA/euvi/CR2081/A284/','list.A284.b4',instrument='euvi'

; mask_euv,'/data1/tomography/DATA/aia/CR2099/171/','list.171.processed.selected.selected.b4',instrument='aia'
; mask_euv,'/data1/tomography/DATA/aia/CR2099/193/','list.193.processed.selected.selected.b4',instrument='aia'
; mask_euv,'/data1/tomography/DATA/aia/CR2099/211/','list.211.processed.selected.selected.b4',instrument='aia'
; mask_euv,'/data1/tomography/DATA/aia/CR2099/335/','list.335.processed.selected.selected.b4',instrument='aia'

pro mask_euv,datadir,listfile,instrument=instrument

n=0
filename=''
openr,1,datadir+listfile
openw,2,datadir+listfile+'.masked'
openw,3,datadir+listfile+'.masked.medians'
readf,1,n
printf,2,n


for i=0,n-1 do begin
readf,1,filename
mreadfits,datadir+filename,hdr,ima

;--mascara-------------------------------------------------------------------
 computegrid,hdr,ra,ta,instrument
 x=ima
 ind=where(ra ge 1.10 AND ( (ta ge 340 and ta le 360.) OR (ta le 5.) ) )
 x(ind)=-666. ; enmascara X
 ima   =x     ; asigna X a IMA (con máscara, sin anillo)

;-- anillo-------------------------------------------------------------------
 ind=where(ra gt 1.26 and ra le 1.27)
 x(ind)=max(ima)
;----------------------------------------------------------------------------

;--chequeo grafico------------------------------------------------------------
;goto,skipgraph
;window,0,xs=1800     
;plot,ima(300,*),/ylog
;oplot,x(300,*),th=3
 if instrument eq 'eit' or instrument eq 'euvi' then eit_colors,hdr.WAVELNTH
 if instrument eq 'aia'                         then aia_lct,wave=hdr.WAVELNTH,/load
 window,0,xs=hdr.naxis1,ys=hdr.naxis2
 x(0,0)=.001
 x(0,1)=500.
 tvscl,alog10(x>.001<500.)
 loadct,0
;close,/all
;stop
skipgraph:
;------------------------------------------------------------------------------

filename=filename+'.masked.3'
record_gif,datadir,filename+'.gif'
mwritefits,hdr,ima, outfile=datadir+filename
printf,2,filename
ind=where(ra le 1. AND ima gt 0.)
printf,3,filename,median(ima(ind))
endfor
close,/all

return
end

pro computegrid,hdr,ra,ta,instrument
 if instrument eq 'euvi' then $
 Rs=hdr.rsun              ; Sun radius in arcsec
 if instrument eq 'aia' then $
 Rs=hdr.rsun_obs          ; Sun radius in arcsec
 px=hdr.cdelt1            ; Pixel size in arcsec                     
 if instrument ne 'eit' then $
 Rs=Rs/px                 ; Sun radius in pixels
 if instrument eq 'eit' then $
 Rs=hdr.solar_r           ; Sun radius in pixels
 px=1./Rs                 ; Pixel size in Rsun units
 iy0=hdr.crpix1-1
 iz0=hdr.crpix2-1
 y = px * (findgen(hdr.naxis1)-iy0)
 z = px * (findgen(hdr.naxis1)-iz0)
 u=1.+fltarr(hdr.naxis1)
 ya=y#u
 za=u#z
 ra = sqrt(ya^2+za^2)
 ta = fltarr(hdr.naxis1,hdr.naxis1)
 p=where(ya gt 0.)
 ta(p) = Acos( za(p) / ra(p) )
 p=where(ya lt 0.)
 ta(p) = 2.*!pi-Acos( za(p) / ra(p) )
 p=where(ya eq 0. AND za gt 0.)
 if p(0) ne -1 then ta(p)=0.
 p=where(ya eq 0. AND za lt 0.)
 if p(0) ne -1 then ta(p)=!pi
 ta=2.*!pi-ta
 ta=ta/!dtor
return
end

