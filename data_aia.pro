
; process_data_aia_lev10,'DEM_Dimmings/',/letitbe,ib1=0,ib2=5

; process_data_aia_lev10,'CR2099/hicad/',/letitbe

;  process_data_aia_lev10,'CR2198/',ib1=0,ib2=3,/rbn
;process_data_aia_lev10,'CR2219/',ib1=0,ib2=3,/rbn
pro process_data_aia_lev10,rotationdir,suffix=suffix,ib1=ib1,ib2=ib2,rbn=rbn,basedir=basedir

common calibration_parameters,Omega_p,PHI0_v,aiacorr

    ; Compute Phik0
     Phik0
    ; get time-dpendent correction factors
     aiacorr = aia_bp_get_corrections()

     if NOT keyword_set(ib1)     then ib1=0
     if NOT keyword_set(ib2)     then ib2=3
     if NOT keyword_set(suffix)  then suffix=''
     if not keyword_set(basedir) then basedir='/media/Data1/';basedir='/data1/'
     dir=basedir+'tomography/DATA/aia/'+rotationdir
; subdir=['094/','131/','171/','193/','211/','335/','304/']
  subdir=['171/','193/','211/','335/','131/','094/','304/'] ; A more reasonable order to process.
   list  ='list.'+strmid(subdir,0,3)+suffix
   listp ='list.'+strmid(subdir,0,3)+suffix+'.processed'
filename=''
       n=0

       for ib=ib1,ib2 do begin
          openr,1,dir+subdir[ib]+list[ib]
          readf,1,n
          openw,2,dir+subdir[ib]+listp[ib]
          printf,2,n
          print,'andando'
          for i=0,n-1 do begin
             readf,1,filename
             aia_prep,dir+subdir[ib]+filename,[0],header15,image15
             print, i
;             ssw_file_delete,'/tmp/AIA*fits'

             newfilename=strmid(header15.DATE_OBS, 0,4)+strmid(header15.DATE_OBS, 5,2)+$
                         strmid(header15.DATE_OBS, 8,2)+'.'+$
                         strmid(header15.DATE_OBS,11,2)+strmid(header15.DATE_OBS,14,2)+'.'+$
                         strmid(subdir[ib],0,3)+'.lev1p5'
             
             goto,skip_table
             print,newfilename
             ix=3000
             iy=lindgen(7)*500+500
             int,strmid(subdir[ib],0,3),reform(image15(ix,iy))
             close,/all
             stop
skip_table:
             
; goto,test
; Normalize by exposure time
             image15 = image15 / header15.EXPTIME
             newfilename=newfilename+'.ETN'

; Divide by Ck0
             divide_by_ck0,image15,header15,newimage15
             image15=newimage15
             newfilename=newfilename+'.Norm-Ck0'

; Rebin to 1024^2
             if keyword_set(rbn) then begin
                rbn,image15,header15,4
                newfilename=newfilename+'.1024'
             endif

; Make -999 all negative pixels
             goto,skip_make_999
             p=where(image15 lt 0.)
             if p(0) ne -1 then image15(p)=-999.
skip_make_999:
             
test:

; Finish making newfilename, write new image, and print newlist
             newfilename=newfilename+suffix+'.fts'
             mwritefits,header15,image15,outfile=dir+subdir[ib]+newfilename   
             printf,2,newfilename
             
          endfor
          close,1
          close,2
       endfor

       return
end

pro rbn,image,hdr,binfactor
 image=rebin(image,hdr.NAXIS1/binfactor,hdr.NAXIS2/binfactor)
 ; C' = 1 + (C-1) * (N/B-1)/(N-1).
 hdr.CRPIX1 = 1 + (hdr.CRPIX1 -1) * (hdr.NAXIS1/binfactor - 1) / (hdr.NAXIS1-1)
 hdr.CRPIX2 = 1 + (hdr.CRPIX2 -1) * (hdr.NAXIS2/binfactor - 1) / (hdr.NAXIS2-1)
 hdr.NAXIS1 =hdr.NAXIS1 /binfactor
 hdr.NAXIS2 =hdr.NAXIS2 /binfactor
 hdr.CDELT1 =hdr.CDELT1 *binfactor
 hdr.CDELT2 =hdr.CDELT2 *binfactor
return
end

pro Phik0
common calibration_parameters,Omega_p,PHI0_v,aiacorr
bandpass=aia_get_response(/dn)
Omega_p=bandpass.platescale
Phi094=max(bandpass.A94.EA)
Phi0131=max(bandpass.A131.EA)
Phi0171=max(bandpass.A171.EA)
Phi0193=max(bandpass.A193.EA)
Phi0211=max(bandpass.A211.EA)
Phi0304=max(bandpass.A304.EA)
Phi0335=max(bandpass.A335.EA)
Phi0_v=[Phi094,Phi0131,Phi0171,Phi0193,Phi0211,Phi0304,Phi0335]
return
end

pro divide_by_ck0,image,header,newimage
common calibration_parameters,Omega_p,PHI0_v,aiacorr

  ;Determine time-dependent correction for image date
  aiacorr_day = strmid(aiacorr.utc    ,0,10)
  image_day   = strmid(header.DATE_OBS,0,10)
  index_day   = where(aiacorr_day eq image_day)
  if n_elements(index_day) eq -1 then begin
    print,'There are no EA corrections tabulated for the date of the data you want to process.'
    stop
 endif
  if n_elements(index_day) gt 1 then begin
    print,'There are many EA corrections tabulated for the date of the data you want to process. Select one.'
    stop
  endif     
  if n_elements(index_day) eq 1 then $
  factors = aiacorr.ea_over_ea0(index_day,*) * aiacorr.ea_aia_over_eve

  ; Compute PHI_k_0 with time-dependent correction
  if header.wavelnth eq  94 then PHI0 = PHI0_v(0) * Omega_p * factors(0)
  if header.wavelnth eq 131 then PHI0 = PHI0_v(1) * Omega_p * factors(1)
  if header.wavelnth eq 171 then PHI0 = PHI0_v(2) * Omega_p * factors(2)
  if header.wavelnth eq 193 then PHI0 = PHI0_v(3) * Omega_p * factors(3)
  if header.wavelnth eq 211 then PHI0 = PHI0_v(4) * Omega_p * factors(4)
  if header.wavelnth eq 304 then PHI0 = PHI0_v(5) * Omega_p * factors(5)
  if header.wavelnth eq 335 then PHI0 = PHI0_v(6) * Omega_p * factors(6)
  Ck0=PHI0*1.e12
  newimage=image/Ck0

return
end

pro makenewhdr,hdr,newhdr

  newhdr=create_struct('crpix1',hdr.crpix1,$
                       'crpix2',hdr.crpix2,$
                       'naxis1',hdr.naxis1,$
                       'naxis2',hdr.naxis2,$
                       'cdelt1',hdr.cdelt1,$
                       'cdelt2',hdr.cdelt2,$
                       'wavelnth',hdr.wavelnth,$
                       'WAVE_STR',hdr.WAVE_STR,$
                       'T_OBS',hdr.T_OBS)

    newhdr=create_struct(newhdr,$
   	'haex_obs',hdr.haex_obs,$
    	'haey_obs',hdr.haey_obs,$
    	'haez_obs',hdr.haez_obs,$
   	'gaex_obs',hdr.gaex_obs,$
    	'gaey_obs',hdr.gaey_obs,$
    	'gaez_obs',hdr.gaez_obs,$
        'crota2'  ,hdr.crota2  ,$ 
   	'DSUN_OBS',hdr.DSUN_OBS,$
    	'HGLN_OBS',hdr.HGLN_OBS,$
    	'CRLN_OBS',hdr.CRLN_OBS,$
    	'CRLT_OBS',hdr.CRLT_OBS,$
    	'RSUN_OBS',hdr.RSUN_OBS,$
        'DATE_OBS',hdr.DATE_OBS)
return
end


pro process_aia,ib1=ib1,ib2=ib2
 process_data_aia_lev10,'CR2097/',/letitbe,ib1=ib1,ib2=ib2
return
end


pro timebin_cycle

  timebinning,'','/data1/tomography/DATA/aia/CR2198/171/','list.171.processed',4
  timebinning,'','/data1/tomography/DATA/aia/CR2198/193/','list.193.processed',4
  timebinning,'','/data1/tomography/DATA/aia/CR2198/211/','list.211.processed',4
  timebinning,'','/data1/tomography/DATA/aia/CR2198/335/','list.335.processed',4
  stop
  return
  
  timebinning,'','/data1/tomography/DATA/aia/CR2192/171/','list.171.processed',4
  
return

; Haciéndolo Albert:
 timebinning,'','/data1/tomography/DATA/aia/CR2099/094/','list.094.processed.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2097/094/','list.094.processed.selected',4



RETURN
; Para FEDE:------------------------------
 timebinning,'','/data1/tomography/DATA/aia/CR2097/171/','list.171.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2097/193/','list.193.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2097/211/','list.211.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2097/335/','list.335.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2097/131/','list.131.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2099/304/','list.304.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2097/094/','list.094.processed.selected.selected',4
;------------------------------------------


; El resto ya fue hecho por Albert.

RETURN

 timebinning,'','/data1/tomography/DATA/aia/CR2099/171/','list.171.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2099/193/','list.193.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2099/211/','list.211.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2099/335/','list.335.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2099/131/','list.131.processed.selected.selected',4
 timebinning,'','/data1/tomography/DATA/aia/CR2099/304/','list.304.processed.selected.selected',4

return
end

;-------------------------------------------------------------------------------------
;timebinning,'','/Storage1TB/tomography/DATA/aia/test/335/','list.335.processed',4
pro timebinning, basedir, data_dir, infilename, ntb

instname='AIA'
outfilenamesuffix='';strmid(infilename,strlen(infilename)-2,2)

dT = 3600.*24./ntb ; time bin size in secs.

basedir='/usr/local/ssw/packages/chianti/'
!path = '+'+basedir+'idl:'+!path
!path=expand_path(!path)
;use_chianti,basedir+'dbase/'


nf=0
x=''
openr,1,data_dir+infilename
readf,1,nf
filenames=strarr(nf)
for i=0,nf-1 do begin
readf,1,x
filenames(i)=x
endfor
close,1

if n_elements(filenames) eq 0 then begin
 print,'NO FILES TO READ!!!'
 stop
endif

p094=where(strmid(filenames,14,3) eq '094')
p131=where(strmid(filenames,14,3) eq '131')
p171=where(strmid(filenames,14,3) eq '171')
p193=where(strmid(filenames,14,3) eq '193')
p211=where(strmid(filenames,14,3) eq '211')
p335=where(strmid(filenames,14,3) eq '335')
p304=where(strmid(filenames,14,3) eq '304')
if p094(0) ne -1 then filenames094=filenames(p094)
if p131(0) ne -1 then filenames131=filenames(p131)
if p171(0) ne -1 then filenames171=filenames(p171)
if p193(0) ne -1 then filenames193=filenames(p193)
if p211(0) ne -1 then filenames211=filenames(p211)
if p335(0) ne -1 then filenames335=filenames(p335)
if p304(0) ne -1 then filenames304=filenames(p304)

;for isp=0,nscft-1 do begin
for ipb=0,6 do begin
 if ipb eq 0 then begin
 if p094(0) eq -1 then goto,nextpb
 nf=n_elements(filenames094)
 file=filenames094
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'094.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'094.out'
 endif
 if ipb eq 1 then begin
 if p131(0) eq -1 then goto,nextpb
 nf=n_elements(filenames131)
 file=filenames131
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'131.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'131.out'
 endif
 if ipb eq 2 then begin
 if p171(0) eq -1 then goto,nextpb
 nf=n_elements(filenames171)
 file=filenames171
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'171.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'171.out'
 endif
 if ipb eq 3 then begin
 if p193(0) eq -1 then goto,nextpb
 nf=n_elements(filenames193)
 file=filenames193
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'193.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'193.out'
 endif
 if ipb eq 4 then begin
 if p211(0) eq -1 then goto,nextpb
 nf=n_elements(filenames211)
 file=filenames211
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'211.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'211.out'
 endif
 if ipb eq 5 then begin
 if p335(0) eq -1 then goto,nextpb
 nf=n_elements(filenames335)
 file=filenames335
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'335.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'335.out'
 endif
 if ipb eq 6 then begin
 if p304(0) eq -1 then goto,nextpb
 nf=n_elements(filenames304)
 file=filenames304
 logfile='time_binning_logfile_'+instname+'.'+outfilenamesuffix+'304.out'
 crpixfile='offset_logfile_'+instname+'.'+outfilenamesuffix+'304.out'
 endif
 openw,1,data_dir+logfile;,/append
 openw,11,data_dir+crpixfile

 i=0
 filename=data_dir+file(i)
 mreadfits,filename,hdr,image
 
 makenewhdr,hdr,newhdr

 dateobs=hdr.date_obs 
 hh=float(strmid(dateobs,11,2))
 mm=float(strmid(dateobs,14,2))
 ss=float(strmid(dateobs,17,5))
 seconds=hh*3600.+mm*60.+ss
 bin=fix(seconds/dT)
 date=strmid(hdr.date_obs,0,10)
 timeobs=strmid(dateobs,11,2)+strmid(dateobs,14,2)

 imsize=fix((size(image))(1))

NEWBIN:
  nib = 0 ; numer of images in current temporal-bin
  bincrpix1=0
  bincrpix2=0
  printf,1,'*********************** NEW BIN *****************************'
     print,'*********************** NEW BIN *****************************'
  printf,1,file(i),' ',seconds/3600./(24./ntb)
     print,file(i),' ',seconds/3600./(24./ntb)
  binimages = fltarr(imsize,imsize,1)
  timestrings = strarr(1)
  hdra=replicate(newhdr,1);replicate(hdr,1)
samebin:
  flag=''
  nib = nib + 1
  binimages(*,*,nib-1) = image
    timestrings(nib-1) = timeobs
           hdra(nib-1) = newhdr;hdr
  oldbin               = bin
  olddate              = date
  if i eq nf-1 then begin
    ilabel=i
    flag='out'
    goto,lastfile
  endif
  i=i+1
  filename=data_dir+file(i)
  mreadfits,filename,hdr,image
  
  makenewhdr,hdr,newhdr

 dateobs=hdr.date_obs 
 hh=float(strmid(dateobs,11,2))
 mm=float(strmid(dateobs,14,2))
 ss=float(strmid(dateobs,17,5))
 seconds=hh*3600.+mm*60.+ss
 bin=fix(seconds/dT)
 date=strmid(hdr.date_obs,0,10)
 timeobs=strmid(dateobs,11,2)+strmid(dateobs,14,2)

  if (date eq olddate) AND (bin eq oldbin) then begin
     printf,1,file(i),' ',seconds/3600./(24./ntb)
        print,file(i),' ',seconds/3600./(24./ntb)
     temp  = fltarr(imsize,imsize,nib+1)
     temp2 = strarr(nib+1) 
     temp3 = replicate(newhdr,nib+1);replicate(hdr,nib+1)
     for im=0,nib-1 do temp(*,*,im)=binimages(*,*,im)
     binimages=temp
     for im=0,nib-1 do temp2(im)=timestrings(im)
     timestrings=temp2
     temp3(0:nib-1)=hdra
     hdra=temp3
     goto,samebin
  endif

 if (date ne olddate) OR (bin ne oldbin) then ilabel=i-1

lastfile:
  ;imprimir SUM y logfile
  if oldbin le 9 then strbin='0'+strmid(string(oldbin),7,1)
  if oldbin gt 9 then strbin=    strmid(string(oldbin),6,2)
  if oldbin gt ntb-1 then STOP ; !!!

  filename=data_dir+strmid(string(file(ilabel)), 0, 8)+'T' + $
                    timestrings(nib/2)+'.'+                  $
                    strmid(string(file(ilabel)),14,28)+      $
                    outfilenamesuffix+'.b.fts'

print,filename

  ; Take out a bad-centered image:
goto,skip1
  printf,1,'Crpix1:',hdra.crpix1
  printf,1,'Crpix2:',hdra.crpix2
  if nib eq 1 then begin
  median_crpix1=hdra.crpix1
  median_crpix2=hdra.crpix2
  endif
  if nib gt 1 then begin
  median_crpix1=median(hdra.crpix1)
  median_crpix2=median(hdra.crpix2)
  endif
  dif1=abs(hdra.crpix1-median_crpix1)
  dif2=abs(hdra.crpix2-median_crpix2)
  iok=where(dif1 le 0.1 AND dif2 le 0.1)
  if iok(0) eq -1 then begin
     print,'No 2 images have same centering'
     iok=[nib/2]
  endif
  hdra      = hdra(iok)
  binimages = binimages(*,*,iok)
  nib       = fix(n_elements(iok))
  printf,1,'Selected images are indexed:',iok
skip1:

;  pbad = where(hdra.crpix1 ne median(hdra.crpix1))
;  if pbad(0) ne -1 then stop 
if p304(0) eq -1 then begin  
  maxEast=fltarr(nib)
  maxWest=fltarr(nib)
  for iii=0,nib-1 do begin
    tempvec=reform(binimages(*,hdra(iii).crpix2,iii))
    indexmaxeast=where(tempvec(0:hdra(iii).crpix1/2) eq max(tempvec(0:hdra(iii).crpix1/2)))
    indexmaxwest=where(tempvec(hdra(iii).crpix1/2:hdra(iii).naxis1-1) eq max(tempvec(hdra(iii).crpix1/2:hdra(iii).naxis1-1)))
    if indexmaxEast(0) ne -1 then maxEast(iii)=median(indexmaxeast)
    if indexmaxWest(0) ne -1 then maxWest(iii)=median(indexmaxwest) + hdra(iii).crpix1/2
    if indexmaxEast(0) eq -1 then maxEast(iii)=0
    if indexmaxWest(0) ne -1 then maxWest(iii)=0
  endfor
  medianmaxEast=median(maxEast)
  medianmaxWest=median(maxWest)
  iok=where( abs(medianmaxEast-maxEast) le 3. AND abs(medianmaxWest-maxWest) le 3. )
  if iok(0) eq -1 then begin
     print,'No 2 images have same centering'
     iok=[0]
  endif
  hdra      = hdra(iok)
  binimages = binimages(*,*,iok)
  nib       = fix(n_elements(iok))
  printf,1,'Selected images are indexed:',iok
  printf,11,n_elements(iok)
endif
 ;

  ;Get pixel by pixel averaged image of the bin
  meanimage=fltarr(imsize,imsize)
  for ix=0,imsize-1 do begin
  for iy=0,imsize-1 do begin
    nvp=0
    sum=0.
;----------------------make a better code using "where"-----------------
    for im=0,nib-1 do begin
                                ; Only add STRICTLY POSITIVE pixels,
                                ; cause in individual images there are
                                ; -999s *and* zeroes.
    if binimages(ix,iy,im) gt 0. then begin
       nvp=nvp+1
       sum=sum+binimages(ix,iy,im)
    endif    
    endfor
    if nvp eq 0 then meanimage(ix,iy)=-999.
    if nvp ne 0 then meanimage(ix,iy)= sum/float(nvp)
;---------------------------------------------------------------------
  endfor
  endfor

  ;Create "median" header for binned file
    medianhdr=hdra(nib/2)

  ;Create "mean" header for binned file
    meanhdr=medianhdr ; equal to median, then average following quantities:
    meanhdr.crpix1  =mean(hdra.crpix1)
    meanhdr.crpix2  =mean(hdra.crpix2)
    meanhdr.cdelt1  =mean(hdra.cdelt1)
    meanhdr.cdelt2  =mean(hdra.cdelt2)

     printf,1,'CARR LONGS:',hdra.CRLN_OBS
    ;Change CARR_LONGS to [0,360]
    p=where(hdra.CRLN_OBS lt 0.)
    if p(0) ne -1 then begin
       correction=fltarr(nib) & correction(p)=360.
       hdra.CRLN_OBS=hdra.CRLN_OBS+correction
    endif
    ;Compute the average of LONGS properly
    if max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) gt 300. then begin
       printf,1,'Maxi diff of CRLN too large!'
       p=where(hdra.CRLN_OBS lt 50.)
       if p(0) ne -1 then begin
          correction=fltarr(nib) & correction(p)=360.
          hdra.CRLN_OBS=hdra.CRLN_OBS+correction 
          meanhdr.CRLN_OBS=mean(hdra.CRLN_OBS)
          if meanhdr.CRLN_OBS gt 360. then meanhdr.CRLN_OBS=meanhdr.CRLN_OBS-360.
       endif
       if p(0) eq -1 then STOP ; !!! Can't be.
    endif
    if max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) lt 50. then begin
       meanhdr.CRLN_OBS=mean(hdra.CRLN_OBS)
       if meanhdr.CRLN_OBS gt 360. then meanhdr.CRLN_OBS=meanhdr.CRLN_OBS-360.
    endif
    if max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) ge  50. and $
       max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) le 300. then STOP ; !!! Can't be.

    ;Change HG_LONGS to [0,360]
    p=where(hdra.HGLN_OBS lt 0.)
    if p(0) ne -1 then begin
       correction=fltarr(nib) & correction(p)=360.
       hdra.HGLN_OBS=hdra.HGLN_OBS+correction
    endif
    ;Compute the average of LONGS properly
    if max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) gt 300. then begin
       p=where(hdra.HGLN_OBS lt 50.)
       if p(0) ne -1 then begin
          correction=fltarr(nib) & correction(p)=360.
          hdra.HGLN_OBS=hdra.HGLN_OBS+correction 
          meanhdr.HGLN_OBS=mean(hdra.HGLN_OBS)
          if meanhdr.HGLN_OBS gt 360. then meanhdr.HGLN_OBS=meanhdr.HGLN_OBS-360.
       endif
       if p(0) eq -1 then STOP ; !!! Can't be.
    endif
    if max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) lt 50. then begin
       meanhdr.HGLN_OBS=mean(hdra.HGLN_OBS)
       if meanhdr.HGLN_OBS gt 360. then meanhdr.HGLN_OBS=meanhdr.HGLN_OBS-360.
    endif
    if max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) ge  50. and $
       max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) le 300. then STOP ; !!! Can't be.
    
    meanhdr.DSUN_OBS=mean(hdra.DSUN_OBS)
    meanhdr.CRLT_OBS=mean(hdra.CRLT_OBS)
    meanhdr.RSUN_OBS=mean(hdra.RSUN_OBS)
    meanhdr.crota2  =mean(hdra.crota2)
    meanhdr.haex_obs=mean(hdra.haex_obs)
    meanhdr.haey_obs=mean(hdra.haey_obs)
    meanhdr.haez_obs=mean(hdra.haez_obs)
    meanhdr.gaex_obs=mean(hdra.gaex_obs)
    meanhdr.gaey_obs=mean(hdra.gaey_obs)
    meanhdr.gaez_obs=mean(hdra.gaez_obs)

  MWRITEFITS,  meanhdr,meanimage, outfile=filename 

  printf,1,'   AVERAGE:',meanhdr.CRLN_OBS
  printf,1,'-----'
  printf,1,filename
  printf,1,nib,' files'

;stop

if flag eq 'out' then goto,nextpb
if i le nf-1 then goto,newbin
nextpb:
close, 1 ; PB-logfile
close,11 ; offset file
endfor
;endfor ; spacecraft

return
end

;--------------------------------------------------------------

pro binstat_roll

 basedir='/Storage1TB/'
;basedir='/data1/'

 PH_DN=[0.506431      ,  0.678774    , 0.891068   ,   1.00571   ,   1.13678 ,  1.80483]
 binstat,basedir+'tomography/DATA/aia/test/094/','list.094.sub-selected','aia094',171,PH_DN[0]
 binstat,basedir+'tomography/DATA/aia/test/131/','list.131.sub-selected','aia131',171,PH_DN[1]
 binstat,basedir+'tomography/DATA/aia/test/171/','list.171.sub-selected','aia171',171,PH_DN[2]
 binstat,basedir+'tomography/DATA/aia/test/193/','list.193.sub-selected','aia193',193,PH_DN[3]
 binstat,basedir+'tomography/DATA/aia/test/211/','list.211.sub-selected','aia211',284,PH_DN[4]
 binstat,basedir+'tomography/DATA/aia/test/335/','list.335.sub-selected','aia335',284,PH_DN[5]

return
end

pro binstat,datadir,listfile,suffix,band,phdn

; universal physical constants that we need
c = 2.99792458d8     ; m/sec
h = 4.13566743d-15   ; eV*sec

npix=0
n=0
imagefilename=''

openr,1,datadir+listfile
readf,1,n,npix
image=fltarr(n,npix,npix)
 Texp=fltarr(n)
for i=0,n-1 do begin
  readf,1,imagefilename
  mreadfits,datadir+imagefilename,hdr,img
  image(i,*,*)=img
   Texp(i,*,*)=hdr.exptime
  if i eq 0 then begin
    lambda = float(hdr.WAVELNTH) ; A
    print,'lambda:',lambda
    nu     = c/(lambda*1.d-10)   ; Hz
    E      = h*nu                ; eV
  endif
; tmp=replicate(hdr,i+1)
; if i ge 1 then tmp(0:i-1)=header
; header=tmp
endfor
close,1

print,(15.*3.65/E),phdn
image_photon=image*phdn

i06=fix(6*findgen( 6))
i12=fix(3*findgen(12))
i36=       indgen(36)
image_sum06_photon=fltarr(npix,npix)
image_sum12_photon=fltarr(npix,npix)
image_sum36_photon=fltarr(npix,npix)
for i=0, 5 do image_sum06_photon = image_sum06_photon + reform(image_photon(i06(i),*,*))
for i=0,11 do image_sum12_photon = image_sum12_photon + reform(image_photon(i12(i),*,*))
for i=0,35 do image_sum36_photon = image_sum36_photon + reform(image_photon(i36(i),*,*))

image_bin06_photon = image_sum06_photon/ 6.
image_bin12_photon = image_sum12_photon/12.
image_bin36_photon = image_sum36_photon/36.

M06= 6.
M12=12.
M36=36.

e_singl_photon = 1./sqrt(reform(image_photon(0,*,*)))

e_sum06_photon = 1./sqrt(image_sum06_photon)
e_sum12_photon = 1./sqrt(image_sum12_photon)
e_sum36_photon = 1./sqrt(image_sum36_photon)

e_bin06_photon = 1./sqrt(M06*image_bin06_photon)
e_bin12_photon = 1./sqrt(M12*image_bin12_photon)
e_bin36_photon = 1./sqrt(M36*image_bin36_photon)

y = (hdr.cdelt1/hdr.rsun_obs) * (findgen(hdr.naxis1)-(hdr.crpix1-1))
z = (hdr.cdelt2/hdr.rsun_obs) * (findgen(hdr.naxis2)-(hdr.crpix2-1))

v1=reform(e_singl_photon(*,hdr.crpix2-1))
v2=reform(e_singl_photon(hdr.crpix1-1,*))
v=[v1,v2]
p=where(v gt 0. and finite(v) ne 0)
emax=(3.*median(v(p)))

window,xs=1000,ys=500
!p.multi=[0,2,1]
!p.charsize=1.5
 plot,y,e_singl_photon(*,hdr.crpix2-1),$
      title='Relat-error time-binned PHOTON images: Equatorial cut',$
     xtitle='Y / Rsun',xr=[-1.3,+1.3],xstyle=1,yr=[0.,emax],ystyle=2
oplot,y,e_bin06_photon(*,hdr.crpix2-1)
oplot,y,e_bin12_photon(*,hdr.crpix2-1)
oplot,y,e_bin36_photon(*,hdr.crpix2-1)

 plot,y,e_singl_photon(hdr.crpix1-1,*),$
      title='Relat-error time-binned PHOTON images: Polar cut',$
     xtitle='Z / Rsun',xr=[-1.3,+1.3],xstyle=1,yr=[0.,emax],ystyle=2
oplot,y,e_bin06_photon(hdr.crpix1-1,*)
oplot,y,e_bin12_photon(hdr.crpix1-1,*)
oplot,y,e_bin36_photon(hdr.crpix1-1,*)
!p.multi=0

record_gif,'/data1/work/image.processing/',suffix+'.relerr.time-binned.photon.gif'
return

stop


eit_colors,band
window,xs=1024,ys=1024
tvscl,alog10(rebin(reform(e_single_photon(0,*,*)),512,512)>.001),0
tvscl,alog10(rebin(e_bin06_photon,512,512)>.001),1
tvscl,alog10(rebin(e_bin12_photon,512,512)>.001),2
tvscl,alog10(rebin(e_bin36_photon,512,512)>.001),3
record_gif,'/data1/work/image.processing/',suffix+'.relerr.time-binned.photon.gif'

stop

end

pro update_header,header
  if header.wavelnth eq  94 then begin
     D_CRPIX1  = -1
     D_CRPIX2  =  0
     PLTSCLFCT =  1.
  endif
  if header.wavelnth eq 131 then begin
     D_CRPIX1  = -3
     D_CRPIX2  =  0
     PLTSCLFCT =  1.
  endif
  if header.wavelnth eq 171 then begin
     D_CRPIX1  = -2
     D_CRPIX2  = -1
     PLTSCLFCT =  1.
  endif
  if header.wavelnth eq 193 then begin
     D_CRPIX1  = -2.25
     D_CRPIX2  =  1
     PLTSCLFCT =  1.
  endif
  if header.wavelnth eq 211 then begin
     D_CRPIX1  = -3.5
     D_CRPIX2  =  1
     PLTSCLFCT =  1.
  endif
  if header.wavelnth eq 335 then begin
     D_CRPIX1  = -4
     D_CRPIX2  = -1.5
     PLTSCLFCT =  1.
  endif
  if header.wavelnth eq 304 then begin
     D_CRPIX1  = -0.5
     D_CRPIX2  =  0
     PLTSCLFCT =  1.
  endif
  header.crpix1=header.crpix1+D_CRPIX1
  header.crpix2=header.crpix2+D_CRPIX2
  header.cdelt1=header.cdelt1*PLTSCLFCT
  header.cdelt2=header.cdelt2*PLTSCLFCT
return
end

pro test_changes

 PROCESS_DATA_AIA_LEV10,'CR2097/',/letitbe,suffix='.test'

 

return
end

;data_aia,'/Storage1TB/tomography/DATA/aia/CR2100/171/'
pro data_aia,dir

filenames=findfile(dir)

if n_elements(filenames) eq 0 then begin
 print,'NO FILES TO READ!'
 stop
endif

testfiles = where(strmid(filenames,0,8) eq 'aia_test')
if testfiles(0) eq -1 then begin
 print,'NO TEST FILES TO READ!'
 stop
endif

oldfilenames=filenames(testfiles)
N=fix(n_elements(oldfilenames))
newfilenames=strarr(N)

for i=0,N-1 do begin
 oldfilename=oldfilenames(i)
 date=strmid(oldfilename,19,4)+strmid(oldfilename,24,2)+strmid(oldfilename,27,2)
 time=strmid(oldfilename,30,4)
 misterioussuffix=strmid(oldfilename,48,6)
 band=strmid(oldfilename,36,5)
 newfilenames(i)=date+'.'+time+'.'+band+'.'+misterioussuffix+'.fts'
endfor

for i=0,N-1 do begin
 read_sdo,dir+oldfilenames(i),header,image
 MWRITEFITS,header,image,outfile=dir+newfilenames(i)
endfor

return
end

pro standard_names
   dir='/Storage1TB/tomography/DATA/aia/test/'
subdir=['094/','131/','171/','193/','211/','335/','304/']
list ='list.txt'
listb='list-b.txt'
filename=''
n=0
for ib=0,6 do begin
openr,1,dir+subdir[ib]+list
openw,2,dir+subdir[ib]+listb
 readf,1,n
printf,2,n
for i=0,n-1 do begin
readf,1,filename
mreadfits,dir+subdir[ib]+filename,header,image
newfilename=strmid(header.DATE_OBS, 0,4)+strmid(header.DATE_OBS, 5,2)+strmid(header.DATE_OBS, 8,2)+'.'+$
            strmid(header.DATE_OBS,11,2)+strmid(header.DATE_OBS,14,2)+$
            '.Z.'+strmid(subdir[ib],0,3)+'.lev1p5-b.fts'
;printf,2,newfilename
;mwritefits,header,image,outfile=dir+subdir[ib]+newfilename
endfor
close,2
close,1
endfor
stop
return
end

pro process_data_aia_lev15
;* Correct CDELT and CRPIX.
;* Also give to Rich tables with CDELT, CRPIX, Ck0.
     dir='/Storage1TB/tomography/DATA/aia/test/'
  subdir=['094/','131/','171/','193/','211/','335/','304/']
   list  ='list.'+strmid(subdir,0,3)+'.sub-selected'
   listpp='list.'+strmid(subdir,0,3)+'.processed'
filename=''
       n=0
for ib=5,5 do begin
 openr,1,dir+subdir[ib]+list[ib]
 readf,1,n
 openw,2,dir+subdir[ib]+listpp[ib]
printf,2,n
 for i=0,n-1 do begin
;for i=0,1 do begin
readf,1,filename
mreadfits,dir+subdir[ib]+filename,header,image

newfilename=strmid(header.DATE_OBS, 0,4)+strmid(header.DATE_OBS, 5,2)+strmid(header.DATE_OBS, 8,2)+'.'+$
            strmid(header.DATE_OBS,11,2)+strmid(header.DATE_OBS,14,2)+$
            '.Z.'+strmid(subdir[ib],0,3)+'.lev1p5'

; Normalize by exposure time
image = image / header.EXPTIME
newfilename=newfilename+'.ETN'

; Divide by Ck0
divide_by_ck0,image,header,newimage
image=newimage
newfilename=newfilename+'.Norm-Ck0'
; Make -999 all negative pixels
p=where(image lt 0.)
if p(0) ne -1 then image(p)=-999.

; Update header, hard-coded corrections for centering info. 
; This is only needed for certain SYNOPTIC data periods, and the
; corrections should be specifically quantified for each data set
; using my aia_proc.pro procedure.
;
; update_header,header
; newfilename=newfilename+'.Uhdr'

; Finish making newfilename, write new image, and print newlist
newfilename=newfilename+'.fts'
mwritefits,header,image,outfile=dir+subdir[ib]+newfilename   
printf,2,newfilename

endfor
close,1
close,2
endfor

return
end

