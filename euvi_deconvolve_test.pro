;+
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
; euvi_deconvolve: This routine processes a list of EUVI raw images with
;                  several useful predefined options (see FLAGS section below).
;
; INPUTS
; listfile  : A string specifyig the name of a file where the filenames 
;             of all images to be processed are listed, oner per line.
;             All images *must* be of size 2048^2.
; directory : A string specifying the full path to where the data images
;             and the listfile are stored.
; nfiles    : An integer specifying the number of files to process, if not
;             provided only the first file in listfile will be processed.
; binfactor : An integer specifying the binfactor to be used if the
;             /REBIN flag is set. The Binfactor must be an integer
;             divisor of the EUVI image size (2048).
; outdir    : if set, output directory is different tha directory.
;
; FLAGS
; /decon    : If set the output image is PSF-deconvolved, as determined
;             by P. Shearer et al. (2012) ApJ, 749L, 8.
; /photons  : If set the output image units is PHOTONS.
; /dnsec    : If set the output image units is DN/SEC.
; /rebin    : If set the output image is rebinned by BINFACTOR.
; /despike  : If set the output image has cosmic rays removed.
; /rescale  : If set the output image is re-scaled by the (close to unity)
;             calibration factors determined from early mission data by
;             Frazin, Vasquez & Kamalabadi (2009), ApJ, 701, 547.
; /rot      : If set the output image is rotated with the North pole up.
; /normalize: For DEM-Tomography applications only. Please contact 
;             albert@iafe.uba.ar if interested. 
;
; OUTPUTS:  The processed images in FITS format, stored in the
;           specified DIRECTORY. The HEADER of each processed image
;           contains modified values of the variables 
;           CROTA, CRPIX, NAXIS, and CDELT,
;           accordingly to if the flags /ROT and/or /REBIN are set.
;           The processed images have filenames with suffixes describing
;           the full process they went through.
;           An ASCII file containing the list of rejected files, stating 
;           the reason for each rejection, is generated in the DIRECTORY.
;
; HISTORY
; A.M. Vasquez -  14 Jan 2014 - Version 1.0
;                 14 Mar 2014 - Version 1.1 - /NORMALIZE flag added,
;                 Bug fixed: /RESCALE was not activated in V 1.0.
;
; TO DO LIST
; Please suggest to A.M. Vasquez (albert@iafe.uba.ar)
;
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;-

pro euvi_deconvolve_test,DIRECTORY=DIRECTORY,LISTFILE=LISTFILE,BINFACTOR=BINFACTOR,NFILES=NFILES,$
                    REBIN=REBIN,DESPIKE=DESPIKE,RESCALE=RESCALE,ROT=ROT,DECON=DECON,$
                    PHOTONS=PHOTONS,DNSEC=DNSEC,NORMALIZE=NORMALIZE,OUTDIR=OUTDIR

; Read list of filenames to process.
if not keyword_set(directory) then directory='./'
if not keyword_set(nfiles)    then nfiles=1 ; will read only the FIRST file in the list by default.
if not keyword_set(outdir)    then outdir=directory

filenames = strarr(nfiles)
tmp = ''
openr,1,directory+listfile
for i=0,nfiles-1 do begin
   readf,1,tmp
   filenames[i]=tmp
endfor
close,1

openw, 1,directory+'list_of_rejected_files.out',/append
printf,1,'Filename                   Status/Reason'

; Read PSFs into memory
; PSF Blind Deconvolution: USES the PSF determined by 
; by P. Shearer et al. (2012) ApJ, 749L, 8.
if keyword_set(decon) then begin
;Albert's attempt to tweak code according to Sam's instructions:
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_171_B_2048.fts'),psf_171B_header,psf_171B
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_195_B_2048.fts'),psf_195B_header,psf_195B
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_284_B_2048.fts'),psf_284B_header,psf_284B
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_304_B_2048.fts'),psf_304B_header,psf_304B
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_171_A_2048.fts'),psf_171A_header,psf_171A
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_195_A_2048.fts'),psf_195A_header,psf_195A
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_284_A_2048.fts'),psf_284A_header,psf_284A
  mreadfits,concat_dir('$EUVDECONPAK_DBASE','psf_304_A_2048.fts'),psf_304A_header,psf_304A
 ;mreadfits,'/data1/work/psfs/psf_171_B_2048.fts',psf_171B_header,psf_171B
 ;mreadfits,'/data1/work/psfs/psf_195_B_2048.fts',psf_195B_header,psf_195B
 ;mreadfits,'/data1/work/psfs/psf_284_B_2048.fts',psf_284B_header,psf_284B
 ;mreadfits,'/data1/work/psfs/psf_304_B_2048.fts',psf_304B_header,psf_304B
 ;mreadfits,'/data1/work/psfs/psf_171_A_2048.fts',psf_171A_header,psf_171A
 ;mreadfits,'/data1/work/psfs/psf_195_A_2048.fts',psf_195A_header,psf_195A
 ;mreadfits,'/data1/work/psfs/psf_284_A_2048.fts',psf_284A_header,psf_284A
 ;mreadfits,'/data1/work/psfs/psf_304_A_2048.fts',psf_304A_header,psf_304A
endif

for i=0,nfiles-1 do begin
if keyword_set(DNSEC)   then begin
; This code makes array IMAGE, where each pixel is in [DN/DEC].
; REMOVE /NORMAL_OFF if scale up to OPEN is desired here.
;if NOT keyword_set(despike) then begin
   secchi_prep,directory+filenames[i],hdr,image,/DN2P_OFF,/CALIMG_OFF,/NORMAL_OFF
   print,'hdr:',hdr.crpix1,median(image),mean(image),stdev(image),hdr.exptime
   hdr0=hdr
;endif 
 if keyword_set(despike) then begin
  ; TNV values as recommended by Jean Pierre Wulsen
    secchi_prep,directory+filenames[i],hdr,image,/DN2P_OFF,/CALIMG_OFF,/NORMAL_OFF,/EXPTIME_OFF
    print,'hdr: ',hdr.crpix1,median(image),mean(image),stdev(image),hdr.exptime
    tnv=8    
   if hdr.wavelnth eq 284 then tnv=4
 ; data benefits from several passes of despiking
   for ii=1,3 do image=despike_gen(image,tn=tnv,/low3)
   image=euvi_correction(image,hdr,/DN2P_OFF,/CALIMG_OFF,/NORMAL_OFF) ; exposure normalization here
   print,'hdr: ',hdr.crpix1,median(image),mean(image),stdev(image),hdr.exptime
   hdr=hdr0
 endif
endif

if keyword_set(PHOTONS) then begin
; This code makes array IMAGE2, where each pixel is in [PHOTONS].
 if NOT keyword_set(despike) then begin
   secchi_prep,directory+filenames(i),hdr,image,/EXPTIME_OFF,/CALIMG_OFF,/NORMAL_OFF
 endif
 if keyword_set(despike) then begin
 ; TNV values as recommended by Jean Pierre Wulsen
   secchi_prep,directory+filenames(i),hdr,image,/DN2P_OFF,/CALIMG_OFF,/NORMAL_OFF,/EXPTIME_OFF
   tnv=8
   if hdr.wavelnth eq 284 then tnv=4
 ; 284 band data benefits from several passes of despiking
   for ii=1,3 do image=despike_gen(image,tn=tnv,/low3)
   image=euvi_correction(image,hdr,/EXPTIME_OFF,/CALIMG_OFF,/NORMAL_OFF) ; change to PHOTONS here
 endif
endif

flag=0 ; to accept file flag must be zero

; Test data for several problems, and set up FLAG if some problem arises
if (hdr.filter ne 'OPEN') AND (hdr.filter ne 'S1') then flag=3
if hdr.NAXIS1 ne hdr.NAXIS2                        then flag=4
if (flag eq 0) AND (hdr.NAXIS1 ne 2048)            then flag=5
; Missing blocks: Discard images for which more than 50% of the data
; is missing.
imissing = where(image eq hdr.blank or finite(image) eq 0)
if flag eq 0 AND (hdr.nmissing ne 0 OR imissing(0) ne -1) then begin
   npmb  = float(n_elements(imissing))
   ndata = float(hdr.NAXIS1) * float(hdr.NAXIS2)
   if npmb/ndata ge 0.50                           then flag=6
endif

; Only process data without problems.
if flag eq 0 then begin

if keyword_set(decon) then begin 
 if hdr.obsrvtry eq 'STEREO_B' then begin
    if hdr.wavelnth eq 171 then image = euvi_decon( image , psf_171B )
    if hdr.wavelnth eq 195 then image = euvi_decon( image , psf_195B )
    if hdr.wavelnth eq 284 then image = euvi_decon( image , psf_284B )
    if hdr.wavelnth eq 304 then image = euvi_decon( image , psf_304B )   
 endif
 if hdr.obsrvtry eq 'STEREO_A' then begin
    if hdr.wavelnth eq 171 then image = euvi_decon( image , psf_171A )
    if hdr.wavelnth eq 195 then image = euvi_decon( image , psf_195A )
    if hdr.wavelnth eq 284 then image = euvi_decon( image , psf_284A )
    if hdr.wavelnth eq 304 then image = euvi_decon( image , psf_304A )   
 endif
endif

;Set missingblocks to "hugenegnum".
 hugenegnum=-1.e20
 if imissing(0) ne -1 then image(imissing)=hugenegnum
 ; Rotate image so that the POSITION ANGLE of the NORTH POLE is ZERO. 
 ; The PIVOTAL point (the disk center) is changed from FITS to IDL indexing convention.
 if keyword_set(rot) then begin
 ANGLE     = -hdr.crota
;image     = rot(image,ANGLE,1,hdr.crpix1-1,hdr.crpix2-1,/pivot,missing=hugenegnum)
 hdr.crota = 0.
 rotstring='rotat'
 endif
 if not keyword_set(rot) then rotstring='NOROT'

newfilename=strmid(filenames(i),0,22)+hdr.filter+'.'+strmid(string(fix(hdr.WAVELNTH)),5,3)+'.'+rotstring

if keyword_set(rebin) then begin
if keyword_set(binfactor) eq 0 then begin
   print,'You asked for rebinning, please re-run setting up the BINFACTOR value.'
   return
end
if hdr.NAXIS1 MOD binfactor ne 0 then begin
   print,'NAXIS1/BINFACTOR must be an integer, please re-run with a proper BINFACTOR.'
   return
end
; Rebin image:
image=rebin(image,hdr.NAXIS1/binfactor,hdr.NAXIS2/binfactor)
; Adjust accordingly keywords: NAXIS, CRPIX, CDELT.
; Note that the FITS indexing convention for CRPIX is to start with "1".
; The *PRECISE* conversions are then:
; NAXIS'               = NAXIS/BINFACTOR.
; (CRPIX'-1)/(CRPIX-1) = (NAXIS'-1)/(NAXIS-1).
; CDELT'               = CDELT*BINFACTOR. 
hdr.CRPIX1 = 1 + (hdr.CRPIX1 -1) * (hdr.NAXIS1/binfactor - 1) / (hdr.NAXIS1-1)
hdr.CRPIX2 = 1 + (hdr.CRPIX2 -1) * (hdr.NAXIS2/binfactor - 1) / (hdr.NAXIS2-1)
hdr.CRPIX1A= 1 + (hdr.CRPIX1A-1) * (hdr.NAXIS1/binfactor - 1) / (hdr.NAXIS1-1)
hdr.CRPIX2A= 1 + (hdr.CRPIX2A-1) * (hdr.NAXIS2/binfactor - 1) / (hdr.NAXIS2-1)
hdr.NAXIS1 = hdr.NAXIS1 /binfactor
hdr.NAXIS2 = hdr.NAXIS2 /binfactor
hdr.CDELT1 = hdr.CDELT1 *binfactor
hdr.CDELT2 = hdr.CDELT2 *binfactor
hdr.CDELT1A= hdr.CDELT1A*binfactor
hdr.CDELT2A= hdr.CDELT2A*binfactor
if hdr.NAXIS1 ge 1000 then newfilename=newfilename+'.'+strmid(hdr.naxis1,4,4)
if hdr.NAXIS1 lt 1000 then newfilename=newfilename+'.'+strmid(hdr.naxis1,5,3)
endif

if keyword_set(DNSEC)   then newfilename=newfilename+'.DNSEC'
if keyword_set(PHOTONS) then newfilename=newfilename+'.PHOTO'
if keyword_set(despike) then newfilename=newfilename+'.DSPK'

;--------------------------------------------------------------------
; Make missing data -999.
; Numbers below threshold are missing block or missing block spread
; after rotation or binning, make them -999.
threshold=-1.e5
p=where(image lt threshold)
if p(0) ne -1 then image(p)=-999.
;--------------------------------------------------------------------

if keyword_set(rescale) then begin
 rescale,image,hdr,newimage,/euvi
 image=newimage
 newfilename=newfilename+'.RS'
endif

if keyword_set(normalize) then begin
 divide_by_ck0,image,hdr,/euvi
 newfilename=newfilename+'.Norm-Ck0'
endif

if keyword_set(decon) then newfilename = newfilename+'.DECON'
newfilename=newfilename+'.fts'
hdr.filename=newfilename

newfilename=outdir+newfilename

print,newfilename

MWRITEFITS, hdr,image, outfile=newfilename

endif else begin
 if flag eq  3 then reason='  Filter not OPEN or S1'
 if flag eq  4 then reason='  NOT square'
 if flag eq  5 then reason='  Square, but NOT 2048^2'
 if flag eq  6 then reason='  +50% of pixels are missing data'
printf,1,filenames(i),reason
endelse
;stop
endfor ; next image
close,1
print,'All your data has been processed.'
return
end

FUNCTION euvi_decon, image, psf
; IDL code for deconvolution function, by P. SHEARER and A.M. VASQUEZ.
; input: psf, image
; output: image1 = deconned image
  psf_size = (size(psf)  ) (1)
  img_size = (size(image)) (1)
  ;--- Configure PSF for deconvolution, including padding and FFT 
  psf_cent = floor(psf_size/2)                    ; psf is, unsurprisingly centered at image center
  psf_pad = dblarr(2 * img_size, 2 * img_size)    ; pad to avoid edge artifacts
  psf_pad(0: img_size - 1, img_size: *) = psf     ; insert psf into pad
  psf_pad = shift(psf_pad, psf_cent * [-1, 1])    ; shift PSF for correct FFT result
  psf_hat = fft(psf_pad, 1)                       ; compute FFT'ed psf
  ;--- Prepare image and deconvolve
  img_pad = dblarr(2 * img_size, 2 * img_size)    ; image is padded as well
  img_pad(0: img_size - 1, img_size: *) = image   ; insert image into padded array
  img_hat = fft(img_pad, 1)                       ; compute FFT'ed padded image
  image1 = real_part(fft(img_hat/psf_hat, -1))    ; Deconvolve and apply inverse FFT
  image1 = image1(0: img_size - 1, img_size: *)   ; Remove padding
return, image1
end

pro rescale,image,hdr,newimage,eit=eit,euvi=euvi
; This routine multiplies all EUVI images by corrective constant 
; calibration factors determined for each band and both spacecraft by
; Frazin, Vasquez & Kamalabadi (2009), ApJ, 701, 547.
  if keyword_set(euvi) then begin
  if hdr.obsrvtry eq 'STEREO_A' then begin
  if hdr.wavelnth eq 171 then Amp=1.00010 ;1.00007
  if hdr.wavelnth eq 195 then Amp=1.02031 ;1.01898
  if hdr.wavelnth eq 284 then Amp=1.03499 ;1.03366
  if hdr.wavelnth eq 304 then Amp=0.99994 ;0.99956
  endif
  if hdr.obsrvtry eq 'STEREO_B' then begin
  if hdr.wavelnth eq 171 then Amp=0.999904 ;0.99993
  if hdr.wavelnth eq 195 then Amp=0.980481 ;0.98172
  if hdr.wavelnth eq 284 then Amp=0.967301 ;0.96847
  if hdr.wavelnth eq 304 then Amp=1.000060 ;1.00045
  endif
  endif
  newimage=image*Amp
  ;Preserve "-999" pixels in newimage
  p=where(image eq -999.)
  if p(0) ne -1 then newimage(p)=-999.
  return
end

pro divide_by_ck0,image,hdr,euvi=euvi
if keyword_set(euvi) then begin
  Omega_p_A=5.92553e-11
  Omega_p_B=5.94216e-11
  if hdr.filter eq 'OPEN' then begin
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 171 then PHI0=2.8326392e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 195 then PHI0=1.4195491e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 284 then PHI0=2.8273739e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 304 then PHI0=4.2421336e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 171 then PHI0=2.4296904e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 195 then PHI0=1.2297586e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 284 then PHI0=2.4906123e-2 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 304 then PHI0=4.4347962e-2 * Omega_p_B
  endif
  if hdr.filter eq 'S1' then begin
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 171 then PHI0=1.3240548e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 195 then PHI0=0.6640560e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 284 then PHI0=0.8998714e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 304 then PHI0=1.2756461e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 171 then PHI0=1.1928132e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 195 then PHI0=0.5919602e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 284 then PHI0=0.8134195e-2 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 304 then PHI0=1.3539108e-2 * Omega_p_B
  endif
  if hdr.filter ne 'OPEN' and hdr.filter ne 'S1' then begin
  print,'--! Filter position not identified !--'
  return
  endif
endif
Ck0=PHI0*1.e12
image=image/Ck0
return
end
