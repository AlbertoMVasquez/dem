pro read_trace_diego,file,alturas,dir=dir
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
  common B_sampled,B_v,Br_v,Bth_v,Bph_v
  common opclstatus,opcls,loopL,WTc
;+                                                                                                                                                                                                                ;
;PURPOSE:                                                                                                                                                                                                        ;
;To read the output of the routine "trace_LDEM"
;
  Nlin=0L
  Npts_max=0.
  fieldtype=0.
  spacing =0.
  radstart=fltarr(alturas)
  Rmax_tom=0.
  dr_tom=0.
  WTc=0.
if not keyword_set(dir) then dir='/data1/work/dem/github_dem/dem/';  dir='/data1/DATA/MLDT/'
  openr,1,dir+file
  readu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
  readu,1,Nlin,Npts_max
      Ne_v = fltarr(Npts_max,Nlin)
      Tm_v = fltarr(Npts_max,Nlin)
      WT_v = fltarr(Npts_max,Nlin)
      Er_v = fltarr(Npts_max,Nlin)
  scoreR_v = fltarr(Npts_max,Nlin)
     rad_v = fltarr(Npts_max,Nlin)
     lat_v = fltarr(Npts_max,Nlin)
     lon_v = fltarr(Npts_max,Nlin)
       s_v = fltarr(Npts_max,Nlin)
       B_v = fltarr(Npts_max,Nlin)
      Br_v = fltarr(Npts_max,Nlin)
     Bth_v = fltarr(Npts_max,Nlin)
     Bph_v = fltarr(Npts_max,Nlin)

 midCell_v = fltarr(Nlin)
    Npts_v = fltarr(Nlin)
     opcls = intarr(Nlin)
     Loopl = fltarr(Nlin)
     str_v = fltarr(Nlin)
    stth_v = dblarr(Nlin)
    stph_v = dblarr(Nlin)

  readu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
readu,1,Ne_v,Tm_v,WT_v,Er_v,scoreR_v
  readu,1,str_v,stth_v,stph_v
  readu,1,B_v,Br_v,Bth_v,Bph_v
;------AGREGADO--------------
  enrad_v = fltarr(Nlin)
   enlon_v = fltarr(Nlin)
   enlat_v = fltarr(Nlin)
;------AGREGADO--------------
   readu,1,enrad_v,enlon_v,enlat_v
;----------------------------
   Tmin=0.
    Tmax=0.
    L=0
 readu,1,Tmin,Tmax,L
    npar   = 0   
    DEMc_v = fltarr(Npts_max,Nlin)
 readu,1,npar,DEMc_v
;stop
; lambda_v = fltarr(Npts_max,Nlin,npar)
; readu,1,lambda_v
  close,1

  return
end


