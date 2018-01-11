pro wrapper

  errorbox_fbe,'x_AIA.CR2099.171.nr26.irm1.26_ureg',0.1
  errorbox_fbe,'x_AIA.CR2099.193.nr26.irm1.26_ureg',0.1
  errorbox_fbe,'x_AIA.CR2099.211.nr26.irm1.26_ureg',0.1
  errorbox_fbe,'x_AIA.CR2099.335.nr26.irm1.26_ureg',0.1
  return
  errorbox_fbe,'x_AIA.CR2099.171.nr26.irm1.26_base',0.1,/base
  errorbox_fbe,'x_AIA.CR2099.193.nr26.irm1.26_base',0.1,/base
  errorbox_fbe,'x_AIA.CR2099.211.nr26.irm1.26_base',0.1,/base
  errorbox_fbe,'x_AIA.CR2099.335.nr26.irm1.26_base',0.1,/base

  errorbox_fbe,'x_AIA.CR2099.171.nr26.irm1.26_oreg',0.1
  errorbox_fbe,'x_AIA.CR2099.193.nr26.irm1.26_oreg',0.1
  errorbox_fbe,'x_AIA.CR2099.211.nr26.irm1.26_oreg',0.1
  errorbox_fbe,'x_AIA.CR2099.335.nr26.irm1.26_oreg',0.1
  return
end

pro errorbox_fbe,fbe_file,Error,base=base
  
  nr=26
  nth=90
  np=180
  fbe=fltarr(nr,nth,np)

  dir='/data1/tomography/bindata/'
  openr,1,dir+fbe_file
  readu,1,fbe
  close,1
  
  band=strmid(fbe_file,13,3)
  factor= 0.
  ;======================================
  ; solo vale para CR-2099
  ; chequear con comparison.pro
  if band eq '171' then  factor=1.1789729    
  if band eq '193' then  factor=1.1929723   
  if band eq '211' then  factor=1.0572914  
  if band eq '335' then  factor=1.2016495   
  ;======================================
  if factor eq 0. then stop
  
  fbe=fbe/factor
  fbe_h=fbe*(1.+Error)
  fbe_l=fbe*(1.-Error)
  
if keyword_set(base) then begin
   openw,1,dir+fbe_file+'.G'
   writeu,1,fbe
   close,1
endif else  begin  
   openw,1,dir+fbe_file+'.H'
   writeu,1,fbe_h
   close,1

   openw,1,dir+fbe_file+'.L'
   writeu,1,fbe_l
   close,1
endelse



  return
end


