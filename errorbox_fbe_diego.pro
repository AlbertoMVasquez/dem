pro wrapper1

;  error_eit = 0.15
  error_eit = 0.075

  errorbox_fbe,'x_eit.171.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.35_ureg'       ,error_eit   ;El string son los FBE que salen de los callsolve
  errorbox_fbe,'x_eit.195.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.35_ureg'       ,error_eit
  errorbox_fbe,'x_eit.284.cr1915.26x90_bf4_ri.98_ro1.025_b4_masked_l0.35_ureg',error_eit
  
  errorbox_fbe,'x_eit.171.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75'            ,error_eit,/base
  errorbox_fbe,'x_eit.195.cr1915.26x90_bf4_ri.98_ro1.025_b4_l0.75'            ,error_eit,/base
  errorbox_fbe,'x_eit.284.cr1915.26x90_bf4_ri.98_ro1.025_b4_masked_l0.75'     ,error_eit,/base

  errorbox_fbe,'x_eit.171.cr1915.26x90_bf4_ri.98_ro1.025_b4_l1.15_oreg'       ,error_eit
  errorbox_fbe,'x_eit.195.cr1915.26x90_bf4_ri.98_ro1.025_b4_l1.15_oreg'       ,error_eit
  errorbox_fbe,'x_eit.284.cr1915.26x90_bf4_ri.98_ro1.025_b4_masked_l1.15_oreg',error_eit

  return
end

pro wrapper2

  error_euvi = 0.075

  errorbox_fbe,'x_euvi.A.171.cr2081.26x90_bf4_ri.98_ro1.025_l0.35_NODECON_ureg' ,error_euvi   ;El string son los FBE que salen de los callsolve
  errorbox_fbe,'x_euvi.A.195.cr2081.26x90_bf4_ri.98_ro1.025_l0.35_NODECON_ureg' ,error_euvi
  errorbox_fbe,'x_euvi.A.284.cr2081.26x90_bf4_ri.98_ro1.025_l0.35_NODECON_ureg' ,error_euvi

  errorbox_fbe,'x_euvi.A.171.cr2081.26x90_bf4_ri.98_ro1.025_l0.75_NODECON_nalai',error_euvi,/base
  errorbox_fbe,'x_euvi.A.195.cr2081.26x90_bf4_ri.98_ro1.025_l0.75_NODECON_nalai',error_euvi,/base
  errorbox_fbe,'x_euvi.A.284.cr2081.26x90_bf4_ri.98_ro1.025_l0.75_NODECON_nalai',error_euvi,/base

  errorbox_fbe,'x_euvi.A.171.cr2081.26x90_bf4_ri.98_ro1.025_l1.15_NODECON_oreg' ,error_euvi
  errorbox_fbe,'x_euvi.A.195.cr2081.26x90_bf4_ri.98_ro1.025_l1.15_NODECON_oreg' ,error_euvi
  errorbox_fbe,'x_euvi.A.284.cr2081.26x90_bf4_ri.98_ro1.025_l1.15_NODECON_oreg' ,error_euvi

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
  
  fbe_h=fbe*(1.+Error)
  fbe_l=fbe*(1.-Error)
                                ;OBS: 0.75 agregado en el file name el
                                ;dia 20/07/2016 para correr eit con el
                                ;mismo error que euvi para paper diego.
if keyword_set(base) then begin
   openw,1,dir+fbe_file+'075.G'
   writeu,1,fbe
   close,1
endif else  begin  
   openw,1,dir+fbe_file+'075.H'
   writeu,1,fbe_h
   close,1

   openw,1,dir+fbe_file+'075.L'
   writeu,1,fbe_l
   close,1
endelse

  return
end


