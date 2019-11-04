pro linear_fit,xx,yy,A,r2,salidafit,xmin=xmin,xmax=xmax,linfit=linfit,ladfit=ladfit,robustfit=robustfit,theilsen=theilsen,xinverted=xinverted,linfit_err=linfit_err,err_y=err_y,prob=prob

  if keyword_set(xmin) and keyword_set(xmax) then begin
     if keyword_set(xinverted) then begin ;sirve cuando de entrada se da 1/x, esto lo solemos usar en ajusto HS de densidad.
        x_max = 1./xmin
        x_min = 1./xmax
     endif else begin
        x_max = xmax
        x_min = xmin
     endelse
     p=where(xx ge x_min and xx le x_max)
     xx=xx(p)
     yy=yy(p)
  endif
  prob1=0
  absdev=0
  if keyword_set (linfit)    then  A = linfit(xx,yy,prob=prob,/double)
  if keyword_set (linfit_err)then  A = linfit(xx,yy,prob=prob,/double,measure_errors=err_y)
  if keyword_set (ladfit)    then  A = ladfit(xx,yy,absdev=absdev,/double)
  if keyword_set (robustfit) then  A = robust_fit(xx,yy)
  if keyword_set (theilsen)  then  A = theil_sen2(xx,yy)
;obs: theilsen puede dar valores infinitos si xx tiene valores
;repetidos. Esto no suele pasar, pero a veces si se utiliza rad_l y
;este hace cosas raras entonces puede pasar. con s_l seguro que no.z
  salidafit =  A[0]+A[1]*xx          
  r2 = r2_fun(yy,salidafit)
;r2 puede dar valores negativos, excepto si la regrsion viene de
;cuadrados minimos. Pero con theil sen y outliers puede pasar y pasa.  
;  if keyword_set(prob) then A = [A,prob1]
  return
end
