function r2_fun,y,yfit
;calcula r2
if n_elements(y) ne n_elements(yfit) then stop
  SStot = total( (y-     mean(y))^2 )
  SSerr = total( (y-yfit  )^2 )
  r2    = 1.-SSerr/SStot
  return,r2
end
