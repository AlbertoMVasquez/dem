pro linear_fit,xx,yy,xmin,xmax,A,r2,yfit,linfit=linfit,ladfit=ladfit,robustfit=robustfit,theilsen=theilsen
  p=where(xx ge xmin and xx le xmax)
  xx=xx(p)
  yy=yy(p)
  if keyword_set (linfit)    then  A = linfit(xx,yy)
  if keyword_set (ladfit)    then  A = ladfit(xx,yy,absdev=absdev)
  if keyword_set (robustfit) then  A = robust_fit(xx,yy)
  if keyword_set (theilsen)  then  A = theil_sen(xx,yy)

  yfit =  A[0]+A[1]*xx          
  meanyy= mean(yy)
  SStot = total( (yy-meanyy)^2 )
  SSerr = total( (yy-yfit  )^2 )
  r2    = 1.-SSerr/SStot
  return
end
