
 .r mldt_diego_v2.pro
 test,ind=[1],filelabel='crap-diego'

 window,2
 rr=1.+.2*findgen(100)/99.

   rminhs = min(rad_l2) 
  rmaxhs = max(rad_l2)
  ;HS-fit to Ne(r)
  xfit = rad_l2
  yfit =  Ne_l2
 fithslinear,xfit,yfit,rminhs,rmaxhs,A,corr2
       Ne0(ileg+1) = a[0] 
  lambda_N(ileg+1) = 1./A[1]  ; Rsun
              Tfit = mu * mH * gsun * (lambda_N(ileg+1)*rsun) / kB
     Tefit(ileg+1) = bb*Tfit
     r2N  (ileg+1) = corr2  

  window,2
  plot,rad_l2,ne_l2,psym=4
  oplot,rr,Ne0(ileg+1) *  exp( -(1./lambda_N(ileg+1)) *(rr-1.)/rr )
  print,corr2

  rminhs = min(rad_l2) 
  rmaxhs = max(rad_l2)
  ;HS-fit to Ne(r)
  xfit = rad_l2
  yfit =  Ne_l2
 fiths,xfit,yfit,rminhs,rmaxhs,A,corr2  
       Ne0(ileg+1) = a[0] 
  lambda_N(ileg+1) = 1./A[1]  ; Rsun
              Tfit = mu * mH * gsun * (lambda_N(ileg+1)*rsun) / kB
     Tefit(ileg+1) = bb*Tfit
     r2N  (ileg+1) = corr2  

    oplot,rr,Ne0(ileg+1) *  exp( -(1./lambda_N(ileg+1)) *(rr-1.)/rr ),linestyle=2,th=3
    print,corr2


