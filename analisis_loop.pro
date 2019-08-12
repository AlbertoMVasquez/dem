N_e usualmente yy= alog(y) xx = 1/x

A = linfit(xx,yy,prob=prob1,/double)
salidafit =  A[0]+A[1]*xx
r2 = r2_fun(yy,salidafit)


err_y = yy * 0.1 ;10%
B = linfit(xx,yy,prob=prob2,/double,measure_errors=err_y)
salidafit =  B[0]+B[1]*xx
r2 = r2_fun(yy,salidafit)


C = ladfit(xx,yy,absdev,/double)
salidafit =  C[0]+C[1]*xx
r2 = r2_fun(yy,salidafit)


D = theil_sen2(xx,yy)
salidafit =  D[0]+D[1]*xx
r2 = r2_fun(yy,salidafit)



en el ploteo de x e y 
exp(A[0]+A[1])*exp( -A[1]*(1. - 1./xx) )



sta1 = lincorr(xx,yy,/double,T_STAT=t1)

sta2 = hipotesis_chi(yy,salidafit,error_y)
