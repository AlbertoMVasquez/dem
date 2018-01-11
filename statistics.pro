
     openw,1,'./newfigs/statistics.txt',/append
     printf,1,output_file
     printf,1,'=================================================================================='
     printf,1,'               Ne1² [cm-6]  Ne2² [cm-6]     Tm1 [K]      Tm2 [K]' 
     printf,1,'  base:      ',Ne1_array(0)^2,Ne2_array(0)^2,Tm1_array(0),Tm2_array(0);,WT1_array(0)
     printf,1,'  mean:      ',mean(Ne1_array(p)^2),mean(Ne2_array(p)^2),$
           mean(Tm1_array(p)),mean(Tm2_array(p));,mean(WT1_array(p))
     printf,1,'  StDev/mean:',stdev(Ne1_array(p)^2)/mean(Ne1_array(p)^2),stdev(Ne2_array(p)^2)/mean(Ne2_array(p)^2),$
           stdev(Tm1_array(p))/mean(Tm1_array(p)),stdev(Tm2_array(p))/mean(Tm2_array(p));,stdev(WT1_array(p))/mean(WT1_array(p))
     printf,1,'=================================================================================='
     printf,1,''
     printf,1,''
     close,1
