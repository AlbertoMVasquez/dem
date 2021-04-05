



;trace_struct_cr2219_demt_awsomfield_startp6rsun.sav

pro estadistica_diego_2020


  restore,'trace_struct_cr2223_demt_awsomfield_cocen_startp6rsun_ajuste_alto.sav'
  demt2223 = datos

  restore,'trace_struct_cr2223_awsom_awsomfield_cocen_startp6rsun_ajuste_alto.sav'
  awsom2223 = datos

;chequeo de datos

  

stop
ok_open1  = where(demt2223.opclstat  eq 0. and demt2223.gradt_erry   ne -555. and demt2223.lincorr_pearson_t   ge 0.5 )
ok_open1  = where(awsom2223.opclstat eq 0. and awsom2223.gradt_erry  ne -555. and awsom2223.lincorr_pearson_t  ge 0.5 )

suf='testeo'
  histoplot, demt2223.Er_prom(ok_open1),data2=awsom2223.Er_prom(ok_open2),win=1,tit='CR-2223 - Type 0',xtit='Er_prom' ,filename='histo'+suf+'Er_prom',$
             label1='DEMT',label2='AWSoM',min=.5,max=2.,color=[0,0],linestyle=[0,2]

  histoplot, demt2223.Ec_prom(ok_open1),data2=awsom2223.Ec_prom(ok_open2),win=1,tit='CR-2223 - Type 0',xtit='Ec_prom' ,filename='histo'+suf+'Er_prom',$
             label1='DEMT',label2='AWSoM',min=.5,max=2.,color=[0,0],linestyle=[0,2]



  
  return
end
