
    plot,t_x/1.e6,ldem_array(0,*),xr=[0.5,4.0],yrange=[0,max(ldem_array(p,0:200))],charsize=2,ystyle=1,xstyle=1,$
                  xtitle='Te [MK]',background=255,color=0,/nodata
    for i=1,Np-1 do oplot,t_x/1.e6,ldem_array(p(i),*),color=0
    loadct,12
    oplot,t_x/1.e6,ldem_array(0,*),th=4,color=200
    loadct,0
    record_gif,'./newfigs/',output_file

