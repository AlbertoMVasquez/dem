pro transform_b_cart_to_sph,th,ph,B_cart,B_sph
                                ;theta phi y campob B en cartesianas
                                ;son entradas, mientras que B_sph salida
  sinth=sin(th)
  costh=cos(th)
  sinph=sin(ph)
  cosph=cos(ph)
  er =[sinth*cosph,sinth*sinph, costh]
  eth=[costh*cosph,costh*sinph,-sinth]
  eph=[-sinph     ,cosph      , 0    ]
  Br =total(B_cart*er )
  Bth=total(B_cart*eth)
  Bph=total(B_cart*eph)
  B_sph=[Br,Bth,Bph]
  return
end
