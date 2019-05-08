pro cart_to_sphcoord,V,sphcoord
;transforma x,y,z en r,th,ph
;V es un vector de 3 componentes x,y,z  
;sphcoord es el vector salida de 3 componentes r,th,ph
  x=V[0]*1d
  y=V[1]*1d
  z=V[2]*1d
  r  = sqrt(x^2+y^2+z^2)
  th = acos(z/r)
; devuelve siempre un valor de ph entre 0 y 2*!pi
; x = 3. & r = x/cos(30.*!dtor) & y = r*sin(30.*!dtor)
; x = +abs(x) & y = +abs(y) 
  ph =          atan(y/x)
  if x gt 0 and y lt 0. then ph = 2*!dpi + atan(y/x)
  if x lt 0             then ph =   !dpi - atan(y/abs(x))
  sphcoord=[r,th*180/!pi,ph*180/!pi]
  return
end
