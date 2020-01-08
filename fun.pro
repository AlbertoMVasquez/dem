FUNCTION FUN,x
;funciona para colortable 12                                                                                                                                                                                                                  
  case x of
     0: y = 100               ;azul
     1: y = 200               ;rojo
      2: y = 120               ;violeta
      3: y =  20               ;verde
      4: y =  90               ;cyan
      5: y = 130              ;fuccia                                                                                                                                                                                                        
   endcase
   return,y
END
