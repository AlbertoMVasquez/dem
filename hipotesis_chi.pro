FUNCTION HIPOTESIS_CHI, Y, YFIT, error_y=error_y
;-
; ----------------------------------------------------------

; options for compilation (recommended by RSI)
  COMPILE_OPT idl2

; watch out for errors
  on_error,2

; ---------------------------------------------------------
; Check arguments

  n = N_ELEMENTS(y)
  if (n le 3) then begin
      print,'** Insufficient data in LINCCORR'
      return, -1
  endif

  ny = N_ELEMENTS(y)
  if (ny ne n) then begin
      print,'** X and Y arrays are not the same size in LINCORR'
      return, -1
  endif

; ---------------------------------------------------------
; Main routine

; compute the statistic
;chi = sum [((y -mx-b)/sigma)^2]
  chi = total( ((Y-Yfit)/error_y)^2 )
;  chi = total( ( (Y-Yfit)^2 )/yfit )

; compute the degrees of freedom
;como yfit=mx+b, definir m y b consume 2 grados de libertad
  df = n - 2

  if (chi gt 500.) then begin

      p = 0.0

 endif else begin

; calculate one-side 'tail area' probability 

        p =  (1.0 - CHISQR_PDF(chi, df))

  endelse

; ---------------------------------------------------------
; Return to user
  return, [p,chi]

END
