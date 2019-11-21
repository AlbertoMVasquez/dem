.r /data1/tomography/SolarTom_idl/xread.pro
.r mapa_perfil.pro

rads = [1.025,1.065,1.105,1.155,1.205]
fileA='Ne_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'
fileB='Ne_awsom_2082_1.85_short'
fileC='R_CR2082_DEMT-EUVI_behind_H1-L.35.2.3_r3d'

mapa_perfil,fileA,fileB=fileB,fileC=fileC,win=1,unit=1.e8,rads=rads
;repetir lo de arriba para la velocidad radial de awsom.

ESTADISTICA_DIEGO,/paper,/up
