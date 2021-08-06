!Fortran-for-pi_pi_stacking 

!This is a Fortran script handy for counting intramolecular pi_pi_stacking

PROGRAM PISTACKING

INTEGER :: isteps

INTEGER,PARAMETER :: n_steps=100001

INTEGER,PARAMETER :: nsteps_max=250000

DOUBLE PRECISION, DIMENSION(nsteps_max) :: X, Y, Z, ANG

INTEGER, DIMENSION(nsteps_max) :: cord


OPEN(UNIT=22,FILE='XXX',action='READ')

OPEN(UNIT=33,FILE='YYY',action='READ')

OPEN(UNIT=44,FILE='ZZZ',action='READ')

OPEN(UNIT=55,FILE='AAA',action='READ')

OPEN(UNIT=66,FILE='PPP') 

DO isteps=1,n_steps

  READ(22,*) X(isteps)
  
  READ(33,*) Y(isteps)
  
  READ(44,*) Z(isteps)
  
  READ(55,*) ANG(isteps)
  
  cord(1)=0
  
   IF(Z(isteps)<=0.4.AND.Z(isteps)>=0.33.AND.SQRT(X(isteps)**2+Y(isteps)**2)<=0.13.AND.ANG(isteps)<=40) THEN
   
   cord(isteps)=cord(isteps)+1 
   
   END IF
   
WRITE(66,*) cord(isteps)

END DO

close(unit=22)

close(unit=33)

close(unit=44)

close(unit=55)

close(unit=66)

END PROGRAM PISTACKING
