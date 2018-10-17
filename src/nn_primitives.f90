C FILE: FIB3.F
      SUBROUTINE FIB(A,N)
C
C     CALCULATE FIRST N FIBONACCI NUMBERS
C
      INTEGER N
      REAL*8 A(N)
      CHARACTER(len=4), dimension(3) :: hidden_activation
      CHARACTER(len=30) :: target_names
      namelist /smth/ hidden_activation
      namelist /smht/ target_names
Cf2py intent(in) n
Cf2py intent(out) a
Cf2py depend(n) a
      DO I=1,N
         IF (I.EQ.1) THEN
            A(I) = 0.0D0
         ELSEIF (I.EQ.2) THEN
            A(I) = 1.0D0
         ELSE 
            A(I) = A(I-1) + A(I-2)
         ENDIF
      ENDDO
       open(10,file='nml')
       read(10,nml=smth)
       write(*,nml=smth)
       read(10,nml=smht)
       write(*,nml=smht)
       write(*, *) hidden_activation(1)

       close(10)
       end
      END
C END FILE FIB3.F
