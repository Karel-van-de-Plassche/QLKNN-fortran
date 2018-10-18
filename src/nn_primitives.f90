! FILE: FIB3.F
      SUBROUTINE FIB(A,N)
!
!     CALCULATE FIRST N FIBONACCI NUMBERS
!
      INTEGER N
      REAL*8 A(N)
      CHARACTER(len=4), dimension(3) :: hidden_activation
      REAL, DIMENSION(:,:), allocatable :: weights_input
      REAL, DIMENSION(128,128) :: biases_input
      REAL, DIMENSION(3,128,128) :: weights_hidden
      REAL, DIMENSION(128,128) :: biases_hidden
      REAL, DIMENSION(128,128) :: weights_output
      REAL, DIMENSION(128,128) :: biases_output
      namelist /net/ hidden_activation, weights_input, biases_input, &
          weights_hidden, biases_hidden, weights_output, biases_output

!f2py intent(in) n
!f2py intent(out) a
!f2py depend(n) a

      DO I=1,N
         IF (I.EQ.1) THEN
            A(I) = 0.0D0
         ELSEIF (I.EQ.2) THEN
            A(I) = 1.0D0
         ELSE 
            A(I) = A(I-1) + A(I-2)
         ENDIF
      ENDDO
       open(10,file='efitem_gb_div_efetem_gb.nml')
       read(10,nml=net)
       write(*,nml=net)
       write(*, *) hidden_activation(1)

       close(10)
       end
      END
! END FILE FIB3.F
