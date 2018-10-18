! FILE: FIB3.F
      SUBROUTINE FIB(A,N)
!
!     CALCULATE FIRST N FIBONACCI NUMBERS
!
      INTEGER N
      REAL*8 A(N)
      INTEGER :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
      CHARACTER(len=4), dimension(3) ::      hidden_activation
      REAL, DIMENSION(:,:), ALLOCATABLE ::   weights_input
      REAL, DIMENSION(:), ALLOCATABLE ::     biases_input
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: weights_hidden
      REAL, DIMENSION(:,:), ALLOCATABLE ::   biases_hidden
      REAL, DIMENSION(:,:), ALLOCATABLE ::   weights_output

      REAL, DIMENSION(:), ALLOCATABLE ::   feature_prescale_bias
      REAL, DIMENSION(:), ALLOCATABLE ::   feature_prescale_factor
      REAL, DIMENSION(:), ALLOCATABLE ::   target_prescale_bias
      REAL, DIMENSION(:), ALLOCATABLE ::   target_prescale_factor

      REAL, DIMENSION(:,:), ALLOCATABLE ::   res
      namelist /sizes/ n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
      type NetworkType
        CHARACTER(len=4), dimension(3) :: hidden_activation
        REAL, DIMENSION(:,:), ALLOCATABLE :: weights_input
        REAL, DIMENSION(:), ALLOCATABLE :: biases_input
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: weights_hidden
        REAL, DIMENSION(:,:), ALLOCATABLE :: biases_hidden
        REAL, DIMENSION(:,:), ALLOCATABLE :: weights_output
        REAL :: biases_output
      end type NetworkType

      namelist /net/ biases_hidden, hidden_activation, weights_input, biases_input, &
       weights_hidden, weights_output, biases_output, &
   feature_prescale_bias, feature_prescale_factor, target_prescale_bias,&
   target_prescale_factor
      REAL, DIMENSION(3, 9) :: inp
      inp(1,:) = (/ 1.,2.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
      inp(2,:) = (/ 1.,13.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
      inp(3,:) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
      !  Ati  Ate   An         q      smag         x  Ti_Te
      !1.0   2.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
      !1.0  13.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001


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
       open(10,file='efiITG_GB.nml')
       read(10,nml=sizes)
       write(*, *) 'n_hidden_layers', n_hidden_layers
       write(*, *) 'n_hidden_nodes', n_hidden_nodes
       write(*, *) 'n_inputs', n_inputs
       write(*, *) 'n_outputs', n_outputs
       ALLOCATE(weights_input(n_hidden_nodes, n_inputs))
       ALLOCATE(biases_input(n_hidden_nodes))
       ALLOCATE(biases_hidden(n_hidden_layers-1, n_hidden_nodes))
       ALLOCATE(weights_hidden(n_hidden_layers-1, n_hidden_nodes, n_hidden_nodes))
       ALLOCATE(weights_output(1, n_hidden_nodes))
       ALLOCATE(feature_prescale_bias(n_inputs))
       ALLOCATE(feature_prescale_factor(n_inputs))
       ALLOCATE(target_prescale_bias(n_outputs))
       ALLOCATE(target_prescale_factor(n_outputs))
       read(10,nml=net)
       close(10)
       !write(*,nml=net)
       ALLOCATE(res(n_inputs, n_hidden_nodes))

       write(*,*) 'net inp'
       write(*,*) inp(1,:)
       do i = 1, 3
        inp(i,:) = feature_prescale_factor * inp(i,:) + &
        feature_prescale_bias
       end do

       res = MATMUL(inp, TRANSPOSE(weights_input))
       do i = 1, 3
           res(i, :) = res(i, :) + biases_input
       end do
       res = TANH(res)

       do lay = 1, n_hidden_layers - 1
         res = MATMUL(res, TRANSPOSE(weights_hidden(lay, :, :)))
         do i = 1, 3
             res(i, :) = res(i, :) + biases_hidden(lay, :)
         end do
         res = TANH(res)
       end do

       res = MATMUL(res, TRANSPOSE(weights_output))
       do i = 1, 3
           res(i, :) = res(i, :) + biases_output
       end do

       do i = 1, 3
        res(i,:) = DOT_PRODUCT(1/target_prescale_factor, res(i,:) - &
        target_prescale_bias)
       end do
       write(*,*) 'net out'
       write(*,*) res


       end
      END
! END FILE FIB3.F
