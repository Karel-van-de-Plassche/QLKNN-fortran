program qlknn_test
    use qlknn_primitives
    implicit none
    real, dimension(10,3) :: input
    print *, "Hello World!"
    input(:,1) = (/ 1.,2.,5.,2.,0.66,0.4,0.45,1.,1e-3, 0.2 /)
    input(:,2) = (/ 1.,13.,5.,2.,0.66,0.4,0.45,1.,1e-3, 0.8 /)
    input(:,3) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
    call evaluate_QLKNN_10D(input)

end program qlknn_test
