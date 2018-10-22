program qlknn_test
    use qlknn_primitives
    implicit none
    real, dimension(10,24)::input
    namelist /test/ input
    print *, "Hello World!"
    open(10,file='test.nml')
    read(10,nml=test)

    call evaluate_QLKNN_10D(input)

end program qlknn_test
