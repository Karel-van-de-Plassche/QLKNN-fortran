program qlknn_test
    use qlknn_primitives
    implicit none
    integer n_trails, trial, verbosity, rho
    real, dimension(10,24) :: input
    real :: start, finish
    real, dimension(10) :: perturb
    type(networktype), dimension(20) :: nets
    type(networktype), dimension(19) :: rotdiv_nets
    namelist /test/ input
    print *, "Hello World!"
    open(10,file='test.nml')
    read(10,nml=test)

    n_trails = 1
    verbosity = 0
    call load_nets(nets, rotdiv_nets)
    call cpu_time(start)
    do trial = 1,n_trails
        !perturb = 1e-1 * (/rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand(), rand()/)
        !do rho = 1, 24
        !    input(:, rho) = input(:, rho) + perturb
        !end do
        call evaluate_QLKNN_10D(input, nets, rotdiv_nets, verbosity)
    end do
    call cpu_time(finish)
    print '("Time = ",f9.3," milliseconds.")',1e3*(finish-start)/n_trails

end program qlknn_test
