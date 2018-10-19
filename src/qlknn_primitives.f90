! file: fib3.f
module qlknn_primitives
    use qlknn_disk_io
    implicit none
contains
    subroutine fib()
        integer i, lay
        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
        character(len=4), dimension(3) ::      hidden_activation
        real, dimension(:,:), allocatable ::   weights_input
        real, dimension(:), allocatable ::     biases_input
        real, dimension(:,:,:), allocatable :: weights_hidden
        real, dimension(:,:), allocatable ::   biases_hidden
        real, dimension(:,:), allocatable ::   weights_output

        real, dimension(:), allocatable ::   feature_prescale_bias
        real, dimension(:), allocatable ::   feature_prescale_factor
        real, dimension(:), allocatable ::   target_prescale_bias
        real, dimension(:), allocatable ::   target_prescale_factor
        real :: biases_output

        real, dimension(:,:), allocatable ::   res
        namelist /sizes/ n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
        type networktype
            real, dimension(:,:), allocatable ::   weights_input
            real, dimension(:), allocatable ::     biases_input
            real, dimension(:,:,:), allocatable :: weights_hidden
            real, dimension(:,:), allocatable ::   biases_hidden
            real, dimension(:,:), allocatable ::   weights_output

            real, dimension(:), allocatable ::   feature_prescale_bias
            real, dimension(:), allocatable ::   feature_prescale_factor
            real, dimension(:), allocatable ::   target_prescale_bias
            real, dimension(:), allocatable ::   target_prescale_factor
            real :: biases_output
        end type networktype

        namelist /netfile/ biases_hidden, hidden_activation, weights_input, biases_input, &
            weights_hidden, weights_output, biases_output, &
            feature_prescale_bias, feature_prescale_factor, target_prescale_bias,&
            target_prescale_factor
        real, dimension(3, 9) :: inp
        type(networktype) net
        inp(1,:) = (/ 1.,2.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(2,:) = (/ 1.,13.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(3,:) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        !  ati  ate   an         q      smag         x  ti_te
        !1.0   2.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
        !1.0  13.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001

        call load_net('efiITG_GB.nml')
        open(10,file='efiITG_GB.nml')
        read(10,nml=sizes)
        write(*, *) 'n_hidden_layers', n_hidden_layers
        write(*, *) 'n_hidden_nodes', n_hidden_nodes
        write(*, *) 'n_inputs', n_inputs
        write(*, *) 'n_outputs', n_outputs
        allocate(weights_input(n_hidden_nodes, n_inputs))
        allocate(biases_input(n_hidden_nodes))
        allocate(biases_hidden(n_hidden_layers-1, n_hidden_nodes))
        allocate(weights_hidden(n_hidden_layers-1, n_hidden_nodes, n_hidden_nodes))
        allocate(weights_output(1, n_hidden_nodes))
        allocate(feature_prescale_bias(n_inputs))
        allocate(feature_prescale_factor(n_inputs))
        allocate(target_prescale_bias(n_outputs))
        allocate(target_prescale_factor(n_outputs))
        read(10,nml=netfile)
        close(10)
        !write(*,nml=net)
        net%weights_input = weights_input
        net%biases_input = biases_input
        net%biases_hidden = biases_hidden
        net%weights_hidden = weights_hidden
        net%weights_output = weights_output
        net%target_prescale_bias = target_prescale_bias
        net%target_prescale_factor = target_prescale_factor
        net%feature_prescale_bias = feature_prescale_bias
        net%feature_prescale_factor = feature_prescale_factor



        !allocate(res(n_inputs, n_hidden_nodes))

        !write(*,*) 'net inp'
        !write(*,*) inp(1,:)
        !do i = 1, 3
        !inp(i,:) = feature_prescale_factor * inp(i,:) + &
        !    feature_prescale_bias
        !end do

        !res = matmul(inp, transpose(weights_input))
        !do i = 1, 3
        !res(i, :) = res(i, :) + biases_input
        !end do
        !res = tanh(res)

        !do lay = 1, n_hidden_layers - 1
        !res = matmul(res, transpose(weights_hidden(lay, :, :)))
        !do i = 1, 3
        !res(i, :) = res(i, :) + biases_hidden(lay, :)
        !end do
        !res = tanh(res)
        !end do

        !res = matmul(res, transpose(weights_output))
        !do i = 1, 3
        !    res(i, :) = res(i, :) + biases_output
        !end do

        !do i = 1, 3
        !res(i,:) = dot_product(1/target_prescale_factor, res(i,:) - &
        !    target_prescale_bias)
        !end do
        !write(*,*) 'net out'
        !write(*,*) res

    end subroutine fib
end module qlknn_primitives
