! file: fib3.f
module qlknn_primitives
    use qlknn_disk_io
    use qlknn_types
    implicit none
contains
    subroutine fib()
        integer i, lay

        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
        type(networktype) :: net
        real, dimension(:,:), allocatable ::   res
        real, dimension(3, 9) :: inp
        inp(1,:) = (/ 1.,2.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(2,:) = (/ 1.,13.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(3,:) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        !  ati  ate   an         q      smag         x  ti_te
        !1.0   2.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
        !1.0  13.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001

        call load_net('efiITG_GB.nml', net)
        write(*,*) 'biases:', net%biases_output
        n_hidden_layers = size(net%weights_hidden, 1) + 1
        n_hidden_nodes = size(net%weights_hidden, 2)
        n_inputs = size(net%weights_input, 2)
        n_outputs = size(net%weights_output, 1)
        write(*, *) 'n_hidden_layers', n_hidden_layers
        write(*, *) 'n_hidden_nodes', n_hidden_nodes
        write(*, *) 'n_inputs', n_inputs
        write(*, *) 'n_outputs', n_outputs

        !allocate(res(n_inputs, n_hidden_nodes))

        write(*,*) 'net inp'
        write(*,*) inp(1,:)
        do i = 1, 3
        inp(i,:) = net%feature_prescale_factor * inp(i,:) + &
            net%feature_prescale_bias
        end do

        res = matmul(inp, transpose(net%weights_input))
        do i = 1, 3
        res(i, :) = res(i, :) + net%biases_input
        end do
        res = tanh(res)

        do lay = 1, n_hidden_layers - 1
        res = matmul(res, transpose(net%weights_hidden(lay, :, :)))
        do i = 1, 3
        res(i, :) = res(i, :) + net%biases_hidden(lay, :)
        end do
        res = tanh(res)
        end do

        res = matmul(res, transpose(net%weights_output))
        do i = 1, 3
            res(i, :) = res(i, :) + net%biases_output
        end do

        do i = 1, 3
        res(i,:) = dot_product(1/net%target_prescale_factor, res(i,:) - &
            net%target_prescale_bias)
        end do
        write(*,*) 'net out'
        write(*,*) res

    end subroutine fib
end module qlknn_primitives
