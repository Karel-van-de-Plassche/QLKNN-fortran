! file: fib3.f
module qlknn_primitives
    use qlknn_disk_io
    use qlknn_types
    use net_efiitg_gb
    implicit none
contains
    subroutine fib()
        integer rho, lay

        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
        type(networktype) :: net
        type(networktype), dimension(19) :: nets
        real, dimension(:,:), allocatable ::   res
        real, dimension(9,3) :: inp
        inp(:,1) = (/ 1.,2.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(:,2) = (/ 1.,13.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(:,3) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        !  ati  ate   an         q      smag         x  ti_te
        !1.0   2.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
        !1.0  13.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001

        net = efiitg_gb()
        nets(1) = net

        write(*,*) 'biases:', net%biases_output
        n_hidden_layers = size(net%weights_hidden, 3) + 1
        n_hidden_nodes = size(net%weights_hidden, 2)
        n_inputs = size(net%weights_input, 1)
        n_outputs = size(net%weights_output, 2)
        write(*, *) 'n_hidden_layers', n_hidden_layers
        write(*, *) 'n_hidden_nodes', n_hidden_nodes
        write(*, *) 'n_inputs', n_inputs
        write(*, *) 'n_outputs', n_outputs

        !allocate(res(n_inputs, n_hidden_nodes))


        write(*,*) 'net inp'
        write(*,*) inp(1,:)
        do rho = 1, 3
            inp(:,rho) = net%feature_prescale_factor * inp(:,rho) + &
                net%feature_prescale_bias
        end do

        res = matmul(transpose(net%weights_input), inp)
        do rho = 1, 3
            res(:,rho) = res(:,rho) + net%biases_input
        end do
        res = tanh(res)

        do lay = 1, n_hidden_layers - 1
            res = matmul(transpose(net%weights_hidden(:, :, lay)), res)
            do rho = 1, 3
                res(:, rho) = res(:, rho) + net%biases_hidden(:, lay)
            end do
            res = tanh(res)
        end do

        res = matmul(transpose(net%weights_output), res)
        do rho = 1, 3
            res(:, rho) = res(:, rho) + net%biases_output
        end do

        do rho = 1, 3
            res(:,rho) = dot_product(1/net%target_prescale_factor, res(:,rho) - &
                net%target_prescale_bias)
        end do

        write(*,*) 'net out'
        write(*,*) res

    end subroutine fib
end module qlknn_primitives
