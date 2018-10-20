! file: fib3.f
module qlknn_primitives
    use qlknn_disk_io
    use qlknn_types
    use net_efiitg_gb
    use net_efeetg_gb
    use net_efeitg_gb_div_efiitg_gb
    use net_efetem_gb
    use net_efiitg_gb
    use net_efitem_gb_div_efetem_gb
    use net_pfeitg_gb_div_efiitg_gb
    use net_pfetem_gb_div_efetem_gb
    use net_dfeitg_gb_div_efiitg_gb
    use net_dfetem_gb_div_efetem_gb
    use net_vteitg_gb_div_efiitg_gb
    use net_vtetem_gb_div_efetem_gb
    use net_vceitg_gb_div_efiitg_gb
    use net_vcetem_gb_div_efetem_gb
    use net_dfiitg_gb_div_efiitg_gb
    use net_dfitem_gb_div_efetem_gb
    use net_vtiitg_gb_div_efiitg_gb
    use net_vtitem_gb_div_efetem_gb
    use net_vciitg_gb_div_efiitg_gb
    use net_vcitem_gb_div_efetem_gb
    use net_efeitg_gb_div_efeitg_gb_rot0
    use net_efetem_gb_div_efetem_gb_rot0
    use net_efiitg_gb_div_efiitg_gb_rot0
    use net_efitem_gb_div_efitem_gb_rot0
    use net_pfeitg_gb_div_pfeitg_gb_rot0
    use net_pfetem_gb_div_pfetem_gb_rot0
    use net_dfeitg_gb_div_dfeitg_gb_rot0
    use net_dfetem_gb_div_dfetem_gb_rot0
    use net_vteitg_gb_div_vteitg_gb_rot0
    use net_vtetem_gb_div_vtetem_gb_rot0
    use net_vceitg_gb_div_vceitg_gb_rot0
    use net_vcetem_gb_div_vcetem_gb_rot0
    use net_dfiitg_gb_div_dfiitg_gb_rot0
    use net_dfitem_gb_div_dfitem_gb_rot0
    use net_vtiitg_gb_div_vtiitg_gb_rot0
    use net_vtitem_gb_div_vtitem_gb_rot0
    use net_vciitg_gb_div_vciitg_gb_rot0
    use net_vcitem_gb_div_vcitem_gb_rot0
    use net_gam_leq_gb
    implicit none
contains
    subroutine fib()
        integer trial, n_trails
        real :: start, finish
        type(networktype), dimension(38) :: nets
        real, dimension(:,:), allocatable ::   res
        real, dimension(9,3) :: inp
        inp(:,1) = (/ 1.,2.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(:,2) = (/ 1.,13.,5.,2.,0.66,0.4,0.45,1.,1e-3 /)
        inp(:,3) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        !  ati  ate   an         q      smag         x  ti_te
        !1.0   2.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
        !1.0  13.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001

        nets( 1) = efeetg_gb()
        nets( 2) = efeitg_gb_div_efiitg_gb()
        nets( 3) = efetem_gb()
        nets( 4) = efiitg_gb()
        nets( 5) = efitem_gb_div_efetem_gb()
        nets( 6) = pfeitg_gb_div_efiitg_gb()
        nets( 7) = pfetem_gb_div_efetem_gb()
        nets( 8) = dfeitg_gb_div_efiitg_gb()
        nets( 9) = dfetem_gb_div_efetem_gb()
        nets(10) = vteitg_gb_div_efiitg_gb()
        nets(11) = vtetem_gb_div_efetem_gb()
        nets(12) = vceitg_gb_div_efiitg_gb()
        nets(13) = vcetem_gb_div_efetem_gb()
        nets(14) = dfiitg_gb_div_efiitg_gb()
        nets(15) = dfitem_gb_div_efetem_gb()
        nets(16) = vtiitg_gb_div_efiitg_gb()
        nets(17) = vtitem_gb_div_efetem_gb()
        nets(18) = vciitg_gb_div_efiitg_gb()
        nets(19) = vcitem_gb_div_efetem_gb()
        nets(20) = efeitg_gb_div_efeitg_gb_rot0()
        nets(21) = efetem_gb_div_efetem_gb_rot0()
        nets(22) = efiitg_gb_div_efiitg_gb_rot0()
        nets(23) = efitem_gb_div_efitem_gb_rot0()
        nets(24) = pfeitg_gb_div_pfeitg_gb_rot0()
        nets(25) = pfetem_gb_div_pfetem_gb_rot0()
        nets(26) = dfeitg_gb_div_dfeitg_gb_rot0()
        nets(27) = dfetem_gb_div_dfetem_gb_rot0()
        nets(28) = vteitg_gb_div_vteitg_gb_rot0()
        nets(29) = vtetem_gb_div_vtetem_gb_rot0()
        nets(30) = vceitg_gb_div_vceitg_gb_rot0()
        nets(31) = vcetem_gb_div_vcetem_gb_rot0()
        nets(32) = dfiitg_gb_div_dfiitg_gb_rot0()
        nets(33) = dfitem_gb_div_dfitem_gb_rot0()
        nets(34) = vtiitg_gb_div_vtiitg_gb_rot0()
        nets(35) = vtitem_gb_div_vtitem_gb_rot0()
        nets(36) = vciitg_gb_div_vciitg_gb_rot0()
        nets(37) = vcitem_gb_div_vcitem_gb_rot0()
        nets(38) = gam_leq_gb()

        write(*,*) 'net inp'
        write(*,*) inp(1,:)
        call cpu_time(start)

        n_trails = 100
        do trial = 1,n_trails
            CALL evaluate_network(inp, nets(4), res)
        end do
        call cpu_time(finish)
        print '("Time = ",f6.3," milliseconds.")',1000*(finish-start)/n_trails

        write(*,*) 'net out'
        write(*,*) res

    end subroutine fib

    subroutine evaluate_network(input, net, output)
        real, dimension(:,:), intent(in) :: input
        type(networktype), intent(in) :: net
        real, dimension(:,:), allocatable, intent(out) ::   output

        integer rho, lay
        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs, n_rho
        real, dimension(:,:), allocatable :: inp_resc

        n_hidden_layers = size(net%weights_hidden, 3) + 1
        n_hidden_nodes = size(net%weights_hidden, 2)
        n_inputs = size(net%weights_input, 2)
        n_outputs = size(net%weights_output, 1)
        n_rho = size(input, 2)
        !write(*, *) 'n_hidden_layers', n_hidden_layers
        !write(*, *) 'n_hidden_nodes', n_hidden_nodes
        !write(*, *) 'n_inputs', n_inputs
        !write(*, *) 'n_outputs', n_outputs
        !write(*, *) 'n_rho', n_rho
        allocate(inp_resc(lbound(input,1):ubound(input,1), lbound(input,2):ubound(input,2)))

        do rho = 1, n_rho
            inp_resc(:,rho) = net%feature_prescale_factor * input(:,rho) + &
                net%feature_prescale_bias
        end do

        output = matmul(net%weights_input, inp_resc)
        do rho = 1, n_rho
            output(:,rho) = output(:,rho) + net%biases_input
        end do
        output = tanh(output)

        do lay = 1, n_hidden_layers - 1
            output = matmul(net%weights_hidden(:, :, lay), output)
            do rho = 1, n_rho
                output(:, rho) = output(:, rho) + net%biases_hidden(:, lay)
            end do
            output = tanh(output)
        end do

        output = matmul(net%weights_output, output)
        do rho = 1, n_rho
            output(:, rho) = output(:, rho) + net%biases_output
        end do

        do rho = 1, n_rho
            output(:,rho) = dot_product(1/net%target_prescale_factor, output(:,rho) - &
                net%target_prescale_bias)
        end do
    end subroutine
end module qlknn_primitives
