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
    include "mkl_vml.fi"
    include "mkl_blas.fi"
contains
    subroutine evaluate_QLKNN_10D(input)
        real, dimension(:,:), intent(in) :: input

        integer trial, n_trails, n_rho, ii, jj, rho, n_nets, n_rotdiv, idx
        real :: start, finish
        type(networktype), dimension(38) :: nets, rotdiv_nets
        real, dimension(:), allocatable :: res, x, y
        real, dimension(:,:), allocatable :: net_result, rotdiv_result
        real, dimension(:), allocatable :: gam_leq
        real, dimension(:,:), allocatable :: net_input
        real, dimension(:,:), allocatable ::rotdiv_input
        type (qlknn_options) :: opts
        logical, dimension(20) :: net_evaluate
        logical, dimension(19) :: rotdiv_evaluate
        integer, dimension(8), parameter :: idx_ITG = (/2, 6, 8, 10, 12, 14, 16, 18/)
        integer, dimension(8), parameter :: idx_TEM = (/5, 7, 9, 11, 13, 15, 17, 19/)
        integer, parameter :: leading_ITG = 4, leading_TEM = 3, leading_ETG = 1
        ! zeff ati  ate   an         q      smag         x  ti_te logNustar
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
        nets(20) = gam_leq_gb()
        n_nets = 20

        rotdiv_nets(2) = efeitg_gb_div_efeitg_gb_rot0()
        rotdiv_nets(3) = efetem_gb_div_efetem_gb_rot0()
        rotdiv_nets(4) = efiitg_gb_div_efiitg_gb_rot0()
        rotdiv_nets(5) = efitem_gb_div_efitem_gb_rot0()
        rotdiv_nets(6) = pfeitg_gb_div_pfeitg_gb_rot0()
        rotdiv_nets(7) = pfetem_gb_div_pfetem_gb_rot0()
        rotdiv_nets(8) = dfeitg_gb_div_dfeitg_gb_rot0()
        rotdiv_nets(9) = dfetem_gb_div_dfetem_gb_rot0()
        rotdiv_nets(10) = vteitg_gb_div_vteitg_gb_rot0()
        rotdiv_nets(11) = vtetem_gb_div_vtetem_gb_rot0()
        rotdiv_nets(12) = vceitg_gb_div_vceitg_gb_rot0()
        rotdiv_nets(13) = vcetem_gb_div_vcetem_gb_rot0()
        rotdiv_nets(14) = dfiitg_gb_div_dfiitg_gb_rot0()
        rotdiv_nets(15) = dfitem_gb_div_dfitem_gb_rot0()
        rotdiv_nets(16) = vtiitg_gb_div_vtiitg_gb_rot0()
        rotdiv_nets(17) = vtitem_gb_div_vtitem_gb_rot0()
        rotdiv_nets(18) = vciitg_gb_div_vciitg_gb_rot0()
        rotdiv_nets(19) = vcitem_gb_div_vcitem_gb_rot0()
        n_rotdiv = 19

        n_rho = size(input, 2)
        allocate(net_input(9, n_rho))
        allocate(net_result(n_rho, n_nets))
        allocate(rotdiv_input(8, n_rho))
        allocate(rotdiv_result(n_rho, n_rotdiv))

        allocate(res(n_rho)) !Debug

        net_input = input(1:9, :)
        rotdiv_input = input((/3, 7, 5, 6, 2, 4, 8, 10/), :)

        write(*,*) 'input, n_rho=', n_rho
        do rho = 1, n_rho
            WRITE(*,'(*(F7.2 X))'), (input(ii, rho), ii=1,10)
        end do
        write(*,*) 'net_input'
        write(*,*) net_input(:,1)
        write(*,*) 'rotdiv_input'
        write(*,*) rotdiv_input(:,1)
        call cpu_time(start)
        CALL default_qlknn_options(opts)
        call print_qlknn_options(opts)
        call get_networks_to_evaluate(opts, net_evaluate, rotdiv_evaluate)
        net_result = 0.

        write(*,*) net_evaluate, rotdiv_evaluate
        ! Impose input constants

        ! Evaluate all neural networks
        do ii =1,n_nets
            if (net_evaluate(ii)) then
                call evaluate_network_mkl(net_input, nets(ii), net_result(:, ii))
            end if
        end do
        do ii =1,n_rotdiv
            if (rotdiv_evaluate(ii)) then
                call evaluate_network_mkl(rotdiv_input, rotdiv_nets(ii), rotdiv_result(:, ii))
            end if
        end do

        ! Clip leading fluxes to 0
        WRITE(*,*) net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)).lt.0
        WHERE (net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)).lt.0) &
                net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)) = 0

        ! Clip stable modes to 0, based on leading flux
        do ii = 1, size(idx_ITG,1)
            idx = idx_ITG(ii)
            CALL vdmul(n_rho, net_result(:, idx), net_result(:, leading_ITG), net_result(:, idx))
        end do
        do ii = 1, size(idx_TEM,1)
            idx = idx_TEM(ii)
            CALL vdmul(n_rho, net_result(:, idx), net_result(:, leading_TEM), net_result(:, idx))
        end do


        WRITE(*,'(A)') 'net_result'
        do rho = 1, n_rho
            WRITE(*,'(*(F7.2 X))'), (net_result(rho, ii), ii=1,n_nets)
        end do

        WRITE(*,'(A)') 'rotdiv_result'
        do rho = 1, n_rho
            WRITE(*,'(*(F7.2 X))'), (rotdiv_result(rho, ii), ii=1,n_rotdiv)
        end do

        n_trails = 1
        do trial = 1,n_trails
            CALL evaluate_network_mkl(net_input, nets(1), res, 0)
        end do
        call cpu_time(finish)
        print '("Time = ",f6.3," milliseconds.")',1000*(finish-start)/n_trails

        write(*,*) 'net out'
        write(*,*) res

    end subroutine evaluate_QLKNN_10D

    subroutine evaluate_network_mkl(input, net, output_1d, verbosityin)
        real, dimension(:,:), intent(in) :: input
        type(networktype), intent(in) :: net
        integer, optional, intent(in) :: verbosityin
        integer:: verbosity
        real, dimension(:,:), allocatable ::   output
        real, dimension(:), intent(out) :: output_1d
        integer num

        integer rho, lay, ii
        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs, n_rho
        real, dimension(:,:), allocatable :: inp_resc
        real, dimension(:,:), allocatable :: B_hidden, B_output

        if(present(verbosityin))then
            verbosity=verbosityin
        else
            verbosity = 0
        end if

        n_hidden_layers = size(net%weights_hidden, 3) + 1
        n_hidden_nodes = size(net%weights_hidden, 2)
        n_inputs = size(net%weights_input, 2)
        n_outputs = size(net%weights_output, 1)
        n_rho = size(input, 2)
        if (n_outputs > 1) then
            ERROR STOP 'Expected 1D output from network!'
        end if
        if (.NOT. size(output_1d) == n_rho) then
            ERROR STOP 'Passed output_1d has wrong shape!'
        end if
        if (.NOT. n_inputs == size(input, 1)) then
            ERROR STOP 'Passed input has wrong number of inputs!'
        end if
        allocate(inp_resc(lbound(input,1):ubound(input,1), lbound(input,2):ubound(input,2)))
        allocate(B_hidden(n_hidden_nodes, n_rho))
        allocate(B_output(n_outputs, n_rho))

        if (verbosity >= 2) then
            write(*, *) 'n_hidden_layers', n_hidden_layers
            write(*, *) 'n_hidden_nodes', n_hidden_nodes
            write(*, *) 'n_inputs', n_inputs
            write(*, *) 'n_outputs', n_outputs
            write(*, *) 'n_rho', n_rho
            write(*,*) 'input'
            do rho = 1, n_rho
                write(*,'(*(f7.2 x))') input(:, rho)
            end do
        end if
        if (verbosity >= 2) write(*,*) 'inp_resc'
        do rho = 1, n_rho
            inp_resc(:,rho) = net%feature_prescale_factor * input(:,rho) + &
                net%feature_prescale_bias
            if (verbosity >= 2) write(*,'(*(f7.2 x))') inp_resc(:, rho)
        end do

        do rho = 1, n_rho
            B_hidden(:, rho) = net%biases_input
        end do
        CALL dgemm('N', 'N', n_hidden_nodes, n_rho, n_inputs, 1., net%weights_input, n_hidden_nodes, inp_resc, n_inputs, 1., &
        B_hidden, n_hidden_nodes)
        output = B_hidden
        CALL vdtanh(n_rho * n_hidden_nodes, output, output)

        do lay = 1, n_hidden_layers - 1
            do rho = 1, n_rho
                B_hidden(:, rho) = net%biases_hidden(:, lay)
            end do
            CALL dgemm('N', 'N', n_hidden_nodes, n_rho, n_hidden_nodes, 1., net%weights_hidden(:, :, lay), n_hidden_nodes, output, &
            n_hidden_nodes, 1., &
            B_hidden, n_hidden_nodes)
            output = B_hidden
            CALL vdtanh(n_rho * n_hidden_nodes, output, output)
        end do

        do rho = 1, n_rho
            B_output(:, rho) = net%biases_output
        end do
        CALL dgemm('N', 'N', n_outputs, n_rho, n_hidden_nodes, &
            1., net%weights_output, n_outputs, &
            output, n_hidden_nodes, &
            1., B_output, n_outputs)
        output = B_output
        if (verbosity >= 2) write(*,*) 'output_layer'
        if (verbosity >= 2) write(*,'(*(f7.2 x))') output

        do rho = 1, n_rho
            output(:,rho) = dot_product(1/net%target_prescale_factor, output(:,rho) - &
                net%target_prescale_bias)
        end do
        if (verbosity >= 2) write(*,*) 'output_descaled'
        if (verbosity >= 2) write(*,'(*(f7.2 x))') output

        output_1d(:) = output(1, :)
        !WRITE(*,'(*(F7.2 X))'), (output(1, ii), ii=1,n_rho)
        deallocate(inp_resc)
        deallocate(B_hidden)
        deallocate(B_output)
    end subroutine evaluate_network_mkl

    subroutine evaluate_network(input, net, output)
        real, dimension(:,:), intent(in) :: input
        type(networktype), intent(in) :: net
        real, dimension(:,:), allocatable, intent(out) ::   output

        integer rho, lay
        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs, n_rho
        real, dimension(:,:), allocatable :: inp_resc
        real, dimension(:,:), allocatable :: B_hidden, B_output

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
        allocate(B_hidden(n_hidden_nodes, n_rho))
        allocate(B_output(n_outputs, n_rho))

        do rho = 1, n_rho
            inp_resc(:,rho) = net%feature_prescale_factor * input(:,rho) + &
                net%feature_prescale_bias
        end do

        output = matmul(net%weights_input, inp_resc)
        do rho = 1, n_rho
            B_hidden(:, rho) = net%biases_input
        end do
        output = output + B_hidden
        output = tanh(output)

        do lay = 1, n_hidden_layers - 1
            output = matmul(net%weights_hidden(:, :, lay), output)
            do rho = 1, n_rho
                B_hidden(:, rho) = net%biases_hidden(:, lay)
            end do
            output = output + B_hidden
            output = tanh(output)
        end do

        output = matmul(net%weights_output, output)
        do rho = 1, n_rho
            B_output(:, rho) = net%biases_output
        end do
        output = output + B_output

        do rho = 1, n_rho
            output(:,rho) = dot_product(1/net%target_prescale_factor, output(:,rho) - &
                net%target_prescale_bias)
        end do
        deallocate(inp_resc)
        deallocate(B_hidden)
        deallocate(B_output)
    end subroutine
end module qlknn_primitives
