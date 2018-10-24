! file: fib3.f
module qlknn_primitives
    use qlknn_disk_io
    use qlknn_types
    use qlknn_all_nets

    implicit none
#ifdef USE_MKL
    include "mkl_vml.fi"
    include "mkl_blas.fi"
#endif

    integer, dimension(8), parameter :: idx_ITG = (/2, 6, 8, 10, 12, 14, 16, 18/)
    integer, dimension(8), parameter :: idx_TEM = (/5, 7, 9, 11, 13, 15, 17, 19/)
    integer, parameter :: leading_ITG = 4, leading_TEM = 3, leading_ETG = 1
contains
    subroutine evaluate_QLKNN_10D(input, nets, rotdiv_nets, verbosity)
        real, dimension(:,:), intent(in) :: input
        real, dimension(:,:), allocatable :: input_clipped
        integer, intent(in) :: verbosity
        type(networktype), dimension(20), intent(in) :: nets
        type(networktype), dimension(19), intent(in) :: rotdiv_nets

        integer trial, n_rho, ii, jj, rho, n_nets, n_rotdiv, idx
        real, dimension(:), allocatable :: res, x, y
        real, dimension(:,:), allocatable :: net_result, rotdiv_result, merged_net_result
        real, dimension(:), allocatable :: gam_leq
        real, dimension(:,:), allocatable :: net_input
        real, dimension(:,:), allocatable ::rotdiv_input
        type (qlknn_options) :: opts
        logical, dimension(20) :: net_evaluate
        logical, dimension(19) :: rotdiv_evaluate
        ! zeff ati  ate   an         q      smag         x  ti_te logNustar
        !1.0   2.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
        !1.0  13.000000  5.0  2.0  0.660156  0.399902  0.449951    1.0      0.001
        n_nets = 20
        n_rotdiv = 19

        n_rho = size(input, 2)
        allocate(net_input(9, n_rho))
        allocate(net_result(n_rho, n_nets))
        allocate(merged_net_result(n_rho, 9))
        allocate(rotdiv_input(8, n_rho))
        allocate(rotdiv_result(n_rho, n_rotdiv))

        allocate(res(n_rho)) !Debug

        if (verbosity >= 2) then
            write(*,*) net_evaluate, rotdiv_evaluate
        end if
        CALL default_qlknn_options(opts)
        if (verbosity >= 1) then
            call print_qlknn_options(opts)
            write(*,*) 'input, n_rho=', n_rho
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (input(ii, rho), ii=1,10)
            end do
        end if

        ! Impose input constants
        call impose_input_constraints(input, input_clipped, opts, verbosity)

        net_input = input(1:9, :)
        rotdiv_input = input((/3, 7, 5, 6, 2, 4, 8, 10/), :)

        if (verbosity >= 2) then
            write(*,*) 'net_input'
            write(*,*) net_input(:,1)
            write(*,*) 'rotdiv_input'
            write(*,*) rotdiv_input(:,1)
        end if
        call get_networks_to_evaluate(opts, net_evaluate, rotdiv_evaluate)
        net_result = 0.
        rotdiv_result = 0.

        ! Evaluate all neural networks
        do ii =1,n_nets
            if (net_evaluate(ii)) then
                call evaluate_network(net_input, nets(ii), net_result(:, ii), verbosity)
            end if
        end do
        do ii =2,n_rotdiv
            if (rotdiv_evaluate(ii)) then
                call evaluate_network(rotdiv_input, rotdiv_nets(ii), rotdiv_result(:, ii), verbosity)
            end if
        end do
        ! Use the efi rotdiv for efe
        rotdiv_result(:, 1) = 1.
        rotdiv_result(:, 2) = rotdiv_result(:, 4)
        rotdiv_result(:, 3) = rotdiv_result(:, 5)

        ! Clip leading fluxes to 0
        call impose_leading_flux_constraints(net_result, verbosity)
        if (verbosity >= 2) then
            WRITE(*,'(A)') 'net_result (pre-div-multiplicate)'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (net_result(rho, ii), ii=1,n_nets)
            end do
        end if

        if (verbosity >= 2) then
            WRITE(*,'(A)') 'rotdiv_result'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (rotdiv_result(rho, ii), ii=1,n_rotdiv)
            end do
        end if

        ! Clip leading rotdivs to 0
        call impose_leading_flux_constraints(rotdiv_result, verbosity)

        call multiply_div_networks(net_result, verbosity)

        if (verbosity >= 2) then
            WRITE(*,'(A)') 'net_result'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (net_result(rho, ii), ii=1,n_nets)
            end do
        end if

        ! Multiply with rotdiv
        do idx = 1, n_nets - 1
            CALL vdmul(n_rho, net_result(:, idx), rotdiv_result(:, idx), net_result(:, idx))
        end do

        if (verbosity >= 2) then
            WRITE(*,'(A)') 'rotdiv multiplied net_result'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (net_result(rho, ii), ii=1,n_nets)
            end do
        end if

        ! Clip based on leading
        call apply_stability_clipping(net_result, verbosity)
        if (verbosity >= 2) then
            WRITE(*,'(A)') 'rotdiv stability clipped'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (net_result(rho, ii), ii=1,n_nets)
            end do
        end if

        call impose_output_constraints(net_result, opts, verbosity)
        if (verbosity >= 1) then
            WRITE(*,'(A)') 'rotdiv output clipped'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (net_result(rho, ii), ii=1,n_nets)
            end do
        end if

        ! Merge ETG/ITG/TEM modes together
        call merge_modes(net_result, merged_net_result, verbosity)
        if (verbosity >= 1) then
            WRITE(*,'(A)') 'rotdiv modes merged'
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (merged_net_result(rho, ii), ii=1,7)
            end do
        end if


    end subroutine evaluate_QLKNN_10D

    subroutine evaluate_network(input, net, output_1d, verbosityin)
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
        character(len=200) :: error_msg

        if(present(verbosityin)) then
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
            write(error_msg,*) 'Passed input has wrong number of inputs! It is ', size(input, 1), ', should be ', n_inputs
            ERROR STOP error_msg
        end if
        allocate(inp_resc(lbound(input,1):ubound(input,1), lbound(input,2):ubound(input,2)))
        allocate(B_hidden(n_hidden_nodes, n_rho))
        allocate(B_output(n_outputs, n_rho))

        if (verbosity >= 3) then
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
        if (verbosity >= 3) write(*,*) 'evaluating network'
        if (verbosity >= 3) write(*,*) 'inp_resc'
        do rho = 1, n_rho
            inp_resc(:,rho) = net%feature_prescale_factor * input(:,rho) + &
                net%feature_prescale_bias
            if (verbosity >= 3) write(*,'(*(f7.2 x))') inp_resc(:, rho)
        end do

        do rho = 1, n_rho
            B_hidden(:, rho) = net%biases_input
        end do
        CALL dgemm('N', 'N', n_hidden_nodes, n_rho, n_inputs, 1., net%weights_input, n_hidden_nodes, inp_resc, n_inputs, 1., &
        B_hidden, n_hidden_nodes)
        output = B_hidden
        CALL vdtanh(n_rho * n_hidden_nodes, output, output)
        if (verbosity >= 3) then
            write(*,*) 'input_layer post_tanh. (1:10, :)'
            do rho = 1, n_rho
                write(*,'(*(f7.2 x))') output(1:10, rho)
            end do
        end if

        do lay = 1, n_hidden_layers - 1
            do rho = 1, n_rho
                B_hidden(:, rho) = net%biases_hidden(:, lay)
            end do
            CALL dgemm('N', 'N', n_hidden_nodes, n_rho, n_hidden_nodes, 1., net%weights_hidden(:, :, lay), n_hidden_nodes, output, &
            n_hidden_nodes, 1., &
            B_hidden, n_hidden_nodes)
            output = B_hidden
            if (verbosity >= 3) then
                write(*,*) 'hidden_layer ', lay, ' pre_tanh. (1:10, :)'
                write(*,*) shape(output)
                do rho = 1, n_rho
                    write(*,'(*(f7.2 x))') output(1:10, rho)
                end do
            end if
            CALL vdtanh(n_rho * n_hidden_nodes, output, output)
            if (verbosity >= 3) then
                write(*,*) 'hidden_layer ', lay, ' post_tanh. (1:10, :)'
                write(*,*) shape(output)
                do rho = 1, n_rho
                    write(*,'(*(f7.2 x))') output(1:10, rho)
                end do
            end if
        end do

        do rho = 1, n_rho
            B_output(:, rho) = net%biases_output
        end do
        CALL dgemm('N', 'N', n_outputs, n_rho, n_hidden_nodes, &
            1., net%weights_output, n_outputs, &
            output, n_hidden_nodes, &
            1., B_output, n_outputs)
        output = B_output
        if (verbosity >= 3) write(*,*) 'output_layer'
        if (verbosity >= 3) write(*,'(*(f7.2 x))') output

        do rho = 1, n_rho
            output(:,rho) = dot_product(1/net%target_prescale_factor, output(:,rho) - &
                net%target_prescale_bias)
        end do
        if (verbosity >= 3) write(*,*) 'output_descaled'
        if (verbosity >= 3) write(*,'(*(f7.2 x))') output

        output_1d(:) = output(1, :)
        !WRITE(*,'(*(F7.2 X))'), (output(1, ii), ii=1,n_rho)
        deallocate(inp_resc)
        deallocate(B_hidden)
        deallocate(B_output)
    end subroutine evaluate_network

    subroutine impose_output_constraints(output, opts, verbosity)
        real, dimension(:,:), intent(inout) :: output
        type (qlknn_options), intent(in) :: opts
        integer, intent(in) :: verbosity

        integer :: ii, rho, n_rho
        real, dimension(20) :: output_min, output_max

        n_rho = size(output, 1)

        output_min = opts%min_output + (1-opts%margin_output) * abs(opts%min_output)
        output_max = opts%max_output - (1-opts%margin_output) * abs(opts%max_output)
        do rho = 1, n_rho
            where ((output(rho, :) < output_min) .AND. opts%constrain_outputs) &
                    output(rho, :) = output_min
            where ((output(rho, :) > output_max) .AND. opts%constrain_outputs) &
                    output(rho, :) = output_max
        end do

        if (verbosity >= 2) then
            write(*,*) 'output clipped, n_rho=', n_rho
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (output(ii, rho), ii=1,10)
            end do
        end if
    end subroutine impose_output_constraints

    subroutine impose_input_constraints(input, input_clipped, opts, verbosity)
        real, dimension(:,:), intent(in) :: input
        type (qlknn_options), intent(in) :: opts
        integer, intent(in) :: verbosity
        real, dimension(:,:), allocatable, intent(out) :: input_clipped

        integer :: ii, rho, n_rho
        real, dimension(10) :: input_min, input_max

        n_rho = size(input, 2)

        input_min = opts%min_input + (1-opts%margin_input) * abs(opts%min_input)
        input_max = opts%max_input - (1-opts%margin_input) * abs(opts%max_input)
        input_clipped = input
        do rho = 1, n_rho
            where ((input_clipped(:, rho) < input_min) .AND. opts%constrain_inputs) &
                    input_clipped(:, rho) = input_min
            where ((input_clipped(:, rho) > input_max) .AND. opts%constrain_inputs) &
                    input_clipped(:, rho) = input_max
        end do

        if (verbosity >= 2) then
            write(*,*) 'input clipped, n_rho=', n_rho
            do rho = 1, n_rho
                WRITE(*,'(*(F7.2 X))'), (input_clipped(ii, rho), ii=1,10)
            end do
        end if
    end subroutine impose_input_constraints

    subroutine apply_stability_clipping(net_result, verbosity)
        real, dimension(:,:), intent(inout):: net_result
        integer, intent(in) :: verbosity
        integer :: ii, idx
        ! Clip leading fluxes to 0
        if (verbosity >= 2) then
            WRITE(*,*) net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)).le.0
        end if
        WHERE (net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)).le.0) &
               net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)) = 0
        do ii = 1, size(idx_ITG,1)
            idx = idx_ITG(ii)
            WHERE (net_result(:, leading_ITG).le.0) &
                   net_result(:, idx) = 0
        end do
        do ii = 1, size(idx_TEM,1)
            idx = idx_TEM(ii)
            WHERE (net_result(:, leading_TEM).le.0) &
                   net_result(:, idx) = 0
        end do
    end subroutine apply_stability_clipping

    subroutine impose_leading_flux_constraints(net_result, verbosity)
        real, dimension(:,:), intent(inout):: net_result
        integer, intent(in) :: verbosity
        ! Clip leading fluxes to 0
        if (verbosity >= 2) then
            WRITE(*,*) net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)).le.0
        end if
        WHERE (net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)).le.0) &
               net_result(:, (/leading_ETG, leading_ITG, leading_TEM/)) = 0
    end subroutine impose_leading_flux_constraints

    subroutine multiply_div_networks(net_result, verbosity)
        real, dimension(:,:), intent(inout):: net_result
        integer, intent(in) :: verbosity
        integer :: ii, idx, n_rho
        n_rho = size(net_result, 1)
        do ii = 1, size(idx_ITG,1)
            idx = idx_ITG(ii)
            CALL vdmul(n_rho, net_result(:, idx), net_result(:, leading_ITG), net_result(:, idx))
        end do
        do ii = 1, size(idx_TEM,1)
            idx = idx_TEM(ii)
            CALL vdmul(n_rho, net_result(:, idx), net_result(:, leading_TEM), net_result(:, idx))
        end do
    end subroutine multiply_div_networks

    subroutine merge_modes(net_result, merged_net_result, verbosity)
        real, dimension(:,:), intent(in):: net_result
        integer, intent(in) :: verbosity
        real, dimension(:,:), intent(out), allocatable :: merged_net_result
        integer :: n_rho, ii
        n_rho = size(net_result, 1)
        !'efe_GB' % 1
        !'efi_GB' % 2
        !'pfe_GB' % 3
        !'dfe_GB' % 4
        !'vte_GB' % 5
        !'dfi_GB' % 6
        !'vti_GB' % 7
        allocate(merged_net_result(n_rho, 7))
        do ii = 1, n_rho
            merged_net_result(ii, 1) = sum(net_result(ii, 1:3))
            merged_net_result(ii, 2) = sum(net_result(ii, 4:5))
            merged_net_result(ii, 3) = sum(net_result(ii, 6:7))
            merged_net_result(ii, 4) = sum(net_result(ii, 8:9))
            merged_net_result(ii, 5) = sum(net_result(ii, 10:13))
            merged_net_result(ii, 6) = sum(net_result(ii, 14:15))
            merged_net_result(ii, 7) = sum(net_result(ii, 16:19))
        end do
    end subroutine merge_modes

#ifndef USE_MKL
    subroutine vdmul(n, a, b, y)
        integer, intent(in) :: n
        REAL, dimension(:), intent(in) :: a, b
        REAL, dimension(:), intent(out) :: y
        integer :: ii
        y = a * b
    end subroutine vdmul

    subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
        character(len=1), intent(in) :: transa, transb
        integer, intent(in) :: m, n, k, lda, ldb, ldc
        REAL, intent(in) :: alpha, beta
        REAL, dimension(:,:), intent(in) :: a, b
        REAL, dimension(:,:), intent(inout) :: c
        c = matmul(a, b) + c
    end subroutine

    subroutine vdtanh(n, a, y)
        integer, intent(in) :: n
        REAL, dimension(:,:), intent(in) :: a
        REAL, dimension(:,:), intent(out) :: y
        y = tanh(a)
    end subroutine
#endif

end module qlknn_primitives
