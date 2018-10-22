module qlknn_types
    implicit none
    type networktype
        real, dimension(:,:), allocatable ::   weights_input
        real, dimension(:), allocatable ::     biases_input
        real, dimension(:,:,:), allocatable :: weights_hidden
        real, dimension(:,:), allocatable ::   biases_hidden
        real, dimension(:,:), allocatable ::   weights_output
        real, dimension(:), allocatable :: biases_output

        character(len=4), dimension(:), allocatable :: hidden_activation

        real, dimension(:), allocatable ::   feature_prescale_bias
        real, dimension(:), allocatable ::   feature_prescale_factor
        real, dimension(:), allocatable ::   target_prescale_bias
        real, dimension(:), allocatable ::   target_prescale_factor
    end type networktype

    type qlknn_options
        logical :: use_ion_diffusivity_networks
        logical :: apply_victor_rule
        logical :: use_effective_diffusivity
        logical :: calc_heat_transport
        logical :: calc_part_transport
        logical :: use_ETG
        logical :: use_ITG
        logical :: use_TEM
        logical :: apply_stability_clipping
        logical, dimension(10) :: constrain_inputs
        real, dimension(10) :: margin
        real, dimension(10) :: min_input, max_input
        logical, dimension(9) :: constrain_outputs
        real, dimension(9) :: min_output, max_output
        logical :: rotdiv_TEM
        logical :: rotdiv_ITG
    end type qlknn_options
contains
    subroutine default_qlknn_options(opts)
        type (qlknn_options), intent(out) :: opts

        opts%use_ion_diffusivity_networks = .false.
        opts%apply_victor_rule = .false.
        opts%use_effective_diffusivity = .false.
        opts%calc_heat_transport = .true.
        opts%calc_part_transport = .true.
        opts%use_etg = .true.
        opts%use_itg = .true.
        opts%use_tem = .true.
        opts%apply_stability_clipping = .true.
        opts%constrain_inputs = .true.
        opts%min_input = (/1., 0., 0., -5., 0.66, -1., .09, 0.25, -5., -100./)
        opts%max_input = (/3., 14., 14., 6., 15., 5., .99, 2.5, 0., 100./)
        opts%margin = 0.95
        opts%constrain_outputs = .true.
        opts%min_output = -100
        opts%max_output = 100
        opts%rotdiv_tem = .true.
        opts%rotdiv_itg = .true.
    end subroutine default_qlknn_options

    subroutine print_qlknn_options(opts)
        type (qlknn_options), intent(in) :: opts
        integer :: i

        WRITE(*,*) 'use_ion_diffusivity_networks', opts%use_ion_diffusivity_networks
        WRITE(*,*) 'apply_victor_rule'           , opts%apply_victor_rule
        WRITE(*,*) 'use_effective_diffusivity'   , opts%use_effective_diffusivity
        WRITE(*,*) 'calc_heat_transport'         , opts%calc_heat_transport
        WRITE(*,*) 'calc_part_transport'         , opts%calc_part_transport
        WRITE(*,*) 'use_etg'                     , opts%use_etg
        WRITE(*,*) 'use_itg'                     , opts%use_itg
        WRITE(*,*) 'use_tem'                     , opts%use_tem
        WRITE(*,*) 'apply_stability_clipping'    , opts%apply_stability_clipping
        WRITE(*,*) 'constrain_inputs'            , opts%constrain_inputs
        WRITE(*,'(AX,*(F8.3 X))') 'margin'       , (opts%margin(i), i=1,10)
        WRITE(*,'(AX,*(F8.3 X))') 'min_input'    , (opts%min_input(i), i=1,10)
        WRITE(*,'(AX,*(F8.3 X))') 'max_input'    , (opts%max_input(i), i=1,10)
        WRITE(*,*) 'constrain_outputs'           , opts%constrain_outputs
        WRITE(*,'(AX,*(F6.1 X))') 'min_output'   , (opts%min_output(i), i=1,9)
        WRITE(*,'(AX,*(F6.1 X))') 'max_output'   , (opts%max_output(i), i=1,9)
        WRITE(*,*) 'rotdiv_tem'                  , opts%rotdiv_tem
        WRITE(*,*) 'rotdiv_itg'                  , opts%rotdiv_itg
    end subroutine print_qlknn_options

    subroutine get_networks_to_evaluate(opts, net_evaluate, rotdiv_evaluate)
        type (qlknn_options), intent(in) :: opts
        logical, dimension(20), intent(out) :: net_evaluate
        logical, dimension(19), intent(out) :: rotdiv_evaluate
        net_evaluate(:) = .FALSE.
        rotdiv_evaluate(:) = .FALSE.
        if (opts%use_etg) then
            net_evaluate(1) = .TRUE.
        end if
        if (opts%use_itg) then
            net_evaluate(2:4:2) = .TRUE.
            if (opts%rotdiv_itg) then
                ! Use efi net for efe to keep thresholds matched
                rotdiv_evaluate(4) = .TRUE.
            end if
        end if
        if (opts%use_tem) then
            net_evaluate(3:5:2) = .TRUE.
            if (opts%rotdiv_tem) then
                ! Use efi net for efe to keep thresholds matched
                rotdiv_evaluate(5) = .TRUE.
            end if
        end if

        if (opts%use_effective_diffusivity) then
            if (opts%use_itg) then
                net_evaluate(6) = .TRUE.
                if (opts%rotdiv_itg) then
                    rotdiv_evaluate(6) = .TRUE.
                end if
            end if
            if (opts%use_tem) then
                net_evaluate(7) = .TRUE.
                if (opts%rotdiv_tem) then
                    rotdiv_evaluate(7) = .TRUE.
                end if
            end if
        else
            if (opts%use_ion_diffusivity_networks) then
                if (opts%use_itg) then
                    net_evaluate(14:18:2) = .TRUE.
                    if (opts%rotdiv_itg) then
                        rotdiv_evaluate(14:18:2) = .TRUE.
                    end if
                end if
                if (opts%use_tem) then
                    net_evaluate(15:19:2) = .TRUE.
                    if (opts%rotdiv_tem) then
                        rotdiv_evaluate(15:19:2) = .TRUE.
                    end if
                end if
            else
                if (opts%use_itg) then
                    net_evaluate(8:12:2) = .TRUE.
                    if (opts%rotdiv_itg) then
                        rotdiv_evaluate(8:12:2) = .TRUE.
                    end if
                end if
                if (opts%use_tem) then
                    net_evaluate(9:13:2) = .TRUE.
                    if (opts%rotdiv_tem) then
                        rotdiv_evaluate(9:13:2) = .TRUE.
                    end if
                end if
            end if
        end if

        if (opts%apply_victor_rule) then
            net_evaluate(20) = .TRUE.
        end if
    end subroutine get_networks_to_evaluate
end module qlknn_types
