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
        logical, dimension(9) :: constrain_outputs
        real, dimension(9) :: out_bound
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
        opts%margin = 0.95
        opts%constrain_outputs = .true.
        opts%out_bound = 100
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
        WRITE(*,'(AX,*(F4.3 X))') 'margin'       , (opts%margin(i), i=1,10)
        WRITE(*,*) 'constrain_outputs'           , opts%constrain_outputs
        WRITE(*,'(AX,*(F5.1 X))') 'out_bound'    , (opts%out_bound(i), i=1,9)
        WRITE(*,*) 'rotdiv_tem'                  , opts%rotdiv_tem
        WRITE(*,*) 'rotdiv_itg'                  , opts%rotdiv_itg
    end subroutine print_qlknn_options
end module qlknn_types
