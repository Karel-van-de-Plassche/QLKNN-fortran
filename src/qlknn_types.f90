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
contains
end module qlknn_types
