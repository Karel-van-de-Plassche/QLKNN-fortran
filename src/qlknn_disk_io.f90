! file: disk_io.f
module qlknn_disk_io
    use qlknn_types

    implicit none
contains
    subroutine load_net(filename, net)
        integer :: n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
        character(len=*), intent(in) :: filename

        real, dimension(:,:), allocatable ::   weights_input
        real, dimension(:), allocatable ::     biases_input
        real, dimension(:,:,:), allocatable :: weights_hidden
        real, dimension(:,:), allocatable ::   biases_hidden
        real, dimension(:,:), allocatable ::   weights_output
        real, dimension(:), allocatable :: biases_output

        character(len=4), dimension(:), allocatable ::      hidden_activation

        real, dimension(:), allocatable ::   feature_prescale_bias
        real, dimension(:), allocatable ::   feature_prescale_factor
        real, dimension(:), allocatable ::   target_prescale_bias
        real, dimension(:), allocatable ::   target_prescale_factor

        type(networktype), intent(out) :: net
        namelist /sizes/ n_hidden_layers, n_hidden_nodes, n_inputs, n_outputs
        namelist /netfile/ biases_hidden, hidden_activation, weights_input, biases_input, &
            weights_hidden, weights_output, biases_output, &
            feature_prescale_bias, feature_prescale_factor, target_prescale_bias,&
            target_prescale_factor
        open(10,file=filename)
        read(10,nml=sizes)
        write(*, *) 'n_hidden_layers', n_hidden_layers
        write(*, *) 'n_hidden_nodes', n_hidden_nodes
        write(*, *) 'n_inputs', n_inputs
        write(*, *) 'n_outputs', n_outputs
        allocate(weights_input(n_hidden_nodes, n_inputs))
        allocate(biases_input(n_hidden_nodes))
        allocate(weights_hidden(n_hidden_layers-1, n_hidden_nodes, n_hidden_nodes))
        allocate(biases_hidden(n_hidden_layers-1, n_hidden_nodes))
        allocate(weights_output(1, n_hidden_nodes))
        allocate(hidden_activation(n_hidden_layers))
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
        net%biases_output = biases_output

        net%hidden_activation = hidden_activation

        net%target_prescale_bias = target_prescale_bias
        net%target_prescale_factor = target_prescale_factor
        net%feature_prescale_bias = feature_prescale_bias
        net%feature_prescale_factor = feature_prescale_factor

    end subroutine load_net
end module qlknn_disk_io
