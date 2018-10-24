! file: disk_io.f
module qlknn_disk_io
    use qlknn_types

    implicit none
    character(len=32), dimension(20), parameter, private :: net_names = &
        (/ 'net_efeetg_gb                   ', &
           'net_efeitg_gb_div_efiitg_gb     ', &
           'net_efetem_gb                   ', &
           'net_efiitg_gb                   ', &
           'net_efitem_gb_div_efetem_gb     ', &
           'net_pfeitg_gb_div_efiitg_gb     ', &
           'net_pfetem_gb_div_efetem_gb     ', &
           'net_dfeitg_gb_div_efiitg_gb     ', &
           'net_dfetem_gb_div_efetem_gb     ', &
           'net_vteitg_gb_div_efiitg_gb     ', &
           'net_vtetem_gb_div_efetem_gb     ', &
           'net_vceitg_gb_div_efiitg_gb     ', &
           'net_vcetem_gb_div_efetem_gb     ', &
           'net_dfiitg_gb_div_efiitg_gb     ', &
           'net_dfitem_gb_div_efetem_gb     ', &
           'net_vtiitg_gb_div_efiitg_gb     ', &
           'net_vtitem_gb_div_efetem_gb     ', &
           'net_vciitg_gb_div_efiitg_gb     ', &
           'net_vcitem_gb_div_efetem_gb     ', &
           'net_gam_leq_gb                  ' /)
   character(len=32), dimension(18), parameter, private :: rotdiv_names = &
         (/ 'net_efeitg_gb_div_efeitg_gb_rot0', &
            'net_efetem_gb_div_efetem_gb_rot0', &
            'net_efiitg_gb_div_efiitg_gb_rot0', &
            'net_efitem_gb_div_efitem_gb_rot0', &
            'net_pfeitg_gb_div_pfeitg_gb_rot0', &
            'net_pfetem_gb_div_pfetem_gb_rot0', &
            'net_dfeitg_gb_div_dfeitg_gb_rot0', &
            'net_dfetem_gb_div_dfetem_gb_rot0', &
            'net_vteitg_gb_div_vteitg_gb_rot0', &
            'net_vtetem_gb_div_vtetem_gb_rot0', &
            'net_vceitg_gb_div_vceitg_gb_rot0', &
            'net_vcetem_gb_div_vcetem_gb_rot0', &
            'net_dfiitg_gb_div_dfiitg_gb_rot0', &
            'net_dfitem_gb_div_dfitem_gb_rot0', &
            'net_vtiitg_gb_div_vtiitg_gb_rot0', &
            'net_vtitem_gb_div_vtitem_gb_rot0', &
            'net_vciitg_gb_div_vciitg_gb_rot0', &
            'net_vcitem_gb_div_vcitem_gb_rot0' /)

    type(networktype), dimension(20), save :: nets
    type(networktype), dimension(19), save :: rotdiv_nets
contains
    subroutine load_all_nets_from_disk(folder)
        character(len=*), intent(in) :: folder
        character(len=32) :: net_name
        character(len=4096) :: filepath
        type(networktype) :: net
        integer :: ii, row
        do ii = 1,20
            net_name = net_names(ii)
            filepath = folder // '/' // trim(net_name) // '.nml'
            write(*,*) 'Loading ', trim(filepath)
            call load_net_from_disk(filepath, nets(ii))
            net = nets(ii)
        end do
        do ii = 1,18
            net_name = rotdiv_names(ii)
            filepath = folder // '/' // trim(net_name) // '.nml'
            write(*,*) 'Loading ', trim(filepath)
            call load_net_from_disk(filepath, rotdiv_nets(ii+1))
        end do

    end subroutine load_all_nets_from_disk

    subroutine load_net_from_disk(filename, net)
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
        open(10,file=filename,ACTION='READ')
        read(10,nml=sizes)
        !write(*, *) 'n_hidden_layers', n_hidden_layers
        !write(*, *) 'n_hidden_nodes', n_hidden_nodes
        !write(*, *) 'n_inputs', n_inputs
        !write(*, *) 'n_outputs', n_outputs
        allocate(weights_input(n_hidden_nodes, n_inputs))
        allocate(biases_input(n_hidden_nodes))
        allocate(weights_hidden(n_hidden_layers-1, n_hidden_nodes, n_hidden_nodes))
        allocate(biases_hidden(n_hidden_layers-1, n_hidden_nodes))
        allocate(weights_output(1, n_hidden_nodes))
        allocate(biases_output(n_outputs))
        allocate(hidden_activation(n_hidden_layers))
        allocate(feature_prescale_bias(n_inputs))
        allocate(feature_prescale_factor(n_inputs))
        allocate(target_prescale_bias(n_outputs))
        allocate(target_prescale_factor(n_outputs))
        read(10,nml=netfile)
        close(10)
        !write(*,nml=net)
        net%weights_input = reshape(weights_input, shape=[n_hidden_nodes, n_inputs], order=[1, 2])
        net%biases_input = biases_input
        net%biases_hidden = reshape(biases_hidden, shape=[n_hidden_nodes, n_hidden_layers-1], order=[2,1])
        net%weights_hidden = reshape(weights_hidden, shape=[n_hidden_nodes, n_hidden_nodes,  n_hidden_layers-1], order=[3,1,2])
        net%weights_output = weights_output
        net%biases_output = biases_output

        net%hidden_activation = hidden_activation

        net%target_prescale_bias = target_prescale_bias
        net%target_prescale_factor = target_prescale_factor
        net%feature_prescale_bias = feature_prescale_bias
        net%feature_prescale_factor = feature_prescale_factor

    end subroutine load_net_from_disk
end module qlknn_disk_io
