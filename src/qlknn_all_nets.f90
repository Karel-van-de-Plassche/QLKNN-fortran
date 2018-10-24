module qlknn_all_nets
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
    use net_gam_leq_gb
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

    implicit none

    type(networktype), dimension(20), save :: nets
    type(networktype), dimension(19), save :: rotdiv_nets
contains
    subroutine load_nets()
        logical, save :: first_call = .TRUE.
        if (first_call) then
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
            first_call = .FALSE.
        end if
    end subroutine load_nets
end module qlknn_all_nets
