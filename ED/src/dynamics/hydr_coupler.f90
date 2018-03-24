Module hydr_coupler
  implicit none

Contains

  subroutine hydr_diag_coupler(csite, ipa, hydr, can_temp, soil_water, ntxt)
    use ed_state_vars, only: sitetype, patchtype
    use hydr_init, only: hydr_diag_vars
    use hydr_state_vars, only: hydr_vars
    use grid_coms, only: nzg
    use soil_coms, only: soil
    implicit none
    
    type(sitetype), target :: csite
    type(patchtype), pointer :: cpatch
    type(hydr_vars) :: hydr
    integer, intent(in) :: ipa
    integer :: ico
    integer, intent(in) :: ntxt
    real, intent(in) :: can_temp
    real, dimension(nzg), intent(in) :: soil_water
    
    cpatch => csite%patch(ipa)
    
    !ntxt = csite%ntext_soil(nzg,ipa)
    do ico = 1, cpatch%ncohorts
       call hydr_diag_vars(cpatch%hite(ico), &
            can_temp, &
            soil_water, soil(ntxt)%slmsts, soil(ntxt)%slpots, &
            soil(ntxt)%slbs, cpatch%krdepth(ico), &
            hydr%heartwood_diameter(ico), &
            hydr%water_mass_ph_rt(ico),   &
            hydr%water_mass_xy_sm(ico),  &
            hydr%water_mass_ca_sm(ico), &
            hydr%water_mass_ph_sm(ico),  &
            hydr%water_mass_st_sm(ico), &
            hydr%water_mass_ph_cn(ico), &
            hydr%water_mass_st_cn(ico), &
            hydr%sucrose_mass_ph_rt(ico), &
            hydr%sucrose_mass_st_rt(ico), &
            hydr%sucrose_mass_ca_sm(ico), &
            hydr%sucrose_mass_ph_sm(ico), &
            hydr%sucrose_mass_st_sm(ico), &
            hydr%sucrose_mass_ph_cn(ico), &
            hydr%sucrose_mass_st_cn(ico),  &
            hydr%volume_st_rt(ico), hydr%volume_xy_sm(ico), &
            hydr%volume_ph_sm(ico), hydr%volume_st_sm(ico), &
            hydr%volume_ca_sm(ico), &
            hydr%turgor_pressure_ph_rt(ico), &
            hydr%turgor_pressure_xy_sm(ico), &
            hydr%turgor_pressure_ca_sm(ico), &
            hydr%turgor_pressure_ph_sm(ico), &
            hydr%turgor_pressure_st_sm(ico), &
            hydr%turgor_pressure_ph_cn(ico), &
            hydr%turgor_pressure_st_cn(ico), &
            hydr%soil_water_potential(ico), &
            hydr%turgor_pressure_xy_rt(ico), &
            hydr%xylem_diameter(ico), &
            hydr%cambium_diameter(ico), &
            hydr%phloem_diameter(ico), &
            hydr%storage_diameter(ico), &
            hydr%surface_area_xy(ico), &
            hydr%surface_area_ca(ico), &
            hydr%surface_area_ph(ico), &
            hydr%vert_resistance_xy(ico), &
            hydr%vert_resistance_ph(ico), &
            hydr%sucrose_conc_ph_rt(ico), &
            hydr%sucrose_conc_st_rt(ico), &
            hydr%sucrose_conc_ca_sm(ico), &
            hydr%sucrose_conc_ph_sm(ico), &
            hydr%sucrose_conc_st_sm(ico), &
            hydr%sucrose_conc_ph_cn(ico), &
            hydr%sucrose_conc_st_cn(ico), &
            hydr%osmotic_pot_ph_rt(ico), &
            hydr%osmotic_pot_st_rt(ico), &
            hydr%osmotic_pot_ca_sm(ico), &
            hydr%osmotic_pot_ph_sm(ico), &
            hydr%osmotic_pot_st_sm(ico), &
            hydr%osmotic_pot_ph_cn(ico), &
            hydr%osmotic_pot_st_cn(ico),  &
            hydr%horiz_resistance_xy_ca_sm(ico), &
            hydr%horiz_resistance_ca_ph_sm(ico), &
            hydr%horiz_resistance_ph_st_sm(ico),  &
            hydr%bulk_elast_mod_xy_sm(ico),  &
            hydr%bulk_elast_mod_xy_cn(ico), &
            hydr%bulk_elast_mod_ph_rt(ico),  &
            hydr%bulk_elast_mod_ph_sm(ico), &
            hydr%bulk_elast_mod_ph_cn(ico), &
            hydr%bulk_elast_mod_st_sm(ico),  &
            hydr%bulk_elast_mod_st_cn(ico),  &
            hydr%bulk_elast_mod_ca_sm(ico),  &
            hydr%turgor_pressure_xy_cn(ico), &
            hydr%turgor_pressure_xy_sm_base(ico),  &
            hydr%turgor_pressure_xy_cn_base(ico),  &
            hydr%water_mass_xy_cn(ico))

    enddo
    return
  end subroutine hydr_diag_coupler

  subroutine hydr_init_coupler(csite,ipa, ntxt)

    use ed_state_vars, only: patchtype, sitetype
    use grid_coms, only: nzg
    use soil_coms,     only : soil
    use pft_coms,      only : sla
    use hydr_init, only: hydr_init_vars

    implicit none

    type(sitetype), target :: csite
    type(patchtype), pointer :: cpatch
    integer :: ico
    integer, intent(in) :: ntxt
    integer, intent(in) :: ipa

    cpatch => csite%patch(ipa)

    do ico = 1, cpatch%ncohorts
              
      !ntxt = csite%ntext_soil(nzg, ipa)  !soil texture index
              
       call hydr_init_vars(cpatch%hite(ico), &
            cpatch%bleaf(ico), sla(cpatch%pft(ico)), &
            cpatch%dbh(ico), csite%can_temp(ipa)-273.15,  &
            cpatch%bsapwooda(ico) + cpatch%bsapwoodb(ico), cpatch%bdead(ico), &
            csite%soil_water(:,ipa), soil(ntxt)%slmsts, &
            soil(ntxt)%slpots, soil(ntxt)%slbs, cpatch%krdepth(ico), &
            cpatch%hydr%root_midpoint(ico), &
            cpatch%hydr%stem_midpoint(ico), &
            cpatch%hydr%crown_midpoint(ico), &
            cpatch%hydr%volume_st_cn(ico), &
            cpatch%hydr%volume_xy_cn(ico), &
            cpatch%hydr%volume_ph_cn(ico), &
            cpatch%hydr%volume_st_rt(ico), &
            cpatch%hydr%volume_xy_rt(ico), &
            cpatch%hydr%volume_ph_rt(ico), &
            cpatch%hydr%xylem_diameter(ico), &
            cpatch%hydr%heartwood_diameter(ico), &
            cpatch%hydr%cambium_diameter(ico), &
            cpatch%hydr%phloem_diameter(ico), &
            cpatch%hydr%storage_diameter(ico), &
            cpatch%hydr%volume_xy_sm(ico), &
            cpatch%hydr%volume_ph_sm(ico), &
            cpatch%hydr%volume_st_sm(ico), &
            cpatch%hydr%volume_ca_sm(ico), &
            cpatch%hydr%surface_area_xy(ico), &
            cpatch%hydr%surface_area_ca(ico), &
            cpatch%hydr%surface_area_ph(ico), &
            cpatch%hydr%water_mass_ph_rt(ico), &
            cpatch%hydr%water_mass_xy_sm(ico), &
            cpatch%hydr%water_mass_xy_rt(ico), &
            cpatch%hydr%water_mass_ca_sm(ico), &
            cpatch%hydr%water_mass_ph_sm(ico), &
            cpatch%hydr%water_mass_st_sm(ico), &
            cpatch%hydr%water_mass_xy_cn(ico), &
            cpatch%hydr%water_mass_ph_cn(ico), &
            cpatch%hydr%water_mass_st_cn(ico), &
            cpatch%hydr%sucrose_mass_ph_rt(ico), &
            cpatch%hydr%sucrose_mass_st_rt(ico), &
            cpatch%hydr%sucrose_mass_ca_sm(ico), &
            cpatch%hydr%sucrose_mass_ph_sm(ico), &
            cpatch%hydr%sucrose_mass_st_sm(ico), &
            cpatch%hydr%sucrose_mass_ph_cn(ico), &
            cpatch%hydr%sucrose_mass_st_cn(ico), &
            cpatch%hydr%sucrose_conc_ph_rt(ico), &
            cpatch%hydr%sucrose_conc_st_rt(ico), &
            cpatch%hydr%sucrose_conc_ca_sm(ico), &
            cpatch%hydr%sucrose_conc_ph_sm(ico), &
            cpatch%hydr%sucrose_conc_st_sm(ico), &
            cpatch%hydr%sucrose_conc_ph_cn(ico), &
            cpatch%hydr%sucrose_conc_st_cn(ico), &
            cpatch%hydr%target_sucrose_conc(ico), &
            cpatch%hydr%turgor_pressure_xy_rt(ico), &
            cpatch%hydr%turgor_pressure_ph_rt(ico), &
            cpatch%hydr%turgor_pressure_xy_sm(ico), &
            cpatch%hydr%turgor_pressure_ca_sm(ico), &
            cpatch%hydr%turgor_pressure_ph_sm(ico), &
            cpatch%hydr%turgor_pressure_st_sm(ico), &
            cpatch%hydr%turgor_pressure_xy_cn(ico), &
            cpatch%hydr%turgor_pressure_ph_cn(ico), &
            cpatch%hydr%turgor_pressure_st_cn(ico), &
            cpatch%hydr%soil_water_potential(ico), &
            cpatch%hydr%starch_mass_rt(ico), &
            cpatch%hydr%starch_mass_sm(ico), &
            cpatch%hydr%starch_mass_cn(ico), &
            cpatch%hydr%osmotic_pot_ph_rt(ico), &
            cpatch%hydr%osmotic_pot_st_rt(ico), &
            cpatch%hydr%osmotic_pot_ca_sm(ico), &
            cpatch%hydr%osmotic_pot_ph_sm(ico), &
            cpatch%hydr%osmotic_pot_st_sm(ico), &
            cpatch%hydr%osmotic_pot_ph_cn(ico), &
            cpatch%hydr%osmotic_pot_st_cn(ico), &
            cpatch%hydr%horiz_resistance_xy_ca_sm(ico), &
            cpatch%hydr%horiz_resistance_ca_ph_sm(ico), &
            cpatch%hydr%horiz_resistance_ph_st_sm(ico), &
            cpatch%hydr%vert_resistance_xy(ico), &
            cpatch%hydr%vert_resistance_ph(ico), &
            cpatch%hydr%bulk_elast_mod_xy_sm(ico), &
            cpatch%hydr%bulk_elast_mod_xy_cn(ico), &
            cpatch%hydr%bulk_elast_mod_ph_rt(ico), &
            cpatch%hydr%bulk_elast_mod_ph_sm(ico), &
            cpatch%hydr%bulk_elast_mod_ph_cn(ico), &
            cpatch%hydr%bulk_elast_mod_st_sm(ico), &
            cpatch%hydr%bulk_elast_mod_st_cn(ico), &
            cpatch%hydr%bulk_elast_mod_ca_sm(ico), &
            cpatch%hydr%turgor_pressure_xy_sm_base(ico), &
            cpatch%hydr%turgor_pressure_xy_cn_base(ico))
    enddo

    return
  end subroutine hydr_init_coupler

  subroutine hydr_root_water_extract(ntext_soil, soil_water, soil_fracliq, &
       ncohorts, krdepth, crown_area, nplant, broot, pft, d_soil_water, &
       d_soil_energy, soil_tempk, water_extracted_cohort)

    use grid_coms, only: nzg
    use soil_coms, only: soil8, slz, dslz, dslzi8, soil
    use pft_coms, only: root_beta, SRA
    use consts_coms, only: pi1, wdnsi8, cliq8, tsupercool_liq8

    implicit none

    integer :: k
    integer, dimension(nzg), intent(in) :: ntext_soil
    real(kind=8), dimension(nzg), intent(in) :: soil_water
    real(kind=8), dimension(nzg), intent(in) :: soil_fracliq
    real(kind=8), dimension(nzg), intent(inout) :: d_soil_energy
    real(kind=8), dimension(nzg), intent(inout) :: d_soil_water
    real(kind=8), dimension(nzg), intent(in) :: soil_tempk
    integer :: nsoil
    real :: wgpfrac
    real, dimension(nzg) :: layer_psi
    integer :: ico
    integer, intent(in) :: ncohorts
    integer, dimension(nzg), intent(in) :: krdepth
    real :: current_layer_depth
    real :: above_layer_depth
    real :: cohort_crown_area
    real, intent(in), dimension(ncohorts) :: crown_area
    real, intent(in), dimension(ncohorts) :: nplant
    real, intent(in), dimension(ncohorts) :: broot
    integer, intent(in), dimension(ncohorts) :: pft
    real :: RAI
    real, dimension(nzg) :: soil_water_cond_layer
    real :: soil_water_cond_total
    real :: water_extracted
    real, dimension(ncohorts), intent(in) :: water_extracted_cohort
    real :: water_root_extract_layer

    do ico = 1, ncohorts

       do k = krdepth(ico), nzg
          soil_water_cond_layer(k) = 0.
          current_layer_depth = -slz(k)
          if(k+1 <= nzg)then
             above_layer_depth = -slz(k+1)
          else
             above_layer_depth = 0.
          endif

          cohort_crown_area = crown_area(ico) / nplant(ico)
          if(cohort_crown_area == 0.)then
             RAI = 0.
          else
             RAI = broot(ico) * (root_beta(pft(ico))**(above_layer_depth /  &
                  (-slz(krdepth(ico)))) - &
                  root_beta(pft(ico))**(current_layer_depth / &
                  (-slz(krdepth(ico))))) * &
                  SRA(pft(ico)) / &
                  (2. * cohort_crown_area)
          endif
          
          nsoil = ntext_soil(k)
          wgpfrac = min(1., soil_water(k) * soil_fracliq(k) / &
               soil(nsoil)%slmsts)
          soil_water_cond_layer(k) = soil(nsoil)%slcons * &
               wgpfrac**(2. * soil(nsoil)%slbs + 3.) * 1.e3 * sqrt(RAI) / &
               (pi1 * dslz(k)) * 2. * cohort_crown_area
          
       enddo

       soil_water_cond_total = 0.
       do k = krdepth(ico), nzg
          soil_water_cond_total = soil_water_cond_total +  &
               soil_water_cond_layer(k)
       enddo
       if(soil_water_cond_total <= 0.)then
          do k = krdepth(ico), nzg
             soil_water_cond_layer(k) = 1. / (nzg - krdepth(ico) + 1)
          enddo
          soil_water_cond_total = 1.
       endif
       
       water_extracted = water_extracted_cohort(ico) * 1.0e-6 * nplant(ico)

       do k = krdepth(ico), nzg
          water_root_extract_layer = soil_water_cond_layer(k) /  &
               soil_water_cond_total * water_extracted * 1 * dslzi8(k)
          d_soil_water(k) = d_soil_water(k) - water_root_extract_layer
          d_soil_energy(k) = d_soil_energy(k) - water_root_extract_layer * &
               cliq8 * (soil_tempk(k) - tsupercool_liq8)
       enddo
       
    enddo

    return
  end subroutine hydr_root_water_extract

end Module hydr_coupler
