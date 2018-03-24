!====================
! HOOKS
!====================
! 0. Talk to Dohyoung about allometries.
! 1. Transpiration numbers look ok.
! 2. I am now feeding back the soil water debit consistent with the hydr.  
!    Therefore, Xiangtao's feedback (rk4_driver.f90, lines 1366-1369) is 
!    now commented.
! 3a. I hooked to photosyn_driv.f90.  This involved replacing Xiangtao's
!    psi_leaf with hydr%turgor_pressure_xy_cn in 2 calls to katul.
! 3b. The calls to katul also include past_psi_leaf as an input argument.
!    past_psi_leaf is updated in average_utils, and depends on predawn_psi_leaf.
!    predawn_psi_leaf is updated in rk4_driver, line 1330.  I switched
!    out the original psi_leaf for turgor_pressure_xy_cn.
! 4. I need to feed back the calculated wood growth, respiration, phenology.
! 5. Can probably loosen the assumptions that root turgor equals soil
!    water potential, and that root water mass is constant.
! 6. All parameter values should be checked.
! 7. Be wary of temperature sensitivities.
! 8. Starting with initial heights equal to 1.75 * hgt_min
! 9. Using only PFT 40
! 10. It looks like Xiangtao's SLA update scheme is active.
! 11. Viscosity should be temperature-dependent?
! 12. Sapwood allometry not linked.
!======================
! TESTS
! 1. In an attempt to speed up the model, I am assuming that 
!    lateral sucrose transfers are instantaneous.  Not much change.
! 2. Turned off all fluxes except xylem flow.    Runs.
! 3. Diagnostically updating ca_sm, ph_sm, and st_sm turgor.

!====================

Module hydr_derivs
  implicit none

Contains

  subroutine hydr_derivs_master(hydr, d_hydr, ncohorts, tempk, fs_open, &
       psi_open, psi_closed, pft, gpp, leaf_resp, nplant, transp)
    use hydr_state_vars, only: hydr_vars
    use consts_coms, only: grav, wdns
    use pft_coms, only: rho
    implicit none
    type(hydr_vars) :: hydr
    type(hydr_vars) :: d_hydr
    integer, intent(in) :: ncohorts
    real, intent(in) :: tempk
    integer :: ico
    real, parameter :: grav_pot = grav * wdns * 1.0e-6 ! MPa/m
!    real :: transpiration
    real(kind=8), intent(in), dimension(ncohorts) :: transp
    real :: d_maint_resp
    real :: d_growth_resp
    real :: wood_density
    real :: d_wood_growth  ! mg/s
    real :: net_photosyn ! mol/s
    real(kind=8), dimension(ncohorts), intent(in) :: psi_open
    real(kind=8), dimension(ncohorts), intent(in) :: psi_closed
    real(kind=8), dimension(ncohorts), intent(in) :: fs_open
    real, dimension(ncohorts), intent(in) :: nplant
    real(kind=8), dimension(ncohorts), intent(in) :: gpp
    real(kind=8), dimension(ncohorts), intent(in) :: leaf_resp
    integer, dimension(ncohorts), intent(in) :: pft
    real :: d_soil_water ! mg/s

    do ico = 1, ncohorts

       ! NEED TO SET TRANSPIRATION RATE.  Original units: kg/m2Ground/s.
       ! New units: mg/s.
!       transpiration = (fs_open(ico) * psi_open(ico) + (1. - fs_open(ico)) * &
!            psi_closed(ico)) / nplant(ico) * 1.0e6
!print*,transpiration,transp(ico)/nplant(ico)*1.0e6
       ! Original units: umol/m2/s.  New units: mol/s.
       net_photosyn = (gpp(ico) - leaf_resp(ico)) * 1.0e-6 / nplant(ico)

       wood_density = rho(pft(ico)) * 1.0e9 ! mg wood/m3

       call hydr_derivs_cohort(  &
            hydr%volume_st_cn(ico), d_hydr%volume_st_cn(ico), &
            hydr%volume_xy_cn(ico), d_hydr%volume_xy_cn(ico), &
            hydr%volume_ph_cn(ico), d_hydr%volume_ph_cn(ico), &
            hydr%volume_xy_rt(ico), d_hydr%volume_xy_rt(ico), &
            hydr%volume_ph_rt(ico), d_hydr%volume_ph_rt(ico), &
            hydr%volume_st_rt(ico), d_hydr%volume_st_rt(ico), &
            hydr%volume_xy_sm(ico), d_hydr%volume_xy_sm(ico), &
            hydr%volume_ca_sm(ico), d_hydr%volume_ca_sm(ico), &
            hydr%volume_ph_sm(ico), d_hydr%volume_ph_sm(ico), &
            hydr%volume_st_sm(ico), d_hydr%volume_st_sm(ico), &
            hydr%water_mass_xy_rt(ico), d_hydr%water_mass_xy_rt(ico), &
            hydr%water_mass_ph_rt(ico), d_hydr%water_mass_ph_rt(ico), &
            hydr%water_mass_ph_sm(ico), d_hydr%water_mass_ph_sm(ico), &
            hydr%water_mass_ca_sm(ico), d_hydr%water_mass_ca_sm(ico), &
            hydr%water_mass_xy_sm(ico), d_hydr%water_mass_xy_sm(ico), &
            hydr%water_mass_st_sm(ico), d_hydr%water_mass_st_sm(ico), &
            hydr%water_mass_ph_cn(ico), d_hydr%water_mass_ph_cn(ico), &
            hydr%water_mass_xy_cn(ico), d_hydr%water_mass_xy_cn(ico), &
            hydr%water_mass_st_cn(ico), d_hydr%water_mass_st_cn(ico), &
            hydr%sucrose_mass_ph_rt(ico), d_hydr%sucrose_mass_ph_rt(ico), &
            hydr%sucrose_mass_st_rt(ico), d_hydr%sucrose_mass_st_rt(ico), &
            hydr%sucrose_mass_ph_sm(ico), d_hydr%sucrose_mass_ph_sm(ico), &
            hydr%sucrose_mass_ca_sm(ico), d_hydr%sucrose_mass_ca_sm(ico), &
            hydr%sucrose_mass_st_sm(ico), d_hydr%sucrose_mass_st_sm(ico), &
            hydr%sucrose_mass_ph_cn(ico), d_hydr%sucrose_mass_ph_cn(ico), &
            hydr%sucrose_mass_st_cn(ico), d_hydr%sucrose_mass_st_cn(ico), &
            hydr%starch_mass_rt(ico), d_hydr%starch_mass_rt(ico), &
            hydr%starch_mass_sm(ico), d_hydr%starch_mass_sm(ico), &
            hydr%starch_mass_cn(ico), d_hydr%starch_mass_cn(ico), &
            hydr%turgor_pressure_xy_rt(ico),  &
            d_hydr%turgor_pressure_xy_rt(ico), &
            hydr%turgor_pressure_ph_rt(ico),   &
            d_hydr%turgor_pressure_ph_rt(ico), &
            hydr%turgor_pressure_xy_sm(ico),  &
            d_hydr%turgor_pressure_xy_sm(ico), &
            hydr%turgor_pressure_ca_sm(ico),  &
            d_hydr%turgor_pressure_ca_sm(ico), &
            hydr%turgor_pressure_ph_sm(ico),  &
            d_hydr%turgor_pressure_ph_sm(ico), &
            hydr%turgor_pressure_st_sm(ico),  &
            d_hydr%turgor_pressure_st_sm(ico), &
            hydr%turgor_pressure_xy_cn(ico), &
            d_hydr%turgor_pressure_xy_cn(ico), &
            hydr%turgor_pressure_ph_cn(ico), &
            d_hydr%turgor_pressure_ph_cn(ico), &
            hydr%turgor_pressure_st_cn(ico), &
            d_hydr%turgor_pressure_st_cn(ico), grav_pot, &
            hydr%crown_midpoint(ico), hydr%stem_midpoint(ico), &
            hydr%root_midpoint(ico), hydr%vert_resistance_xy(ico), &
            hydr%osmotic_pot_ca_sm(ico), hydr%horiz_resistance_xy_ca_sm(ico), &
            hydr%bulk_elast_mod_xy_sm(ico), real(transp(ico))/nplant(ico)*1.0e6, &
            hydr%osmotic_pot_ph_cn(ico), hydr%osmotic_pot_st_cn(ico), &
            hydr%bulk_elast_mod_xy_cn(ico), hydr%vert_resistance_ph(ico), &
            hydr%osmotic_pot_ph_rt(ico), hydr%bulk_elast_mod_ph_rt(ico), &
            hydr%osmotic_pot_ph_sm(ico), hydr%osmotic_pot_st_sm(ico), &
            hydr%horiz_resistance_ca_ph_sm(ico), &
            hydr%horiz_resistance_ph_st_sm(ico),  &
            hydr%bulk_elast_mod_ph_sm(ico), hydr%bulk_elast_mod_ph_cn(ico), &
            hydr%bulk_elast_mod_ca_sm(ico), hydr%bulk_elast_mod_st_sm(ico), &
            hydr%bulk_elast_mod_st_cn(ico), hydr%sucrose_conc_ph_rt(ico), &
            hydr%sucrose_conc_ph_sm(ico), hydr%sucrose_conc_ph_cn(ico), &
            hydr%sucrose_conc_ca_sm(ico), hydr%sucrose_conc_st_cn(ico), &
            hydr%sucrose_conc_st_sm(ico), hydr%sucrose_conc_st_rt(ico), &
            d_maint_resp, d_growth_resp, wood_density, d_wood_growth, tempk, &
            net_photosyn, hydr%water_root_uptake(ico), &
            hydr%vert_water_flow_xy_rt_sm(ico),   &
            hydr%vert_water_flow_xy_sm_cn(ico), &
            hydr%vert_water_flow_ph_cn_sm(ico),  &
            hydr%vert_water_flow_ph_sm_rt(ico), &
            hydr%vert_sucrose_flow_ph_cn_sm(ico),  &
            hydr%vert_sucrose_flow_ph_sm_rt(ico), &
            hydr%loading_rate(ico), hydr%unloading_rate(ico))

    enddo

    return
  end subroutine hydr_derivs_master

  subroutine hydr_derivs_cohort(volume_st_cn, d_volume_st_cn, &
       volume_xy_cn, d_volume_xy_cn, &
       volume_ph_cn, d_volume_ph_cn, &
       volume_xy_rt, d_volume_xy_rt, &
       volume_ph_rt, d_volume_ph_rt, &
       volume_st_rt, d_volume_st_rt, &
       volume_xy_sm, d_volume_xy_sm, &
       volume_ca_sm, d_volume_ca_sm, &
       volume_ph_sm, d_volume_ph_sm, &
       volume_st_sm, d_volume_st_sm, &
       water_mass_xy_rt, d_water_mass_xy_rt, &
       water_mass_ph_rt, d_water_mass_ph_rt, &
       water_mass_ph_sm, d_water_mass_ph_sm, &
       water_mass_ca_sm, d_water_mass_ca_sm, &
       water_mass_xy_sm, d_water_mass_xy_sm, &
       water_mass_st_sm, d_water_mass_st_sm, &
       water_mass_ph_cn, d_water_mass_ph_cn, &
       water_mass_xy_cn, d_water_mass_xy_cn, &
       water_mass_st_cn, d_water_mass_st_cn, &
       sucrose_mass_ph_rt, d_sucrose_mass_ph_rt, &
       sucrose_mass_st_rt, d_sucrose_mass_st_rt, &
       sucrose_mass_ph_sm, d_sucrose_mass_ph_sm, &
       sucrose_mass_ca_sm, d_sucrose_mass_ca_sm, &
       sucrose_mass_st_sm, d_sucrose_mass_st_sm, &
       sucrose_mass_ph_cn, d_sucrose_mass_ph_cn, &
       sucrose_mass_st_cn, d_sucrose_mass_st_cn, &
       starch_mass_rt, d_starch_mass_rt, &
       starch_mass_sm, d_starch_mass_sm, &
       starch_mass_cn, d_starch_mass_cn, &
       turgor_pressure_xy_rt, d_turgor_pressure_xy_rt, &
       turgor_pressure_ph_rt, d_turgor_pressure_ph_rt, &
       turgor_pressure_xy_sm, d_turgor_pressure_xy_sm, &
       turgor_pressure_ca_sm, d_turgor_pressure_ca_sm, &
       turgor_pressure_ph_sm, d_turgor_pressure_ph_sm, &
       turgor_pressure_st_sm, d_turgor_pressure_st_sm, &
       turgor_pressure_xy_cn, d_turgor_pressure_xy_cn, &
       turgor_pressure_ph_cn, d_turgor_pressure_ph_cn, &
       turgor_pressure_st_cn, d_turgor_pressure_st_cn, &
       grav_pot, crown_midpoint, stem_midpoint, root_midpoint, &
       vert_resistance_xy, osmotic_pot_ca_sm, horiz_resistance_xy_ca_sm, &
       bulk_elast_mod_xy_sm, transpiration, osmotic_pot_ph_cn, &
       osmotic_pot_st_cn, bulk_elast_mod_xy_cn, vert_resistance_ph, &
       osmotic_pot_ph_rt, bulk_elast_mod_ph_rt, osmotic_pot_ph_sm, &
       osmotic_pot_st_sm, horiz_resistance_ca_ph_sm, &
       horiz_resistance_ph_st_sm, bulk_elast_mod_ph_sm, &
       bulk_elast_mod_ph_cn, bulk_elast_mod_ca_sm, bulk_elast_mod_st_sm, &
       bulk_elast_mod_st_cn, sucrose_conc_ph_rt, sucrose_conc_ph_sm, &
       sucrose_conc_ph_cn, sucrose_conc_ca_sm, sucrose_conc_st_cn, &
       sucrose_conc_st_sm, sucrose_conc_st_rt, d_maint_resp, d_growth_resp, &
       wood_density, d_wood_growth, tempk, net_photosyn, d_soil_water, &
       vert_water_flow_xy_rt_sm, vert_water_flow_xy_sm_cn, &
       vert_water_flow_ph_cn_sm, vert_water_flow_ph_sm_rt, &
       vert_sucrose_flow_ph_cn_sm, vert_sucrose_flow_ph_sm_rt, &
       loading_rate, unloading_rate)

    use consts_coms, only: wdns
    use hydr_consts_coms, only: threshold_turgor, phloem_prod_frac, &
         xylem_prod_frac, water_fraction_stem, growth_efficiency, &
         wood_sucrose_equiv, mm_sucrose

    implicit none

    real, intent(in) :: volume_st_cn
    real, intent(in) :: volume_xy_cn
    real, intent(in) :: volume_ph_cn
    real, intent(in) :: volume_xy_rt
    real, intent(in) :: volume_ph_rt
    real, intent(in) :: volume_st_rt
    real, intent(in) :: volume_xy_sm
    real, intent(in) :: volume_ca_sm
    real, intent(in) :: volume_ph_sm
    real, intent(in) :: volume_st_sm
    real, intent(in) :: water_mass_xy_rt
    real, intent(in) :: water_mass_ph_rt
    real, intent(in) :: water_mass_ph_sm
    real, intent(in) :: water_mass_ca_sm
    real, intent(in) :: water_mass_xy_sm
    real, intent(in) :: water_mass_st_sm
    real, intent(in) :: water_mass_ph_cn
    real, intent(in) :: water_mass_xy_cn
    real, intent(in) :: water_mass_st_cn
    real, intent(in) :: sucrose_mass_ph_rt
    real, intent(in) :: sucrose_mass_st_rt
    real, intent(in) :: sucrose_mass_ph_sm
    real, intent(in) :: sucrose_mass_ca_sm
    real, intent(in) :: sucrose_mass_st_sm
    real, intent(in) :: sucrose_mass_ph_cn
    real, intent(in) :: sucrose_mass_st_cn
    real, intent(in) :: starch_mass_rt
    real, intent(in) :: starch_mass_sm
    real, intent(in) :: starch_mass_cn
    real, intent(in) :: turgor_pressure_xy_rt
    real, intent(in) :: turgor_pressure_ph_rt
    real, intent(in) :: turgor_pressure_xy_sm
    real, intent(in) :: turgor_pressure_ca_sm
    real, intent(in) :: turgor_pressure_ph_sm
    real, intent(in) :: turgor_pressure_st_sm
    real, intent(in) :: turgor_pressure_xy_cn
    real, intent(in) :: turgor_pressure_ph_cn
    real, intent(in) :: turgor_pressure_st_cn

    real, intent(out) :: d_volume_st_cn
    real, intent(out) :: d_volume_xy_cn
    real, intent(out) :: d_volume_ph_cn
    real, intent(out) :: d_volume_xy_rt
    real, intent(out) :: d_volume_ph_rt
    real, intent(out) :: d_volume_st_rt
    real, intent(out) :: d_volume_xy_sm
    real, intent(out) :: d_volume_ca_sm
    real, intent(out) :: d_volume_ph_sm
    real, intent(out) :: d_volume_st_sm
    real, intent(out) :: d_water_mass_xy_rt
    real, intent(out) :: d_water_mass_ph_rt
    real, intent(out) :: d_water_mass_ph_sm
    real, intent(out) :: d_water_mass_ca_sm
    real, intent(out) :: d_water_mass_xy_sm
    real, intent(out) :: d_water_mass_st_sm
    real, intent(out) :: d_water_mass_ph_cn
    real, intent(out) :: d_water_mass_xy_cn
    real, intent(out) :: d_water_mass_st_cn
    real, intent(out) :: d_sucrose_mass_ph_rt
    real, intent(out) :: d_sucrose_mass_st_rt
    real, intent(out) :: d_sucrose_mass_ph_sm
    real, intent(out) :: d_sucrose_mass_ca_sm
    real, intent(out) :: d_sucrose_mass_st_sm
    real, intent(out) :: d_sucrose_mass_ph_cn
    real, intent(out) :: d_sucrose_mass_st_cn
    real, intent(out) :: d_starch_mass_rt
    real, intent(out) :: d_starch_mass_sm
    real, intent(out) :: d_starch_mass_cn
    real, intent(out) :: d_turgor_pressure_xy_rt
    real, intent(out) :: d_turgor_pressure_ph_rt
    real, intent(out) :: d_turgor_pressure_xy_sm
    real, intent(out) :: d_turgor_pressure_ca_sm
    real, intent(out) :: d_turgor_pressure_ph_sm
    real, intent(out) :: d_turgor_pressure_st_sm
    real, intent(out) :: d_turgor_pressure_xy_cn
    real, intent(out) :: d_turgor_pressure_ph_cn
    real, intent(out) :: d_turgor_pressure_st_cn
    real, intent(out) :: d_maint_resp
    real, intent(out) :: d_growth_resp
    real, intent(out) :: d_wood_growth

    real, intent(in) :: net_photosyn
    real, intent(in) :: wood_density
    real, intent(in) :: grav_pot
    real, intent(in) :: crown_midpoint
    real, intent(in) :: stem_midpoint
    real, intent(in) :: root_midpoint
    real, intent(in) :: vert_resistance_xy
    real, intent(in) :: vert_resistance_ph
    real, intent(in) :: horiz_resistance_xy_ca_sm
    real, intent(in) :: horiz_resistance_ca_ph_sm
    real, intent(in) :: horiz_resistance_ph_st_sm
    real, intent(in) :: bulk_elast_mod_xy_sm
    real, intent(in) :: bulk_elast_mod_ph_sm
    real, intent(in) :: bulk_elast_mod_xy_cn
    real, intent(in) :: bulk_elast_mod_ph_rt
    real, intent(in) :: bulk_elast_mod_ph_cn
    real, intent(in) :: bulk_elast_mod_ca_sm
    real, intent(in) :: bulk_elast_mod_st_sm
    real, intent(in) :: bulk_elast_mod_st_cn
    real, intent(in) :: transpiration
    real, intent(in) :: osmotic_pot_ca_sm
    real, intent(in) :: osmotic_pot_ph_rt
    real, intent(in) :: osmotic_pot_ph_cn
    real, intent(in) :: osmotic_pot_st_cn
    real, intent(in) :: osmotic_pot_ph_sm
    real, intent(in) :: osmotic_pot_st_sm
    real, intent(in) :: sucrose_conc_ph_rt
    real, intent(in) :: sucrose_conc_ph_sm
    real, intent(in) :: sucrose_conc_ca_sm
    real, intent(in) :: sucrose_conc_st_cn
    real, intent(in) :: sucrose_conc_st_sm
    real, intent(in) :: sucrose_conc_st_rt
    real, intent(in) :: sucrose_conc_ph_cn
    real, intent(in) :: tempk
    real :: horiz_water_flow_xy_ph_rt
    real :: horiz_water_flow_xy_ph_cn
    real :: horiz_water_flow_xy_st_cn
    real :: horiz_water_flow_xy_ca_sm
    real, intent(out) :: vert_water_flow_ph_sm_rt
    real, intent(out) :: vert_water_flow_ph_cn_sm
    real, intent(out) :: vert_water_flow_xy_rt_sm
    real, intent(out) :: vert_water_flow_xy_sm_cn
    real :: horiz_water_flow_ca_ph_sm
    real :: horiz_water_flow_ph_st_sm
    real :: horiz_water_flow_st_ph_cn
    real :: stress_relax_rate
    real, intent(out) :: vert_sucrose_flow_ph_sm_rt
    real, intent(out) :: vert_sucrose_flow_ph_cn_sm
    real :: horiz_sucrose_flow_ph_ca_sm
    real :: horiz_sucrose_flow_ph_st_sm
    real, intent(out) :: loading_rate
    real, intent(out) :: unloading_rate
    real :: respiration_maint_st_sm
    real :: respiration_maint_st_rt
    real :: respiration_maint_ca_sm
    real :: respiration_growth_ca_sm
    real :: starch_conversion_rate_rt
    real :: starch_conversion_rate_sm
    real :: starch_conversion_rate_cn
    real, intent(out) :: d_soil_water

    real :: water_density = wdns * 1.0e6

    d_volume_st_cn = 0.
    d_volume_xy_cn = 0.
    d_volume_ph_cn = 0.
    d_volume_xy_rt = 0.
    d_volume_ph_rt = 0.
    d_volume_st_rt = 0.
    d_volume_xy_sm = 0.
    d_volume_ca_sm = 0.
    d_volume_ph_sm = 0.
    d_volume_st_sm = 0.
    d_water_mass_xy_rt = 0.
    d_water_mass_ph_rt = 0.
    d_water_mass_ph_sm = 0.
    d_water_mass_ca_sm = 0.
    d_water_mass_xy_sm = 0.
    d_water_mass_st_sm = 0.
    d_water_mass_ph_cn = 0.
    d_water_mass_xy_cn = 0.
    d_water_mass_st_cn = 0.
    d_sucrose_mass_ph_rt = 0.
    d_sucrose_mass_st_rt = 0.
    d_sucrose_mass_ph_sm = 0.
    d_sucrose_mass_ca_sm = 0.
    d_sucrose_mass_st_sm = 0.
    d_sucrose_mass_ph_cn = 0.
    d_sucrose_mass_st_cn = 0.
    d_starch_mass_rt = 0.
    d_starch_mass_sm = 0.
    d_starch_mass_cn = 0.
    d_turgor_pressure_xy_rt = 0.
    d_turgor_pressure_ph_rt = 0.
    d_turgor_pressure_xy_sm = 0.
    d_turgor_pressure_ca_sm = 0.
    d_turgor_pressure_ph_sm = 0.
    d_turgor_pressure_st_sm = 0.
    d_turgor_pressure_xy_cn = 0.
    d_turgor_pressure_ph_cn = 0.
    d_turgor_pressure_st_cn = 0.
    d_maint_resp = 0.
    d_growth_resp = 0.
    d_wood_growth = 0.
    d_soil_water = 0.

    d_water_mass_xy_cn = d_water_mass_xy_cn - transpiration
    ! Unbalanced; should be added to atmosphere.

    d_sucrose_mass_st_cn = d_sucrose_mass_st_cn + net_photosyn *   &
         mm_sucrose / 12.
    ! Unbalanced; should be debited from atmosphere.

    ! Assume that water_mass_xy_rt is a constant.  This means that root
    ! water uptake exactly balances (vert_water_flow_xy_rt_sm + 
    ! horiz_water_flow_xy_ph_rt).

    call vert_water_flows(turgor_pressure_xy_rt, turgor_pressure_xy_sm, &
         turgor_pressure_xy_cn, grav_pot, crown_midpoint, stem_midpoint,  &
         root_midpoint, vert_resistance_xy, vert_water_flow_xy_rt_sm, &
         vert_water_flow_xy_sm_cn, vert_water_flow_ph_sm_rt, &
         vert_water_flow_ph_cn_sm, turgor_pressure_ph_rt, &
         turgor_pressure_ph_sm, turgor_pressure_ph_cn, vert_resistance_ph)

    d_water_mass_xy_sm = d_water_mass_xy_sm - vert_water_flow_xy_sm_cn
    d_water_mass_xy_cn = d_water_mass_xy_cn + vert_water_flow_xy_sm_cn

    d_water_mass_xy_sm = d_water_mass_xy_sm + vert_water_flow_xy_rt_sm
    d_soil_water = d_soil_water - vert_water_flow_xy_rt_sm

    d_water_mass_ph_rt = d_water_mass_ph_rt + vert_water_flow_ph_sm_rt
    d_water_mass_ph_sm = d_water_mass_ph_sm - vert_water_flow_ph_sm_rt

    d_water_mass_ph_sm = d_water_mass_ph_sm + vert_water_flow_ph_cn_sm
    d_water_mass_ph_cn = d_water_mass_ph_cn - vert_water_flow_ph_cn_sm

    call horiz_water_flows(turgor_pressure_xy_sm, turgor_pressure_ca_sm, &
         osmotic_pot_ca_sm, horiz_resistance_xy_ca_sm,   &
         horiz_water_flow_xy_ca_sm, horiz_water_flow_xy_ph_cn, &
         horiz_water_flow_xy_st_cn, turgor_pressure_xy_cn, &
         turgor_pressure_ph_cn, osmotic_pot_ph_cn, osmotic_pot_st_cn, &
         turgor_pressure_st_cn, horiz_water_flow_xy_ph_rt, &
         turgor_pressure_xy_rt, turgor_pressure_ph_rt, osmotic_pot_ph_rt, &
         horiz_water_flow_ca_ph_sm, horiz_water_flow_ph_st_sm, &
         turgor_pressure_ph_sm, turgor_pressure_st_sm, &
         osmotic_pot_ph_sm, osmotic_pot_st_sm, &
         horiz_resistance_ca_ph_sm, horiz_resistance_ph_st_sm, &
         horiz_water_flow_st_ph_cn)

    d_water_mass_xy_sm = d_water_mass_xy_sm - horiz_water_flow_xy_ca_sm
    d_water_mass_ca_sm = d_water_mass_ca_sm + horiz_water_flow_xy_ca_sm

    d_water_mass_xy_cn = d_water_mass_xy_cn - horiz_water_flow_xy_ph_cn
    d_water_mass_ph_cn = d_water_mass_ph_cn + horiz_water_flow_xy_ph_cn

    d_water_mass_xy_cn = d_water_mass_xy_cn - horiz_water_flow_xy_st_cn
    d_water_mass_st_cn = d_water_mass_st_cn + horiz_water_flow_xy_st_cn

    d_water_mass_ph_rt = d_water_mass_ph_rt + horiz_water_flow_xy_ph_rt
    d_soil_water = d_soil_water - horiz_water_flow_xy_ph_rt

    d_water_mass_ph_sm = d_water_mass_ph_sm + horiz_water_flow_ca_ph_sm
    d_water_mass_ca_sm = d_water_mass_ca_sm - horiz_water_flow_ca_ph_sm
    
    d_water_mass_ph_sm = d_water_mass_ph_sm - horiz_water_flow_ph_st_sm
    d_water_mass_st_sm = d_water_mass_st_sm + horiz_water_flow_ph_st_sm
    
    d_water_mass_ph_cn = d_water_mass_ph_cn + horiz_water_flow_st_ph_cn
    d_water_mass_st_cn = d_water_mass_st_cn - horiz_water_flow_st_ph_cn

    ! Vertical sucrose flows
    call vert_sucrose_flows(water_density, vert_water_flow_ph_sm_rt,  &
       vert_water_flow_ph_cn_sm, sucrose_conc_ph_cn,  &
       sucrose_conc_ph_sm, sucrose_conc_ph_rt,  &
       vert_sucrose_flow_ph_sm_rt, vert_sucrose_flow_ph_cn_sm)

    d_sucrose_mass_ph_rt = d_sucrose_mass_ph_rt + vert_sucrose_flow_ph_sm_rt
    d_sucrose_mass_ph_sm = d_sucrose_mass_ph_sm - vert_sucrose_flow_ph_sm_rt

    d_sucrose_mass_ph_sm = d_sucrose_mass_ph_sm + vert_sucrose_flow_ph_cn_sm
    d_sucrose_mass_ph_cn = d_sucrose_mass_ph_cn - vert_sucrose_flow_ph_cn_sm

    ! Horizontal sucrose flows
    call horiz_sucrose_flow(sucrose_conc_ph_sm, sucrose_conc_ca_sm, &
       sucrose_conc_st_sm, horiz_sucrose_flow_ph_ca_sm, &
       horiz_sucrose_flow_ph_st_sm)

    d_sucrose_mass_ph_sm = d_sucrose_mass_ph_sm - horiz_sucrose_flow_ph_ca_sm
    d_sucrose_mass_ca_sm = d_sucrose_mass_ca_sm + horiz_sucrose_flow_ph_ca_sm

    d_sucrose_mass_ph_sm = d_sucrose_mass_ph_sm - horiz_sucrose_flow_ph_st_sm
    d_sucrose_mass_st_sm = d_sucrose_mass_st_sm + horiz_sucrose_flow_ph_st_sm

    ! Sugar dynamics
    call sugar_dynamics(sucrose_conc_st_cn, sucrose_conc_st_rt, &
         sucrose_conc_ph_rt, loading_rate, unloading_rate, &
         sucrose_conc_st_sm, sucrose_conc_ca_sm, &
         volume_st_rt, volume_st_sm, volume_ca_sm, &
         respiration_maint_st_rt, respiration_maint_st_sm, &
         respiration_maint_ca_sm, starch_conversion_rate_rt, &
         starch_conversion_rate_sm, starch_conversion_rate_cn, tempk, &
         starch_mass_rt, starch_mass_sm, starch_mass_cn)
    
    d_sucrose_mass_ph_rt = d_sucrose_mass_ph_rt - unloading_rate
    d_sucrose_mass_st_rt = d_sucrose_mass_st_rt + unloading_rate
    d_sucrose_mass_ph_cn = d_sucrose_mass_ph_cn + loading_rate
    d_sucrose_mass_st_cn = d_sucrose_mass_st_cn - loading_rate

    d_sucrose_mass_ca_sm = d_sucrose_mass_ca_sm - respiration_maint_ca_sm
    d_maint_resp = d_maint_resp + respiration_maint_ca_sm

    d_sucrose_mass_st_sm = d_sucrose_mass_st_sm - respiration_maint_st_sm
    d_maint_resp = d_maint_resp + respiration_maint_st_sm

    d_sucrose_mass_st_rt = d_sucrose_mass_st_rt - respiration_maint_st_rt
    d_maint_resp = d_maint_resp + respiration_maint_st_rt

    d_sucrose_mass_st_rt = d_sucrose_mass_st_rt - starch_conversion_rate_rt
    d_starch_mass_rt = d_starch_mass_rt + starch_conversion_rate_rt

    d_sucrose_mass_st_sm = d_sucrose_mass_st_sm - starch_conversion_rate_sm
    d_starch_mass_sm = d_starch_mass_sm + starch_conversion_rate_sm

    d_sucrose_mass_st_cn = d_sucrose_mass_st_cn - starch_conversion_rate_cn
    d_starch_mass_cn = d_starch_mass_cn + starch_conversion_rate_cn

    ! Associated turgor rates of change
!!!    d_turgor_pressure_xy_sm = bulk_elast_mod_xy_sm / water_mass_xy_sm * &
!!!         d_water_mass_xy_sm
!!!    d_turgor_pressure_xy_cn = bulk_elast_mod_xy_cn / water_mass_xy_cn * &
!!!         d_water_mass_xy_cn
    d_turgor_pressure_ph_rt = bulk_elast_mod_ph_rt / water_mass_ph_rt * &
         d_water_mass_ph_rt
    d_turgor_pressure_ph_sm = bulk_elast_mod_ph_sm / water_mass_ph_sm * &
         d_water_mass_ph_sm
    d_turgor_pressure_ph_cn = bulk_elast_mod_ph_cn / water_mass_ph_cn * &
         d_water_mass_ph_cn
    d_turgor_pressure_ca_sm = bulk_elast_mod_ca_sm / water_mass_ca_sm * &
         d_water_mass_ca_sm
    d_turgor_pressure_st_sm = bulk_elast_mod_st_sm / water_mass_st_sm * &
         d_water_mass_st_sm
    d_turgor_pressure_st_cn = bulk_elast_mod_st_cn / water_mass_st_cn * &
         d_water_mass_st_cn

    call stress_relaxation(bulk_elast_mod_ca_sm, turgor_pressure_ca_sm, &
         stress_relax_rate)
    d_turgor_pressure_ca_sm = d_turgor_pressure_ca_sm + stress_relax_rate

    ! Associated volume rates of change
    d_volume_xy_sm = 1. / water_density * d_water_mass_xy_sm
    d_volume_ph_sm = 1. / water_density * d_water_mass_ph_sm
    d_volume_ca_sm = 1. / water_density * d_water_mass_ca_sm
    d_volume_st_sm = 1. / water_density * d_water_mass_st_sm

    if(d_water_mass_ca_sm > 0.0 .and.  &
         turgor_pressure_ca_sm > threshold_turgor)then

       respiration_growth_ca_sm = (1./growth_efficiency - 1.) / &
            (water_density * water_fraction_stem) * d_water_mass_ca_sm *  &
            wood_density * wood_sucrose_equiv
       d_sucrose_mass_ca_sm = d_sucrose_mass_ca_sm -   &
            respiration_growth_ca_sm / (1.0 - growth_efficiency)
       d_growth_resp = d_growth_resp + respiration_growth_ca_sm
       d_wood_growth = d_wood_growth + respiration_growth_ca_sm * &
            growth_efficiency / (1.0 - growth_efficiency)

       d_water_mass_ph_sm = d_water_mass_ph_sm + phloem_prod_frac *   &
            d_water_mass_ca_sm
       d_water_mass_xy_sm = d_water_mass_xy_sm + xylem_prod_frac *   &
            d_water_mass_ca_sm

       d_volume_ph_sm = d_volume_ph_sm + phloem_prod_frac / &
            (water_density * water_fraction_stem) * d_water_mass_ca_sm
       d_volume_xy_sm = d_volume_xy_sm + xylem_prod_frac / &
            (water_density * water_fraction_stem) * d_water_mass_ca_sm

       d_water_mass_ca_sm = 0.
       d_volume_ca_sm = 0.

    endif

    return
  end subroutine hydr_derivs_cohort

  subroutine sugar_dynamics(sucrose_conc_st_cn, sucrose_conc_st_rt, &
       sucrose_conc_ph_rt, loading_rate, unloading_rate, &
       sucrose_conc_st_sm, sucrose_conc_ca_sm, &
       volume_st_rt, volume_st_sm, volume_ca_sm, &
       respiration_maint_st_rt, respiration_maint_st_sm, &
       respiration_maint_ca_sm, starch_conversion_rate_rt, &
       starch_conversion_rate_sm, starch_conversion_rate_cn, tempk, &
       starch_mass_rt, starch_mass_sm, starch_mass_cn)

    use consts_coms, only: rmol
    use hydr_consts_coms, only: Vmax_loading, michmen_loading, &
         ref_unload_rate, resp_const_tissue, resp_const_sucrose, &
         water_fraction_root, water_fraction_stem, starch_conversion_const, &
         target_osm_pot, mm_sucrose

    implicit none

    real, intent(in) :: sucrose_conc_st_cn
    real, intent(in) :: sucrose_conc_st_sm
    real, intent(in) :: sucrose_conc_ca_sm
    real, intent(in) :: sucrose_conc_st_rt
    real, intent(in) :: sucrose_conc_ph_rt
    real, intent(in) :: volume_st_rt
    real, intent(in) :: volume_st_sm
    real, intent(in) :: volume_ca_sm
    real, intent(in) :: tempk
    real, intent(out) :: loading_rate
    real, intent(out) :: unloading_rate
    real, intent(out) :: respiration_maint_st_rt
    real, intent(out) :: respiration_maint_st_sm
    real, intent(out) :: respiration_maint_ca_sm
    real, intent(out) :: starch_conversion_rate_rt
    real, intent(out) :: starch_conversion_rate_sm
    real, intent(out) :: starch_conversion_rate_cn
    real :: target_sucrose_conc
    real, parameter :: univ_gas_const=rmol*1.0e-6 ! MJ/mol/K
    real :: starch_lim_factor
    real, intent(in) :: starch_mass_rt
    real, intent(in) :: starch_mass_sm
    real, intent(in) :: starch_mass_cn

    loading_rate = Vmax_loading * sucrose_conc_st_cn / (michmen_loading +   &
         sucrose_conc_st_cn)

    if(sucrose_conc_st_rt > 0.)then
       unloading_rate = ref_unload_rate * sucrose_conc_ph_rt /   &
            sucrose_conc_st_rt
    else
       unloading_rate = 0.
    endif

    respiration_maint_st_rt = (resp_const_tissue + resp_const_sucrose * &
         sucrose_conc_st_rt) * (1.0 - water_fraction_root) * volume_st_rt
    respiration_maint_st_sm = (resp_const_tissue + resp_const_sucrose * &
         sucrose_conc_st_sm) * (1.0 - water_fraction_stem) * volume_st_sm
    respiration_maint_ca_sm = (resp_const_tissue + resp_const_sucrose * &
         sucrose_conc_ca_sm) * (1.0 - water_fraction_stem) * volume_ca_sm

    target_sucrose_conc = target_osm_pot * mm_sucrose / (univ_gas_const * &
         tempk)
    if(sucrose_conc_st_rt > target_sucrose_conc)then
       starch_lim_factor = 1.
    else
       starch_lim_factor = min(1., starch_mass_rt /  &
            (0.1 * target_sucrose_conc))
    endif
    starch_conversion_rate_rt = starch_conversion_const *  &
         (sucrose_conc_st_rt - target_sucrose_conc) * starch_lim_factor

    if(sucrose_conc_st_sm > target_sucrose_conc)then
       starch_lim_factor = 1.
    else
       starch_lim_factor = min(1., starch_mass_sm /  &
            (0.1 * target_sucrose_conc))
    endif
    starch_conversion_rate_sm = starch_conversion_const *   &
         (sucrose_conc_st_sm - target_sucrose_conc) * starch_lim_factor

    if(sucrose_conc_st_cn > target_sucrose_conc)then
       starch_lim_factor = 1.
    else
       starch_lim_factor = min(1., starch_mass_cn /  &
            (0.1 * target_sucrose_conc))
    endif
    starch_conversion_rate_cn = starch_conversion_const *  &
         (sucrose_conc_st_cn - target_sucrose_conc) * starch_lim_factor

    return
  end subroutine sugar_dynamics

  subroutine horiz_sucrose_flow(sucrose_conc_ph_sm, sucrose_conc_ca_sm, &
       sucrose_conc_st_sm, horiz_sucrose_flow_ph_ca_sm, &
       horiz_sucrose_flow_ph_st_sm)
    use hydr_consts_coms, only: diff_const_sucrose_ph_ca, &
         diff_const_sucrose_ph_st
    implicit none

    real, intent(in) :: sucrose_conc_ph_sm
    real, intent(in) :: sucrose_conc_ca_sm
    real, intent(in) :: sucrose_conc_st_sm
    real, intent(out) :: horiz_sucrose_flow_ph_ca_sm
    real, intent(out) :: horiz_sucrose_flow_ph_st_sm

    horiz_sucrose_flow_ph_ca_sm = diff_const_sucrose_ph_ca *   &
         (sucrose_conc_ph_sm - sucrose_conc_ca_sm)
    horiz_sucrose_flow_ph_st_sm = diff_const_sucrose_ph_st *   &
         (sucrose_conc_ph_sm - sucrose_conc_st_sm)

    return
  end subroutine horiz_sucrose_flow

  subroutine vert_sucrose_flows(water_density, vert_water_flow_ph_sm_rt,  &
       vert_water_flow_ph_cn_sm, sucrose_conc_ph_cn,  &
       sucrose_conc_ph_sm, sucrose_conc_ph_rt,  &
       vert_sucrose_flow_ph_sm_rt, vert_sucrose_flow_ph_cn_sm)
    implicit none
    real, intent(in) :: vert_water_flow_ph_sm_rt
    real, intent(in) :: vert_water_flow_ph_cn_sm
    real, intent(in) :: sucrose_conc_ph_rt
    real, intent(in) :: sucrose_conc_ph_sm
    real, intent(in) :: sucrose_conc_ph_cn
    real, intent(in) :: water_density
    real, intent(out) :: vert_sucrose_flow_ph_sm_rt
    real, intent(out) :: vert_sucrose_flow_ph_cn_sm

    if(vert_water_flow_ph_sm_rt >= 0.0)then
       vert_sucrose_flow_ph_sm_rt = sucrose_conc_ph_sm *   &
            vert_water_flow_ph_sm_rt / water_density
    else
       vert_sucrose_flow_ph_sm_rt = sucrose_conc_ph_rt *   &
            vert_water_flow_ph_sm_rt / water_density
    endif
    
    if(vert_water_flow_ph_cn_sm >= 0.0)then
       vert_sucrose_flow_ph_cn_sm = sucrose_conc_ph_cn *   &
            vert_water_flow_ph_cn_sm / water_density
    else
       vert_sucrose_flow_ph_cn_sm = sucrose_conc_ph_sm *   &
            vert_water_flow_ph_cn_sm / water_density
    endif


    return
  end subroutine vert_sucrose_flows

  subroutine stress_relaxation(bulk_elast_mod_ca_sm, turgor_pressure_ca_sm, &
       stress_relax_rate)
    use hydr_consts_coms, only: cell_wall_extensibility, threshold_turgor
    implicit none

    real, intent(in) :: bulk_elast_mod_ca_sm
    real, intent(in) :: turgor_pressure_ca_sm
    real, intent(out) :: stress_relax_rate

    stress_relax_rate = -bulk_elast_mod_ca_sm * cell_wall_extensibility * &
         max(0., turgor_pressure_ca_sm - threshold_turgor)

    return
  end subroutine stress_relaxation

  subroutine horiz_water_flows(turgor_pressure_xy_sm, turgor_pressure_ca_sm, &
         osmotic_pot_ca_sm, horiz_resistance_xy_ca_sm,  &
         horiz_water_flow_xy_ca_sm, horiz_water_flow_xy_ph_cn, &
         horiz_water_flow_xy_st_cn, turgor_pressure_xy_cn, &
         turgor_pressure_ph_cn, osmotic_pot_ph_cn, osmotic_pot_st_cn, &
         turgor_pressure_st_cn, horiz_water_flow_xy_ph_rt, &
         turgor_pressure_xy_rt, turgor_pressure_ph_rt, osmotic_pot_ph_rt, &
         horiz_water_flow_ca_ph_sm, horiz_water_flow_ph_st_sm,  &
         turgor_pressure_ph_sm, turgor_pressure_st_sm, &
         osmotic_pot_ph_sm, osmotic_pot_st_sm, &
         horiz_resistance_ca_ph_sm, horiz_resistance_ph_st_sm, &
         horiz_water_flow_st_ph_cn)
    use hydr_consts_coms, only: reflection_coefficient, &
         horiz_resistance_xy_ph_cn, horiz_resistance_xy_st_cn, &
         horiz_resistance_xy_ph_rt, horiz_resistance_ph_st_cn
    implicit none
    
    real, intent(in) :: turgor_pressure_xy_rt
    real, intent(in) :: turgor_pressure_ph_rt
    real, intent(in) :: turgor_pressure_xy_sm
    real, intent(in) :: turgor_pressure_ca_sm
    real, intent(in) :: turgor_pressure_ph_sm
    real, intent(in) :: turgor_pressure_st_sm
    real, intent(in) :: turgor_pressure_xy_cn
    real, intent(in) :: turgor_pressure_ph_cn
    real, intent(in) :: turgor_pressure_st_cn
    real, intent(in) :: osmotic_pot_ph_rt
    real, intent(in) :: osmotic_pot_ca_sm
    real, intent(in) :: osmotic_pot_ph_sm
    real, intent(in) :: osmotic_pot_st_sm
    real, intent(in) :: osmotic_pot_ph_cn
    real, intent(in) :: osmotic_pot_st_cn
    real, intent(in) :: horiz_resistance_xy_ca_sm
    real, intent(in) :: horiz_resistance_ca_ph_sm
    real, intent(in) :: horiz_resistance_ph_st_sm
    real, intent(out) :: horiz_water_flow_xy_ca_sm
    real, intent(out) :: horiz_water_flow_xy_ph_cn
    real, intent(out) :: horiz_water_flow_xy_st_cn
    real, intent(out) :: horiz_water_flow_xy_ph_rt
    real, intent(out) :: horiz_water_flow_ca_ph_sm
    real, intent(out) :: horiz_water_flow_ph_st_sm
    real, intent(out) :: horiz_water_flow_st_ph_cn

    horiz_water_flow_xy_ca_sm = (turgor_pressure_xy_sm -   &
         turgor_pressure_ca_sm + reflection_coefficient *   &
         osmotic_pot_ca_sm) / horiz_resistance_xy_ca_sm

    horiz_water_flow_xy_ph_cn = (turgor_pressure_xy_cn -  &
         turgor_pressure_ph_cn + reflection_coefficient *  &
         osmotic_pot_ph_cn) / horiz_resistance_xy_ph_cn
    
    horiz_water_flow_xy_st_cn = (turgor_pressure_xy_cn - &
         turgor_pressure_st_cn + reflection_coefficient *  &
         osmotic_pot_st_cn) / horiz_resistance_xy_st_cn

    horiz_water_flow_xy_ph_rt = (turgor_pressure_xy_rt -  &
         turgor_pressure_ph_rt + reflection_coefficient *  &
         osmotic_pot_ph_rt) / horiz_resistance_xy_ph_rt

    horiz_water_flow_ca_ph_sm = (turgor_pressure_ca_sm -  &
         turgor_pressure_ph_sm - reflection_coefficient *  &
         (osmotic_pot_ca_sm - osmotic_pot_ph_sm)) / horiz_resistance_ca_ph_sm

    horiz_water_flow_ph_st_sm = (turgor_pressure_ph_sm -  &
         turgor_pressure_st_sm - reflection_coefficient *  &
         (osmotic_pot_ph_sm - osmotic_pot_st_sm)) / horiz_resistance_ph_st_sm

    horiz_water_flow_st_ph_cn = (turgor_pressure_st_cn -  &
         turgor_pressure_ph_cn - reflection_coefficient *  &
         (osmotic_pot_st_cn - osmotic_pot_ph_cn)) / horiz_resistance_ph_st_cn

    return
  end subroutine horiz_water_flows

  subroutine vert_water_flows(turgor_pressure_xy_rt,   &
       turgor_pressure_xy_sm, turgor_pressure_xy_cn, grav_pot,  &
       crown_midpoint, stem_midpoint, root_midpoint, vert_resistance_xy, &
       vert_water_flow_xy_rt_sm, vert_water_flow_xy_sm_cn, &
       vert_water_flow_ph_sm_rt, vert_water_flow_ph_cn_sm, &
       turgor_pressure_ph_rt, turgor_pressure_ph_sm, &
       turgor_pressure_ph_cn, vert_resistance_ph)

    implicit none

    real, intent(in) :: turgor_pressure_xy_rt
    real, intent(in) :: turgor_pressure_xy_sm
    real, intent(in) :: turgor_pressure_xy_cn
    real, intent(in) :: turgor_pressure_ph_rt
    real, intent(in) :: turgor_pressure_ph_sm
    real, intent(in) :: turgor_pressure_ph_cn
    real, intent(in) :: grav_pot
    real, intent(in) :: crown_midpoint
    real, intent(in) :: stem_midpoint
    real, intent(in) :: root_midpoint
    real, intent(in) :: vert_resistance_xy
    real, intent(in) :: vert_resistance_ph
    real, intent(out) :: vert_water_flow_xy_rt_sm
    real, intent(out) :: vert_water_flow_xy_sm_cn
    real, intent(out) :: vert_water_flow_ph_sm_rt
    real, intent(out) :: vert_water_flow_ph_cn_sm

    vert_water_flow_xy_rt_sm = (turgor_pressure_xy_rt - &
         turgor_pressure_xy_sm - grav_pot * (stem_midpoint-root_midpoint)) / &
         vert_resistance_xy

    vert_water_flow_xy_sm_cn = (turgor_pressure_xy_sm - &
         turgor_pressure_xy_cn - grav_pot * (crown_midpoint-stem_midpoint)) / &
         vert_resistance_xy

    vert_water_flow_ph_sm_rt = (turgor_pressure_ph_sm - &
         turgor_pressure_ph_rt + grav_pot * (stem_midpoint-root_midpoint)) / &
         vert_resistance_ph

    vert_water_flow_ph_cn_sm = (turgor_pressure_ph_cn - &
         turgor_pressure_ph_sm + grav_pot * (crown_midpoint-stem_midpoint)) / &
         vert_resistance_ph

    return
  end subroutine vert_water_flows

end Module hydr_derivs
