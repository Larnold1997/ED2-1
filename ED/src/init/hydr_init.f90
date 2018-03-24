!!!

Module hydr_init
  use consts_coms, only: rmol
  implicit none

  real, parameter :: univ_gas_const=rmol*1.0e-6 ! MJ/mol/K
  
Contains

  subroutine hydr_diag_vars(stem_height, &
       temperature, &
       soil_water, slmsts, slpots, slbs, krdepth, &
       heartwood_diameter, &
       water_mass_ph_rt, water_mass_xy_sm,  &
       water_mass_ca_sm, water_mass_ph_sm, water_mass_st_sm, &
       water_mass_ph_cn, water_mass_st_cn, &
       sucrose_mass_ph_rt, sucrose_mass_st_rt, sucrose_mass_ca_sm, &
       sucrose_mass_ph_sm, sucrose_mass_st_sm, sucrose_mass_ph_cn, &
       sucrose_mass_st_cn,  &
       volume_st_rt, &
       volume_xy_sm, volume_ph_sm, volume_st_sm, volume_ca_sm, &
       turgor_pressure_ph_rt, turgor_pressure_xy_sm, &
       turgor_pressure_ca_sm, turgor_pressure_ph_sm, turgor_pressure_st_sm, &
       turgor_pressure_ph_cn, turgor_pressure_st_cn, &
       soil_water_potential, turgor_pressure_xy_rt, &
       xylem_diameter, &
       cambium_diameter, phloem_diameter, storage_diameter, &
       surface_area_xy, surface_area_ca, surface_area_ph, &
       vert_resistance_xy, vert_resistance_ph, &
       sucrose_conc_ph_rt, sucrose_conc_st_rt, sucrose_conc_ca_sm, &
       sucrose_conc_ph_sm, sucrose_conc_st_sm, sucrose_conc_ph_cn, &
       sucrose_conc_st_cn, &
       osmotic_pot_ph_rt, osmotic_pot_st_rt, osmotic_pot_ca_sm, &
       osmotic_pot_ph_sm, osmotic_pot_st_sm, osmotic_pot_ph_cn, &
       osmotic_pot_st_cn,  &
       horiz_resistance_xy_ca_sm, horiz_resistance_ca_ph_sm, &
       horiz_resistance_ph_st_sm,  &
       bulk_elast_mod_xy_sm, bulk_elast_mod_xy_cn, &
       bulk_elast_mod_ph_rt, bulk_elast_mod_ph_sm, bulk_elast_mod_ph_cn, &
       bulk_elast_mod_st_sm, bulk_elast_mod_st_cn, bulk_elast_mod_ca_sm, &
       turgor_pressure_xy_cn, &
       turgor_pressure_xy_sm_base, turgor_pressure_xy_cn_base,  &
       water_mass_xy_cn)

    use grid_coms,   only: nzg
    use soil_coms,   only: dslz, slz
    use consts_coms, only: wdns, grav, pi1
    use hydr_state_vars, only: sap_viscosity
    use hydr_consts_coms, only: vert_permeability_xy, vert_permeability_ph, &
         plc_steepness, plc50, water_fraction_root, mm_sucrose, &
         rad_hydraul_conductivity, max_bulk_elast_mod_xy,  &
         max_bulk_elast_mod_ph, max_bulk_elast_mod_ca, max_bulk_elast_mod_st

    
    implicit none

    real, intent(in) :: water_mass_xy_cn
    real, intent(in) :: turgor_pressure_xy_sm_base
    real, intent(in) :: turgor_pressure_xy_cn_base
    real, intent(in) :: temperature  ! Celsius
    real, intent(out) :: turgor_pressure_xy_cn
    real, intent(in) :: stem_height
    real, intent(in), dimension(nzg) :: soil_water
    real, intent(in) :: slmsts
    real, intent(in) :: slpots
    real, intent(in) :: slbs
    integer, intent(in) :: krdepth
    real, intent(in) :: heartwood_diameter
    real, intent(in) :: water_mass_xy_sm
    real, intent(in) :: water_mass_ph_sm
    real, intent(in) :: water_mass_ph_cn
    real, intent(in) :: water_mass_ph_rt
    real, intent(in) :: water_mass_st_cn
    real, intent(in) :: water_mass_st_sm
    real, intent(in) :: water_mass_ca_sm
    real, intent(inout) :: sucrose_mass_ph_sm
    real, intent(in) :: sucrose_mass_ph_rt
    real, intent(in) :: sucrose_mass_st_rt
    real, intent(inout) :: sucrose_mass_ca_sm
    real, intent(inout) :: sucrose_mass_st_sm
    real, intent(in) :: sucrose_mass_ph_cn
    real, intent(in) :: sucrose_mass_st_cn
    real, intent(in) :: volume_st_rt
    real, intent(in) :: volume_st_sm
    real, intent(in) :: volume_xy_sm
    real, intent(in) :: volume_ph_sm
    real, intent(in) :: volume_ca_sm
    real, intent(out) :: turgor_pressure_xy_sm
    real, intent(in) :: turgor_pressure_ph_cn
    real, intent(in) :: turgor_pressure_ph_rt
    real, intent(in) :: turgor_pressure_st_cn
    real, intent(inout) :: turgor_pressure_ph_sm
    real, intent(inout) :: turgor_pressure_st_sm
    real, intent(inout) :: turgor_pressure_ca_sm
    real, intent(out) :: soil_water_potential
    real, intent(out) :: turgor_pressure_xy_rt
    real, intent(out) :: xylem_diameter
    real, intent(out) :: cambium_diameter
    real, intent(out) :: phloem_diameter
    real, intent(out) :: storage_diameter
    real, intent(out) :: surface_area_ca
    real, intent(out) :: surface_area_xy
    real, intent(out) :: surface_area_ph
    real, intent(out) :: vert_resistance_xy
    real, intent(out) :: vert_resistance_ph
    real, intent(out) :: sucrose_conc_ph_rt
    real, intent(out) :: sucrose_conc_ph_sm
    real, intent(out) :: sucrose_conc_st_sm
    real, intent(out) :: sucrose_conc_st_rt
    real, intent(out) :: sucrose_conc_ca_sm
    real, intent(out) :: sucrose_conc_ph_cn
    real, intent(out) :: sucrose_conc_st_cn
    real, intent(out) :: osmotic_pot_st_rt
    real, intent(out) :: osmotic_pot_ph_rt
    real, intent(out) :: osmotic_pot_ca_sm
    real, intent(out) :: osmotic_pot_ph_sm
    real, intent(out) :: osmotic_pot_st_sm
    real, intent(out) :: osmotic_pot_ph_cn
    real, intent(out) :: osmotic_pot_st_cn
    real, intent(out) :: horiz_resistance_xy_ca_sm
    real, intent(out) :: horiz_resistance_ca_ph_sm
    real, intent(out) :: horiz_resistance_ph_st_sm
    real, intent(out) :: bulk_elast_mod_xy_sm
    real, intent(out) :: bulk_elast_mod_xy_cn
    real, intent(out) :: bulk_elast_mod_ph_rt
    real, intent(out) :: bulk_elast_mod_ph_sm
    real, intent(out) :: bulk_elast_mod_ph_cn
    real, intent(out) :: bulk_elast_mod_st_sm
    real, intent(out) :: bulk_elast_mod_st_cn
    real, intent(out) :: bulk_elast_mod_ca_sm
    real :: root_avail_water
    integer :: k
    real :: wgpfrac
    real :: viscosity
    real :: pct_loss_conductance
    real, parameter :: water_density = 1.0e6 * wdns
    real :: total_stem_sucrose_conc

    root_avail_water = 0.
    do k = krdepth, nzg
       root_avail_water = root_avail_water + soil_water(k)*dslz(k) !m3/m2
    enddo
    root_avail_water = root_avail_water / (-slz(krdepth)) !m3/m3
    wgpfrac = root_avail_water / slmsts
    soil_water_potential = wdns * grav * slpots / wgpfrac**slbs * 1.0e-6
    turgor_pressure_xy_rt = soil_water_potential
    
    xylem_diameter = sqrt(4.0 * volume_xy_sm / &
         (pi1 * stem_height) + heartwood_diameter**2)
    surface_area_xy = pi1 * xylem_diameter *   &
         stem_height
    
    cambium_diameter = sqrt(4.0 * volume_ca_sm /  &
         (pi1 * stem_height) + xylem_diameter**2)
    surface_area_ca = pi1 * cambium_diameter *  &
         stem_height
    
    phloem_diameter = sqrt(4.0 * volume_ph_sm /  &
         (pi1 * stem_height) + cambium_diameter**2)
    surface_area_ph = pi1 * phloem_diameter *  &
         stem_height
    
    storage_diameter = sqrt(4.0 * volume_st_sm /  &
         (pi1 * stem_height) + phloem_diameter**2)
    
    sucrose_conc_ph_rt = sucrose_mass_ph_rt *   &
         water_density / water_mass_ph_rt
    sucrose_conc_ph_sm = sucrose_mass_ph_sm *   &
         water_density / water_mass_ph_sm
    sucrose_conc_ph_cn = sucrose_mass_ph_cn *   &
         water_density / water_mass_ph_cn
    sucrose_conc_st_rt = sucrose_mass_st_rt *   &
         water_density / (volume_st_rt * water_fraction_root *  &
         water_density)
    sucrose_conc_st_sm = sucrose_mass_st_sm *   &
         water_density / water_mass_st_sm
    sucrose_conc_st_cn = sucrose_mass_st_cn *   &
         water_density / water_mass_st_cn
    sucrose_conc_ca_sm = sucrose_mass_ca_sm *   &
         water_density / water_mass_ca_sm

    ! Assume horizontal sucrose transfers are instantaneous.
!    total_stem_sucrose_conc = (sucrose_mass_ca_sm + sucrose_mass_ph_sm + &
!         sucrose_mass_st_sm) * water_density / (water_mass_ca_sm + &
!         water_mass_ph_sm + water_mass_st_sm)
!    sucrose_conc_ca_sm = total_stem_sucrose_conc
!    sucrose_conc_ph_sm = total_stem_sucrose_conc
!    sucrose_conc_st_sm = total_stem_sucrose_conc
!    sucrose_mass_ca_sm = sucrose_conc_ca_sm / water_density * water_mass_ca_sm
!    sucrose_mass_ph_sm = sucrose_conc_ph_sm / water_density * water_mass_ph_sm
!    sucrose_mass_st_sm = sucrose_conc_st_sm / water_density * water_mass_st_sm
    
    osmotic_pot_ph_rt = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_ph_rt
    osmotic_pot_ph_sm = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_ph_sm
    osmotic_pot_ph_cn = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_ph_cn
    osmotic_pot_ca_sm = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_ca_sm
    osmotic_pot_st_rt = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_st_rt
    osmotic_pot_st_sm = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_st_sm
    osmotic_pot_st_cn = univ_gas_const / MM_sucrose *   &
         temperature * sucrose_conc_st_cn
    
    horiz_resistance_xy_ca_sm = 1./(rad_hydraul_conductivity * &
         surface_area_xy * water_density)
    horiz_resistance_ca_ph_sm = 1./(rad_hydraul_conductivity *   &
         surface_area_ca * water_density)
    horiz_resistance_ph_st_sm = 1./(rad_hydraul_conductivity *   &
         surface_area_ph * water_density)
    
    bulk_elast_mod_xy_sm = max_bulk_elast_mod_xy
    bulk_elast_mod_xy_cn = max_bulk_elast_mod_xy
    turgor_pressure_xy_sm = turgor_pressure_xy_sm_base +   &
         bulk_elast_mod_xy_sm * log(water_mass_xy_sm)
    turgor_pressure_xy_cn = turgor_pressure_xy_cn_base +   &
         bulk_elast_mod_xy_cn * log(water_mass_xy_cn)

!print*,1,turgor_pressure_ph_sm,turgor_pressure_ca_sm,turgor_pressure_st_sm    
!    turgor_pressure_ph_sm = turgor_pressure_xy_sm + osmotic_pot_ph_sm
!    turgor_pressure_ca_sm = turgor_pressure_xy_sm + osmotic_pot_ca_sm
!    turgor_pressure_st_sm = turgor_pressure_xy_sm + osmotic_pot_st_sm
!print*,2,turgor_pressure_ph_sm,turgor_pressure_ca_sm,turgor_pressure_st_sm    


    bulk_elast_mod_ph_rt = 0.001 + (max_bulk_elast_mod_ph -  &
         0.001) / (1.0 + exp(-20.0*(turgor_pressure_ph_rt-0.1)))
    bulk_elast_mod_ph_sm = 0.001 + (max_bulk_elast_mod_ph -  &
         0.001) / (1.0 + exp(-20.0*(turgor_pressure_ph_sm-0.1)))
    bulk_elast_mod_ph_cn = 0.001 + (max_bulk_elast_mod_ph -  &
         0.001) / (1.0 + exp(-20.0*(turgor_pressure_ph_cn-0.1)))
    bulk_elast_mod_st_sm = 0.001 + (max_bulk_elast_mod_st -  &
         0.001) / (1.0 + exp(-20.0*(turgor_pressure_st_sm-0.1)))
    bulk_elast_mod_st_cn = 0.001 + (max_bulk_elast_mod_st -  &
         0.001) / (1.0 + exp(-20.0*(turgor_pressure_st_cn-0.1)))
    bulk_elast_mod_ca_sm = 0.001 + (max_bulk_elast_mod_ca -  &
         0.001) / (1.0 + exp(-20.0*(turgor_pressure_ca_sm-0.1)))
    ! Q: Not at all sure about the crown, root equation.

    ! Vertical resistances
    call sap_viscosity(sucrose_mass_ph_sm,   &
         water_mass_ph_sm, viscosity)
    vert_resistance_ph = viscosity * stem_height /   &
         (vert_permeability_ph * 0.25 * pi1 *   &
         (phloem_diameter**2 - &
         cambium_diameter**2) * wdns) * 1.0e-12
    
    call sap_viscosity(0.0, water_mass_xy_sm, viscosity)
    vert_resistance_xy = viscosity * stem_height /   &
         (vert_permeability_xy * 0.25 * pi1 *   &
         (xylem_diameter**2 - &
         heartwood_diameter**2) * wdns) * 1.0e-12

    ! Pammenter equation
    pct_loss_conductance = 1.0 /   &
         (1.0 + exp(plc_steepness * (turgor_pressure_xy_sm -  &
         plc50)))
    vert_resistance_xy = vert_resistance_xy /  &
         max(1.0e-10, (1.0 - pct_loss_conductance))
    

    return
  end subroutine hydr_diag_vars

  subroutine hydr_init_vars(stem_height, &
       bleaf, sla, dbh, temperature, &
       bsapwood, bdead, &
       soil_water, slmsts, slpots, slbs, krdepth, &
       root_midpoint, stem_midpoint, crown_midpoint, &
       volume_st_cn, volume_xy_cn, volume_ph_cn, &
       volume_st_rt, volume_xy_rt, volume_ph_rt, &
       xylem_diameter, heartwood_diameter, &
       cambium_diameter, phloem_diameter, storage_diameter, &
       volume_xy_sm, volume_ph_sm, volume_st_sm, volume_ca_sm, &
       surface_area_xy, surface_area_ca, surface_area_ph, &
       water_mass_ph_rt, water_mass_xy_sm, water_mass_xy_rt, &
       water_mass_ca_sm, water_mass_ph_sm, water_mass_st_sm, &
       water_mass_xy_cn, water_mass_ph_cn, water_mass_st_cn, &
       sucrose_mass_ph_rt, sucrose_mass_st_rt, sucrose_mass_ca_sm, &
       sucrose_mass_ph_sm, sucrose_mass_st_sm, sucrose_mass_ph_cn, &
       sucrose_mass_st_cn,  &
       sucrose_conc_ph_rt, sucrose_conc_st_rt, sucrose_conc_ca_sm, &
       sucrose_conc_ph_sm, sucrose_conc_st_sm, sucrose_conc_ph_cn, &
       sucrose_conc_st_cn, target_sucrose_conc,   &
       turgor_pressure_xy_rt, turgor_pressure_ph_rt, turgor_pressure_xy_sm, &
       turgor_pressure_ca_sm, turgor_pressure_ph_sm, turgor_pressure_st_sm, &
       turgor_pressure_xy_cn, turgor_pressure_ph_cn, turgor_pressure_st_cn, &
       soil_water_potential, &
       starch_mass_rt, starch_mass_sm, starch_mass_cn,  &
       osmotic_pot_ph_rt, osmotic_pot_st_rt, osmotic_pot_ca_sm, &
       osmotic_pot_ph_sm, osmotic_pot_st_sm, osmotic_pot_ph_cn, &
       osmotic_pot_st_cn,  &
       horiz_resistance_xy_ca_sm, horiz_resistance_ca_ph_sm, &
       horiz_resistance_ph_st_sm,  &
       vert_resistance_xy, vert_resistance_ph, &
       bulk_elast_mod_xy_sm, bulk_elast_mod_xy_cn, &
       bulk_elast_mod_ph_rt, bulk_elast_mod_ph_sm, bulk_elast_mod_ph_cn, &
       bulk_elast_mod_st_sm, bulk_elast_mod_st_cn, bulk_elast_mod_ca_sm, &
       turgor_pressure_xy_sm_base, turgor_pressure_xy_cn_base)

    use consts_coms, only: wdns, grav, pi1
    use hydr_consts_coms,   only: crown_pct_xy, crown_pct_ph, &
                           water_fraction_root, water_fraction_stem,  &
                           water_fraction_crown, & 
                           vert_permeability_xy, vert_permeability_ph, &
			   plc_steepness, plc50, & 
                           max_bulk_elast_mod_xy, max_bulk_elast_mod_ph, &
                           leaf_thickness, crown_pct_st, target_osm_pot, &
                           mm_sucrose, vmax_loading, michmen_loading, &
                           ref_unload_rate, rad_hydraul_conductivity, &
                           max_bulk_elast_mod_ca, max_bulk_elast_mod_st
    use hydr_state_vars, only: sap_viscosity
    use soil_coms,   only: slz, dslz
    use grid_coms,   only: nzg

    implicit none

    integer, intent(in) :: krdepth
    real, intent(in) :: stem_height, bleaf, sla
    real, intent(in) :: bsapwood, bdead 
    real, intent(in) :: dbh
    real, intent(in) :: temperature  ! Celsius
    real, intent(in) :: slmsts, slpots, slbs
    real, intent(in), dimension(nzg) :: soil_water
    real, intent(out) :: root_midpoint, stem_midpoint, crown_midpoint
    real, intent(out) :: volume_st_rt, volume_st_sm, volume_st_cn
    real, intent(out) :: volume_xy_rt, volume_xy_sm, volume_xy_cn
    real, intent(out) :: volume_ph_rt, volume_ph_sm, volume_ph_cn
    real, intent(out) :: volume_ca_sm
    real, intent(out) :: xylem_diameter, heartwood_diameter
    real, intent(out) :: phloem_diameter, cambium_diameter
    real, intent(out) :: storage_diameter
    real, intent(out) :: surface_area_ca
    real, intent(out) :: surface_area_xy, surface_area_ph
    real, intent(out) :: water_mass_xy_rt, water_mass_xy_sm, water_mass_xy_cn
    real, intent(out) :: water_mass_ph_rt, water_mass_ph_sm, water_mass_ph_cn 
    real, intent(out) :: water_mass_ca_sm
    real, intent(out) :: water_mass_st_sm, water_mass_st_cn
    real, intent(out) :: sucrose_mass_ph_rt, sucrose_mass_st_rt
    real, intent(out) :: sucrose_mass_ca_sm, sucrose_mass_ph_sm
    real, intent(out) :: sucrose_mass_st_sm, sucrose_mass_ph_cn
    real, intent(out) :: sucrose_mass_st_cn
    real, intent(out) :: sucrose_conc_ph_rt, sucrose_conc_st_rt
    real, intent(out) :: sucrose_conc_ph_sm, sucrose_conc_ca_sm
    real, intent(out) :: sucrose_conc_st_sm, sucrose_conc_ph_cn
    real, intent(out) :: sucrose_conc_st_cn
    real, intent(out) :: target_sucrose_conc
    real, intent(out) :: turgor_pressure_xy_rt, turgor_pressure_xy_sm, turgor_pressure_xy_cn
    real, intent(out) :: turgor_pressure_ph_rt, turgor_pressure_ph_sm, turgor_pressure_ph_cn
    real, intent(out) :: turgor_pressure_ca_sm
    real, intent(out) :: turgor_pressure_st_sm
    real, intent(out) :: turgor_pressure_st_cn
    real, intent(out) :: soil_water_potential
    real, intent(out) :: starch_mass_rt, starch_mass_sm, starch_mass_cn
    real, intent(out) :: osmotic_pot_st_rt, osmotic_pot_ph_rt
    real, intent(out) :: osmotic_pot_ca_sm
    real, intent(out) :: osmotic_pot_ph_sm, osmotic_pot_st_sm
    real, intent(out) :: osmotic_pot_ph_cn
    real, intent(out) :: osmotic_pot_st_cn
    real, intent(out) :: horiz_resistance_xy_ca_sm, horiz_resistance_ca_ph_sm
    real, intent(out) :: horiz_resistance_ph_st_sm
    real, intent(out) :: vert_resistance_xy, vert_resistance_ph
    real, intent(out) :: bulk_elast_mod_xy_sm, bulk_elast_mod_xy_cn
    real, intent(out) :: bulk_elast_mod_ph_rt, bulk_elast_mod_ph_sm, bulk_elast_mod_ph_cn
    real, intent(out) :: bulk_elast_mod_st_sm
    real, intent(out) :: bulk_elast_mod_st_cn, bulk_elast_mod_ca_sm
    real, intent(out) :: turgor_pressure_xy_sm_base
    real, intent(out) :: turgor_pressure_xy_cn_base

    ! Assuming root depth is 20% of above-ground crown height.

    real :: crown_volume, wgpfrac, viscosity, pct_loss_conductance, sapwood_area
    real :: water_density
    real :: root_avail_water
    integer :: k
    real :: interp_int, interp_slope

    ! Height midpoints
    root_midpoint  = stem_height * 0.2 * 0.5
    stem_midpoint  = stem_height * 0.5
    crown_midpoint = stem_height

    ! Crown, root volumes
    crown_volume = bleaf * sla * leaf_thickness
    volume_st_cn = crown_volume * crown_pct_st
    volume_xy_cn = crown_volume * crown_pct_xy
    volume_ph_cn = crown_volume * crown_pct_ph
    volume_st_rt = volume_st_cn
    volume_xy_rt = volume_xy_cn
    volume_ph_rt = volume_ph_cn

    ! Diameters (based off of De Schepper's T1).  Only thing that needs
    ! to change for ED initialization type.
    ! Once allometries are changed, replace bdead_old = bdead_new - bsapwood
    interp_slope = (.4056-1.685e-2)/(.417-1.95e-2)
    interp_int = 1.685e-2 - 1.95e-2 * interp_slope
    xylem_diameter   = interp_slope * dbh * 0.01 + interp_int
    heartwood_diameter = xylem_diameter / sqrt(1. + bsapwood / bdead)
    interp_slope = (.4063-.4056-1.7e-2+1.685e-2)/(.417-1.95e-2)
    interp_int = 1.7e-2-1.685e-2-1.95e-2 * interp_slope
    cambium_diameter = xylem_diameter + dbh * 0.01 * interp_slope + interp_int
    interp_slope = (.4086-.4063-1.74e-2+1.70e-2)/(.417-1.95e-2)
    interp_int = 1.74e-2-1.70e-2-1.95e-2 * interp_slope
    phloem_diameter  = cambium_diameter + dbh * 0.01 * interp_slope +  &
         interp_int
    storage_diameter = dbh * 0.01

    ! Stem volumes
    sapwood_area = pi1*0.25*(xylem_diameter**2 - heartwood_diameter**2)
    volume_xy_sm = sapwood_area*stem_height
    volume_ca_sm = pi1*stem_height*0.25*(cambium_diameter**2 - &
                   xylem_diameter**2)
    volume_ph_sm = pi1*stem_height*0.25*(phloem_diameter**2 - &
                   cambium_diameter**2)
    volume_st_sm = pi1*stem_height*0.25*(storage_diameter**2 - &
                   phloem_diameter**2)

    ! Surface areas

    surface_area_xy = sapwood_area
    surface_area_ca = pi1 * cambium_diameter * stem_height
    surface_area_ph = pi1 * phloem_diameter * stem_height
    
    ! Water mass
    water_density = wdns * 1.0e6
    water_mass_xy_rt = water_fraction_root * volume_xy_rt * water_density
    ! Q: Assumed to be constant.
    water_mass_ph_rt = water_fraction_root * volume_ph_rt * water_density
    water_mass_xy_sm = water_fraction_stem * volume_xy_sm * water_density
    water_mass_ca_sm = water_fraction_stem * volume_ca_sm * water_density
    water_mass_ph_sm = water_fraction_stem * volume_ph_sm * water_density
    water_mass_st_sm = water_fraction_stem * volume_st_sm * water_density
    water_mass_xy_cn = water_fraction_crown * volume_xy_cn * water_density
    water_mass_ph_cn = water_fraction_crown * volume_ph_cn * water_density
    water_mass_st_cn = water_fraction_crown * volume_st_cn * water_density

    ! Turgor pressures
    turgor_pressure_ph_rt = 1.1
    turgor_pressure_xy_sm = -0.75
!    turgor_pressure_xy_sm = soil_water_potential - 0.01
    turgor_pressure_ca_sm = 1.12
    turgor_pressure_ph_sm = 1.12
    turgor_pressure_st_sm = 1.12
    turgor_pressure_xy_cn = -1.5
!    turgor_pressure_xy_cn = soil_water_potential - 0.02
    turgor_pressure_ph_cn = 1.13
    turgor_pressure_st_cn = 1.13

    ! Sucrose concentrations
    target_sucrose_conc = target_osm_pot * MM_sucrose / (univ_gas_const * &
         (temperature+273.15))

    sucrose_conc_ph_rt = Vmax_loading * target_sucrose_conc**2 /   &
         (michmen_loading + target_sucrose_conc) / ref_unload_rate

    sucrose_conc_st_rt = target_sucrose_conc
!    sucrose_conc_ph_sm = Vmax_loading * target_sucrose_conc / &
!         (michmen_loading + target_sucrose_conc) * water_density / &
!         (turgor_pressure_ph_sm - turgor_pressure_ph_rt) * vert_resistance_ph
    sucrose_conc_ph_sm = target_sucrose_conc

    sucrose_conc_ca_sm = sucrose_conc_ph_sm
    sucrose_conc_st_sm = target_sucrose_conc
!    sucrose_conc_ph_cn = Vmax_loading * target_sucrose_conc / &
!         (michmen_loading + target_sucrose_conc) * water_density / &
!         (turgor_pressure_ph_cn - turgor_pressure_ph_sm) * vert_resistance_ph
    sucrose_conc_ph_cn = target_sucrose_conc

    sucrose_conc_st_cn = target_sucrose_conc

    sucrose_mass_ph_rt = sucrose_conc_ph_rt / water_density * water_mass_ph_rt
    sucrose_mass_st_rt = sucrose_conc_st_rt / water_density * volume_st_rt * &
         water_fraction_root * water_density
    sucrose_mass_ca_sm = sucrose_conc_ca_sm / water_density * water_mass_ca_sm
    sucrose_mass_ph_sm = sucrose_conc_ph_sm / water_density * water_mass_ph_sm
    sucrose_mass_st_sm = sucrose_conc_st_sm / water_density * water_mass_st_sm
    sucrose_mass_ph_cn = sucrose_conc_ph_cn / water_density * water_mass_ph_cn
    sucrose_mass_st_cn = sucrose_conc_st_cn / water_density * water_mass_st_cn
    
    ! Starch
    starch_mass_rt = sucrose_mass_st_rt
    starch_mass_sm = sucrose_mass_st_rt
    starch_mass_cn = sucrose_mass_st_rt

    ! Effective soil water potential
     ! DK: assuming that soil property is constant with depth &
     ! (i.e. constant slpots and slbs )
     ! root_avail_water: water combined from surface to rooting depth
    root_avail_water = 0.
    do k=krdepth,nzg
       root_avail_water = root_avail_water + soil_water(k)*dslz(k) !m3/m2
    end do
    root_avail_water = root_avail_water / (-slz(krdepth)) !m3/m3

    wgpfrac = root_avail_water / slmsts

    soil_water_potential = wdns * grav * slpots / wgpfrac**slbs * 1.0e-6
    ! Q: Currently, this is assumed to be equal to soil water potential.  
    ! However, see appendix to Zweifel et al. (2007)
    turgor_pressure_xy_rt = soil_water_potential 

    ! Resistances

    ! Need to guess the sucrose mass to calculate viscosity
    call sap_viscosity(0.001* water_mass_ph_sm, water_mass_ph_sm, viscosity)
    vert_resistance_ph = viscosity * stem_height /   &
         (vert_permeability_ph * 0.25 * pi1 * (phloem_diameter**2 - &
         cambium_diameter**2) * wdns)
    ! Pa s m m3 / (m2 m2 kg)
    ! N s m m3 / (m2 m2 m2 kg)
    ! N s / (m2 kg)
    ! kg m / (s m2 kg)
    ! 1 / (s m)  ! units of vert_resistance_ph
    !!!!  mg / (s MPa) = 1e-3 g / (s MPa) = 1e-9 g / (s Pa) = 1e-12 kg / (s Pa)
    !!!!               = 1e-12 kg m s2 / (s kg) = 1e-12 m s 
    !!!! resistance: (MPa s) / mg = 10^12 / (m s)
!!!! 1 / (m s) = 10^-12 (MPa s) / mg
    vert_resistance_ph = vert_resistance_ph * 1.0e-12

    call sap_viscosity(0.0, water_mass_xy_sm, viscosity)
    vert_resistance_xy = viscosity * stem_height /   &
         (vert_permeability_xy * sapwood_area * wdns) * 1.0e-12

    ! Pammenter equation
    !Pammenter N.W. & Willigen C.V. (1998) A mathematical and statistical
    !analysis of the curves illustrating vulnerability of xylem to
    !cavitation. Tree Physiology 18, 589–593.
    pct_loss_conductance = 1.0 /   &
         (1.0 + exp(plc_steepness * (turgor_pressure_xy_sm - plc50)))
    vert_resistance_xy = vert_resistance_xy / max(1.0e-10,   &
         (1.0 - pct_loss_conductance))

    ! Osmotic potentials
    osmotic_pot_st_rt = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_st_rt / MM_sucrose
    osmotic_pot_ph_rt = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_ph_rt / MM_sucrose
    osmotic_pot_ca_sm = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_ca_sm / MM_sucrose
    osmotic_pot_ph_sm = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_ph_sm / MM_sucrose
    osmotic_pot_st_sm = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_st_sm / MM_sucrose
    osmotic_pot_ph_cn = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_ph_cn / MM_sucrose
    osmotic_pot_st_cn = univ_gas_const * (temperature+273.15) *   &
         sucrose_conc_st_cn / MM_sucrose

    horiz_resistance_xy_ca_sm = 1./(rad_hydraul_conductivity *   &
         surface_area_xy * water_density)
    horiz_resistance_ca_ph_sm = 1./(rad_hydraul_conductivity *   &
         surface_area_ca * water_density)
    horiz_resistance_ph_st_sm = 1./(rad_hydraul_conductivity *   &
         surface_area_ph * water_density)
    
    ! 1. Calculate resistance, bulk elastic modulus
    bulk_elast_mod_xy_sm = max_bulk_elast_mod_xy
    bulk_elast_mod_xy_cn = max_bulk_elast_mod_xy
    
    turgor_pressure_xy_sm_base = turgor_pressure_xy_sm -  &
         bulk_elast_mod_xy_sm * log(water_mass_xy_sm)
    turgor_pressure_xy_cn_base = turgor_pressure_xy_cn -  &
         bulk_elast_mod_xy_cn * log(water_mass_xy_cn)

    bulk_elast_mod_ph_rt = 0.001 + (max_bulk_elast_mod_ph - 0.001)/ &
         (1.0 + exp(-20.0*(turgor_pressure_ph_rt-0.1)))
    bulk_elast_mod_ph_sm = 0.001 + (max_bulk_elast_mod_ph - 0.001)/ &
         (1.0 + exp(-20.0*(turgor_pressure_ph_sm-0.1)))
    bulk_elast_mod_ph_cn = 0.001 + (max_bulk_elast_mod_ph - 0.001)/ &
         (1.0 + exp(-20.0*(turgor_pressure_ph_cn-0.1)))
    
    bulk_elast_mod_st_sm = 0.001 + (max_bulk_elast_mod_st - 0.001)/ &
         (1.0 + exp(-20.0*(turgor_pressure_st_sm-0.1)))
    bulk_elast_mod_st_cn = 0.001 + (max_bulk_elast_mod_st - 0.001)/ &
         (1.0 + exp(-20.0*(turgor_pressure_st_cn-0.1)))
    
    bulk_elast_mod_ca_sm = 0.001 + (max_bulk_elast_mod_ca - 0.001)/ &
         (1.0 + exp(-20.0*(turgor_pressure_ca_sm-0.1)))
    ! Q: Not at all sure about the crown, root equation.

!    water_root_uptake = 0.

    return
  end subroutine hydr_init_vars

end Module hydr_init
