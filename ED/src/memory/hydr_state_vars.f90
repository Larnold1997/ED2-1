Module hydr_state_vars
  implicit none

  type hydr_vars
     
     ! Water mass (mg)
     real, dimension(:), allocatable :: water_mass_xy_rt  ! root
     real, dimension(:), allocatable :: water_mass_xy_sm  ! stem
     real, dimension(:), allocatable :: water_mass_xy_cn  ! crown
     real, dimension(:), allocatable :: water_mass_ph_rt  ! root
     real, dimension(:), allocatable :: water_mass_ph_sm  ! stem
     real, dimension(:), allocatable :: water_mass_ph_cn  ! crown
     real, dimension(:), allocatable :: water_mass_ca_sm  ! stem
     real, dimension(:), allocatable :: water_mass_st_cn  ! crown
     real, dimension(:), allocatable :: water_mass_st_sm  ! stem
     
     ! Turgor pressure (MPa)  
     real, dimension(:), allocatable :: turgor_pressure_xy_rt
     real, dimension(:), allocatable :: turgor_pressure_ph_rt
     real, dimension(:), allocatable :: turgor_pressure_xy_sm
     real, dimension(:), allocatable :: turgor_pressure_ca_sm
     real, dimension(:), allocatable :: turgor_pressure_ph_sm
     real, dimension(:), allocatable :: turgor_pressure_st_sm
     real, dimension(:), allocatable :: turgor_pressure_xy_cn
     real, dimension(:), allocatable :: turgor_pressure_ph_cn
     real, dimension(:), allocatable :: turgor_pressure_st_cn
     
     real, dimension(:), allocatable :: turgor_pressure_xy_sm_base
     real, dimension(:), allocatable :: turgor_pressure_xy_cn_base

     ! Mid point of a section (m)
     real, dimension(:), allocatable :: root_midpoint
     real, dimension(:), allocatable :: stem_midpoint
     real, dimension(:), allocatable :: crown_midpoint
     
     ! Surface area (m2)
     real, dimension(:), allocatable :: surface_area_xy
     real, dimension(:), allocatable :: surface_area_ca
     real, dimension(:), allocatable :: surface_area_ph
     
     ! Bulk elastic moduli (MPa)
     real, dimension(:), allocatable :: bulk_elast_mod_xy_sm
     real, dimension(:), allocatable :: bulk_elast_mod_xy_cn
     real, dimension(:), allocatable :: bulk_elast_mod_ph_rt
     real, dimension(:), allocatable :: bulk_elast_mod_ph_sm
     real, dimension(:), allocatable :: bulk_elast_mod_ph_cn
     real, dimension(:), allocatable :: bulk_elast_mod_st_sm
     real, dimension(:), allocatable :: bulk_elast_mod_st_cn
     real, dimension(:), allocatable :: bulk_elast_mod_ca_sm
     
     ! Fluxes (mg / s)
     real, dimension(:), allocatable :: vert_water_flow_xy_rt_sm
     real, dimension(:), allocatable :: vert_water_flow_xy_sm_cn
     real, dimension(:), allocatable :: vert_water_flow_ph_cn_sm
     real, dimension(:), allocatable :: vert_water_flow_ph_sm_rt
     real, dimension(:), allocatable :: horiz_water_flow_xy_ph_rt
     real, dimension(:), allocatable :: horiz_water_flow_xy_ca_sm
     real, dimension(:), allocatable :: horiz_water_flow_ca_ph_sm
     real, dimension(:), allocatable :: horiz_water_flow_ph_st_sm
     real, dimension(:), allocatable :: horiz_water_flow_xy_ph_cn
     real, dimension(:), allocatable :: horiz_water_flow_xy_st_cn
     real, dimension(:), allocatable :: horiz_water_flow_st_ph_cn
     real, dimension(:), allocatable :: vert_sucrose_flow_ph_sm_rt
     real, dimension(:), allocatable :: vert_sucrose_flow_ph_cn_sm     
     real, dimension(:), allocatable :: horiz_sucrose_flow_ph_ca_sm
     real, dimension(:), allocatable :: horiz_sucrose_flow_ph_st_sm

     ! Soil water potential (MPa)
     real, dimension(:), allocatable :: soil_water_potential
     
     ! Vertical resistance (MPa s / mg)
     real, dimension(:), allocatable :: vert_resistance_xy
     real, dimension(:), allocatable :: vert_resistance_ph
     
     ! Volume (m3)
     real, dimension(:), allocatable :: volume_xy_rt
     real, dimension(:), allocatable :: volume_xy_sm
     real, dimension(:), allocatable :: volume_xy_cn
     real, dimension(:), allocatable :: volume_ph_rt
     real, dimension(:), allocatable :: volume_ph_sm
     real, dimension(:), allocatable :: volume_ph_cn
     real, dimension(:), allocatable :: volume_st_rt
     real, dimension(:), allocatable :: volume_st_sm
     real, dimension(:), allocatable :: volume_st_cn
     real, dimension(:), allocatable :: volume_ca_sm
     
     ! Diameter (cm)
     real, dimension(:), allocatable :: xylem_diameter
     real, dimension(:), allocatable :: heartwood_diameter
     real, dimension(:), allocatable :: phloem_diameter
     real, dimension(:), allocatable :: cambium_diameter
     real, dimension(:), allocatable :: storage_diameter
     
     ! Sucrose mass (mg)
     real, dimension(:), allocatable :: sucrose_mass_ph_rt
     real, dimension(:), allocatable :: sucrose_mass_st_rt
     real, dimension(:), allocatable :: sucrose_mass_ca_sm
     real, dimension(:), allocatable :: sucrose_mass_ph_sm
     real, dimension(:), allocatable :: sucrose_mass_st_sm
     real, dimension(:), allocatable :: sucrose_mass_ph_cn
     real, dimension(:), allocatable :: sucrose_mass_st_cn

     ! Sucrose concentration (mg/m3)
     real, dimension(:), allocatable :: sucrose_conc_ph_rt
     real, dimension(:), allocatable :: sucrose_conc_st_rt
     real, dimension(:), allocatable :: sucrose_conc_ca_sm
     real, dimension(:), allocatable :: sucrose_conc_ph_sm
     real, dimension(:), allocatable :: sucrose_conc_st_sm
     real, dimension(:), allocatable :: sucrose_conc_ph_cn
     real, dimension(:), allocatable :: sucrose_conc_st_cn

     ! Starch mass (mg)
     real, dimension(:), allocatable :: starch_mass_rt
     real, dimension(:), allocatable :: starch_mass_sm
     real, dimension(:), allocatable :: starch_mass_cn

     ! Osmotic potentials (MPa)
     real, dimension(:), allocatable :: osmotic_pot_ph_rt
     real, dimension(:), allocatable :: osmotic_pot_st_rt
     real, dimension(:), allocatable :: osmotic_pot_ca_sm
     real, dimension(:), allocatable :: osmotic_pot_ph_sm
     real, dimension(:), allocatable :: osmotic_pot_st_sm
     real, dimension(:), allocatable :: osmotic_pot_ph_cn
     real, dimension(:), allocatable :: osmotic_pot_st_cn

     ! Horizontal resistances (MPa s/mg)
     real, dimension(:), allocatable :: horiz_resistance_xy_ca_sm
     real, dimension(:), allocatable :: horiz_resistance_ca_ph_sm
     real, dimension(:), allocatable :: horiz_resistance_ph_st_sm

     ! Target sucrose concentration (mg/m3)
     real, dimension(:), allocatable :: target_sucrose_conc

     ! Sucrose dynamics
     real, dimension(:), allocatable :: loading_rate
     real, dimension(:), allocatable :: unloading_rate

     real, dimension(:), allocatable :: starch_conversion_rate_rt
     real, dimension(:), allocatable :: starch_conversion_rate_sm
     real, dimension(:), allocatable :: starch_conversion_rate_cn

     real, dimension(:), allocatable :: respiration_maint_st_rt
     real, dimension(:), allocatable :: respiration_maint_st_sm
     real, dimension(:), allocatable :: respiration_maint_ca_sm

     real, dimension(:), allocatable :: respiration_growth_ca_sm

     ! Stress relaxation
     real, dimension(:), allocatable :: stress_relaxation

     real, dimension(:), allocatable :: water_root_uptake

  end type hydr_vars
 

Contains

  subroutine hydr_allocate(hydr, ncohorts)

    implicit none

    type(hydr_vars) :: hydr
    integer, intent(in) :: ncohorts

    allocate(hydr%water_mass_xy_rt(ncohorts))
    allocate(hydr%water_mass_xy_sm(ncohorts))
    allocate(hydr%water_mass_xy_cn(ncohorts))
    allocate(hydr%water_mass_ph_rt(ncohorts))
    allocate(hydr%water_mass_ph_sm(ncohorts))
    allocate(hydr%water_mass_ph_cn(ncohorts))
    allocate(hydr%water_mass_ca_sm(ncohorts))
    allocate(hydr%water_mass_st_cn(ncohorts))
    allocate(hydr%water_mass_st_sm(ncohorts))

    allocate(hydr%turgor_pressure_xy_rt(ncohorts))
    allocate(hydr%turgor_pressure_ph_rt(ncohorts))
    allocate(hydr%turgor_pressure_xy_sm(ncohorts))
    allocate(hydr%turgor_pressure_ca_sm(ncohorts))
    allocate(hydr%turgor_pressure_ph_sm(ncohorts))
    allocate(hydr%turgor_pressure_st_sm(ncohorts))
    allocate(hydr%turgor_pressure_xy_cn(ncohorts))
    allocate(hydr%turgor_pressure_ph_cn(ncohorts))
    allocate(hydr%turgor_pressure_st_cn(ncohorts))

    allocate(hydr%turgor_pressure_xy_sm_base(ncohorts))
    allocate(hydr%turgor_pressure_xy_cn_base(ncohorts))

    allocate(hydr%root_midpoint(ncohorts))
    allocate(hydr%stem_midpoint(ncohorts))
    allocate(hydr%crown_midpoint(ncohorts))

    allocate(hydr%surface_area_xy(ncohorts))
    allocate(hydr%surface_area_ca(ncohorts))
    allocate(hydr%surface_area_ph(ncohorts))

    allocate(hydr%bulk_elast_mod_xy_sm(ncohorts))
    allocate(hydr%bulk_elast_mod_xy_cn(ncohorts))
    allocate(hydr%bulk_elast_mod_ph_rt(ncohorts))
    allocate(hydr%bulk_elast_mod_ph_sm(ncohorts))
    allocate(hydr%bulk_elast_mod_ph_cn(ncohorts))
    allocate(hydr%bulk_elast_mod_st_sm(ncohorts))
    allocate(hydr%bulk_elast_mod_st_cn(ncohorts))
    allocate(hydr%bulk_elast_mod_ca_sm(ncohorts))

    allocate(hydr%vert_water_flow_xy_rt_sm(ncohorts))
    allocate(hydr%vert_water_flow_xy_sm_cn(ncohorts))
    allocate(hydr%vert_water_flow_ph_cn_sm(ncohorts))
    allocate(hydr%vert_water_flow_ph_sm_rt(ncohorts))

    allocate(hydr%horiz_water_flow_xy_ph_rt(ncohorts))
    allocate(hydr%horiz_water_flow_xy_ca_sm(ncohorts))
    allocate(hydr%horiz_water_flow_ca_ph_sm(ncohorts))
    allocate(hydr%horiz_water_flow_ph_st_sm(ncohorts))
    allocate(hydr%horiz_water_flow_xy_ph_cn(ncohorts))
    allocate(hydr%horiz_water_flow_xy_st_cn(ncohorts))
    allocate(hydr%horiz_water_flow_st_ph_cn(ncohorts))

    allocate(hydr%vert_sucrose_flow_ph_sm_rt(ncohorts))
    allocate(hydr%vert_sucrose_flow_ph_cn_sm(ncohorts))

    allocate(hydr%horiz_sucrose_flow_ph_ca_sm(ncohorts))
    allocate(hydr%horiz_sucrose_flow_ph_st_sm(ncohorts))

    allocate(hydr%soil_water_potential(ncohorts))

    allocate(hydr%vert_resistance_xy(ncohorts))
    allocate(hydr%vert_resistance_ph(ncohorts))

    allocate(hydr%volume_xy_rt(ncohorts))
    allocate(hydr%volume_xy_sm(ncohorts))
    allocate(hydr%volume_xy_cn(ncohorts))
    allocate(hydr%volume_ph_rt(ncohorts))
    allocate(hydr%volume_ph_sm(ncohorts))
    allocate(hydr%volume_ph_cn(ncohorts))
    allocate(hydr%volume_st_rt(ncohorts))
    allocate(hydr%volume_st_sm(ncohorts))
    allocate(hydr%volume_st_cn(ncohorts))
    allocate(hydr%volume_ca_sm(ncohorts))

    allocate(hydr%xylem_diameter(ncohorts))
    allocate(hydr%heartwood_diameter(ncohorts))
    allocate(hydr%phloem_diameter(ncohorts))
    allocate(hydr%cambium_diameter(ncohorts))
    allocate(hydr%storage_diameter(ncohorts))

    allocate(hydr%sucrose_mass_ph_rt(ncohorts))
    allocate(hydr%sucrose_mass_st_rt(ncohorts))
    allocate(hydr%sucrose_mass_ca_sm(ncohorts))
    allocate(hydr%sucrose_mass_ph_sm(ncohorts))
    allocate(hydr%sucrose_mass_st_sm(ncohorts))
    allocate(hydr%sucrose_mass_ph_cn(ncohorts))
    allocate(hydr%sucrose_mass_st_cn(ncohorts))

    allocate(hydr%sucrose_conc_ph_rt(ncohorts))
    allocate(hydr%sucrose_conc_st_rt(ncohorts))
    allocate(hydr%sucrose_conc_ca_sm(ncohorts))
    allocate(hydr%sucrose_conc_ph_sm(ncohorts))
    allocate(hydr%sucrose_conc_st_sm(ncohorts))
    allocate(hydr%sucrose_conc_ph_cn(ncohorts))
    allocate(hydr%sucrose_conc_st_cn(ncohorts))

    allocate(hydr%starch_mass_rt(ncohorts))
    allocate(hydr%starch_mass_sm(ncohorts))
    allocate(hydr%starch_mass_cn(ncohorts))

    allocate(hydr%osmotic_pot_ph_rt(ncohorts))
    allocate(hydr%osmotic_pot_st_rt(ncohorts))
    allocate(hydr%osmotic_pot_ca_sm(ncohorts))
    allocate(hydr%osmotic_pot_ph_sm(ncohorts))
    allocate(hydr%osmotic_pot_st_sm(ncohorts))
    allocate(hydr%osmotic_pot_ph_cn(ncohorts))
    allocate(hydr%osmotic_pot_st_cn(ncohorts))

    allocate(hydr%horiz_resistance_xy_ca_sm(ncohorts))
    allocate(hydr%horiz_resistance_ca_ph_sm(ncohorts))
    allocate(hydr%horiz_resistance_ph_st_sm(ncohorts))

    allocate(hydr%target_sucrose_conc(ncohorts))

    allocate(hydr%loading_rate(ncohorts))
    allocate(hydr%unloading_rate(ncohorts))

    allocate(hydr%starch_conversion_rate_rt(ncohorts))
    allocate(hydr%starch_conversion_rate_sm(ncohorts))
    allocate(hydr%starch_conversion_rate_cn(ncohorts))

    allocate(hydr%respiration_maint_st_rt(ncohorts))
    allocate(hydr%respiration_maint_st_sm(ncohorts))
    allocate(hydr%respiration_maint_ca_sm(ncohorts))

    allocate(hydr%respiration_growth_ca_sm(ncohorts))

    allocate(hydr%stress_relaxation(ncohorts))

    allocate(hydr%water_root_uptake(ncohorts))

    return
  end subroutine hydr_allocate


  subroutine hydr_deallocate(hydr)
    implicit none
    type(hydr_vars) :: hydr

    deallocate(hydr%water_mass_xy_rt)
    deallocate(hydr%water_mass_xy_sm)
    deallocate(hydr%water_mass_xy_cn)
    deallocate(hydr%water_mass_ph_rt)
    deallocate(hydr%water_mass_ph_sm)
    deallocate(hydr%water_mass_ph_cn)
    deallocate(hydr%water_mass_ca_sm)
    deallocate(hydr%water_mass_st_cn)
    deallocate(hydr%water_mass_st_sm)

    deallocate(hydr%turgor_pressure_xy_rt)
    deallocate(hydr%turgor_pressure_ph_rt)
    deallocate(hydr%turgor_pressure_xy_sm)
    deallocate(hydr%turgor_pressure_ca_sm)
    deallocate(hydr%turgor_pressure_ph_sm)
    deallocate(hydr%turgor_pressure_st_sm)
    deallocate(hydr%turgor_pressure_xy_cn)
    deallocate(hydr%turgor_pressure_ph_cn)
    deallocate(hydr%turgor_pressure_st_cn)

    deallocate(hydr%turgor_pressure_xy_sm_base)
    deallocate(hydr%turgor_pressure_xy_cn_base)

    deallocate(hydr%root_midpoint)
    deallocate(hydr%stem_midpoint)
    deallocate(hydr%crown_midpoint)

    deallocate(hydr%surface_area_xy)
    deallocate(hydr%surface_area_ca)
    deallocate(hydr%surface_area_ph)

    deallocate(hydr%bulk_elast_mod_xy_sm)
    deallocate(hydr%bulk_elast_mod_xy_cn)
    deallocate(hydr%bulk_elast_mod_ph_rt)
    deallocate(hydr%bulk_elast_mod_ph_sm)
    deallocate(hydr%bulk_elast_mod_ph_cn)
    deallocate(hydr%bulk_elast_mod_st_sm)
    deallocate(hydr%bulk_elast_mod_st_cn)
    deallocate(hydr%bulk_elast_mod_ca_sm)

    deallocate(hydr%vert_water_flow_xy_rt_sm)
    deallocate(hydr%vert_water_flow_xy_sm_cn)
    deallocate(hydr%vert_water_flow_ph_cn_sm)
    deallocate(hydr%vert_water_flow_ph_sm_rt)

    deallocate(hydr%horiz_water_flow_xy_ph_rt)
    deallocate(hydr%horiz_water_flow_xy_ca_sm)
    deallocate(hydr%horiz_water_flow_ca_ph_sm)
    deallocate(hydr%horiz_water_flow_ph_st_sm)
    deallocate(hydr%horiz_water_flow_xy_ph_cn)
    deallocate(hydr%horiz_water_flow_xy_st_cn)
    deallocate(hydr%horiz_water_flow_st_ph_cn)

    deallocate(hydr%vert_sucrose_flow_ph_sm_rt)
    deallocate(hydr%vert_sucrose_flow_ph_cn_sm)

    deallocate(hydr%horiz_sucrose_flow_ph_ca_sm)
    deallocate(hydr%horiz_sucrose_flow_ph_st_sm)

    deallocate(hydr%soil_water_potential)

    deallocate(hydr%vert_resistance_xy)
    deallocate(hydr%vert_resistance_ph)

    deallocate(hydr%volume_xy_rt)
    deallocate(hydr%volume_xy_sm)
    deallocate(hydr%volume_xy_cn)
    deallocate(hydr%volume_ph_rt)
    deallocate(hydr%volume_ph_sm)
    deallocate(hydr%volume_ph_cn)
    deallocate(hydr%volume_st_rt)
    deallocate(hydr%volume_st_sm)
    deallocate(hydr%volume_st_cn)
    deallocate(hydr%volume_ca_sm)

    deallocate(hydr%xylem_diameter)
    deallocate(hydr%heartwood_diameter)
    deallocate(hydr%phloem_diameter)
    deallocate(hydr%cambium_diameter)
    deallocate(hydr%storage_diameter)

    deallocate(hydr%sucrose_mass_ph_rt)
    deallocate(hydr%sucrose_mass_st_rt)
    deallocate(hydr%sucrose_mass_ca_sm)
    deallocate(hydr%sucrose_mass_ph_sm)
    deallocate(hydr%sucrose_mass_st_sm)
    deallocate(hydr%sucrose_mass_ph_cn)
    deallocate(hydr%sucrose_mass_st_cn)

    deallocate(hydr%sucrose_conc_ph_rt)
    deallocate(hydr%sucrose_conc_st_rt)
    deallocate(hydr%sucrose_conc_ca_sm)
    deallocate(hydr%sucrose_conc_ph_sm)
    deallocate(hydr%sucrose_conc_st_sm)
    deallocate(hydr%sucrose_conc_ph_cn)
    deallocate(hydr%sucrose_conc_st_cn)

    deallocate(hydr%starch_mass_rt)
    deallocate(hydr%starch_mass_sm)
    deallocate(hydr%starch_mass_cn)

    deallocate(hydr%osmotic_pot_ph_rt)
    deallocate(hydr%osmotic_pot_st_rt)
    deallocate(hydr%osmotic_pot_ca_sm)
    deallocate(hydr%osmotic_pot_ph_sm)
    deallocate(hydr%osmotic_pot_st_sm)
    deallocate(hydr%osmotic_pot_ph_cn)
    deallocate(hydr%osmotic_pot_st_cn)

    deallocate(hydr%horiz_resistance_xy_ca_sm)
    deallocate(hydr%horiz_resistance_ca_ph_sm)
    deallocate(hydr%horiz_resistance_ph_st_sm)

    deallocate(hydr%target_sucrose_conc)

    deallocate(hydr%loading_rate)
    deallocate(hydr%unloading_rate)

    deallocate(hydr%starch_conversion_rate_rt)
    deallocate(hydr%starch_conversion_rate_sm)
    deallocate(hydr%starch_conversion_rate_cn)

    deallocate(hydr%respiration_maint_st_rt)
    deallocate(hydr%respiration_maint_st_sm)
    deallocate(hydr%respiration_maint_ca_sm)

    deallocate(hydr%respiration_growth_ca_sm)

    deallocate(hydr%stress_relaxation)

    deallocate(hydr%water_root_uptake)

    return
  end subroutine hydr_deallocate

  
  
  subroutine copy_hydrtype_mask(hydr_in, hydr_out, inc, mask, masksz)
    ! All variables should be included here.
    implicit none
    
    type(hydr_vars) :: hydr_in, hydr_out
    integer, intent(in) :: inc, masksz
    logical, dimension(masksz), intent(in) :: mask

    hydr_out%water_mass_xy_rt(1:inc) = pack(hydr_in%water_mass_xy_rt,mask)
    hydr_out%water_mass_xy_sm(1:inc) = pack(hydr_in%water_mass_xy_sm,mask)
    hydr_out%water_mass_xy_cn(1:inc) = pack(hydr_in%water_mass_xy_cn,mask)
    hydr_out%water_mass_ph_rt(1:inc) = pack(hydr_in%water_mass_ph_rt,mask)
    hydr_out%water_mass_ph_sm(1:inc) = pack(hydr_in%water_mass_ph_sm,mask)
    hydr_out%water_mass_ph_cn(1:inc) = pack(hydr_in%water_mass_ph_cn,mask)
    hydr_out%water_mass_ca_sm(1:inc) = pack(hydr_in%water_mass_ca_sm,mask)
    hydr_out%water_mass_st_cn(1:inc) = pack(hydr_in%water_mass_st_cn,mask)
    hydr_out%water_mass_st_sm(1:inc) = pack(hydr_in%water_mass_st_sm,mask)

    hydr_out%turgor_pressure_xy_rt(1:inc) = pack(hydr_in%turgor_pressure_xy_rt,mask)
    hydr_out%turgor_pressure_ph_rt(1:inc) = pack(hydr_in%turgor_pressure_ph_rt,mask)
    hydr_out%turgor_pressure_xy_sm(1:inc) = pack(hydr_in%turgor_pressure_xy_sm,mask)
    hydr_out%turgor_pressure_ca_sm(1:inc) = pack(hydr_in%turgor_pressure_ca_sm,mask)
    hydr_out%turgor_pressure_ph_sm(1:inc) = pack(hydr_in%turgor_pressure_ph_sm,mask)
    hydr_out%turgor_pressure_st_sm(1:inc) = pack(hydr_in%turgor_pressure_st_sm,mask)
    hydr_out%turgor_pressure_xy_cn(1:inc) = pack(hydr_in%turgor_pressure_xy_cn,mask)
    hydr_out%turgor_pressure_ph_cn(1:inc) = pack(hydr_in%turgor_pressure_ph_cn,mask)
    hydr_out%turgor_pressure_st_cn(1:inc) = pack(hydr_in%turgor_pressure_st_cn,mask)

    hydr_out%turgor_pressure_xy_sm_base(1:inc) = pack(hydr_in%turgor_pressure_xy_sm_base,mask)
    hydr_out%turgor_pressure_xy_cn_base(1:inc) = pack(hydr_in%turgor_pressure_xy_cn_base,mask)

    hydr_out%root_midpoint(1:inc) = pack(hydr_in%root_midpoint,mask)
    hydr_out%stem_midpoint(1:inc) = pack(hydr_in%stem_midpoint,mask)
    hydr_out%crown_midpoint(1:inc) = pack(hydr_in%crown_midpoint,mask)
    
    hydr_out%surface_area_xy(1:inc) = pack(hydr_in%surface_area_xy,mask)
    hydr_out%surface_area_ca(1:inc) = pack(hydr_in%surface_area_ca,mask)
    hydr_out%surface_area_ph(1:inc) = pack(hydr_in%surface_area_ph,mask)

    hydr_out%bulk_elast_mod_xy_sm(1:inc) = pack(hydr_in%bulk_elast_mod_xy_sm,mask)
    hydr_out%bulk_elast_mod_xy_cn(1:inc) = pack(hydr_in%bulk_elast_mod_xy_cn,mask)
    hydr_out%bulk_elast_mod_ph_rt(1:inc) = pack(hydr_in%bulk_elast_mod_ph_rt,mask)
    hydr_out%bulk_elast_mod_ph_sm(1:inc) = pack(hydr_in%bulk_elast_mod_ph_sm,mask)
    hydr_out%bulk_elast_mod_ph_cn(1:inc) = pack(hydr_in%bulk_elast_mod_ph_cn,mask)
    hydr_out%bulk_elast_mod_st_sm(1:inc) = pack(hydr_in%bulk_elast_mod_st_sm,mask)
    hydr_out%bulk_elast_mod_st_cn(1:inc) = pack(hydr_in%bulk_elast_mod_st_cn,mask)
    hydr_out%bulk_elast_mod_ca_sm(1:inc) = pack(hydr_in%bulk_elast_mod_ca_sm,mask)

    hydr_out%vert_water_flow_xy_rt_sm(1:inc) = pack(hydr_in%vert_water_flow_xy_rt_sm,mask)
    hydr_out%vert_water_flow_xy_sm_cn(1:inc) = pack(hydr_in%vert_water_flow_xy_sm_cn,mask)
    hydr_out%vert_water_flow_ph_cn_sm(1:inc) = pack(hydr_in%vert_water_flow_ph_cn_sm,mask)
    hydr_out%vert_water_flow_ph_sm_rt(1:inc) = pack(hydr_in%vert_water_flow_ph_sm_rt,mask)

    hydr_out%horiz_water_flow_xy_ph_rt(1:inc) = pack(hydr_in%horiz_water_flow_xy_ph_rt,mask)
    hydr_out%horiz_water_flow_xy_ca_sm(1:inc) = pack(hydr_in%horiz_water_flow_xy_ca_sm,mask)
    hydr_out%horiz_water_flow_ca_ph_sm(1:inc) = pack(hydr_in%horiz_water_flow_ca_ph_sm,mask)
    hydr_out%horiz_water_flow_ph_st_sm(1:inc) = pack(hydr_in%horiz_water_flow_ph_st_sm,mask)
    hydr_out%horiz_water_flow_xy_ph_cn(1:inc) = pack(hydr_in%horiz_water_flow_xy_ph_cn,mask)
    hydr_out%horiz_water_flow_xy_st_cn(1:inc) = pack(hydr_in%horiz_water_flow_xy_st_cn,mask)
    hydr_out%horiz_water_flow_st_ph_cn(1:inc) = pack(hydr_in%horiz_water_flow_st_ph_cn,mask)

    hydr_out%vert_sucrose_flow_ph_sm_rt(1:inc) = pack(hydr_in%vert_sucrose_flow_ph_sm_rt,mask)
    hydr_out%vert_sucrose_flow_ph_cn_sm(1:inc) = pack(hydr_in%vert_sucrose_flow_ph_cn_sm,mask)

    hydr_out%horiz_sucrose_flow_ph_ca_sm(1:inc) = pack(hydr_in%horiz_sucrose_flow_ph_ca_sm,mask)
    hydr_out%horiz_sucrose_flow_ph_st_sm(1:inc) = pack(hydr_in%horiz_sucrose_flow_ph_st_sm,mask)

    hydr_out%soil_water_potential(1:inc) = pack(hydr_in%soil_water_potential,mask)

    hydr_out%vert_resistance_xy(1:inc) = pack(hydr_in%vert_resistance_xy,mask)
    hydr_out%vert_resistance_ph(1:inc) = pack(hydr_in%vert_resistance_ph,mask)

    hydr_out%volume_xy_rt(1:inc) = pack(hydr_in%volume_xy_rt,mask)
    hydr_out%volume_xy_sm(1:inc) = pack(hydr_in%volume_xy_sm,mask)
    hydr_out%volume_xy_cn(1:inc) = pack(hydr_in%volume_xy_cn,mask)
    hydr_out%volume_ph_rt(1:inc) = pack(hydr_in%volume_ph_rt,mask)
    hydr_out%volume_ph_sm(1:inc) = pack(hydr_in%volume_ph_sm,mask)
    hydr_out%volume_ph_cn(1:inc) = pack(hydr_in%volume_ph_cn,mask)
    hydr_out%volume_st_rt(1:inc) = pack(hydr_in%volume_st_rt,mask)
    hydr_out%volume_st_sm(1:inc) = pack(hydr_in%volume_st_sm,mask)
    hydr_out%volume_st_cn(1:inc) = pack(hydr_in%volume_st_cn,mask)
    hydr_out%volume_ca_sm(1:inc) = pack(hydr_in%volume_ca_sm,mask)

    hydr_out%xylem_diameter(1:inc) = pack(hydr_in%xylem_diameter,mask)
    hydr_out%heartwood_diameter(1:inc) = pack(hydr_in%heartwood_diameter,mask)
    hydr_out%phloem_diameter(1:inc) = pack(hydr_in%phloem_diameter,mask)
    hydr_out%cambium_diameter(1:inc) = pack(hydr_in%cambium_diameter,mask)
    hydr_out%storage_diameter(1:inc) = pack(hydr_in%storage_diameter,mask)
    
    hydr_out%sucrose_mass_ph_rt(1:inc) = pack(hydr_in%sucrose_mass_ph_rt,mask)
    hydr_out%sucrose_mass_st_rt(1:inc) = pack(hydr_in%sucrose_mass_st_rt,mask)
    hydr_out%sucrose_mass_ca_sm(1:inc) = pack(hydr_in%sucrose_mass_ca_sm,mask)
    hydr_out%sucrose_mass_ph_sm(1:inc) = pack(hydr_in%sucrose_mass_ph_sm,mask)
    hydr_out%sucrose_mass_st_sm(1:inc) = pack(hydr_in%sucrose_mass_st_sm,mask)
    hydr_out%sucrose_mass_ph_cn(1:inc) = pack(hydr_in%sucrose_mass_ph_cn,mask)
    hydr_out%sucrose_mass_st_cn(1:inc) = pack(hydr_in%sucrose_mass_st_cn,mask)

    hydr_out%sucrose_conc_ph_rt(1:inc) = pack(hydr_in%sucrose_conc_ph_rt,mask)
    hydr_out%sucrose_conc_st_rt(1:inc) = pack(hydr_in%sucrose_conc_st_rt,mask)
    hydr_out%sucrose_conc_ca_sm(1:inc) = pack(hydr_in%sucrose_conc_ca_sm,mask)
    hydr_out%sucrose_conc_ph_sm(1:inc) = pack(hydr_in%sucrose_conc_ph_sm,mask)
    hydr_out%sucrose_conc_st_sm(1:inc) = pack(hydr_in%sucrose_conc_st_sm,mask)
    hydr_out%sucrose_conc_ph_cn(1:inc) = pack(hydr_in%sucrose_conc_ph_cn,mask)
    hydr_out%sucrose_conc_st_cn(1:inc) = pack(hydr_in%sucrose_conc_st_cn,mask)

    hydr_out%starch_mass_rt(1:inc) = pack(hydr_in%starch_mass_rt,mask)
    hydr_out%starch_mass_sm(1:inc) = pack(hydr_in%starch_mass_sm,mask)
    hydr_out%starch_mass_cn(1:inc) = pack(hydr_in%starch_mass_cn,mask)

    hydr_out%osmotic_pot_ph_rt(1:inc) = pack(hydr_in%osmotic_pot_ph_rt,mask)
    hydr_out%osmotic_pot_st_rt(1:inc) = pack(hydr_in%osmotic_pot_st_rt,mask)
    hydr_out%osmotic_pot_ca_sm(1:inc) = pack(hydr_in%osmotic_pot_ca_sm,mask)
    hydr_out%osmotic_pot_ph_sm(1:inc) = pack(hydr_in%osmotic_pot_ph_sm,mask)
    hydr_out%osmotic_pot_st_sm(1:inc) = pack(hydr_in%osmotic_pot_st_sm,mask)
    hydr_out%osmotic_pot_ph_cn(1:inc) = pack(hydr_in%osmotic_pot_ph_cn,mask)
    hydr_out%osmotic_pot_st_cn(1:inc) = pack(hydr_in%osmotic_pot_st_cn,mask)

    hydr_out%horiz_resistance_xy_ca_sm(1:inc) = pack(hydr_in%horiz_resistance_xy_ca_sm,mask)
    hydr_out%horiz_resistance_ca_ph_sm(1:inc) = pack(hydr_in%horiz_resistance_ca_ph_sm,mask)
    hydr_out%horiz_resistance_ph_st_sm(1:inc) = pack(hydr_in%horiz_resistance_ph_st_sm,mask)

    hydr_out%target_sucrose_conc(1:inc) = pack(hydr_in%target_sucrose_conc,mask)
    
    hydr_out%loading_rate(1:inc) = pack(hydr_in%loading_rate,mask)
    hydr_out%unloading_rate(1:inc) = pack(hydr_in%unloading_rate,mask)

    hydr_out%starch_conversion_rate_rt(1:inc) = pack(hydr_in%starch_conversion_rate_rt,mask)
    hydr_out%starch_conversion_rate_sm(1:inc) = pack(hydr_in%starch_conversion_rate_sm,mask)
    hydr_out%starch_conversion_rate_cn(1:inc) = pack(hydr_in%starch_conversion_rate_cn,mask)

    hydr_out%respiration_maint_st_rt(1:inc) = pack(hydr_in%respiration_maint_st_rt,mask)
    hydr_out%respiration_maint_st_sm(1:inc) = pack(hydr_in%respiration_maint_st_sm,mask)
    hydr_out%respiration_maint_ca_sm(1:inc) = pack(hydr_in%respiration_maint_ca_sm,mask)

    hydr_out%respiration_growth_ca_sm(1:inc) = pack(hydr_in%respiration_growth_ca_sm,mask)

    hydr_out%stress_relaxation(1:inc) = pack(hydr_in%stress_relaxation,mask)

    hydr_out%water_root_uptake(1:inc) = pack(hydr_in%water_root_uptake,mask)

    return
  end subroutine copy_hydrtype_mask


  subroutine copy_hydrtype(hydr_in, hydr_out, iin, iout)
    ! All variables should be included here.
    implicit none
    type(hydr_vars) :: hydr_in, hydr_out
    integer, intent(in) :: iin, iout

    hydr_out%water_mass_xy_rt(iout) = hydr_in%water_mass_xy_rt(iin)
    hydr_out%water_mass_xy_sm(iout) = hydr_in%water_mass_xy_sm(iin)
    hydr_out%water_mass_xy_cn(iout) = hydr_in%water_mass_xy_cn(iin)
    hydr_out%water_mass_ph_rt(iout) = hydr_in%water_mass_ph_rt(iin)
    hydr_out%water_mass_ph_sm(iout) = hydr_in%water_mass_ph_sm(iin)
    hydr_out%water_mass_ph_cn(iout) = hydr_in%water_mass_ph_cn(iin)
    hydr_out%water_mass_ca_sm(iout) = hydr_in%water_mass_ca_sm(iin)
    hydr_out%water_mass_st_cn(iout) = hydr_in%water_mass_st_cn(iin)
    hydr_out%water_mass_st_sm(iout) = hydr_in%water_mass_st_sm(iin)

    hydr_out%turgor_pressure_xy_rt(iout) = hydr_in%turgor_pressure_xy_rt(iin)
    hydr_out%turgor_pressure_ph_rt(iout) = hydr_in%turgor_pressure_ph_rt(iin)
    hydr_out%turgor_pressure_xy_sm(iout) = hydr_in%turgor_pressure_xy_sm(iin)
    hydr_out%turgor_pressure_ca_sm(iout) = hydr_in%turgor_pressure_ca_sm(iin)
    hydr_out%turgor_pressure_ph_sm(iout) = hydr_in%turgor_pressure_ph_sm(iin)
    hydr_out%turgor_pressure_st_sm(iout) = hydr_in%turgor_pressure_st_sm(iin)
    hydr_out%turgor_pressure_xy_cn(iout) = hydr_in%turgor_pressure_xy_cn(iin)
    hydr_out%turgor_pressure_ph_cn(iout) = hydr_in%turgor_pressure_ph_cn(iin)
    hydr_out%turgor_pressure_st_cn(iout) = hydr_in%turgor_pressure_st_cn(iin)

    hydr_out%turgor_pressure_xy_sm_base(iout) = hydr_in%turgor_pressure_xy_sm_base(iin)
    hydr_out%turgor_pressure_xy_cn_base(iout) = hydr_in%turgor_pressure_xy_cn_base(iin)

    hydr_out%root_midpoint(iout) = hydr_in%root_midpoint(iin)
    hydr_out%stem_midpoint(iout) = hydr_in%stem_midpoint(iin)
    hydr_out%crown_midpoint(iout) = hydr_in%crown_midpoint(iin)
    
    hydr_out%surface_area_xy(iout) = hydr_in%surface_area_xy(iin)
    hydr_out%surface_area_ca(iout) = hydr_in%surface_area_ca(iin)
    hydr_out%surface_area_ph(iout) = hydr_in%surface_area_ph(iin)

    hydr_out%bulk_elast_mod_xy_sm(iout) = hydr_in%bulk_elast_mod_xy_sm(iin)
    hydr_out%bulk_elast_mod_xy_cn(iout) = hydr_in%bulk_elast_mod_xy_cn(iin)
    hydr_out%bulk_elast_mod_ph_rt(iout) = hydr_in%bulk_elast_mod_ph_rt(iin)
    hydr_out%bulk_elast_mod_ph_sm(iout) = hydr_in%bulk_elast_mod_ph_sm(iin)
    hydr_out%bulk_elast_mod_ph_cn(iout) = hydr_in%bulk_elast_mod_ph_cn(iin)
    hydr_out%bulk_elast_mod_st_sm(iout) = hydr_in%bulk_elast_mod_st_sm(iin)
    hydr_out%bulk_elast_mod_st_cn(iout) = hydr_in%bulk_elast_mod_st_cn(iin)
    hydr_out%bulk_elast_mod_ca_sm(iout) = hydr_in%bulk_elast_mod_ca_sm(iin)

    hydr_out%vert_water_flow_xy_rt_sm(iout) = hydr_in%vert_water_flow_xy_rt_sm(iin)
    hydr_out%vert_water_flow_xy_sm_cn(iout) = hydr_in%vert_water_flow_xy_sm_cn(iin)
    hydr_out%vert_water_flow_ph_cn_sm(iout) = hydr_in%vert_water_flow_ph_cn_sm(iin)
    hydr_out%vert_water_flow_ph_sm_rt(iout) = hydr_in%vert_water_flow_ph_sm_rt(iin)

    hydr_out%horiz_water_flow_xy_ph_rt(iout) = hydr_in%horiz_water_flow_xy_ph_rt(iin)
    hydr_out%horiz_water_flow_xy_ca_sm(iout) = hydr_in%horiz_water_flow_xy_ca_sm(iin)
    hydr_out%horiz_water_flow_ca_ph_sm(iout) = hydr_in%horiz_water_flow_ca_ph_sm(iin)
    hydr_out%horiz_water_flow_ph_st_sm(iout) = hydr_in%horiz_water_flow_ph_st_sm(iin)
    hydr_out%horiz_water_flow_xy_ph_cn(iout) = hydr_in%horiz_water_flow_xy_ph_cn(iin)
    hydr_out%horiz_water_flow_xy_st_cn(iout) = hydr_in%horiz_water_flow_xy_st_cn(iin)
    hydr_out%horiz_water_flow_st_ph_cn(iout) = hydr_in%horiz_water_flow_st_ph_cn(iin)

    hydr_out%vert_sucrose_flow_ph_sm_rt(iout) = hydr_in%vert_sucrose_flow_ph_sm_rt(iin)
    hydr_out%vert_sucrose_flow_ph_cn_sm(iout) = hydr_in%vert_sucrose_flow_ph_cn_sm(iin)

    hydr_out%horiz_sucrose_flow_ph_ca_sm(iout) = hydr_in%horiz_sucrose_flow_ph_ca_sm(iin)
    hydr_out%horiz_sucrose_flow_ph_st_sm(iout) = hydr_in%horiz_sucrose_flow_ph_st_sm(iin)

    hydr_out%soil_water_potential(iout) = hydr_in%soil_water_potential(iin)

    hydr_out%vert_resistance_xy(iout) = hydr_in%vert_resistance_xy(iin)
    hydr_out%vert_resistance_ph(iout) = hydr_in%vert_resistance_ph(iin)

    hydr_out%volume_xy_rt(iout) = hydr_in%volume_xy_rt(iin)
    hydr_out%volume_xy_sm(iout) = hydr_in%volume_xy_sm(iin)
    hydr_out%volume_xy_cn(iout) = hydr_in%volume_xy_cn(iin)
    hydr_out%volume_ph_rt(iout) = hydr_in%volume_ph_rt(iin)
    hydr_out%volume_ph_sm(iout) = hydr_in%volume_ph_sm(iin)
    hydr_out%volume_ph_cn(iout) = hydr_in%volume_ph_cn(iin)
    hydr_out%volume_st_rt(iout) = hydr_in%volume_st_rt(iin)
    hydr_out%volume_st_sm(iout) = hydr_in%volume_st_sm(iin)
    hydr_out%volume_st_cn(iout) = hydr_in%volume_st_cn(iin)
    hydr_out%volume_ca_sm(iout) = hydr_in%volume_ca_sm(iin)

    hydr_out%xylem_diameter(iout) = hydr_in%xylem_diameter(iin)
    hydr_out%heartwood_diameter(iout) = hydr_in%heartwood_diameter(iin)
    hydr_out%phloem_diameter(iout) = hydr_in%phloem_diameter(iin)
    hydr_out%cambium_diameter(iout) = hydr_in%cambium_diameter(iin)
    hydr_out%storage_diameter(iout) = hydr_in%storage_diameter(iin)
    
    hydr_out%sucrose_mass_ph_rt(iout) = hydr_in%sucrose_mass_ph_rt(iin)
    hydr_out%sucrose_mass_st_rt(iout) = hydr_in%sucrose_mass_st_rt(iin)
    hydr_out%sucrose_mass_ca_sm(iout) = hydr_in%sucrose_mass_ca_sm(iin)
    hydr_out%sucrose_mass_ph_sm(iout) = hydr_in%sucrose_mass_ph_sm(iin)
    hydr_out%sucrose_mass_st_sm(iout) = hydr_in%sucrose_mass_st_sm(iin)
    hydr_out%sucrose_mass_ph_cn(iout) = hydr_in%sucrose_mass_ph_cn(iin)
    hydr_out%sucrose_mass_st_cn(iout) = hydr_in%sucrose_mass_st_cn(iin)

    hydr_out%sucrose_conc_ph_rt(iout) = hydr_in%sucrose_conc_ph_rt(iin)
    hydr_out%sucrose_conc_st_rt(iout) = hydr_in%sucrose_conc_st_rt(iin)
    hydr_out%sucrose_conc_ca_sm(iout) = hydr_in%sucrose_conc_ca_sm(iin)
    hydr_out%sucrose_conc_ph_sm(iout) = hydr_in%sucrose_conc_ph_sm(iin)
    hydr_out%sucrose_conc_st_sm(iout) = hydr_in%sucrose_conc_st_sm(iin)
    hydr_out%sucrose_conc_ph_cn(iout) = hydr_in%sucrose_conc_ph_cn(iin)
    hydr_out%sucrose_conc_st_cn(iout) = hydr_in%sucrose_conc_st_cn(iin)

    hydr_out%starch_mass_rt(iout) = hydr_in%starch_mass_rt(iin)
    hydr_out%starch_mass_sm(iout) = hydr_in%starch_mass_sm(iin)
    hydr_out%starch_mass_cn(iout) = hydr_in%starch_mass_cn(iin)

    hydr_out%osmotic_pot_ph_rt(iout) = hydr_in%osmotic_pot_ph_rt(iin)
    hydr_out%osmotic_pot_st_rt(iout) = hydr_in%osmotic_pot_st_rt(iin)
    hydr_out%osmotic_pot_ca_sm(iout) = hydr_in%osmotic_pot_ca_sm(iin)
    hydr_out%osmotic_pot_ph_sm(iout) = hydr_in%osmotic_pot_ph_sm(iin)
    hydr_out%osmotic_pot_st_sm(iout) = hydr_in%osmotic_pot_st_sm(iin)
    hydr_out%osmotic_pot_ph_cn(iout) = hydr_in%osmotic_pot_ph_cn(iin)
    hydr_out%osmotic_pot_st_cn(iout) = hydr_in%osmotic_pot_st_cn(iin)

    hydr_out%horiz_resistance_xy_ca_sm(iout) = hydr_in%horiz_resistance_xy_ca_sm(iin)
    hydr_out%horiz_resistance_ca_ph_sm(iout) = hydr_in%horiz_resistance_ca_ph_sm(iin)
    hydr_out%horiz_resistance_ph_st_sm(iout) = hydr_in%horiz_resistance_ph_st_sm(iin)

    hydr_out%target_sucrose_conc(iout) = hydr_in%target_sucrose_conc(iin)
    
    hydr_out%loading_rate(iout) = hydr_in%loading_rate(iin)
    hydr_out%unloading_rate(iout) = hydr_in%unloading_rate(iin)

    hydr_out%starch_conversion_rate_rt(iout) = hydr_in%starch_conversion_rate_rt(iin)
    hydr_out%starch_conversion_rate_sm(iout) = hydr_in%starch_conversion_rate_sm(iin)
    hydr_out%starch_conversion_rate_cn(iout) = hydr_in%starch_conversion_rate_cn(iin)

    hydr_out%respiration_maint_st_rt(iout) = hydr_in%respiration_maint_st_rt(iin)
    hydr_out%respiration_maint_st_sm(iout) = hydr_in%respiration_maint_st_sm(iin)
    hydr_out%respiration_maint_ca_sm(iout) = hydr_in%respiration_maint_ca_sm(iin)

    hydr_out%respiration_growth_ca_sm(iout) = hydr_in%respiration_growth_ca_sm(iin)

    hydr_out%stress_relaxation(iout) = hydr_in%stress_relaxation(iin)

    hydr_out%water_root_uptake(iout) = hydr_in%water_root_uptake(iin)

    return
  end subroutine copy_hydrtype

  subroutine filltab_hydrtype(nvar, npts, hydr, igr, init,   &
       coglob_id, var_len, var_len_global, max_ptrs)
    use ed_var_tables, only: vtable_edio_r, metadata_edio
    implicit none
    integer, intent(inout) :: nvar
    integer, intent(in) :: npts, igr, init, coglob_id
    integer, intent(in) :: var_len, var_len_global, max_ptrs
    type(hydr_vars) :: hydr
    integer :: ipt

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%water_mass_xy_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_XY_RT :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%water_mass_xy_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_XY_SM :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%water_mass_xy_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_XY_CN :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1 
    call vtable_edio_r(npts, hydr%water_mass_ph_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_PH_RT :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%water_mass_ph_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_PH_SM :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%water_mass_ph_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_PH_CN :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%water_mass_ca_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'WATER_MASS_CA_SM :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%turgor_pressure_xy_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'TURGOR_PRESSURE_XY_RT :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%turgor_pressure_xy_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'TURGOR_PRESSURE_XY_SM :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%turgor_pressure_xy_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'TURGOR_PRESSURE_XY_CN :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%turgor_pressure_ph_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'TURGOR_PRESSURE_PH_RT :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%turgor_pressure_ph_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'TURGOR_PRESSURE_PH_SM :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%turgor_pressure_ph_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'TURGOR_PRESSURE_PH_CN :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%osmotic_pot_ph_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'OSMOTIC_POT_PH_RT :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%osmotic_pot_ph_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'OSMOTIC_POT_PH_SM :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%osmotic_pot_ph_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'OSMOTIC_POT_PH_CN :41:hist:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_water_flow_xy_rt_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_WATER_FLOW_XY_RT_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')
    
    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_water_flow_xy_sm_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_WATER_FLOW_XY_SM_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_water_flow_ph_cn_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_WATER_FLOW_PH_CN_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')
    
    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_water_flow_ph_sm_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_WATER_FLOW_PH_SM_RT :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_sucrose_flow_ph_cn_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_SUCROSE_FLOW_PH_CN_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')
    
    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_sucrose_flow_ph_sm_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_SUCROSE_FLOW_PH_SM_RT :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_xy_ph_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_XY_PH_RT :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_xy_ca_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_XY_CA_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_ca_ph_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_CA_PH_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_ph_st_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_PH_ST_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_xy_ph_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_XY_PH_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_xy_st_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_XY_ST_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%horiz_water_flow_st_ph_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HORIZ_WATER_FLOW_ST_PH_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%soil_water_potential, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'SOIL_WATER_POTENTIAL :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_resistance_xy, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_RESISTANCE_XY :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%vert_resistance_ph, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VERT_RESISTANCE_PH :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_xy_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_XY_RT :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_xy_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_XY_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_xy_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_XY_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_ph_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_PH_RT :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_ph_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_PH_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_ph_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_PH_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%volume_ca_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'VOLUME_CA_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%xylem_diameter, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'XYLEM_DIAMETER :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%heartwood_diameter, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'HEARTWOOD_DIAMETER :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%phloem_diameter, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'PHLOEM_DIAMETER :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%cambium_diameter, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'CAMBIUM_DIAMETER :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%storage_diameter, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'STORAGE_DIAMETER :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%loading_rate, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'LOADING_RATE :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%unloading_rate, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'UNLOADING_RATE :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%starch_mass_rt, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'STARCH_MASS_RT :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%starch_mass_sm, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'STARCH_MASS_SM :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    nvar = nvar + 1
    call vtable_edio_r(npts, hydr%starch_mass_cn, nvar, igr, &
         init, coglob_id, var_len, var_len_global, max_ptrs, &
         'STARCH_MASS_CN :41:year:dail:anal:mont:dcyc')
    call metadata_edio(nvar, igr, 'No metadata available','[NA]','NA')

    return
  end subroutine filltab_hydrtype

  subroutine hydr_zero_vars(hydr)
    implicit none
    type(hydr_vars) :: hydr
    
    hydr%water_mass_xy_rt(:) = 0.
    hydr%water_mass_xy_sm(:) = 0.
    hydr%water_mass_xy_cn(:) = 0.
    hydr%water_mass_ph_rt(:) = 0.
    hydr%water_mass_ph_sm(:) = 0.
    hydr%water_mass_ph_cn(:) = 0.
    hydr%water_mass_ca_sm(:) = 0.
    hydr%water_mass_st_cn(:) = 0.
    hydr%water_mass_st_sm(:) = 0.

    hydr%turgor_pressure_xy_rt(:) = 0.
    hydr%turgor_pressure_ph_rt(:) = 0.
    hydr%turgor_pressure_xy_sm(:) = 0.
    hydr%turgor_pressure_ca_sm(:) = 0.
    hydr%turgor_pressure_ph_sm(:) = 0.
    hydr%turgor_pressure_st_sm(:) = 0.
    hydr%turgor_pressure_xy_cn(:) = 0.
    hydr%turgor_pressure_ph_cn(:) = 0.
    hydr%turgor_pressure_st_cn(:) = 0.

    hydr%turgor_pressure_xy_sm_base(:) = 0.
    hydr%turgor_pressure_xy_cn_base(:) = 0.

    hydr%root_midpoint(:) = 0.
    hydr%stem_midpoint(:) = 0.
    hydr%crown_midpoint(:) = 0.

    hydr%surface_area_xy(:) = 0.
    hydr%surface_area_ca(:) = 0.
    hydr%surface_area_ph(:) = 0.

    hydr%bulk_elast_mod_xy_sm(:) = 0.
    hydr%bulk_elast_mod_xy_cn(:) = 0.
    hydr%bulk_elast_mod_ph_rt(:) = 0.
    hydr%bulk_elast_mod_ph_sm(:) = 0.
    hydr%bulk_elast_mod_ph_cn(:) = 0.
    hydr%bulk_elast_mod_st_sm(:) = 0.
    hydr%bulk_elast_mod_st_cn(:) = 0.
    hydr%bulk_elast_mod_ca_sm(:) = 0.

    hydr%vert_water_flow_xy_rt_sm(:) = 0.
    hydr%vert_water_flow_xy_sm_cn(:) = 0.
    hydr%vert_water_flow_ph_cn_sm(:) = 0.
    hydr%vert_water_flow_ph_sm_rt(:) = 0.

    hydr%horiz_water_flow_xy_ph_rt(:) = 0.
    hydr%horiz_water_flow_xy_ca_sm(:) = 0.
    hydr%horiz_water_flow_ca_ph_sm(:) = 0.
    hydr%horiz_water_flow_ph_st_sm(:) = 0.
    hydr%horiz_water_flow_xy_ph_cn(:) = 0.
    hydr%horiz_water_flow_xy_st_cn(:) = 0.
    hydr%horiz_water_flow_st_ph_cn(:) = 0.

    hydr%vert_sucrose_flow_ph_sm_rt(:) = 0.
    hydr%vert_sucrose_flow_ph_cn_sm(:) = 0.

    hydr%horiz_sucrose_flow_ph_ca_sm(:) = 0.
    hydr%horiz_sucrose_flow_ph_st_sm(:) = 0.

    hydr%soil_water_potential(:) = 0.

    hydr%vert_resistance_xy(:) = 0.
    hydr%vert_resistance_ph(:) = 0.

    hydr%volume_xy_rt(:) = 0.
    hydr%volume_xy_sm(:) = 0.
    hydr%volume_xy_cn(:) = 0.
    hydr%volume_ph_rt(:) = 0.
    hydr%volume_ph_sm(:) = 0.
    hydr%volume_ph_cn(:) = 0.
    hydr%volume_st_rt(:) = 0.
    hydr%volume_st_sm(:) = 0.
    hydr%volume_st_cn(:) = 0.
    hydr%volume_ca_sm(:) = 0.

    hydr%xylem_diameter(:) = 0.
    hydr%heartwood_diameter(:) = 0.
    hydr%phloem_diameter(:) = 0.
    hydr%cambium_diameter(:) = 0.
    hydr%storage_diameter(:) = 0.

    hydr%sucrose_mass_ph_rt(:) = 0.
    hydr%sucrose_mass_st_rt(:) = 0.
    hydr%sucrose_mass_ca_sm(:) = 0.
    hydr%sucrose_mass_ph_sm(:) = 0.
    hydr%sucrose_mass_st_sm(:) = 0.
    hydr%sucrose_mass_ph_cn(:) = 0.
    hydr%sucrose_mass_st_cn(:) = 0.

    hydr%sucrose_conc_ph_rt(:) = 0.
    hydr%sucrose_conc_st_rt(:) = 0.
    hydr%sucrose_conc_ca_sm(:) = 0.
    hydr%sucrose_conc_ph_sm(:) = 0.
    hydr%sucrose_conc_st_sm(:) = 0.
    hydr%sucrose_conc_ph_cn(:) = 0.
    hydr%sucrose_conc_st_cn(:) = 0.

    hydr%starch_mass_rt(:) = 0.
    hydr%starch_mass_sm(:) = 0.
    hydr%starch_mass_cn(:) = 0.

    hydr%osmotic_pot_ph_rt(:) = 0.
    hydr%osmotic_pot_st_rt(:) = 0.
    hydr%osmotic_pot_ca_sm(:) = 0.
    hydr%osmotic_pot_ph_sm(:) = 0.
    hydr%osmotic_pot_st_sm(:) = 0.
    hydr%osmotic_pot_ph_cn(:) = 0.
    hydr%osmotic_pot_st_cn(:) = 0.

    hydr%horiz_resistance_xy_ca_sm(:) = 0.
    hydr%horiz_resistance_ca_ph_sm(:) = 0.
    hydr%horiz_resistance_ph_st_sm(:) = 0.

    hydr%target_sucrose_conc(:) = 0.

    hydr%loading_rate(:) = 0.
    hydr%unloading_rate(:) = 0.

    hydr%starch_conversion_rate_rt(:) = 0.
    hydr%starch_conversion_rate_sm(:) = 0.
    hydr%starch_conversion_rate_cn(:) = 0.

    hydr%respiration_maint_st_rt(:) = 0.
    hydr%respiration_maint_st_sm(:) = 0.
    hydr%respiration_maint_ca_sm(:) = 0.

    hydr%respiration_growth_ca_sm(:) = 0.

    hydr%stress_relaxation(:) = 0.

    hydr%water_root_uptake(:) = 0.

    return
  end subroutine hydr_zero_vars

subroutine sap_viscosity(sucrose_mass, water_mass, viscosity_interp)
    ! DOES NOT ACCOUNT FOR TEMPERATURE SENSITIVITY.
    implicit none

    real, intent(in) :: sucrose_mass, water_mass
    real, intent(out) :: viscosity_interp
    ! mPa s; 0.5%, then increments of 1% by mass.
    real, dimension(39), parameter :: masspct= (/   &
         0.0, 0.5, 1., 2., 3., 4., &
         5., 6., 7., 8., 9., 10., &
         11., 12., 13., 14., 15., 16., &
         17., 18., 19., 20., 22., 24., &
         26., 28., 30., 32., 34., 36., &
         38., 40., 42., 44., 46., 48., &
         50., 60., 70. /)

    real, dimension(39), parameter :: visc = (/   &
         1.002, 1.015, 1.028, 1.055, 1.084, 1.114, &
         1.146, 1.179, 1.215, 1.254, 1.294, 1.336, &
         1.381, 1.429, 1.480, 1.534, 1.592, 1.653, &
         1.719, 1.790, 1.865, 1.945, 2.124, 2.331, &
         2.573, 2.855, 3.187, 3.762, 4.052, 4.621, &
         5.315, 6.162, 7.234, 8.596, 10.301, 12.515, &
         15.431, 58.487, 481.561 /)
    
    real :: sucrose_mass_pct, slope, intercept
    integer :: i

    sucrose_mass_pct = sucrose_mass / (sucrose_mass + water_mass) * 100.

    slope = 0.
    intercept=0.
    do i = 2,39
       if(sucrose_mass_pct < masspct(i))then
          slope = (visc(i) - visc(i-1)) / (masspct(i)-masspct(i-1))
          intercept = visc(i-1) - slope * masspct(i-1)
          exit
       endif
    enddo
    if(slope > 0.)then
       viscosity_interp = slope * sucrose_mass_pct + intercept
       ! Change units from (mPa s) to (Pa s)
       viscosity_interp = 0.001 * viscosity_interp
    else
       print*,'unable to calculate viscosity'
       print*,sucrose_mass,water_mass
       stop
    endif

    return
  end subroutine sap_viscosity


end Module hydr_state_vars
