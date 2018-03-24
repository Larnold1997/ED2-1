Module hydr_consts_coms
  implicit none


  ! all the constants
  real :: vert_permeability_xy = 1.5e-12  ! m2
  real :: vert_permeability_ph = 6.0e-12  ! m2

  real :: max_bulk_elast_mod_xy = 1021.   ! MPa
  real :: max_bulk_elast_mod_st = 8.5     ! MPa
  real :: max_bulk_elast_mod_ph = 10.     ! MPa
  real :: max_bulk_elast_mod_ca = 10.     ! MPa

  real, parameter :: crown_pct_xy = 0.0016
  real, parameter :: crown_pct_ph = 0.0004
  real, parameter :: crown_pct_st = 1. - crown_pct_xy - crown_pct_ph
  real :: water_fraction_root = 0.5
  real :: water_fraction_stem = 0.5
  real :: water_fraction_crown = 0.85
  real :: plc_steepness = 6.7 !MPa
  real :: plc50 = -2.0 !MPa
  real, parameter :: rad_hydraul_conductivity = 1.0e-6  ! m MPa-1 s-1
  real, parameter :: MM_sucrose = 342.30e3 ! mg mol-1

  real, parameter :: leaf_thickness = 0.001 ! m
  real, parameter :: target_osm_pot = 1. ! MPa
  real, parameter :: vmax_loading = 0.12  ! mg sucrose/s
  real, parameter :: michmen_loading = 1.61e8 ! mg/m3
  real, parameter :: ref_unload_rate = 0.048 ! mg/s
  real, parameter :: reflection_coefficient = 1.

  real, parameter :: horiz_resistance_xy_ph_rt = 0.07 ! MPa s/mg
  real, parameter :: horiz_resistance_xy_ph_cn = 1.01 ! MPa s/mg
  real, parameter :: horiz_resistance_xy_st_cn = 1.01 ! MPa s/mg
  real, parameter :: horiz_resistance_ph_st_cn = 1.014 ! MPa s/mg

  real, parameter :: cell_wall_extensibility = 8.6e-5 ! 1/MPa/s
  real, parameter :: threshold_turgor = 0.895 ! MPa
  real, parameter :: phloem_prod_frac = 0.2  ! unitless
  real, parameter :: xylem_prod_frac = 1.0 - phloem_prod_frac ! unitless

  real, parameter :: diff_const_sucrose_ph_st = 3.9e-10 ! m3/s
  real, parameter :: diff_const_sucrose_ph_ca = 1.3e-9 ! m3/s

  real, parameter :: resp_const_tissue = 533.0 ! mg sucrose/m3/s
  real, parameter :: growth_efficiency = 0.7 ! dimensionless
  real, parameter :: resp_const_sucrose = 2.4e-7 ! /s
  real, parameter :: wood_sucrose_equiv = 1.13 ! mg sucrose / mg wood

  real, parameter :: starch_conversion_const = 4.6e-4 ! m3/s

end Module hydr_consts_coms
