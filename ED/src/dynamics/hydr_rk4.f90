Module hydr_rk4
  implicit none

  real, parameter :: tiny = 1.0e-20
  real, parameter :: min_pool_size = 1.0e-30

Contains

  subroutine hydr_rk4_sanity(hydr, ncohorts, reject_step)
    use hydr_state_vars, only: hydr_vars
    implicit none
    type(hydr_vars) :: hydr
    integer, intent(in) :: ncohorts
    logical, intent(inout) :: reject_step
    integer :: ico

    do ico = 1, ncohorts

       if(hydr%volume_st_cn(ico) < 0.)reject_step = .true.
       if(hydr%volume_xy_cn(ico) < 0.)reject_step = .true.
       if(hydr%volume_ph_cn(ico) < 0.)reject_step = .true.
       if(hydr%volume_xy_rt(ico) < 0.)reject_step = .true.
       if(hydr%volume_ph_rt(ico) < 0.)reject_step = .true.
       if(hydr%volume_st_rt(ico) < 0.)reject_step = .true.
       if(hydr%volume_xy_sm(ico) < 0.)reject_step = .true.
       if(hydr%volume_ca_sm(ico) < 0.)reject_step = .true.
       if(hydr%volume_ph_sm(ico) < 0.)reject_step = .true.
       if(hydr%volume_st_sm(ico) < 0.)reject_step = .true.

       if(hydr%water_mass_xy_rt(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_ph_rt(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_ph_sm(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_ca_sm(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_xy_sm(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_st_sm(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_ph_cn(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_xy_cn(ico) < 0.)reject_step = .true.
       if(hydr%water_mass_st_cn(ico) < 0.)reject_step = .true.
       
       if(hydr%sucrose_mass_ph_rt(ico) < 0.)reject_step = .true.
       if(hydr%sucrose_mass_st_rt(ico) < 0.)reject_step = .true.
       if(hydr%sucrose_mass_ca_sm(ico) < 0.)reject_step = .true.
       if(hydr%sucrose_mass_ph_sm(ico) < 0.)reject_step = .true.
       if(hydr%sucrose_mass_st_sm(ico) < 0.)reject_step = .true.
       if(hydr%sucrose_mass_ph_cn(ico) < 0.)reject_step = .true.
       if(hydr%sucrose_mass_st_cn(ico) < 0.)reject_step = .true.
       
       if(hydr%starch_mass_rt(ico) < 0.)reject_step = .true.
       if(hydr%starch_mass_sm(ico) < 0.)reject_step = .true.
       if(hydr%starch_mass_cn(ico) < 0.)reject_step = .true.

    enddo

    return
  end subroutine hydr_rk4_sanity

  subroutine hydr_rk4_errmax(scale, errv, errmax, ncohorts)
    use hydr_state_vars, only: hydr_vars
    implicit none

    type(hydr_vars) :: scale
    type(hydr_vars) :: errv
    real(kind=8), intent(inout) :: errmax
    integer, intent(in) :: ncohorts
    integer :: ico
    real :: err
!print*,1,errmax
    do ico = 1, ncohorts
       err = abs(errv%volume_st_cn(ico) / scale%volume_st_cn(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_xy_cn(ico) / scale%volume_xy_cn(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_ph_cn(ico) / scale%volume_ph_cn(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_xy_rt(ico) / scale%volume_xy_rt(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_ph_rt(ico) / scale%volume_ph_rt(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_st_rt(ico) / scale%volume_st_rt(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_xy_sm(ico) / scale%volume_xy_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_ca_sm(ico) / scale%volume_ca_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_ph_sm(ico) / scale%volume_ph_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%volume_st_sm(ico) / scale%volume_st_sm(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,11,errmax
       err = abs(errv%water_mass_xy_rt(ico) / scale%water_mass_xy_rt(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,111,errmax
       err = abs(errv%water_mass_ph_rt(ico) / scale%water_mass_ph_rt(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,112,errmax
       err = abs(errv%water_mass_ph_sm(ico) / scale%water_mass_ph_sm(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,113,errmax
       err = abs(errv%water_mass_ca_sm(ico) / scale%water_mass_ca_sm(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,114,errmax
       err = abs(errv%water_mass_xy_sm(ico) / scale%water_mass_xy_sm(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,115,errmax
       err = abs(errv%water_mass_st_sm(ico) / scale%water_mass_st_sm(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,116,errmax
       err = abs(errv%water_mass_ph_cn(ico) / scale%water_mass_ph_cn(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,117,errmax
       err = abs(errv%water_mass_xy_cn(ico) / scale%water_mass_xy_cn(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,118,errmax
       err = abs(errv%water_mass_st_cn(ico) / scale%water_mass_st_cn(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,12,errmax
       err = abs(errv%sucrose_mass_ph_rt(ico) / scale%sucrose_mass_ph_rt(ico))
       errmax = max(errmax, err)
       err = abs(errv%sucrose_mass_st_rt(ico) / scale%sucrose_mass_st_rt(ico))
       errmax = max(errmax, err)
       err = abs(errv%sucrose_mass_ca_sm(ico) / scale%sucrose_mass_ca_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%sucrose_mass_ph_sm(ico) / scale%sucrose_mass_ph_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%sucrose_mass_st_sm(ico) / scale%sucrose_mass_st_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%sucrose_mass_ph_cn(ico) / scale%sucrose_mass_ph_cn(ico))
       errmax = max(errmax, err)
       err = abs(errv%sucrose_mass_st_cn(ico) / scale%sucrose_mass_st_cn(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,13,errmax
       err = abs(errv%starch_mass_rt(ico) / scale%starch_mass_rt(ico))
       errmax = max(errmax, err)
       err = abs(errv%starch_mass_sm(ico) / scale%starch_mass_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%starch_mass_cn(ico) / scale%starch_mass_cn(ico))
       errmax = max(errmax, err)
!if(ico==69)print*,14,errmax
!       err = abs(errv%turgor_pressure_xy_rt(ico) /   &
!            scale%turgor_pressure_xy_rt(ico))
!       errmax = max(errmax, err)
       err = abs(errv%turgor_pressure_ph_rt(ico) /   &
            scale%turgor_pressure_ph_rt(ico))
       errmax = max(errmax, err)
!       err = abs(errv%turgor_pressure_xy_sm(ico) /   &
!            scale%turgor_pressure_xy_sm(ico))
!       errmax = max(errmax, err)
       err = abs(errv%turgor_pressure_ca_sm(ico) /   &
            scale%turgor_pressure_ca_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%turgor_pressure_ph_sm(ico) /   &
            scale%turgor_pressure_ph_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%turgor_pressure_st_sm(ico) /   &
            scale%turgor_pressure_st_sm(ico))
       errmax = max(errmax, err)
       err = abs(errv%turgor_pressure_ph_cn(ico) /   &
            scale%turgor_pressure_ph_cn(ico))
       errmax = max(errmax, err)
!       err = abs(errv%turgor_pressure_xy_cn(ico) /   &
!            scale%turgor_pressure_xy_cn(ico))
!       errmax = max(errmax, err)
       err = abs(errv%turgor_pressure_st_cn(ico) /   &
            scale%turgor_pressure_st_cn(ico))
       errmax = max(errmax, err)
!if(ico == 69)print*,15,errmax
    enddo
!print*,2,errmax
    return
  end subroutine hydr_rk4_errmax

  subroutine hydr_rk4_inc(y, dy, fac, ncohorts)
    use hydr_state_vars, only: hydr_vars
    implicit none

    type(hydr_vars) :: y
    type(hydr_vars) :: dy
    real, intent(in) :: fac
    integer, intent(in) :: ncohorts
    integer :: ico

    do ico = 1, ncohorts
       y%volume_st_cn(ico) = y%volume_st_cn(ico) +  &
            dy%volume_st_cn(ico) * fac
       y%volume_xy_cn(ico) = y%volume_xy_cn(ico) +  &
            dy%volume_xy_cn(ico) * fac
       y%volume_ph_cn(ico) = y%volume_ph_cn(ico) +  &
            dy%volume_ph_cn(ico) * fac
       y%volume_xy_rt(ico) = y%volume_xy_rt(ico) +  &
            dy%volume_xy_rt(ico) * fac
       y%volume_ph_rt(ico) = y%volume_ph_rt(ico) +  &
            dy%volume_ph_rt(ico) * fac
       y%volume_st_rt(ico) = y%volume_st_rt(ico) +  &
            dy%volume_st_rt(ico) * fac
       y%volume_xy_sm(ico) = y%volume_xy_sm(ico) +  &
            dy%volume_xy_sm(ico) * fac
       y%volume_ca_sm(ico) = y%volume_ca_sm(ico) +  &
            dy%volume_ca_sm(ico) * fac
       y%volume_ph_sm(ico) = y%volume_ph_sm(ico) +  &
            dy%volume_ph_sm(ico) * fac
       y%volume_st_sm(ico) = y%volume_st_sm(ico) +  &
            dy%volume_st_sm(ico) * fac

       y%water_mass_xy_rt(ico) = y%water_mass_xy_rt(ico) +   &
            dy%water_mass_xy_rt(ico) * fac
       y%water_mass_ph_rt(ico) = y%water_mass_ph_rt(ico) +   &
            dy%water_mass_ph_rt(ico) * fac
       y%water_mass_ph_sm(ico) = y%water_mass_ph_sm(ico) +   &
            dy%water_mass_ph_sm(ico) * fac
       y%water_mass_ca_sm(ico) = y%water_mass_ca_sm(ico) +   &
            dy%water_mass_ca_sm(ico) * fac
       y%water_mass_xy_sm(ico) = y%water_mass_xy_sm(ico) +   &
            dy%water_mass_xy_sm(ico) * fac
       y%water_mass_st_sm(ico) = y%water_mass_st_sm(ico) +   &
            dy%water_mass_st_sm(ico) * fac
       y%water_mass_ph_cn(ico) = y%water_mass_ph_cn(ico) +   &
            dy%water_mass_ph_cn(ico) * fac
       y%water_mass_xy_cn(ico) = y%water_mass_xy_cn(ico) +   &
            dy%water_mass_xy_cn(ico) * fac
       y%water_mass_st_cn(ico) = y%water_mass_st_cn(ico) +   &
            dy%water_mass_st_cn(ico) * fac

       y%sucrose_mass_ph_rt(ico) = y%sucrose_mass_ph_rt(ico) +   &
            dy%sucrose_mass_ph_rt(ico) * fac
       y%sucrose_mass_st_rt(ico) = y%sucrose_mass_st_rt(ico) +   &
            dy%sucrose_mass_st_rt(ico) * fac
       y%sucrose_mass_ca_sm(ico) = y%sucrose_mass_ca_sm(ico) +   &
            dy%sucrose_mass_ca_sm(ico) * fac
       y%sucrose_mass_ph_sm(ico) = y%sucrose_mass_ph_sm(ico) +   &
            dy%sucrose_mass_ph_sm(ico) * fac
       y%sucrose_mass_st_sm(ico) = y%sucrose_mass_st_sm(ico) +   &
            dy%sucrose_mass_st_sm(ico) * fac
       y%sucrose_mass_ph_cn(ico) = y%sucrose_mass_ph_cn(ico) +   &
            dy%sucrose_mass_ph_cn(ico) * fac
       y%sucrose_mass_st_cn(ico) = y%sucrose_mass_st_cn(ico) +   &
            dy%sucrose_mass_st_cn(ico) * fac

       y%starch_mass_rt(ico) = y%starch_mass_rt(ico) +   &
            dy%starch_mass_rt(ico) * fac
       y%starch_mass_sm(ico) = y%starch_mass_sm(ico) +   &
            dy%starch_mass_sm(ico) * fac
       y%starch_mass_cn(ico) = y%starch_mass_cn(ico) +   &
            dy%starch_mass_cn(ico) * fac

!       y%turgor_pressure_xy_rt(ico) = y%turgor_pressure_xy_rt(ico) + &
!            dy%turgor_pressure_xy_rt(ico) * fac
       y%turgor_pressure_ph_rt(ico) = y%turgor_pressure_ph_rt(ico) + &
            dy%turgor_pressure_ph_rt(ico) * fac
!       y%turgor_pressure_xy_sm(ico) = y%turgor_pressure_xy_sm(ico) + &
!            dy%turgor_pressure_xy_sm(ico) * fac
       y%turgor_pressure_ca_sm(ico) = y%turgor_pressure_ca_sm(ico) + &
            dy%turgor_pressure_ca_sm(ico) * fac
       y%turgor_pressure_ph_sm(ico) = y%turgor_pressure_ph_sm(ico) + &
            dy%turgor_pressure_ph_sm(ico) * fac
       y%turgor_pressure_st_sm(ico) = y%turgor_pressure_st_sm(ico) + &
            dy%turgor_pressure_st_sm(ico) * fac
       y%turgor_pressure_ph_cn(ico) = y%turgor_pressure_ph_cn(ico) + &
            dy%turgor_pressure_ph_cn(ico) * fac
!       y%turgor_pressure_xy_cn(ico) = y%turgor_pressure_xy_cn(ico) + &
!            dy%turgor_pressure_xy_cn(ico) * fac
       y%turgor_pressure_st_cn(ico) = y%turgor_pressure_st_cn(ico) + &
            dy%turgor_pressure_st_cn(ico) * fac

       y%water_root_uptake(ico) = y%water_root_uptake(ico) +  &
            dy%water_root_uptake(ico) * fac

    enddo

    return
  end subroutine hydr_rk4_inc

  subroutine hydr_rk4_scale(y, dy, scale, htry, ncohorts)
    use hydr_state_vars, only: hydr_vars
    implicit none

    type(hydr_vars) :: y
    type(hydr_vars) :: dy
    type(hydr_vars) :: scale
    real, intent(in) :: htry
    integer, intent(in) :: ncohorts
    integer :: ico

    do ico = 1, ncohorts
       scale%volume_st_cn(ico) = abs(y%volume_st_cn(ico)) +  &
            abs(dy%volume_st_cn(ico) * htry) + tiny
       scale%volume_xy_cn(ico) = abs(y%volume_xy_cn(ico)) +  &
            abs(dy%volume_xy_cn(ico) * htry) + tiny
       scale%volume_ph_cn(ico) = abs(y%volume_ph_cn(ico)) +  &
            abs(dy%volume_ph_cn(ico) * htry) + tiny
       scale%volume_xy_rt(ico) = abs(y%volume_xy_rt(ico)) +  &
            abs(dy%volume_xy_rt(ico) * htry) + tiny
       scale%volume_ph_rt(ico) = abs(y%volume_ph_rt(ico)) +  &
            abs(dy%volume_ph_rt(ico) * htry) + tiny
       scale%volume_st_rt(ico) = abs(y%volume_st_rt(ico)) +  &
            abs(dy%volume_st_rt(ico) * htry) + tiny
       scale%volume_xy_sm(ico) = abs(y%volume_xy_sm(ico)) +  &
            abs(dy%volume_xy_sm(ico) * htry) + tiny
       scale%volume_ca_sm(ico) = abs(y%volume_ca_sm(ico)) +  &
            abs(dy%volume_ca_sm(ico) * htry) + tiny
       scale%volume_ph_sm(ico) = abs(y%volume_ph_sm(ico)) +  &
            abs(dy%volume_ph_sm(ico) * htry) + tiny
       scale%volume_st_sm(ico) = abs(y%volume_st_sm(ico)) +  &
            abs(dy%volume_st_sm(ico) * htry) + tiny

       scale%water_mass_xy_rt(ico) = abs(y%water_mass_xy_rt(ico)) +   &
            abs(dy%water_mass_xy_rt(ico) * htry) + tiny
       scale%water_mass_ph_rt(ico) = abs(y%water_mass_ph_rt(ico)) +   &
            abs(dy%water_mass_ph_rt(ico) * htry) + tiny
       scale%water_mass_ph_sm(ico) = abs(y%water_mass_ph_sm(ico)) +   &
            abs(dy%water_mass_ph_sm(ico) * htry) + tiny
       scale%water_mass_ca_sm(ico) = abs(y%water_mass_ca_sm(ico)) +   &
            abs(dy%water_mass_ca_sm(ico) * htry) + tiny
       scale%water_mass_xy_sm(ico) = abs(y%water_mass_xy_sm(ico)) +   &
            abs(dy%water_mass_xy_sm(ico) * htry) + tiny
       scale%water_mass_st_sm(ico) = abs(y%water_mass_st_sm(ico)) +   &
            abs(dy%water_mass_st_sm(ico) * htry) + tiny
       scale%water_mass_ph_cn(ico) = abs(y%water_mass_ph_cn(ico)) +   &
            abs(dy%water_mass_ph_cn(ico) * htry) + tiny
       scale%water_mass_xy_cn(ico) = abs(y%water_mass_xy_cn(ico)) +   &
            abs(dy%water_mass_xy_cn(ico) * htry) + tiny
       scale%water_mass_st_cn(ico) = abs(y%water_mass_st_cn(ico)) +   &
            abs(dy%water_mass_st_cn(ico) * htry) + tiny

       scale%sucrose_mass_ph_rt(ico) = abs(y%sucrose_mass_ph_rt(ico)) +   &
            abs(dy%sucrose_mass_ph_rt(ico) * htry) + tiny
       scale%sucrose_mass_st_rt(ico) = abs(y%sucrose_mass_st_rt(ico)) +   &
            abs(dy%sucrose_mass_st_rt(ico) * htry) + tiny
       scale%sucrose_mass_ca_sm(ico) = abs(y%sucrose_mass_ca_sm(ico)) +   &
            abs(dy%sucrose_mass_ca_sm(ico) * htry) + tiny
       scale%sucrose_mass_ph_sm(ico) = abs(y%sucrose_mass_ph_sm(ico)) +   &
            abs(dy%sucrose_mass_ph_sm(ico) * htry) + tiny
       scale%sucrose_mass_st_sm(ico) = abs(y%sucrose_mass_st_sm(ico)) +   &
            abs(dy%sucrose_mass_st_sm(ico) * htry) + tiny
       scale%sucrose_mass_ph_cn(ico) = abs(y%sucrose_mass_ph_cn(ico)) +   &
            abs(dy%sucrose_mass_ph_cn(ico) * htry) + tiny
       scale%sucrose_mass_st_cn(ico) = abs(y%sucrose_mass_st_cn(ico)) +   &
            abs(dy%sucrose_mass_st_cn(ico) * htry) + tiny

       scale%starch_mass_rt(ico) = abs(y%starch_mass_rt(ico)) +   &
            abs(dy%starch_mass_rt(ico) * htry) + tiny
       scale%starch_mass_sm(ico) = abs(y%starch_mass_sm(ico)) +   &
            abs(dy%starch_mass_sm(ico) * htry) + tiny
       scale%starch_mass_cn(ico) = abs(y%starch_mass_cn(ico)) +   &
            abs(dy%starch_mass_cn(ico) * htry) + tiny

!       scale%turgor_pressure_xy_rt(ico) = abs(y%turgor_pressure_xy_rt(ico)) + &
!            abs(dy%turgor_pressure_xy_rt(ico) * htry) + tiny
       scale%turgor_pressure_ph_rt(ico) = abs(y%turgor_pressure_ph_rt(ico)) + &
            abs(dy%turgor_pressure_ph_rt(ico) * htry) + tiny
!       scale%turgor_pressure_xy_sm(ico) = abs(y%turgor_pressure_xy_sm(ico)) + &
!            abs(dy%turgor_pressure_xy_sm(ico) * htry) + tiny
       scale%turgor_pressure_ca_sm(ico) = abs(y%turgor_pressure_ca_sm(ico)) + &
            abs(dy%turgor_pressure_ca_sm(ico) * htry) + tiny
       scale%turgor_pressure_ph_sm(ico) = abs(y%turgor_pressure_ph_sm(ico)) + &
            abs(dy%turgor_pressure_ph_sm(ico) * htry) + tiny
       scale%turgor_pressure_st_sm(ico) = abs(y%turgor_pressure_st_sm(ico)) + &
            abs(dy%turgor_pressure_st_sm(ico) * htry) + tiny
       scale%turgor_pressure_ph_cn(ico) = abs(y%turgor_pressure_ph_cn(ico)) + &
            abs(dy%turgor_pressure_ph_cn(ico) * htry) + tiny
!       scale%turgor_pressure_xy_cn(ico) = abs(y%turgor_pressure_xy_cn(ico)) + &
!            abs(dy%turgor_pressure_xy_cn(ico) * htry) + tiny
       scale%turgor_pressure_st_cn(ico) = abs(y%turgor_pressure_st_cn(ico)) + &
            abs(dy%turgor_pressure_st_cn(ico) * htry) + tiny

    enddo

    return
  end subroutine hydr_rk4_scale

end Module hydr_rk4
