
! This file is part of the FORTAX library;

! FORTAX is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! FORTAX is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with FORTAX.  If not, see <http://www.gnu.org/licenses/>.




! fortax_library
! -----------------------------------------------------------------------
! module provides easy access to all the other modules that are part of
! the fortax_library

module fortax_library

    use fortax_calc,        only :  FORTAX_calcNetInc => calcNetInc

    use fortax_extra,       only :  FORTAX_setMinAmount => setMinAmount,                        &
                                    FORTAX_abolishNIFee => abolishNIFee,                        &
                                    FORTAX_disableTaperRounding => disableTaperRounding,        &
                                    FORTAX_fsMinAppAmt => fsMinAppAmt,                          &
                                    FORTAX_taperMatGrant => taperMatGrant,                      &
                                    FORTAX_imposeUC => imposeUC

    use fortax_kinks,       only :  FORTAX_evalKinksHours => evalKinksHours,                    &
                                    FORTAX_evalKinksEarn => evalKinksEarn,                      &
                                    FORTAX_kinkshours => kinkshours,                            &
                                    FORTAX_kinksearn => kinksearn,                              &
                                    FORTAX_kinksccexp => kinksccexp,                            &
                                    FORTAX_kinks_desc => kinks_desc

    use fortax_prices,      only :  FORTAX_loadIndex => loadIndex,                              &
                                    FORTAX_uprateSys => uprateSys,                              &
                                    FORTAX_uprateFactor => uprateFactor,                        &
                                    FORTAX_loadSysIndex => loadSysIndex,                        &
                                    FORTAX_getSysIndex => getSysIndex,                          &
                                    operator(*)

    use fortax_read,        only :  FORTAX_readFortaxParams => readFortaxParams

    use fortax_type,        only :  fam_t, net_t, sys_t, rpi_t, sysIndex_t, bcout_t,            &
                                    FORTAX_fam_init => fam_init,                                &
                                    FORTAX_net_init => net_init,                                &
                                    FORTAX_sys_init => sys_init,                                &
                                    FORTAX_fam_saveF90 => fam_saveF90,                          &
                                    FORTAX_sys_saveF90 => sys_saveF90,                          &
                                    FORTAX_lab => lab,                                          &
                                    FORTAX_maxkids => maxkids,                                  &
                                    FORTAX_maxkinks => maxkinks,                                &
                                    FORTAX_len_sysindex => len_sysindex,                        &
                                    FORTAX_fam_gen => fam_gen,                                  &
                                    FORTAX_fam_desc => fam_desc,                                &
                                    FORTAX_net_desc => net_desc,                                &
                                    operator(+), operator(*), operator(/)

    use fortax_write,       only :  FORTAX_fortaxPrint => fortaxPrint,                          &
                                    FORTAX_fortaxWrite => fortaxWrite

    use fortax_realtype,    only :  FORTAX_dp => dp,                                            &
                                    FORTAX_sp => sp,                                            &
                                    FORTAX_qp => qp

end module fortax_library
