# fortax-library

FORTAX library is a Fortran library for calculating accurate budget constraints based upon the rules of the actual UK tax and benefit system. It has been used in a number of research papers (see `FortaxPapers.md`). It uses [Fypp](https://github.com/aradi/fypp), a Python powered preprocessor, and the [JSON-Fortran](https://github.com/jacobwilliams/json-fortran) JSON Fortran API.

The library should be compiled and linked to your application. It has been successfully compiled with recent versions of the Intel Fortran compiler and the GNU gfortran compiler, although other compilers that support Fortran 2008 features should work. A `makefile` is provided. User routines are available through the `fortax_library` module. A basic example showing how to use the FORTAX library is provided below.

```
program FORTAX_example1

    use fortax_library, dp => FORTAX_dp

    ! To use FORTAX, we will always need three derived types. sys_t
    ! describes the tax system, fam_t describes the family (tax unit),
    ! and net_t contains all the calculated incomes for this family
    ! under sys

    type(sys_t) :: sys
    type(fam_t) :: fam
    type(net_t) :: net

    ! Load the tax system using "FORTAX_readFortaxParams". These are saved 
    ! as JSON files. The systems can be manipulated, and new systems can
    ! be saved with "FORTAX_writeFortaxParams"

    call FORTAX_readFortaxParams(sys, "systems/fortax/April06.json")

    ! Specify a family using "FORTAX_fam_gen" here we consider a single 
    ! parent with two children (aged 0 and 4) who is working 20 hours
    ! a week, earning 300 pounds and with child care expenditure of 100 
    ! pounds. Note that FORTAX works with weekly values. Anything that is
    ! not set will take a default value. Components of fam_t can be set
    ! using variable names as keywords. Adult components have a 1 and 2
    ! suffix attached. If no optional arguments are specified, this is
    ! the same as calling "FORTAX_fam_init".

    call FORTAX_fam_gen(fam, earn1 = 300.0_dp, hrs1 = 20.0_dp, ccexp = 100.0_dp, kidage = [0, 4])

    ! If we wish to look at all the components of fam we can call the
    ! "FORTAX_fam_desc" subroutine.

    call FORTAX_fam_desc(fam)

    ! To calculate tax and benefit amounts we use "FORTAX_calcNetInc".
    ! This takes the system sys (type sys_t), the family fam (type fam_t),
    ! and returns net incomes and income components in net (type net_t).

    call FORTAX_calcNetInc(sys, fam, net)

    ! We can summarise all the components in net using "FORTAX_net_desc"

    call FORTAX_net_desc(net)

    ! All amounts in FORTAX are at the weekly level. If we wish to get
    ! the annual equivalent (for example) we can just multiply by 52.

    net = net * 52

    call FORTAX_net_desc(net)

end program FORTAX_example1
```

Other examples that demonstrate other features of FORTAX (including price uprating, and piece-wise linear representations of the entire budget constraint are also provided).

