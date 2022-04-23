# FORTAX Library

FORTAX library is a Fortran library for calculating accurate budget constraints based upon the rules of the actual UK tax and benefit system. It has been used in a number of research papers (see `FortaxPapers.md`). It uses [Fypp](https://github.com/aradi/fypp), a Python powered preprocessor, and the [JSON-Fortran](https://github.com/jacobwilliams/json-fortran) JSON Fortran API.

The library should be compiled and linked to your application. It has been successfully compiled with recent versions of the Intel Fortran compiler and the GNU gfortran compiler, although other compilers that support Fortran 2008 features should work. A `makefile` is provided.

## Basic use

User routines are available through the `fortax_library` module. So a typical use may begin with
```
use fortax_library, dp => FORTAX_dp
```
To use FORTAX, we will typically need three derived types. First, the derived type `sys_t` describes the tax system, and a number of different system files are provided with FORTAX in the `systems` directory. Second, `fam_t` describes the family (tax unit). Third `net_t` contains all the calculated incomes for this family. So, `sys_t` and `fam_t` are inputs to FORTAX, while `net_t` is the output.
```
type(sys_t) :: sys
type(fam_t) :: fam
type(net_t) :: net
```
We can load a tax system using `FORTAX_readFortaxParams`. These are saved as `JSON` files. The systems can be manipulated, and new systems can be saved with `FORTAX_writeFortaxParams`. For example,
```
call FORTAX_readFortaxParams(sys, "systems/fortax/April06.json")
```
will load the system (in this example saved in `"systems/fortax/April06.json"`) and store it in `sys` (of type `sys_t`).

There are different ways to specify `fam` (type `fam_t`). This contains anything about the family (or tax unit) that is relevant for calculating net incomes. For example, demographics, labour supply, and earnings. For example, we could use `FORTAX_fam_gen`. To generate a single parent with two children (aged 0 and 4) who is working 20 hours a week, earning 300 pounds and with child care expenditure of 100 pounds (note that FORTAX works with weekly values) we would do the following.
```
call FORTAX_fam_gen(fam, earn1 = 300.0_dp, hrs1 = 20.0_dp, ccexp = 100.0_dp, kidage = [0, 4])
```
Any componet of `fam` that is not set will take a default value. Components can be set using variable names as keywords. Adult components have a 1 and 2
suffix attached. If no optional arguments are specified (i.e. we do `call FORTAX_fam_gen(fam)`, this is the same as calling `FORTAX_fam_init(fam)`.

If we wish to look at all the components of `fam` we can use the `FORTAX_fam_desc` subroutine.
```
call FORTAX_fam_desc(fam)
```
The output in this example would be
```
==============================================================
                      fam_desc (FAMILY):                      
==============================================================
             Married/cohabiting (couple)                no (0)
                       Married (married)                no (0)
           Childcare expenditure (ccexp)              100.0000
              Maintenance income (maint)                0.0000
              Number of children (nkids)                     2
             Age of children (kidage[1])                     0
             Age of children (kidage[2])                     4
        Number of other adults (nothads)                     0
                 Housing tenure (tenure)      own_outright (1)
                     Housing rent (rent)                0.0000
              Housing rent cap (rentcap)                0.0000
                         Region (region)        north_east (1)
               Council tax band (ctband)             bandd (4)
   Council tax band-D ratio (banddratio)                1.0000
                Interview date (intdate)              19900101
==============================================================
                     fam_desc (ADULT 1):                      
==============================================================
                               Age (age)                    25
                 Self-employed (selfemp)                no (0)
                     Hours-of-work (hrs)               20.0000
                         Earnings (earn)              300.0000
==============================================================
```
The main calculation routine to calculate tax and benefit amounts is `FORTAX_calcNetInc`. This takes the system `sys` (type `sys_t`), the family `fam` (type `fam_t`), and returns net incomes and income components in `net` (type `net_t`).
```
call FORTAX_calcNetInc(sys, fam, net)
```
The components of `net` would then typically be used with some user-supplied routine (for example, to calculate the utility associated with different labour supply alternatives). For example, net (disposable) income is `net%tu%dispinc` If we wish to summarise all the components in `net` (which will also list all variable names) we can use `FORTAX_net_desc`.
```
call FORTAX_net_desc(net)
```
This would return
```
==============================================================
                     net_desc (TAX UNIT):                     
==============================================================
           Pre-tax earnings (pretaxearn)              300.0000
         Post-tax earnings (posttaxearn)              237.9335
                   Child benefit (chben)               29.1500
              Maternity grant (matgrant)                9.6154
                 Family Credit/WFTC (fc)                0.0000
                Working Tax Credit (wtc)               69.7000
                  Child Tax Credit (ctc)               88.8462
           Childcare expenditure (ccexp)              100.0000
                 Income Support (incsup)                0.0000
                  Housing Benefit (hben)                0.0000
              Community Charge (polltax)                0.0000
   Community Charge Benefit (polltaxben)                0.0000
                      Council Tax (ctax)               18.0261
           Council Tax Benefit (ctaxben)                0.0000
  Universal Credit maximum award (maxuc)                0.0000
                   Universal Credit (uc)                0.0000
             Disposable income (dispinc)              417.2189
                 Pre-tax income (pretax)              300.0000
                  Total net tax (nettax)             -117.2189
           Childcare subsidy (chcaresub)               80.0000
           Free school meals value (fsm)                0.0000
 Total benefits and Tax Credits (totben)              197.3115
==============================================================
                     net_desc (ADULT 1):                      
==============================================================
                Taxable income (taxable)              203.1731
                     Income tax (inctax)               39.7365
             National Insurance (natins)               22.3300
  National Insurance, class 1 (natinsc1)               22.3300
  National Insurance, class 2 (natinsc2)                0.0000
  National Insurance, class 4 (natinsc4)                0.0000
           Pre-tax earnings (pretaxearn)              300.0000
         Post-tax earnings (posttaxearn)              237.9335
==============================================================
                     net_desc (ADULT 2):                      
==============================================================
                Taxable income (taxable)                0.0000
                     Income tax (inctax)                0.0000
             National Insurance (natins)                0.0000
  National Insurance, class 1 (natinsc1)                0.0000
  National Insurance, class 2 (natinsc2)                0.0000
  National Insurance, class 4 (natinsc4)                0.0000
           Pre-tax earnings (pretaxearn)                0.0000
         Post-tax earnings (posttaxearn)                0.0000
==============================================================
```
Note that amounts in FORTAX are at the weekly level. If we wish to get the annual equivalent (for example) we can just multiply by 52.
```
net = net * 52
```
If we wish to calculate marginal tax rates holding hours-of-work fixed, we could compute a forward difference
```
type(net_t) :: net1, net2, dnet
real(dp) :: mtr, dearn

call FORTAX_fam_gen(fam, earn1 = 300.0_dp, hrs1 = 20.0_dp, ccexp = 100.0_dp, kidage = [0, 4])
call FORTAX_calcNetInc(sys, fam, net1)

dearn = 1e-4_dp
fam%ad(1)%earn = fam%ad(1)%earn + dearn
call FORTAX_calcNetInc(sys, fam, net2)
```
With the information in `net2` and `net1` we can calculate the total marginal effective tax rate as
```
mtr = (net2%tu%nettax - net2%tu%nettax) / dearn
```
Alternatively, we could calculate the marginal tax rates for all income income components as
```
dnet = (net2 - net1) / dearn
```
which would then allow us to understand how all the different components of income are changing as earnings increase.

## Piecewise linear budget sets

FORTAX can also calculate exact piecewise linear representations of the budget constraint as hours or earnings (and select other measures) are varied continuously over some interval. This requires the derived type `bcout_t`. Thus, we may begin our program in this case with
```
use fortax_library, dp => FORTAX_dp

type(sys_t) :: sys
type(fam_t) :: fam
type(bcout_t) :: bc

! Load the tax system. These are saved as JSON files
call FORTAX_readFortaxParams(sys, "systems/fortax/April06.json")

! Generate a family
call FORTAX_fam_gen(fam, ccexp = 100.0_dp, kidage = [0, 4])
```
Note that I have not specified any earnings or labour supply in `fam` as these are going to be varied in the budget constraint routine and so will have no impact in this particular example.

Suppose we wish to calculate the piecewise linear representation of the budget constraint as we vary hours over some interval. We can do this using the `FORTAX_kinksHours` subroutine. If say, we wish to vary hours from 0 to 50 (with a constant hourly wage of 6 pounds) we would do the following
```
call FORTAX_kinksHours(sys, fam, ad = 1, wage = 6.0_dp, hours1 = 0.0_dp, hours2 = 50.0_dp, bcout = bc)
```
The `ad` keyword specifies the adult whose labour supply we are varying. As we only have a single adult in our example, this can only take a value of 1. If we had a couple, this could take the value of either 1 or 2. When varying the labour supply of an adult in a couple, the individual whose labour supply is not being varied will take the values specified in `fam`.

The budget constraint information is stored in `bc` (type `bcout_t`). We can conveniently summarise this information using `FORTAX_kinks_desc`.
```
call FORTAX_kinks_desc(bc)
```
In this example, this would return
```
==============================================================
                          tu%dispinc                          
==============================================================
         Hours        Earnings          Income           Rate
==============================================================
         0.000           0.000         185.062        1.00000
         3.333          20.000         205.062        0.00000
        12.908          77.450         205.062        1.00000
        13.727          82.364         209.975        0.80000
        16.000          96.000         220.884        9.99900*
        16.000          96.000         358.710        0.80000
        16.138          96.827         359.372        0.72000
        16.167          97.000         359.497        0.63200
        16.731         100.385         361.636        0.33600
        23.029         138.173         374.333        0.24000
        30.000         180.000         384.371        9.99900*
        30.000         180.000         394.833        0.24000
        31.491         188.947         396.980        0.30000
        50.000         300.000         430.296        0.30000
==============================================================
```
This describes the piecewise linear representation of the budget constraint (family net income, `net%tu%dispinc`) so the rate here is the slope of the budget constraint. FORTAX correctly identifies the location of the hours-of-work discontinuities (due to rules in the UK tax credit system), and encodes positive / negative instances a rate of (+/-)9.999. (`FORTAX_kinks_desc` also visually indicates discontinuities using an asterisk next to the rate.)

Once the budget constraint has been calculated with `FORTAX_kinksHours` it is very simple to obtain the incomes at an arbitray value of hours over the interval that it was calculated (so 0 and 50 in our example here) using `FORTAX_evalKinksHours`. If we wish to calculate net income at 40 hours we would do
```
call FORTAX_evalKinksHours(bc, 40.0_dp, earnings, netincome, rate)
```
where `earnings`, `netincome`, and `rate` are `real(dp)` variables. This would return `earnings = 240.00`, `netincome = 412.30`, and rate = `0.30`. The value of `netincome` is the same as the value of `net%tu%dispinc` if we had alternatively done
```
call FORTAX_fam_gen(fam, ccexp = 100.0_dp, kidage = [0, 4], earn1 = 240.0_dp, hrs1 = 40.0_dp)
call FORTAX_calcNetInc(sys, fam , net)
```
The budget constraint routines in FORTAX are not limited to evaluating the overall net income measure (`net%tu%dispinc`). It can work with any component of `net_t` and also calculate arbitrary combinations of income measures. For example, to calculate the total amount of income tax and national insurance
of adult 1 (`ad = 1`) when labour supply is varied, we can specify a vector `taxout`.
```
call FORTAX_kinksHours(sys, fam, ad = 1, wage = 6.0_dp, hours1 = 0.0_dp, hours2 = 50.0_dp, bcout = bc, &
		                   taxlevel = 'ad1', taxout = ['inctax', 'natins'])

call FORTAX_kinks_desc(bc)
```
The output of `FORTAX_kinks_desc` is
```
==============================================================
                   ad(1)%(inctax + natins)                    
==============================================================
         Hours        Earnings          Income           Rate
==============================================================
         0.000           0.000           0.000        0.00000
        16.138          96.827           0.000        0.10000
        16.167          97.000           0.017        0.21000
        23.029         138.173           8.664        0.33000
        50.000         300.000          62.067        0.33000
==============================================================
```
Here `Income` is the combined income and National Insurance liability, while `Rate` is the combined marginal tax rate.

In some settings we may wish to vary earnings at fixed hours. For this purpose, the user can use the `FORTAX_kinksEarn` and `FORTAX_evalKinksEarn` subroutines. Internally, both `FORTAX_kinksHours` and `FORTAX_kinksEarn` are repeatedly calling `FORTAX_calcNetInc` to construct the budget constraint. Whether it is more appropriate for a user to call `FORTAX_calcNetInc` directly, or to first summarise the entire budget constraint using `FORTAX_kinksHours` or `FORTAX_kinksEarn` is application specific.

## Price uprating

The FORTAX library provides a number of user routines to manipulate tax and benefit systems (type `sys_t`). Suppose we start our program with an existing system file that is provided with FORTAX.
```
use fortax_library, dp => FORTAX_dp

type(sys_t) :: sys

! Load the tax system
call FORTAX_readFortaxParams(sys, "systems/fortax/April06.json")
```
We now may wish to uprate the system. Suppose we wish to uprate all monetary amounts by 10%. In this case we can use `FORTAX_upratesys`.
```
call FORTAX_upratesys(sys, factor = 1.1_dp)
```
An alternative way to perform uprating is to just multiply `sys` by a given factor. That is, we can do
```
sys = sys * 1.1_dp
```
Rather than specifying an uprating factor directly, FORTAX can also work with a database of price indices. The default database is stored in `prices/rpi.csv` which specifies a date and a price index. Price indices use the derived type `rpi_t`.
```
call FORTAX_loadindex(rpi)
```
would store the default price index data in `rpi` (type `rpi_t`). Alternative databases can be specified by passing an optional file path. Then to obtain the uprating factor from some date `date0` to date `date1` we can use the function `FORTAX_uprateFactor`. Both `date0` and `date1` are integers and represent dates in the `YYYYMMDD` format.
```
factor = upratefactor(rpi, date0, date1)
```
If we wish to save a modified tax system we can simply use the `FORTAX_writeFortaxParams` subroutine.
```
call FORTAX_writeFortaxParams(sys, fname)
```
# Interfaces

The FORTAX library can be called by other programming languages and software packages. The Fortran C bindings are defined in `fortax_library_c.f90`. A [Julia](https://julialang.org/) interface is provided in `fortax_julia.jl`. A [Stata](https://www.stata.com/) plugin, which has more limited functionality, is available [here](https://github.com/ajshephard/fortax-stata).

### Julia

The Julia version can be used much like the Fortran version. Almost all the functions in the `fortax_library.f90` module are accessible through Julia.
Consider the following example where we load a system and calculate incomes for a specified family type.
```
sys = readFortaxParams("systems/fortax/April06.json")
fam = fam_gen(earn1 = 300.0, hrs1 = 20.0, ccexp = 100.0, kidage = [0, 4])
net = calcNetInc(sys, fam)
net_desc(net)
```
The output from `net_desc` is identical to that which we saw before. The Julia structs defining `fam_t`, `sys_t`, etc. are immutable. If you wish to manually change any of the fields the user can use [Setfield.jl](https://github.com/jw3126/Setfield.jl), for example.
