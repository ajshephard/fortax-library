using StaticArrays

const fortaxlib="/path/to/fortax.so"

include("fortax_julia_type.jl")

# wrappers for c bindings

# fortax_calc

function calcNetInc(sys::sys_t, fam::fam_t)
    return ccall((:C_FORTAX_calcNetInc, fortaxlib), net_t, (Ref{sys_t}, Ref{fam_t}), sys, fam)
end

# fortax_extra

function setMinAmount(sys::sys_t, minamt)
    sysref = Ref{sys_t}(sys)
    ccall((:C_FORTAX_setMinAmount, fortaxlib), Cvoid, (Ref{sys_t}, Ref{Cdouble}), sysref, minamt)
    return sysref[]
end

function abolishNIFee(sys::sys_t)
    sysref = Ref{sys_t}(sys)
    ccall((:C_FORTAX_abolishNIFee, fortaxlib), Cvoid, (Ref{sys_t},), sysref)
    return sysref[]
end
 
function disableTaperRounding(sys::sys_t)
    sysref = Ref{sys_t}(sys)
    ccall((:C_FORTAX_disableTaperRounding, fortaxlib), Cvoid, (Ref{sys_t},), sysref)
    return sysref[]
end

function fsMinAppAmt(sys::sys_t, inappamt)
    sysref = Ref{sys_t}(sys)
    c_inappamt = Cint(inappamt)
    ccall((:C_FORTAX_fsMinAppAmt, fortaxlib), Cvoid, (Ref{sys_t}, Ref{Cint}), sysref, c_inappamt)
    return sysref[]
end

function taperMatGrant(sys::sys_t, taper)
    sysref = Ref{sys_t}(sys)
    c_taper = Cint(taper)
    ccall((:C_FORTAX_taperMatGrant, fortaxlib), Cvoid, (Ref{sys_t}, Ref{Cint}), sysref, c_taper)
    return sysref[]
end

function imposeUc(sys::sys_t)
    sysref = Ref{sys_t}(sys)
    ccall((:C_FORTAX_imposeUc, fortaxlib), Cvoid, (Ref{sys_t},), sysref)
    return sysref[]
end

# fortax_kinks

function evalKinksHours(bc::bcout_t, hours)
    earn = Ref{Cdouble}()
    net = Ref{Cdouble}()
    mtr = Ref{Cdouble}()
    ccall((:C_FORTAX_evalKinksHours, fortaxlib), Cvoid, (Ref{bcout_t}, Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}, Ptr{Cvoid}, Ptr{Cvoid}), 
                                                  bc, hours, earn, net, mtr, C_NULL, C_NULL)
    return earn[], net[], mtr[]
end

function evalKinksEarn(bc::bcout_t, earn)
    hours = Ref{Cdouble}()
    net = Ref{Cdouble}()
    mtr = Ref{Cdouble}()    
    ccall((:C_FORTAX_evalKinksEarn, fortaxlib), Cvoid, (Ref{bcout_t}, Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}, Ptr{Cvoid}, Ptr{Cvoid}), 
                                                  bc, earn, hours, net, mtr, C_NULL, C_NULL)
    return hours[], net[], mtr[]
end

function kinksHours(sys::sys_t, fam::fam_t, ad, wage, hours1, hours2)
    ad_c = Cint(ad)
    return ccall((:C_FORTAX_kinksHoursDefault, fortaxlib), bcout_t, (Ref{sys_t}, Ref{fam_t}, Ref{Cint}, Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}), 
                 sys, fam, ad_c, wage, hours1, hours2)
end

function kinksHours(sys::sys_t, fam::fam_t, ad, wage, hours1, hours2, taxlevel, taxout)

    # convert taxout (vector of strings) to a matrix of char
    len_taxout = Cint(length(taxout))
    mat_taxout = zeros(Cchar, 16, len_taxout)
    for i in 1:len_taxout
        for (j, c) in enumerate(taxout[i])
            mat_taxout[j, i] = c
        end
    end
    len_taxlevel = Cint(length(taxlevel))
    ad_c = Cint(ad)
    return ccall((:C_FORTAX_kinksHours, fortaxlib), bcout_t, (Ref{sys_t}, Ref{fam_t}, Ref{Cint}, 
                 Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}, Ptr{UInt8}, Ref{Cint}, Ptr{UInt8}, Ref{Cint}), 
                 sys, fam, ad_c, wage, hours1, hours2, taxlevel, len_taxlevel, mat_taxout, len_taxout)
end

function kinksEarn(sys::sys_t, fam::fam_t, ad, hours, earn1, earn2)
    ad_c = Cint(ad)
    return ccall((:C_FORTAX_kinksEarnDefault, fortaxlib), bcout_t, (Ref{sys_t}, Ref{fam_t}, Ref{Cint}, Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}), 
                 sys, fam, ad_c, hours, earn1, earn2)
end

function kinksEarn(sys::sys_t, fam::fam_t, ad_c, hours, earn1, earn2, taxlevel, taxout)

    # convert taxout (vector of strings) to a matrix of char
    len_taxout = Cint(length(taxout))
    mat_taxout = zeros(Cchar, 16, len_taxout)
    for i in 1:len_taxout
        for (j, c) in enumerate(taxout[i])
            mat_taxout[j, i] = c
        end
    end
    len_taxlevel = Cint(length(taxlevel))
    ad_c = Cint(ad)
    return ccall((:C_FORTAX_kinksEarn, fortaxlib), bcout_t, (Ref{sys_t}, Ref{fam_t}, Ref{Cint}, 
                 Ref{Cdouble}, Ref{Cdouble}, Ref{Cdouble}, Ptr{UInt8}, Ref{Cint}, Ptr{UInt8}, Ref{Cint}), 
                 sys, fam, ad_c, hours, earn1, earn2, taxlevel, len_taxlevel, mat_taxout, len_taxout)
end

function kinks_desc(bcout::bcout_t)
    ccall((:C_FORTAX_kinks_desc, fortaxlib), Cvoid, (Ref{bcout_t},), bcout)
end

# fortax_prices

function loadIndex(fname::String)
    rpiref = Ref{rpi_t}()
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_loadIndex, fortaxlib), Cvoid, (Ref{rpi_t}, Ptr{UInt8}, Ref{Cint}), rpiref, fname, len_fname)
    return rpiref[]
end

function uprateFactor(rpi::rpi_t, date0, date1)
    date0c = Cint(date0)
    date1c = Cint(date1)
    return ccall((:C_FORTAX_uprateFactor, fortaxlib), Cdouble, (Ref{rpi_t}, Ref{Cint}, Ref{Cint}), rpi, date0c, date1c)
end

function uprateSys(sys::sys_t, factor)
    sysref = Ref{sys_t}(sys)
    ccall((:C_FORTAX_uprateSys, fortaxlib), Cvoid, (Ref{sys_t}, Ref{Cdouble}, Ptr{Cvoid}), sysref, factor, C_NULL)
    return sysref[]
end

function uprateSys(sys::sys_t, factor, newdate)
    sysref = Ref{sys_t}(sys)
    c_newdate = Cint(newdate)
    ccall((:C_FORTAX_uprateSys, fortaxlib), Cvoid, (Ref{sys_t}, Ref{Cdouble}, Ptr{Cint}), sysref, factor, c_newdate)
    return sysref[]
end

# fortax_read

function readFortaxParams(fname::String)
    sysref = Ref{sys_t}()
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_readFortaxParams, fortaxlib), Cvoid, (Ref{sys_t}, Ptr{UInt8}, Ref{Cint}, Ptr{Cvoid}), sysref, fname, len_fname, C_NULL)
    return sysref[]
end

# fortax_type

function fam_init()
    famref = Ref{fam_t}()
    ccall((:C_FORTAX_fam_init, fortaxlib), Cvoid, (Ref{fam_t}, ), famref)
    return famref[]
end

function fam_refresh(fam)
    famref = Ref{fam_t}(fam)
    ccall((:C_FORTAX_fam_refresh, fortaxlib), Cvoid, (Ref{fam_t}, ), famref)
    return famref[]
end

function net_init()
    netref = Ref{net_t}()
    ccall((:C_FORTAX_net_init, fortaxlib), Cvoid, (Ref{net_t}, ), netref)
    return netref[]
end

function sys_init()
    sysref = Ref{sys_t}()
    ccall((:C_FORTAX_sys_init, fortaxlib), Cvoid, (Ref{sys_t}, ), sysref)
    return sysref[]
end

function fam_saveF90(fam::fam_t)
    ccall((:C_FORTAX_fam_saveF90, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{Cvoid}, Ptr{Cvoid}), fam, C_NULL, C_NULL)
end

function fam_saveF90(fam::fam_t, fname::String)
    len_fname = length(fname)
    ccall((:C_FORTAX_fam_saveF90, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{UInt8}, Ref{Cint}), fam, fname, len_fname)
end

function fam_desc(fam::fam_t)
    ccall((:C_FORTAX_fam_desc, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{Cvoid}, Ptr{Cvoid}), fam, C_NULL, C_NULL)
end

function fam_desc(fam::fam_t, fname::String)
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_fam_desc, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{UInt8}, Ref{Cint}), fam, fname, len_fname)
end

function net_desc(net::net_t)
    ccall((:C_FORTAX_net_desc, fortaxlib), Cvoid, (Ref{net_t}, Ptr{Cvoid}, Ptr{Cvoid}), net, C_NULL, C_NULL)
end

function net_desc(net::net_t, fname::String)
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_net_desc, fortaxlib), Cvoid, (Ref{net_t}, Ptr{UInt8}, Ref{Cint}), net, fname, len_fname)
end

function fam_desc(fam::fam_t, fname::String)
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_fam_desc, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{UInt8}, Ref{Cint}), fam, fname, len_fname)
end

function sys_desc(sys::sys_t)
    ccall((:C_FORTAX_sys_desc, fortaxlib), Cvoid, (Ref{sys_t}, Ptr{Cvoid}, Ptr{Cvoid}), sys, C_NULL, C_NULL)
end

function sys_desc(sys::sys_t, fname::String)
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_sys_desc, fortaxlib), Cvoid, (Ref{sys_t}, Ptr{UInt8}, Ref{Cint}), sys, fname, len_fname)
end

function writeFortaxParams(sys::sys_t, fname::String)
    len_fname = Cint(length(fname))
    ccall((:C_FORTAX_writeFortaxParams, fortaxlib), Cvoid, (Ref{sys_t}, Ptr{UInt8}, Ref{Cint}), sys, fname, len_fname)
end

@generated function Base.:+(x::netad_t, y::netad_t)
    expr = ( :(x.$f + y.$f) for f in fieldnames(netad_t) ) 
    :( netad_t($(expr...)) )
end

@generated function Base.:-(x::netad_t, y::netad_t)
    expr = ( :(x.$f - y.$f) for f in fieldnames(netad_t) ) 
    :( netad_t($(expr...)) )
end

@generated function Base.:*(x::netad_t, y::Number)
    expr = ( :(x.$f * y) for f in fieldnames(netad_t) ) 
    :( netad_t($(expr...)) )
end

@generated function Base.:*(y::Number, x::netad_t)
    expr = ( :(x.$f * y) for f in fieldnames(netad_t) ) 
    :( netad_t($(expr...)) )
end

@generated function Base.:/(x::netad_t, y::Number)
    expr = ( :(x.$f / y) for f in fieldnames(netad_t) ) 
    :( netad_t($(expr...)) )
end

@generated function Base.:+(x::nettu_t, y::nettu_t)
    expr = ( :(x.$f + y.$f) for f in fieldnames(nettu_t) ) 
    :( nettu_t($(expr...)) )
end

@generated function Base.:-(x::nettu_t, y::nettu_t)
    expr = ( :(x.$f - y.$f) for f in fieldnames(nettu_t) ) 
    :( nettu_t($(expr...)) )
end

@generated function Base.:*(x::nettu_t, y::Number)
    expr = ( :(x.$f * y) for f in fieldnames(nettu_t) ) 
    :( nettu_t($(expr...)) )
end

@generated function Base.:*(y::Number, x::nettu_t)
    expr = ( :(x.$f * y) for f in fieldnames(nettu_t) ) 
    :( nettu_t($(expr...)) )
end

@generated function Base.:/(x::nettu_t, y::Number)
    expr = ( :(x.$f / y) for f in fieldnames(nettu_t) ) 
    :( nettu_t($(expr...)) )
end

@generated function Base.:+(x::net_t, y::net_t)
    expr = ( :(x.$f + y.$f) for f in fieldnames(net_t) ) 
    :( net_t($(expr...)) )
end

@generated function Base.:-(x::net_t, y::net_t)
    expr = ( :(x.$f - y.$f) for f in fieldnames(net_t) ) 
    :( net_t($(expr...)) )
end

@generated function Base.:*(x::net_t, y::Number)
    expr = ( :(x.$f * y) for f in fieldnames(net_t) ) 
    :( net_t($(expr...)) )
end

@generated function Base.:*(y::Number, x::net_t)
    expr = ( :(x.$f * y) for f in fieldnames(net_t) ) 
    :( net_t($(expr...)) )
end

@generated function Base.:/(x::net_t, y::Number)
    expr = ( :(x.$f / y) for f in fieldnames(net_t) ) 
    :( net_t($(expr...)) )
end

function Base.:*(x::sys_t, y::Number)
    return uprateSys(x, y)
end

function Base.:*(y::Number, x::sys_t)
    return uprateSys(x, y)
end

function Base.:/(x::sys_t, y::Number)
    return uprateSys(x, 1.0 / y)
end
