using StaticArrays
using Parameters
using Setfield

const fortaxlib="fortax.so"

const maxRPI = 1024
const maxkids = 10

const lab_ctax = (banda = 1, bandb = 2, bandc = 3, bandd = 4, 
                  bande = 5, bandf = 6, bandg = 7, bandh = 8)

const lab_tenure = (own_outright = 1, mortgage = 2, part_own = 3,
                    social_renter = 4, private_renter = 5, rent_free = 6,
                    other = 7)

const lab_region = (north_east = 1, north_west = 2, yorks = 3, east_midlands = 4,
                    west_midlands = 5, eastern = 6, london = 7, south_east = 8,
                    south_west = 9, wales = 10, scotland = 11, northern_ireland = 12)

@with_kw struct famad_t
    age::Cint = 25
    selfemp::Bool = false
    hrs::Cdouble = 0.0
    earn::Cdouble = 0.0
end

@with_kw struct fam_t
    couple::Bool = false
    married::Bool = false
    ccexp::Cdouble = 0.0
    maint::Cdouble = 0.0
    nkids::Cint = 0
    kidage::SVector{maxkids, Cint} = zeros(maxkids)
    yngkid::Cint = 0
    nothads::Cint = 0
    tenure::Cint = lab_tenure.own_outright
    rent::Cdouble = 0.0
    rentcap::Cdouble = 0.0
    region::Cint = lab_region.north_east
    ctband::Cint = lab_ctax.bandd
    banddratio::Cdouble = 1.0
    intdate::Cint = 19900101
    ad::SVector{2, famad_t} = @SVector [famad_t() for _ = 1:2]
    #ad::NTuple{2, famad_t} = (famad_t(), famad_t())
end

@with_kw struct netad_t
    taxable::Cdouble
    inctax::Cdouble
    natins::Cdouble
    natinsc1::Cdouble
    natinsc2::Cdouble
    natinsc4::Cdouble
    pretaxearn::Cdouble
    posttaxearn::Cdouble
end

@with_kw struct nettu_t
    pretaxearn::Cdouble
    posttaxearn::Cdouble
    chben::Cdouble
    matgrant::Cdouble
    fc::Cdouble
    wtc::Cdouble
    ctc::Cdouble
    ccexp::Cdouble
    incsup::Cdouble
    hben::Cdouble
    polltax::Cdouble
    polltaxben::Cdouble
    ctax::Cdouble
    ctaxben::Cdouble
    maxuc::Cdouble
    uc::Cdouble
    dispinc::Cdouble
    pretax::Cdouble
    nettax::Cdouble
    chcaresub::Cdouble
    fsm::Cdouble
    totben::Cdouble
end

@with_kw struct net_t
    ad::SVector{2, netad_t} = @SVector [netad_t() for _ = 1:2]
    tu::nettu_t = nettu_t()
end

@with_kw struct rpi_t
    ndate::Cint = 0
    date::SVector{maxRPI, Cint} = zeros(maxRPI)
    index::SVector{maxRPI, Cdouble} = ones(maxRPI)
end

function fam_desc(fam::fam_t)
    ccall((:C_FORTAX_fam_desc, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{Cvoid}, Ptr{Cvoid}), fam, C_NULL, C_NULL)
end

function fam_desc(fam::fam_t, fname::String)
    len_fname = length(fname)
    ccall((:C_FORTAX_fam_desc, fortaxlib), Cvoid, (Ref{fam_t}, Ptr{UInt8}, Ref{Cint}), fam, fname, len_fname)
end

function net_desc(net::net_t)
    ccall((:C_FORTAX_net_desc, fortaxlib), Cvoid, (Ref{net_t}, Ptr{Cvoid}, Ptr{Cvoid}), net, C_NULL, C_NULL)
end

function net_desc(net::net_t, fname::String)
    len_fname = length(fname)
    ccall((:C_FORTAX_net_desc, fortaxlib), Cvoid, (Ref{net_t}, Ptr{UInt8}, Ref{Cint}), net, fname, len_fname)
end

function fam_init()
    famref = Ref{fam_t}()
    ccall((:C_FORTAX_fam_init, fortaxlib), Cvoid, (Ref{fam_t}, ), famref)
    return famref[]
end

function net_init()
    netref = Ref{net_t}()
    ccall((:C_FORTAX_net_init, fortaxlib), Cvoid, (Ref{net_t}, ), netref)
    return netref[]
end

# fortax_prices

function load_index()
    rpiref = Ref{rpi_t}()
    ccall((:C_FORTAX_loadIndex, fortaxlib), Cvoid, (Ref{rpi_t}, Ptr{Cvoid}, Ptr{Cvoid}), rpiref, C_NULL, C_NULL)
    return rpiref[]
end

function load_index(fname::String)
    rpiref = Ref{rpi_t}()
    len_fname = length(fname)
    ccall((:C_FORTAX_loadIndex, fortaxlib), Cvoid, (Ref{rpi_t}, Ptr{UInt8}, Ref{Cint}), rpiref, fname, len_fname)
    return rpiref[]
end

function uprate_factor(rpi::rpi_t, date0::Integer, date1::Integer)
    date0c = Cint(date0)
    date1c = Cint(date1)
    return ccall((:C_FORTAX_uprateFactor, fortaxlib), Cdouble, (Ref{rpi_t}, Ref{Cint}, Ref{Cint}), rpi, date0c, date1c)
end
