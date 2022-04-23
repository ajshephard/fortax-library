fypp -Iincludes -F fortax_julia.fypp > julia/fortax_julia_type.jl
julia -e 'using JuliaFormatter; format_file("julia/fortax_julia_type.jl", remove_extra_newlines = true)'

