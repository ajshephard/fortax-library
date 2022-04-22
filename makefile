#
# BUILD options: release, debug
# FC options: ifort, gfortran
#
# You may still need to change the name of the Fortran compiler
# and the compile options
#

ifeq ($(BUILD),)
	BUILD=release
endif

ifeq ($(F90),)
	F90=ifort
endif

ifeq ($(OUTDIR),)
	OUTDIR = $(F90).$(BUILD)
endif

#===================================================================
ifeq ($(F90),ifort)
	FFLAGS = -fPIC -sox -stand f18 -gen-interfaces $(GPROF) -module $(OUTDIR)
	ifeq ($(BUILD),release)
	  	FFLAGS += -O3 -inline speed -inline-forceinline -no-prec-div -diag-disable 5268,7025
	else
	  	FFLAGS += -O0 -g -traceback -debug extended -check all
	endif
	FPP = -fpp
endif #ifort

ifeq ($(F90),gfortran)
	FFLAGS = -fPIC -ffree-line-length-none -ffree-form -std=f2018 -J$(OUTDIR)
	ifeq ($(BUILD),release)
  		FFLAGS += -O3
	else
	  	FFLAGS += -O0 -g
	endif
	FPP = -cpp
endif #gfortran

FYPPFLAGS = -Iincludes

JSONINC = includes/json
JSONOBJECTS = json_kinds.o \
	json_parameters.o \
	json_string_utilities.o \
	json_value_module.o \
	json_file_module.o \
	json_module.o

OBJECTS = fortax_library.o \
	fortax_library_c.o \
	fortax_realtype.o \
	fortax_util.o \
	fortax_type.o \
	fortax_calc.o \
	fortax_extra.o \
	fortax_prices.o \
	fortax_read.o \
	fortax_write.o \
	fortax_kinks.o

VPATH = .:$(OUTDIR)         

all: fortax.a fortax.so

fortax.a: $(OBJECTS) $(JSONOBJECTS)
	ar rc $(OUTDIR)/fortax.a $(addprefix $(OUTDIR)/,$(OBJECTS)) $(addprefix $(OUTDIR)/,$(JSONOBJECTS))

fortax.so: $(OBJECTS) $(JSONOBJECTS)
	$(F90) -shared $(addprefix $(OUTDIR)/,$(OBJECTS)) $(addprefix $(OUTDIR)/,$(JSONOBJECTS)) -lirc -o $(OUTDIR)/fortax.so

$(JSONOBJECTS):
	@mkdir -p $(OUTDIR)
	$(F90) $(FFLAGS) $(FPP) -I${JSONINC} -c $(@:.o=.f90) -o $(OUTDIR)/$@

$(OBJECTS):
	@mkdir -p $(OUTDIR)
	$(F90) $(FFLAGS) -c $(@:.o=.f90) -o $(OUTDIR)/$@

# ---------------------------------------------------------------------------------------
# file dependencies
# ---------------------------------------------------------------------------------------

# pre-processing
%.f90: %.fpp fortax.fypp include_files.fypp
	fypp $(FYPPFLAGS) $< $@

json_kinds.o: json_kinds.f90

json_parameters.o: json_parameters.f90 \
	json_kinds.o

json_string_utilities.o: json_string_utilities.f90 \
	json_kinds.o \
	json_parameters.o

json_value_module.o: json_value_module.f90 \
	json_kinds.o \
	json_parameters.o \
	json_string_utilities.o

json_file_module.o: json_file_module.f90 \
	json_kinds.o \
	json_parameters.o \
	json_string_utilities.o \
	json_value_module.o

json_module.o: json_module.f90 \
	json_kinds.o \
	json_parameters.o \
	json_string_utilities.o \
	json_value_module.o \
	json_file_module.o

fortax_realtype.o: fortax_realtype.f90

fortax_util.o: fortax_util.f90 \
	fortax_realtype.o

fortax_type.o: fortax_type.f90 \
	fortax_realtype.o \
	fortax_util.o

fortax_calc.o: fortax_calc.f90 \
	fortax_realtype.o \
	fortax_type.o

fortax_extra.o: fortax_extra.f90 \
	fortax_realtype.o \
	fortax_type.o \
	fortax_util.o

fortax_prices.o:fortax_prices.f90 \
	fortax_realtype.o \
	fortax_type.o \
	fortax_util.o

fortax_read.o: fortax_read.f90 \
	fortax_realtype.o \
	fortax_util.o \
	fortax_type.o \
	json_module.o

fortax_write.o: fortax_write.f90 \
	fortax_type.o \
	fortax_realtype.o \
	fortax_util.o \
	json_module.o

fortax_kinks.o: fortax_kinks.f90 \
	fortax_type.o \
	fortax_util.o \
	fortax_realtype.o \
	fortax_calc.o

fortax_library.o: fortax_library.f90 \
	fortax_calc.o \
	fortax_extra.o \
	fortax_kinks.o \
	fortax_prices.o \
	fortax_read.o \
	fortax_realtype.o \
	fortax_type.o \
	fortax_util.o \
	fortax_write.o

fortax_library_c.o: fortax_library_c.f90 \
	fortax_library.o

clean:
	rm -f fortax_extra.f90 fortax_kinks.f90 fortax_prices.f90 fortax_read.f90 \
	fortax_type.f90 fortax_write.f90 $(OUTDIR)/*.*
