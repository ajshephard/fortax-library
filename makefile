#
# BUILD options: release, debug
# FC options: ifort, gfortran, g95, pgf90, sunf95
#
# You may still need to change the name of the Fortran compiler
# and the compile options
#

TMPDIR = /tmp

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
	FFLAGS = -fpp -sox -stand f03 -static -fPIC -gen-interfaces $(GPROF) -module $(OUTDIR)
	ifeq ($(BUILD),release)
	  	FFLAGS += -O3 -inline speed -inline-forceinline -no-prec-div -xHost -diag-disable 5268,7025
	else
	  	FFLAGS += -O0 -g -mp -traceback -debug extended -check all
	endif
	FFLAGS += -Iincludes -Iincludes/label -Iincludes/system
	CPP =
endif #ifort

ifeq ($(F90),gfortran)
	FFLAGS = -ffixed-line-length-none -ffree-line-length-none -ffree-form -x f95 -J$(OUTDIR)
	CFLAGS = -cpp -E -P -x c -ansi
	ifeq ($(BUILD),release)
  		FFLAGS += -O3
	else
	  	FFLAGS += -O0 -g
	endif
	CPP = gcc
endif #gfortran

ifeq ($(F90),g95)
	FFLAGS = -ffree-line-length-huge -std=f2003 -ffree-form -fmod=$(OUTDIR)
	CFLAGS = -cpp -E -P -x c -ansi
	ifeq ($(BUILD),release)
  		FFLAGS += -O3
	else
	  	FFLAGS += -O0 -g
	endif
	CPP = gcc
endif #g95

ifeq ($(F90),pgf90)
	FFLAGS = -Mfree -module $(OUTDIR)
	# CFLAGS = -cpp -Mpreprocess -Mfree -Mcpp=c89 -E
	CFLAGS = -cpp -E -P -x c -ansi
	ifeq ($(BUILD),release)
  		FFLAGS += -O3
	else
	  	FFLAGS += -O0 -g
	endif
	CPP = gcc
endif #pgf90

ifeq ($(F90),sunf95)
	FFLAGS = -free -fpp -moddir=$(OUTDIR)
	ifeq ($(BUILD),release)
  		FFLAGS += -xO3
	else
	  	FFLAGS += -xO0 -g
	endif
	CPP = 
endif #sunf95

XMLINC = includes/xml
XMLOBJECTS = xmlparse.o \
	read_xml_primitives.o \
	write_xml_primitives.o \
	xmltaxben_t.o \
	xmlfortax_t.o \
	xmlfamcompare_t.o

OBJECTS = fortax_library.o \
	fortax_realtype.o \
	fortax_util.o \
	fortax_type.o \
	fortax_calc.o \
	fortax_extra.o \
	fortax_prices.o \
	fortax_read.o \
	fortax_taxbenread.o \
	fortax_write.o \
	fortax_kinks.o \
	fortax_compare.o

VPATH = .:$(OUTDIR)

all: fortax.a

fortax.a: $(OBJECTS) $(XMLOBJECTS)
	ar rc $(OUTDIR)/fortax.a $(addprefix $(OUTDIR)/,$(OBJECTS)) $(addprefix $(OUTDIR)/,$(XMLOBJECTS))

$(XMLOBJECTS):
	@mkdir -p $(OUTDIR)
	$(F90) $(FFLAGS) -I$(XMLINC) -c $(@:.o=.f90) -o $(OUTDIR)/$@

ifeq ($(CPP),)
$(OBJECTS):
	@mkdir -p $(OUTDIR)
	$(F90) $(FFLAGS) -c $(@:.o=.f90) -o $(OUTDIR)/$@
else
$(OBJECTS):
	@mkdir -p $(OUTDIR)
	$(CPP) $(CFLAGS) $(@:.o=.f90) > $(TMPDIR)/$(@:.o=.f90)
	$(F90) $(FFLAGS) -c $(TMPDIR)/$(@:.o=.f90) -o $(OUTDIR)/$@
endif

# ---------------------------------------------------------------------------------------
# file dependencies
# ---------------------------------------------------------------------------------------

xmlparse.o: xmlparse.f90

read_xml_primitives.o: read_xml_primitives.f90 \
	xmlparse.o \
	$(XMLINC)/read_from_buffer.inc \
	$(XMLINC)/read_xml_array.inc \
	$(XMLINC)/read_xml_scalar.inc

write_xml_primitives.o: write_xml_primitives.f90 \
	xmlparse.o

fortax_realtype.o: fortax_realtype.f90

fortax_util.o: fortax_util.f90 \
	fortax_realtype.o

fortax_compare.o: fortax_compare.f90 \
	fortax_realtype.o \
	fortax_type.o \
	fortax_util.o \
	fortax_write.o \
	fortax_read.o \
	xmlfamcompare_t.o \
	fortax_calc.o

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
	xmlfortax_t.o \
	fortax_util.o \
	fortax_type.o

fortax_taxbenread.o: fortax_taxbenread.f90 \
	fortax_write.o \
	fortax_realtype.o \
	xmltaxben_t.o \
	fortax_util.o \
	fortax_type.o

fortax_write.o: fortax_write.f90 \
	fortax_type.o \
	xmlparse.o \
	fortax_realtype.o \
	fortax_util.o

fortax_kinks.o: fortax_kinks.f90 \
	fortax_type.o \
	fortax_util.o \
	fortax_realtype.o \
	fortax_calc.o

fortax_library.o: fortax_library.f90 \
	fortax_calc.o \
	fortax_compare.o \
	fortax_extra.o \
	fortax_kinks.o \
	fortax_prices.o \
	fortax_read.o \
	fortax_realtype.o \
	fortax_taxbenread.o \
	fortax_type.o \
	fortax_util.o \
	fortax_write.o

xmltaxben_t.o: xmltaxben_t.f90 \
	read_xml_primitives.o \
	xmlparse.o

xmlfortax_t.o: xmlfortax_t.f90 \
	read_xml_primitives.o \
	write_xml_primitives.o \
	xmlparse.o

xmlfamcompare_t.o: xmlfamcompare_t.f90 \
	read_xml_primitives.o \
	write_xml_primitives.o \
	xmlparse.o

clean:
	rm -f $(OUTDIR)/*.*