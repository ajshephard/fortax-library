SYSINCLUDES = syslist.inc ccben.inc fc.inc rebatesys.inc chben.inc inctax.inc ctc.inc incsup.inc ctax.inc natins.inc ctaxben.inc ntc.inc wtc.inc extra.inc
XMLINCLUDES = read_from_buffer.inc read_xml_array.inc read_xml_scalar.inc
INCLUDESPATH = includes
MODPATH = ./
OUTPATH = ./

OBJECTS  = fortax_library.o fortax_realtype.o fortax_util.o fortax_type.o fortax_calc.o fortax_extra.o fortax_prices.o fortax_read.o fortax_taxbenread.o fortax_write.o fortax_kinks.o fortax_compare.o
XMLOBJECTS = xmlparse.o read_xml_primitives.o write_xml_primitives.o xmltaxben_t.o xmlfortax_t.o xmlfamcompare_t.o

F90 = ifort
# F90 = g95
F90 = gfortran
# F90 = pgfortran
F90 = sunf95

ifeq ($(F90),gfortran)
	FFLAGS = -O3 -ffixed-line-length-none -ffree-line-length-none -ffree-form -x f95
	FFLAGS = -O3 -ffree-form -x f95
	CFLAGS = -cpp -E -P -x c -ansi
	CPP = gcc
	FEXT = f
endif

ifeq ($(F90),g95)
	FFLAGS = -O3 -ffree-line-length-huge -std=f2003 -ffree-form
	CFLAGS = -cpp -E -P -x c -ansi
	CPP = gcc
	FEXT = f
endif

ifeq ($(F90),ifort)
	# GPROF = -g -p
	#DIAGDISABLE = -diag-disable 5268,7025
	FFLAGS = -O3 -fpp -stand f03 -warn all -inline speed -inline-forceinline -no-prec-div -xHost -static -fPIC -gen-interfaces $(DIAGDISABLE) $(GPROF) -module $(MODPATH) -I$(INCLUDESPATH) -assume source_include 
	# -I$(INCLUDESPATH)/system -I$(INCLUDESPATH)/label
	FEXT = f90
endif

ifeq ($(F90),pgfortran)
	F90 = /opt/pgi/linux86-64/2013/bin/pgfortran
	CPP = $(F90)
	FFLAGS = -O3 -Mfree
	CFLAGS = -O3 -Mpreprocess -Mfree -Mcpp=c89 -E
	FEXT = f
endif

ifeq ($(F90),sunf95)
	F90 = ~/Downloads/SolarisStudio12.3-linux-x86-bin/solarisstudio12.3/bin/sunf95
	FFLAGS = -xO3 -free -fpp
	FEXT = f90
endif

all:$(OBJECTS) $(XMLOBJECTS)
	ar rc $(OUTPATH)/fortax.a $(OBJECTS) $(XMLOBJECTS)

ifeq ($(FEXT),f)
%.$(FEXT):%.f90
	$(CPP) $(CFLAGS) $< > $@
endif

xmlparse.o:xmlparse.$(FEXT)
	$(F90) $(FFLAGS) -c $<

read_xml_primitives.o:read_xml_primitives.$(FEXT) xmlparse.o \
	$(addprefix $(INCLUDESPATH)/xml/, $(XMLINCLUDES))
	$(F90) $(FFLAGS) -I$(INCLUDESPATH)/xml -c $<

write_xml_primitives.o:write_xml_primitives.$(FEXT) xmlparse.o
	$(F90) $(FFLAGS) -c $<

xmltaxben_t.o:xmltaxben_t.$(FEXT) read_xml_primitives.o xmlparse.o
	$(F90) $(FFLAGS) -O1 -c $<

xmlfortax_t.o:xmlfortax_t.$(FEXT) read_xml_primitives.o write_xml_primitives.o xmlparse.o
	$(F90) $(FFLAGS) -O1 -c $<

xmlfamcompare_t.o:xmlfamcompare_t.$(FEXT) read_xml_primitives.o write_xml_primitives.o xmlparse.o
	$(F90) $(FFLAGS) -O1 -c $<

fortax_realtype.o:fortax_realtype.$(FEXT)
	$(F90) $(FFLAGS) -c $<

fortax_util.o:fortax_util.$(FEXT) fortax_realtype.o
	$(F90) $(FFLAGS) -c $<

fortax_compare.o:fortax_compare.$(FEXT) fortax_realtype.o fortax_type.o fortax_util.o fortax_write.o fortax_read.o xmlfamcompare_t.o fortax_calc.o
	$(F90) $(FFLAGS) -c $<

fortax_type.o:fortax_type.$(FEXT) fortax_realtype.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/, sys_t.inc sys_init.inc fam_t.inc famad_t.inc nettu_t.inc netad_t.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c $<

fortax_calc.o:fortax_calc.$(FEXT) fortax_realtype.o fortax_type.o
	$(F90) $(FFLAGS) $(DEFINES) -c $<

fortax_extra.o:fortax_extra.$(FEXT) fortax_realtype.o fortax_type.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/, fortax_minamt.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c $<

fortax_prices.o:fortax_prices.$(FEXT) fortax_realtype.o fortax_type.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/, fortax_uprate.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c $<

fortax_read.o:fortax_read.$(FEXT) fortax_realtype.o xmlfortax_t.o fortax_util.o fortax_type.o \
	$(addprefix $(INCLUDESPATH)/,fortax_typeread.inc fortax_read.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c $<

fortax_taxbenread.o:fortax_taxbenread.$(FEXT) fortax_write.o fortax_realtype.o xmltaxben_t.o fortax_util.o fortax_type.o \
	$(addprefix $(INCLUDESPATH)/,fortax_typeread.inc fortax_read.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c $<

fortax_write.o:fortax_write.$(FEXT) fortax_type.o xmlparse.o fortax_realtype.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/,fortax_write.inc fortax_print.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c $<

fortax_kinks.o:fortax_kinks.$(FEXT) fortax_type.o fortax_util.o fortax_realtype.o fortax_calc.o
	$(F90) $(FFLAGS) -c $<

fortax_library.o:fortax_library.$(FEXT) fortax_calc.o fortax_compare.o fortax_extra.o fortax_kinks.o fortax_prices.o fortax_read.o fortax_realtype.o fortax_taxbenread.o fortax_type.o fortax_util.o fortax_write.o
	$(F90) $(FFLAGS) -c $<

clean:
	rm -f $(OBJECTS)  $(XMLOBJECTS) *.mod *.a *.f
