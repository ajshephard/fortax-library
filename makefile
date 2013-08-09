SYSINCLUDES = syslist.inc ccben.inc fc.inc rebatesys.inc chben.inc inctax.inc ctc.inc incsup.inc ctax.inc natins.inc ctaxben.inc ntc.inc wtc.inc extra.inc
XMLINCLUDES = read_from_buffer.inc read_xml_array.inc read_xml_scalar.inc
INCLUDESPATH = includes
MODPATH = ./
OUTPATH = ./

OBJECTS  = fortax_realtype.o fortax_util.o fortax_type.o fortax_calc.o fortax_extra.o fortax_prices.o fortax_read.o fortax_taxbenread.o fortax_write.o fortax_kinks.o fortax_compare.o
XMLOBJECTS = xmlparse.o read_xml_prims.o write_xml_prims.o xmltaxben_t.o xmlfortax_t.o xmlfamcompare_t.o

# ------------------Macro-Defs---------------------
DIAGDISABLE = -diag-disable 5268,7025
FFLAGS = -O0 -debug -g -traceback -fpp -check bounds -check all -warn unused -stand f03 -fPIC -gen-interfaces $(DIAGDISABLE) -module $(MODPATH)
#FFLAGS = -O1 -fpp -stand f03 -fPIC -gen-interfaces -module ../modules-dev
# GPROF = -g -p
#FFLAGS = -O3 -fpp -stand f03 -warn all -inline speed -inline-forceinline -no-prec-div -xHost -static -fPIC -gen-interfaces $(DIAGDISABLE) $(GPROF) -module $(MODPATH)
F90 = ifort
#DEFINES = -D_famcouple_=.false. -D_fammarried_=.false. -D_famkids_=.true.
# -------------------End-macro-Defs---------------------------

all:$(OBJECTS) $(XMLOBJECTS)
	ar rc $(OUTPATH)/fortax.a $(OBJECTS) $(XMLOBJECTS)

xmlparse.o:xmlparse.f90  
	$(F90) $(FFLAGS) -c xmlparse.f90 

read_xml_prims.o:read_xml_prims.f90 xmlparse.o \
	$(addprefix $(INCLUDESPATH)/xml/, $(XMLINCLUDES))
	$(F90) $(FFLAGS) -c read_xml_prims.f90 

write_xml_prims.o:write_xml_prims.f90 xmlparse.o 
	$(F90) $(FFLAGS) -c write_xml_prims.f90 

xmltaxben_t.o:xmltaxben_t.f90 read_xml_prims.o xmlparse.o 
	$(F90) $(FFLAGS) -O1 -c xmltaxben_t.f90 

xmlfortax_t.o:xmlfortax_t.f90 read_xml_prims.o write_xml_prims.o xmlparse.o 
	$(F90) $(FFLAGS) -O1 -c xmlfortax_t.f90 

xmlfamcompare_t.o:xmlfamcompare_t.f90 read_xml_prims.o write_xml_prims.o xmlparse.o 
	$(F90) $(FFLAGS) -O1 -c xmlfamcompare_t.f90

fortax_realtype.o:fortax_realtype.f90
	$(F90) $(FFLAGS) -c fortax_realtype.f90 

fortax_util.o:fortax_util.f90 fortax_realtype.o 
	$(F90) $(FFLAGS) -c fortax_util.f90 

fortax_compare.o:fortax_compare.f90 fortax_realtype.o fortax_type.o fortax_util.o fortax_write.o fortax_read.o xmlfamcompare_t.o fortax_calc.o
	$(F90) $(FFLAGS) -c fortax_compare.f90

fortax_type.o:fortax_type.f90 fortax_realtype.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/, sys_t.inc sys_init.inc fam_t.inc famad_t.inc nettu_t.inc netad_t.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c fortax_type.f90 

fortax_calc.o:fortax_calc.f90 fortax_realtype.o fortax_type.o
	$(F90) $(FFLAGS) $(DEFINES) -c fortax_calc.f90

fortax_extra.o:fortax_extra.f90 fortax_realtype.o fortax_type.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/, fortax_minamt.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c fortax_extra.f90

fortax_prices.o:fortax_prices.f90 fortax_realtype.o fortax_type.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/, fortax_uprate.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c fortax_prices.f90

fortax_read.o:fortax_read.f90 fortax_realtype.o xmlfortax_t.o fortax_util.o fortax_type.o \
	$(addprefix $(INCLUDESPATH)/,fortax_typeread.inc fortax_read.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c fortax_read.f90 

fortax_taxbenread.o:fortax_taxbenread.f90 fortax_realtype.o xmltaxben_t.o fortax_util.o fortax_type.o \
	$(addprefix $(INCLUDESPATH)/,fortax_typeread.inc fortax_read.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c fortax_taxbenread.f90 
	
fortax_write.o:fortax_write.f90 fortax_type.o xmlparse.o fortax_realtype.o fortax_util.o \
	$(addprefix $(INCLUDESPATH)/,fortax_write.inc fortax_print.inc) \
	$(addprefix $(INCLUDESPATH)/system/, $(SYSINCLUDES))
	$(F90) $(FFLAGS) -c fortax_write.f90

fortax_kinks.o:fortax_kinks.f90 fortax_type.o fortax_util.o fortax_realtype.o fortax_calc.o
	$(F90) $(FFLAGS) -c fortax_kinks.f90

clean:
	rm -f $(OBJECTS)  $(XMLOBJECTS) *.mod *.a
