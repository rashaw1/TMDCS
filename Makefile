F90 = gfortran
FFLAGS = -O 

%.o: %.f90
	$(F90) $(FFLAGS) -c $*.f90

objfiles = constants.o input.o system.o thermostat.o integrator.o theforce.o tmdcs.o

all: tmdcs

tmdcs: $(objfiles)
	$(F90) $(FFLAGS) $(objfiles) -o tmdcs.x

clean:
	rm -f $(objfiles) core* tmdcs.x *.mod
