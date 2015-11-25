F90 = gfortran
FFLAGS = -O 
GNUPLOT = gnuplot

%.o: %.f90
	$(F90) $(FFLAGS) -c $*.f90

objfiles = constants.o system.o thermostat.o integrator.o theforce.o input.o tmdcs.o 

all: tmdcs

tmdcs: $(objfiles)
	$(F90) $(FFLAGS) $(objfiles) -o tmdcs.x

clean:
	rm -f $(objfiles) core* tmdcs.x *.mod *.dat *.png *~
