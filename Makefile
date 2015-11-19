F90 = gfortran
FFLAGS = -Wall -O0 -g -fcheck=bounds
GNUPLOT = gnuplot

%.o: %.f90
	$(F90) $(FFLAGS) -c $*.f90

objfiles = constants.o system.o integrator.o theforce.o tmdcs.o

all: tmdcs

tmdcs: $(objfiles)
	$(F90) $(FFLAGS) $(objfiles) -o tmdcs.x

clean:
	rm -f $(objfiles) core* main.x *.mod *.dat *.png *~
