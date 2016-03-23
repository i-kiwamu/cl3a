CLTARGET = bench-cl3a
FTARGET = bench-blas
CPPTARGET = bench-eigen

ROS = ros

FC = gfortran
BLASINC = /usr/local/opt/openblas/include
BLASLIB = /usr/local/opt/openblas/lib
FFLAGS = -O3 -fimplicit-none -fbounds-check -lopenblas -L$(BLASLIB) -I$(BLASINC)

CPP = clang++
EIGENINC = /usr/local/Cellar/eigen/3.2.8/include/eigen3/
CPPFLAGS = -O3 -I$(EIGENINC)


.PHONY: all
all: $(CLTARGET) $(FTARGET) $(CPPTARGET)

.SUFFIXES:
.SUFFIXES: .ros

$(CLTARGET): $(CLTARGET).ros
	$(ROS) build $<

.SUFFIXES: .f03

$(FTARGET): $(FTARGET).f03
	$(FC) $< -o $@ $(FFLAGS)

.SUFFIXES: .cpp

$(CPPTARGET): $(CPPTARGET).cpp
	$(CPP) $< -o $@ $(CPPFLAGS)

.PHONY: clean
clean:
	rm -f *.mod $(CLTARGET) $(FTARGET) $(CPPTARGET)
