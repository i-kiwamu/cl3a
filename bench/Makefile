CLTARGET = bench-cl
FTARGET = bench-fort

ROS = ros

BLASINC = /usr/local/opt/openblas/include
BLASLIB = /usr/local/opt/openblas/lib

FC = gfortran
FFLAGS = -O3 -fimplicit-none -fbounds-check -lopenblas -L$(BLASLIB) -I$(BLASINC)


.PHONY: all
all: $(CLTARGET) $(FTARGET)

.SUFFIXES:
.SUFFIXES: .ros

$(CLTARGET): $(CLTARGET).ros
	$(ROS) build $<

.SUFFIXES: .f03

$(FTARGET): $(FTARGET).f03
	$(FC) $< -o $@ $(FFLAGS)

.PHONY: clean
clean:
	rm -f *.mod $(CLTARGET) $(FTARGET)
