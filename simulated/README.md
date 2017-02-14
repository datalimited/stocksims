#

## Scenarios

* Life history: SP, DE, LP
		* SP Small Pelagic: Linf=30cm, ages=1:8, fbar=2:8, steep=0.7
		* DE Demersal: Linf=100cm, ages=1:20, fbar=4:20, steep=0.6
		* LP Large Pelagic: Linf=250cm, ages=1:30, fbar=6:30, steep=0.85
* Initial depletion: ID10, ID40, ID60
* Effort/F dynamics, x value: ED0.1, ED0.3, ED0.6, FD
* Selectivity: SELFD, SELF, SELD, SELDF
* Length of time series: TS20, TS40, TS60
* Under-reporting catch %: UR0, UR10, UR25, UR50

##



## out list
* lh: FLPar, output of call to gislasim()
* code: simulation code, see table above
* stock: FLStock
* refpts: FLPar, FLBRP refpts of original object by lh()
* val: data.frame, values of variables in scenarios table



# ADD other effort dynamics

- One way trip: exponential to 0.80 Fcrash

- Up to 0.80 Fcrash, growing by 20%/year, then stay at 0.80Fcrash for 4 years, and then down to FMSY by 15%/year

- CONTRAST in time series

- Catch ERROR, 20% CV rlnorm

# GRID

- LH (3) SP, DE, LP
- AR (2) NR, AR
- ID (3) ID0, ID30, ID60
- ED (4) ED0, ED0.6, OW, RC
- TS (2) TS20, TS60
- UR (2) UR0, UR50

