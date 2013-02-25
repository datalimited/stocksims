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
