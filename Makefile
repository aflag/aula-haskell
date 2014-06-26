.PHONY: quicksort-coverage clean

clean:
	rm -f *.hi *.o *.tix quicksort ex7 wc2 wc1

wc1: wc1.hs
	ghc --make wc1

wc2: wc2.hs
	ghc --make wc1

quicksort-tests: quicksort.hs
	ghc -fhpc quicksort.hs --make
	./quicksort
	hpc report quicksort
