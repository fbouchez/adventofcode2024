pmine: ceres.py
	python3 ceres.py < input-mine.txt

psmall: ceres.py
	python3 ceres.py < input-small.txt

pmined: ceres.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: ceres.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: ceres.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: ceres.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: ceres
	./ceres < input-mine.txt

optsmall: ceres
	./ceres < input-small.txt

ceres: ceres.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm ceres

.PHONY: mine small opt optsmall clean clear
