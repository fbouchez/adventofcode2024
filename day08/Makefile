pmine: resonant.py
	python3 resonant.py < input-mine.txt

psmall: resonant.py
	python3 resonant.py < input-small.txt

pmined: resonant.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: resonant.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: resonant.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: resonant.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: resonant
	./resonant < input-mine.txt

optsmall: resonant
	./resonant < input-small.txt

resonant: resonant.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm resonant

.PHONY: mine small opt optsmall clean clear
