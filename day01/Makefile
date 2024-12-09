pmine: location.py
	python3 location.py < input-mine.txt

psmall: location.py
	python3 location.py < input-small.txt

pmined: location.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: location.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: location.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: location.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: location
	./location < input-mine.txt

optsmall: location
	./location < input-small.txt

location: location.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm location

.PHONY: mine small opt optsmall clean clear
