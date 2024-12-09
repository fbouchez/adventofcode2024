pmine: guard.py
	python3 guard.py < input-mine.txt

psmall: guard.py
	python3 guard.py < input-small.txt

pmined: guard.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: guard.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: guard.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: guard.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: guard
	./guard < input-mine.txt

optsmall: guard
	./guard < input-small.txt

guard: guard.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm guard

.PHONY: mine small opt optsmall clean clear
