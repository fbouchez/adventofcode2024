pmine: bridge-repair.py
	python3 bridge-repair.py < input-mine.txt

psmall: bridge-repair.py
	python3 bridge-repair.py < input-small.txt

pmined: bridge-repair.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: bridge-repair.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: bridge-repair.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: bridge-repair.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: bridge-repair
	./bridge-repair < input-mine.txt

optsmall: bridge-repair
	./bridge-repair < input-small.txt

bridge-repair: bridge-repair.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm bridge-repair

.PHONY: mine small opt optsmall clean clear
