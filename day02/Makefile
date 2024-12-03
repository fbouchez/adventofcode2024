pmine: rednosed.py
	python3 rednosed.py < input-mine.txt

psmall: rednosed.py
	python3 rednosed.py < input-small.txt

pmined: rednosed.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: rednosed.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: rednosed.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: rednosed.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: rednosed
	./rednosed < input-mine.txt

optsmall: rednosed
	./rednosed < input-small.txt

rednosed: rednosed.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm rednosed

.PHONY: mine small opt optsmall clean clear
