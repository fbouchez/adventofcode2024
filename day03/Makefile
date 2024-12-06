pmine: mullit.py
	python3 mullit.py < input-mine.txt

psmall: mullit.py
	python3 mullit.py < input-small.txt

pmined: mullit.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: mullit.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: mullit.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: mullit.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: mullit
	./mullit < input-mine.txt

optsmall: mullit
	./mullit < input-small.txt

mullit: mullit.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm mullit

.PHONY: mine small opt optsmall clean clear
