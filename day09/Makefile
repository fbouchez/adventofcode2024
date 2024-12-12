pmine: disk-fragmenter.py
	python3 disk-fragmenter.py < input-mine.txt

psmall: disk-fragmenter.py
	python3 disk-fragmenter.py < input-small.txt

pmined: disk-fragmenter.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: disk-fragmenter.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: disk-fragmenter.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: disk-fragmenter.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: disk-fragmenter
	./disk-fragmenter < input-mine.txt

optsmall: disk-fragmenter
	./disk-fragmenter < input-small.txt

disk-fragmenter: disk-fragmenter.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm disk-fragmenter

.PHONY: mine small opt optsmall clean clear
