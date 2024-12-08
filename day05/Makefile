pmine: print-queue.py
	python3 print-queue.py < input-mine.txt

psmall: print-queue.py
	python3 print-queue.py < input-small.txt

pmined: print-queue.py
	loopwait 'python3 $< -d < input-mine.txt' $<

psmalld: print-queue.py
	loopwait 'python3 $< -d < input-small.txt' $<

mined: print-queue.hs
	loopwait 'runhaskell $< < input-mine.txt' $<

smalld: print-queue.hs
	loopwait 'runhaskell $< < input-small.txt' $<

optmine: print-queue
	./print-queue < input-mine.txt

optsmall: print-queue
	./print-queue < input-small.txt

print-queue: print-queue.hs
	ghc -O2 $< -o $@

clean:
	haskell-clean-aux

clear: clean
	rm print-queue

.PHONY: mine small opt optsmall clean clear
