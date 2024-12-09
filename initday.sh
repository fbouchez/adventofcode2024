#!/bin/bash

if [[ "x$1" == "x-h" ]] ; then
  echo "Usage $0"
  echo -e "\tPrepare current directory for today's AoC puzzle"
  echo -e "\tSelect example input first (triple click in example box)"
  echo -e "\tthen run this script and follow instructions."
  exit 0
fi


f="input-small.txt"
echo "Will use the following as small input (triple click in example box):"
xclip -o
echo -e "--EOF"
read -p "Press <enter> to validate or C-c to abort."
xclip -o > $f

f="input-mine.txt"
echo -e "--EOF\nNow put regular input in clipboard and press <enter>"
read
xclip -o
xclip -o > $f
echo -e "--EOF\nContents saved in $f"


f="AoC.hs"
if [[ -L $f ]]; then
  echo "Link already exists: $f"
else
  echo "Linking $f"
  ln -s ../Haskell/AoC.hs
  echo "Linking AoC.py"
  ln -s ../Python/AoC.py
fi

echo -n "Enter filename for today's puzzle: "
read execname
haskname=$execname.hs
pyname=$execname.py

if [[ -e $haskname ]]; then
  echo "$haskname already exists"
else
  echo "Copying template to ${haskname}"
  cp ../Haskell/template.hs ${haskname}
  cp ../Python/template.py ${pyname}
  chmod +x ${pyname}
fi

if [[ -e "Makefile" ]]; then
  echo "Makefile already exists"
else
  echo "Creating Makefile"
  cat <<EOF > Makefile
pmine: ${pyname}
	python3 ${pyname} < input-mine.txt

psmall: ${pyname}
	python3 ${pyname} < input-small.txt

pmined: ${pyname}
	loopwait 'python3 \$< -d < input-mine.txt' \$<

psmalld: ${pyname}
	loopwait 'python3 \$< -d < input-small.txt' \$<

mined: ${haskname}
	loopwait 'runhaskell \$< < input-mine.txt' \$<

smalld: ${haskname}
	loopwait 'runhaskell \$< < input-small.txt' \$<

optmine: ${execname}
	./${execname} < input-mine.txt

optsmall: ${execname}
	./${execname} < input-small.txt

${execname}: ${haskname}
	ghc -O2 \$< -o \$@

clean:
	haskell-clean-aux

clear: clean
	rm $execname

.PHONY: mine small opt optsmall clean clear
EOF
fi

function yes_or_no {
    while true; do
        read -p "$* [y/n]: " yn
        case $yn in
            [Yy]*) return 0  ;;  
            [Nn]*) echo "Aborted" ; return  1 ;;
        esac
    done
}


yes_or_no "Do you want to commit files in git?"
ret=$?

if [[ $ret -eq 1 ]]; then
  exit 0;
fi

echo "Initial adding of files in git."
git add AoC.hs AoC.py ${haskname} ${pyname} Makefile input-small.txt 
git ci -m "Initial commit for $execname"
