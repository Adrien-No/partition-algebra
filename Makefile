##
# Project Title
#
# @file
# @version 0.1



# end

mtest:
	dune runtest -f --profile release # construit les .dot

mbin:
	make mtest
	dune build -f --profile release
	_build/default/bin/main.exe
	./.convert.sh

show:
	eog img/permanent/output.png &

clear:
	# rm -f img/diagram*.png
	# rm -f img/*.dot

send:
	echo "Sending email ..."
	mailx -s "research" "sequences@oeis.org" < request-oeis.txt
