##
# Project Title
#
# @file
# @version 0.1



# end

mtest:
	# -Kneato
	dune runtest -f --profile release # construit les .dot
	# dot -Kneato -Tpng img/diagram_test.dot > img/diagram_test.png

mbin:
	make mtest
	dune build -f --profile release
	_build/default/bin/main.exe
	./convert.sh

show:
	# eog img/$(IMG)
	eog img/final_output.png &

clear:
	rm -f img/diagram*.png
	rm -f img/*.dot
