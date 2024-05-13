##
# Project Title
#
# @file
# @version 0.1



# end

mtest:
#-Kneato
	dune test --profile release; dot -Kneato -Tpng img/diagram_test.dot > img/diagram_test.png

mbin:
	make mtest;
	dune build --profile release; _build/default/bin/main.exe; dot -Kneato -Tpng img/diagram_test.dot > img/diagram_test.png
show:
	#eog img/$(IMG)
	eog img/diagram_test.png &
