all: go

go: go.adb
	rm -f *.gcda
	touch go.adb
	gnatmake -march=native -O3 -fprofile-generate -gnat12 -gnatp -gnatn go -bargs -shared -cargs -Wa,-q 
	./go
	touch go.adb
	gnatmake -march=native -O3 -fprofile-use -gnat12 -gnatp -gnatn go -bargs -shared -cargs -Wa,-q 

check:
	touch go.adb
	gnatmake -march=native -Wall -O3 -gnat12 -gnato -fstack-check go -bargs -shared -cargs -Wa,-q

debug:
	touch go.adb
	gnatmake -march=native -gnatG -O3 -ggdb -gnat12 -gnata -gnatv go -cargs -Wa,-q

clean:
	rm -f go *.o *.ali *.gcda
