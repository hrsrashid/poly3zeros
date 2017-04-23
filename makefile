export FSFLAG = -I
export CXX    = c++
export FC 		= gfortran
export CC     = cc

default: main.o poly3zerosModule.o
	gfortran -o main main.o poly3zerosModule.o
	./main

%.o: %.f90
	gfortran -I. -Wall -o $@ -c $^

test:
	funit poly3zerosModule

test-watch:
	echo "poly3zerosModule.f90\npoly3zerosModule.fun" | entr make test

clean:
	rm -f *.o
	rm -f main
