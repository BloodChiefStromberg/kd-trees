FLAGS = -Wall -g

default: fortran

fortran: main.f
	f95 $(FLAGS) $< -o $@
