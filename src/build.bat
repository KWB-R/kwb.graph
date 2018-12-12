CLS

del connected.dll

SET OPTIONS=-m32 -I"C:/Programme/R/R-3.0.1/include" -DNDEBUG -I"d:/RCompile/CRANpkg/extralibs64/local/include" -O3 -Wall -std=gnu99

gcc %OPTIONS% -c connected.c -o connected.o
gcc %OPTIONS% -c helperFunctions.c -o helperFunctions.o

R CMD SHLIB connected.c helperFunctions.c
