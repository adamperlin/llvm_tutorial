CC=clang++
EXE=main
ALL: 
	$(CC) -g -O3 main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o $(EXE)

clean:
	rm -rf $(EXE)


