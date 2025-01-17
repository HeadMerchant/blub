# Compiler
CXX = g++

# Compiler flags
CXXFLAGS = -Wall -std=c++11

# Target executable
TARGET = $(OUTDIR)/comppiler

# Source files
SRCS = main.cpp greeting.cpp

OUTDIR = output
$(shell mkdir -p $(OUTDIR))

# Object files
OBJS = $(OUTDIR)/main.o $(OUTDIR)/greeting.o $(OUTDIR)/filereader.o

# Build target
$(TARGET): $(OBJS)
	# Link object files to create the executable
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# Compile main.cpp to main.o
$(OUTDIR)/main.o: main.cpp greeting.h filereader.h
	# Compile main.cpp into an object file
	$(CXX) $(CXXFLAGS) -c main.cpp -o $(OUTDIR)/main.o

# Compile greeting.cpp to greeting.o
$(OUTDIR)/greeting.o: greeting.cpp greeting.h
	# Compile greeting.cpp into an object file
	$(CXX) $(CXXFLAGS) -c greeting.cpp -o $(OUTDIR)/greeting.o

$(OUTDIR)/filereader.o: filereader.cpp filereader.h
	# Compile greeting.cpp into an object file
	$(CXX) $(CXXFLAGS) -c filereader.cpp -o $(OUTDIR)/filereader.o

# Clean up build files
clean:
	# Remove object files and the executable
	rm -rf $(OUTDIR)