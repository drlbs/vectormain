# Makefile for Vector Operations Library
#
# Usage:
#   make          - Build the program
#   make clean    - Remove compiled files
#   make run      - Build and run the program

# Compiler and flags
FC = gfortran
FFLAGS = -Wall -Wextra -g -fcheck=all

# Object files (order matters: modules must come first!)
OBJS = vector_module.o external_func.o vector_main.o

# Target executable
TARGET = vector_demo

# Default target: build the program
all: $(TARGET)

# Link all object files into executable
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Compile the module (creates vector_ops.mod)
vector_module.o: vector_module.f90
	$(FC) $(FFLAGS) -c $<

# Compile the external function
external_func.o: external_func.f90
	$(FC) $(FFLAGS) -c $<

# Compile main program (depends on module being compiled first)
vector_main.o: vector_main.f90 vector_module.o
	$(FC) $(FFLAGS) -c $<

# Clean up compiled files
clean:
	rm -f *.o *.mod $(TARGET)

# Build and run
run: $(TARGET)
	./$(TARGET)

# Phony targets (not actual files)
.PHONY: all clean run