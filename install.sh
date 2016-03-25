#!/bin/bash
make clean
export GC_BIN=/apps2/netcdf/4.2-ics/bin
export GC_INCLUDE=/apps2/netcdf/4.2-ics/include
export GC_LIB=/apps2/netcdf/4.2-ics/lib
make -j12 all MET=geos5 GRID=2x25 COMPILER=ifort
