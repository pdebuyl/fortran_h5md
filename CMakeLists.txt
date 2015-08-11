cmake_minimum_required(VERSION 2.6)

project(fortran_h5md Fortran)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

find_package(HDF5 COMPONENTS Fortran)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_Fortran_INCLUDE_DIR})

add_library(fortran_h5md h5md_module.f90)
target_link_libraries(fortran_h5md ${HDF5_Fortran_LIBRARIES} ${CMAKE_DL_LIBS})

add_subdirectory(examples EXCLUDE_FROM_ALL)
