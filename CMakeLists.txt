cmake_minimum_required(VERSION 2.6)

project(fortran_h5md Fortran)
if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.12)
  cmake_policy(SET CMP0074 NEW)
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

find_package(HDF5 COMPONENTS Fortran REQUIRED)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY}
${HDF5_Fortran_INCLUDE_DIR} ${HDF5_Fortran_INCLUDE_DIRS})

add_library(fortran_h5md h5md_module.f90)
target_link_libraries(fortran_h5md ${HDF5_Fortran_LIBRARIES} ${CMAKE_DL_LIBS})

add_subdirectory(examples EXCLUDE_FROM_ALL)
