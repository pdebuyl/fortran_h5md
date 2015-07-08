program create
  use hdf5
  use h5md_module
  implicit none


  type(h5md_file_t) :: f
  integer :: error

  call h5open_f(error)

  call f% create('simple_colloid.h5', 'fortran_h5md:create_simple_colloid', '0.0 dev', 'Pierre de Buyl')

  call f% close()

  call h5close_f(error)

end program create
