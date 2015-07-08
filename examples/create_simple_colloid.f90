program create
  use hdf5
  use h5md_module
  implicit none


  type(h5md_file_t) :: f
  integer :: error

  call h5open_f(error)

  call f% create('simple_colloid.h5', H5F_ACC_TRUNC_F)

  call f% close()

  call h5close_f(error)

end program create
