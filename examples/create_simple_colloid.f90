program create
  use hdf5
  use h5md_module
  implicit none

  integer, parameter :: N = 4
  double precision :: pos(3, N)
  double precision :: edges(3)
  double precision, allocatable :: new_pos(:, :)

  type(h5md_file_t) :: f
  type(h5md_element_t) :: e
  integer(HID_T) :: colloids, box_group
  integer :: error

  call h5open_f(error)

  call f% create('simple_colloid.h5', 'fortran_h5md:create_simple_colloid', '0.0 dev', 'Pierre de Buyl')
  call h5gcreate_f(f% particles, 'colloids', colloids, error)

  call h5gcreate_f(colloids, 'box', box_group, error)

  call h5md_write_attribute(box_group, 'dimension', 3)
  call h5md_write_attribute(box_group, 'boundary', ['none', 'none', 'none'])
  edges = [8, 8, 8]
  call e% create_fixed(box_group, 'edges', edges)

  pos(:, 1) = [1, 1, 1]
  pos(:, 2) = [3, 1, 1]
  pos(:, 3) = [1, 3, 1]
  pos(:, 4) = [3, 3, 1]

  call e% create_fixed(colloids, 'position', pos)

  call e% read_fixed(colloids, 'position', new_pos)

  write(*, *) new_pos

  call h5gclose_f(colloids, error)

  call f% close()

  call h5close_f(error)

end program create
