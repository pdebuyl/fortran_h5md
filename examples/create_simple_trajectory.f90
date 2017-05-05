program create
  use hdf5
  use h5md_module
  implicit none

  integer, parameter :: N = 4096
  integer, parameter :: interval = 10
  double precision :: pos(3, N), v(3, N)
  double precision :: edges(3)

  type(h5md_file_t) :: f
  type(h5md_element_t) :: pos_e, vel_e, e, temp_e
  integer(HID_T) :: colloids, box_group
  integer :: error
  integer :: h5md_mode

  integer :: t
  double precision :: temperature

  call h5open_f(error)

  call f% create('simple_trajectory.h5', 'fortran_h5md:create_simple_trajectory', '0.0 dev', 'Pierre de Buyl')
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
  v = 0

  h5md_mode = ior(H5MD_FIXED,H5MD_STORE_TIME)
  call pos_e% create_time(colloids, 'position', pos, h5md_mode, step=interval, time=0.1d0*interval)
  call vel_e% create_time(colloids, 'velocity', v, h5md_mode, step=interval, time=0.1d0*interval)
  call h5gclose_f(colloids, error)

  call temp_e% create_time(f% observables, 'temperature', temperature, h5md_mode, time=0.1d0)

  call pos_e% append(pos)
  call vel_e% append(v)
  temperature = sum(v**2)/(3*N)
  call temp_e% append(temperature, 0)

  do t = 1, 100

     call random_number(v)
     v = (v - 0.5d0)

     pos = pos + v/10

     temperature = sum(v**2)/(3*N)
     call temp_e% append(temperature, t)

     if (modulo(t, interval) == 0) then
        call pos_e% append(pos)
        call vel_e% append(v)
     end if

  end do

  call pos_e% close()
  call vel_e% close()
  call temp_e% close()

  call f% close()

  call h5close_f(error)

end program create
