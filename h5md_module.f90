module h5md_module
  use hdf5
  implicit none

  private

  public :: h5md_file_t, h5md_element_t
  public :: h5md_check_valid, h5md_check_exists

  integer, parameter :: H5MD_FIXED = 1
  integer, parameter :: H5MD_TIME = 2
  integer, parameter :: H5MD_LINEAR = 4

  type h5md_file_t
     integer(HID_T) :: id
     integer(HID_T) :: particles
     integer(HID_T) :: observables
     integer(HID_T) :: connectivity
     integer(HID_T) :: parameters
     integer :: error
   contains
     procedure :: create => h5md_file_create
     procedure :: open => h5md_file_open
     procedure :: close => h5md_file_close
  end type h5md_file_t

  type h5md_element_t
     integer :: type
     integer(HID_T) :: id
     integer(HID_T) :: v
     integer(HID_T) :: s
     integer(HID_T) :: t
     logical :: has_time
     integer :: error
     integer :: Nmax
   contains
     procedure :: open_time => h5md_element_open_time
  end type h5md_element_t

  interface h5md_write_attribute
     module procedure h5md_write_attribute_cs
     module procedure h5md_write_attribute_i1
  end interface h5md_write_attribute

contains

  subroutine h5md_file_create(this, filename, creator, creator_version, author, author_email)
    class(h5md_file_t), intent(out) :: this
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: creator, creator_version, author
    character(len=*), intent(in), optional :: author_email

    integer(HID_T) :: g1, g2, s, a

    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, this% id, this% error)

    call h5gcreate_f(this% id, 'h5md', g1, this% error)
    call h5md_write_attribute(g1, 'version', [1, 1])
    call h5gcreate_f(g1, 'author', g2, this% error)
    call h5md_write_attribute(g2, 'name', author)
    if (present(author_email)) then
       call h5md_write_attribute(g2, 'email', author_email)
    end if
    call h5gclose_f(g2, this% error)
    call h5gcreate_f(g1, 'creator', g2, this% error)
    call h5md_write_attribute(g2, 'name', creator)
    call h5md_write_attribute(g2, 'version', creator_version)
    call h5gclose_f(g2, this% error)

    call h5gcreate_f(this% id, 'particles', this% particles, this% error)

  end subroutine h5md_file_create

  subroutine h5md_file_open(this, filename, flags)
    class(h5md_file_t), intent(out) :: this
    character(len=*), intent(in) :: filename
    integer, intent(in) :: flags

    integer(HID_T) :: g1
    integer(HID_T) :: att, space
    logical :: flag, link_exists
    integer :: rank, version(2)
    integer(HSIZE_T) :: dims(1), maxdims(1)

    call h5fopen_f(filename, flags, this% id, this% error)
    call check_error(this% error, 'error opening the file '//filename)

    call h5oexists_by_name_f(this% id, 'h5md', link_exists, this% error)
    if (.not. link_exists) stop 'h5md group not found'
    call h5gopen_f(this% id, 'h5md', g1, this% error)
    call check_error(this% error, '/h5md group not found in H5MD file')

    call h5aopen_f(g1, 'version', att, this% error)
    call check_error(this% error, 'h5md version not found in H5MD file')
    call h5aget_space_f(att, space, this% error)
    call h5sis_simple_f(space, flag, this% error)
    if (.not. flag) then
       write(*,*) 'h5md version not a simple dataspace'
       stop
    end if
    call h5sget_simple_extent_ndims_f(space, rank, this% error)
    if (rank /= 1) then
       write(*,*) 'h5md version not of rank 1'
       stop
    end if
    call h5sget_simple_extent_dims_f(space, dims, maxdims, this% error)
    if (dims(1) /= 2) then
       write(*,*) 'h5md version not of length 2'
       stop
    end if
    call h5aread_f(att, H5T_NATIVE_INTEGER, version, dims, this% error)
    call h5aclose_f(att, this% error)
    call h5sclose_f(space, this% error)
    call h5gclose_f(g1, this% error)

    if (version(1) /= 1) then
       stop 'unsupported H5MD version'
    end if
    if ( (version(2) /= 0) .and. (version(2) /= 1) ) then
       stop 'unsupported H5MD version'
    end if

    call h5lexists_f(this% id, 'particles', link_exists, this% error)

    if (.not. link_exists) then
       write(*,*) 'particles group not found'
    else
       call h5gopen_f(this% id, 'particles', this% particles, this% error)
       call check_error(this% error, '/particles group opening error')
    end if

  end subroutine h5md_file_open

  subroutine h5md_file_close(this)
    class(h5md_file_t), intent(inout) :: this

    logical :: valid

    call h5iis_valid_f(this% particles, valid, this% error)
    if (valid) then
       call h5gclose_f(this% particles, this% error)
    end if
    call h5fclose_f(this% id, this% error)

  end subroutine h5md_file_close

  subroutine h5md_element_open_time(this, loc, name)
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name

    integer(HID_T) :: space
    integer(HSIZE_T), allocatable :: dims(:), maxdims(:)
    logical :: link_exists
    integer :: classtype, rank

    call h5lexists_f(loc, name, link_exists, this% error)
    call check_true(link_exists, 'element '//name//' not found')

    call h5gopen_f(loc, name, this% id, this% error)
    call h5dopen_f(this% id, 'value', this% v, this% error)
    call h5dopen_f(this% id, 'step', this% s, this% error)
    call h5dget_space_f(this% s, space, this% error)
    call h5sget_simple_extent_type_f(space, classtype, this% error)
    if (classtype == H5S_SCALAR_F) then
       this% type = H5MD_LINEAR
    else
       this% type = H5MD_TIME
    end if
    call h5sclose_f(space, this% error)

    call h5lexists_f(this% id, 'time', link_exists, this% error)
    if (link_exists) then
       call h5dopen_f(this% id, 'time', this% t, this% error)
       call h5dget_space_f(this% s, space, this% error)
       call h5sget_simple_extent_type_f(space, classtype, this% error)
       if (this% type == H5MD_LINEAR) then
          if (classtype /= H5S_SCALAR_F) then
             stop 'inconsistent step and time datasets'
          end if
       end if
       call h5sclose_f(space, this% error)
    end if

    call h5dget_space_f(this% v, space, this% error)
    call h5sget_simple_extent_type_f(space, classtype, this% error)
    if (classtype /= H5S_SIMPLE_F) then
       stop 'non simple dataspace'
    end if

    call h5sget_simple_extent_ndims_f(space, rank, this% error)
    allocate(dims(rank))
    allocate(maxdims(rank))
    call h5sget_simple_extent_dims_f(space, dims, maxdims, this% error)

    this% Nmax = dims(2)

    call h5sclose_f(space, this% error)

    deallocate(dims)
    deallocate(maxdims)

  end subroutine h5md_element_open_time

  subroutine check_error(e, msg)
    integer, intent(in) :: e
    character(len=*), intent(in) :: msg

    if (e /= 0) then
       write(*,*) msg
       stop
    end if

  end subroutine check_error

  subroutine check_true(flag, msg)
    logical, intent(in) :: flag
    character(len=*), intent(in) :: msg

    if (.not. flag) then
       write(*,*) msg
       stop
    end if

  end subroutine check_true

  subroutine h5md_check_valid(id, msg, strict)
    integer(HID_T), intent(in) :: id
    character(len=*), intent(in) :: msg
    logical, intent(in), optional :: strict

    logical :: valid
    integer :: error

    call h5iis_valid_f(id, valid, error)
    if (.not. valid) then
       write(*, *) msg
       if (present(strict)) then
          if (strict) stop
       else
          stop
       end if
    end if

  end subroutine h5md_check_valid

  subroutine h5md_check_exists(loc, name, msg)
    integer(HID_T), intent(in) :: loc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: msg

    logical :: exists
    integer :: error


    call h5lexists_f(loc, name, exists, error)

    if (.not. exists) then
       write(*, *) msg
       stop
    end if

  end subroutine h5md_check_exists

  subroutine h5md_write_attribute_cs(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    integer(HID_T) :: a, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    call h5screate_f(H5S_SCALAR_F, s, error)
    dims(1) = len(value)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, t, error)
    call h5tset_size_f(t, dims(1), error)
    call h5tset_strpad_f(t, H5T_STR_NULLTERM_F, error)
    call h5acreate_f(loc, name, t, s, a, error)
    call h5awrite_f(a, t, value, dims, error)
    call h5aclose_f(a, error)
    call h5tclose_f(t, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_cs

  subroutine h5md_write_attribute_i1(loc, name, value)
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    integer, intent(in) :: value(:)

    integer(HID_T) :: a, s, t
    integer :: error
    integer(HSIZE_T) :: dims(1)

    dims(1) = size(value, dim=1)
    call h5screate_simple_f(1, dims, s, error)
    t = H5T_NATIVE_INTEGER
    call h5acreate_f(loc, name, t, s, a, error)
    call h5awrite_f(a, t, value, dims, error)
    call h5aclose_f(a, error)
    call h5sclose_f(s, error)

  end subroutine h5md_write_attribute_i1

end module h5md_module
