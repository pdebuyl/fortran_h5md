
F_TYPE = {}
F_TYPE['d'] = 'double precision'
F_TYPE['i'] = 'integer'

H5_TYPE = {}
H5_TYPE['d'] = 'H5T_NATIVE_DOUBLE'
H5_TYPE['i'] = 'H5T_NATIVE_INTEGER'

DECL = {}
DECL[0] = ''
DECL[1] = '(:)'
DECL[2] = '(:,:)'
DECL[2] = '(:,:)'
DECL[3] = '(:,:,:)'


create_time_template = """
  subroutine h5md_element_create_time_{datatype}{datarank}(this, loc, name, data, mode, step, time, chunks)
    integer, parameter :: rank = {rank}
    class(h5md_element_t), intent(out) :: this
    integer(HID_T), intent(inout) :: loc
    character(len=*), intent(in) :: name
    {f_type}, intent(in) :: data{datashape}
    integer, intent(in) :: mode
    integer, intent(in), optional :: step
    double precision, intent(in), optional :: time
    integer, intent(in), optional :: chunks(rank)

    integer(HSIZE_T) :: dims(rank), maxdims(rank), chunk_dims(rank)
    integer(HID_T) :: s, plist

    call h5gcreate_f(loc, name, this% id, this% error)
    call h5md_check_valid(this% id, 'invalid id in create_time')

{dimsblock}
    
    if (present(chunks)) then
      chunk_dims = chunks
    else
      chunk_dims = chunking(dims, maxdims)
    end if

    call h5screate_simple_f(rank, dims, s, this% error, maxdims)

    call h5pcreate_f(H5P_DATASET_CREATE_F, plist, this% error)
    call h5pset_chunk_f(plist, rank, chunk_dims, this% error)
    call h5dcreate_f(this% id, 'value', {h5_type}, s, this% v, this% error, plist)
    call h5pclose_f(plist, this% error)
    call h5sclose_f(s, this% error)

    if (iand(mode, H5MD_LINEAR) == H5MD_LINEAR) then
      if (.not. present(step)) error stop 'no step in h5md_element_create_time'
      if (iand(mode, H5MD_STORE_TIME) == H5MD_STORE_TIME) then
          if (.not. present(time)) error stop 'no step in h5md_element_create_time'
      end if
    end if

    call h5md_populate_step_time(this, mode, step, time)

  end subroutine h5md_element_create_time_{datatype}{datarank}
"""

def get_dimsblock(rank):
    if rank>1:
        return \
"""
    dims(1:rank-1) = shape(data)
    dims(rank) = 0
    maxdims(1:rank-1) = shape(data)
    maxdims(rank) = H5S_UNLIMITED_F
"""
    else:
        return \
"""
    dims(rank) = 0
    maxdims(rank) = H5S_UNLIMITED_F
"""

def gen(datatype, datarank):
    assert datatype in F_TYPE
    
    print(create_time_template.format(datatype=datatype,
                                      datarank=datarank if datarank>0 else 's',
                                      rank=datarank+1,
                                      datashape=DECL[datarank],
                                      h5_type=H5_TYPE[datatype],
                                      f_type=F_TYPE[datatype],
                                      dimsblock=get_dimsblock(datarank+1),
                                  ))

import itertools

for dt, dr in itertools.product(F_TYPE.keys(), range(0, 4)):
    gen(dt, dr)

