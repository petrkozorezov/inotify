#include <ei.h>
#include "inotify_utils.h"
#include "inotify_erlang.h"
#include "erl_comm.h"

int write_tuple(const char *msg, const char *value)
{
  ei_x_buff result;
  if (ei_x_new_with_version(&result) 
      || ei_x_encode_tuple_header(&result, 2)
      || ei_x_encode_atom(&result, msg) 
      || ei_x_encode_atom(&result, value)
     ) 
  {
    ei_x_free(&result);
    return(-1);
  }
  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}

int write_tupleU(const char *msg, ulong value)
{
  ei_x_buff result;
  if (ei_x_new_with_version(&result) 
      || ei_x_encode_tuple_header(&result, 2)
      || ei_x_encode_atom(&result, msg) 
      || ei_x_encode_ulong(&result, value)
     ) 
  {
    ei_x_free(&result);
    return(-1);
  }
  write_cmd(&result);
  ei_x_free(&result);
  return(0);
}
