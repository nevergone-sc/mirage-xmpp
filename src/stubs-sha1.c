/***********************************************************************/
/*                                                                     */
/*                      The Cryptokit library                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id: stubs-sha1.c 53 2010-08-30 10:53:00Z gildor-admin $ */

#include "sha1.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#define Context_val(v) ((struct sha1_ctx *) String_val(v))

CAMLprim value caml_sha1_init(value unit)
{
  value ctx = alloc_string(sizeof(struct sha1_ctx));
  sha1_init(Context_val(ctx));
  return ctx;
}

CAMLprim value caml_sha1_update(value ctx, value src, value ofs, value len)
{
  sha1_update(Context_val(ctx), &Byte_u(src, Long_val(ofs)), Long_val(len));
  return Val_unit;
}

CAMLprim value caml_sha1_final(value ctx)
{
  CAMLparam1(ctx);
  CAMLlocal1(res);

  res = alloc_string(20);
  sha1_finalize(Context_val(ctx), (sha1_digest *) &Byte_u(res, 0));
  CAMLreturn(res);
}
