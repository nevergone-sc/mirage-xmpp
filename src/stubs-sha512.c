/***********************************************************************/
/*                                                                     */
/*                      The Cryptokit library                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2004 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id: stubs-sha256.c 53 2010-08-30 10:53:00Z gildor-admin $ */

#include "sha512.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#define Context_val(v) ((struct sha512_ctx *) String_val(v))

CAMLprim value caml_sha512_init(value unit)
{
  value ctx = alloc_string(sizeof(struct sha512_ctx));
  sha512_init(Context_val(ctx));
  return ctx;
}

CAMLprim value caml_sha512_update(value ctx, value src, value ofs, value len)
{
  sha512_update(Context_val(ctx), &Byte_u(src, Long_val(ofs)), Long_val(len));
  return Val_unit;
}

CAMLprim value caml_sha512_final(value ctx)
{
  CAMLparam1(ctx);
  CAMLlocal1(res);

  sha512_digest digest;

  res = alloc_string(64);
  sha512_finalize(Context_val(ctx), &digest);
  sha512_to_bin(&digest, &Byte(res, 0));
  CAMLreturn(res);
}
