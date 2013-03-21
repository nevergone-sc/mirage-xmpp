(********************************************************************
* Copyright 2008;2011 Jehan Hysseo  (email : jehan at zemarmot.net) {{{
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 3 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA }}}
 *  ******************************************************************)

(** {!Unicode} defines an internal representation of a Unicode text.
{!Unicode.UTF8} contains functions manipulating/transforming UTF-8 strings
(represented by "normal strings" that are supposed to be encoded into UTF-8)
back and forth into this Unicode/ISO/CEI 10646 representation.
@author Jehan Hysseo
@see <http://en.wikipedia.org/wiki/UTF-8> for comprehensive reading about UTF-8.
@see <http://en.wikipedia.org/wiki/Unicode> for comprehensive reading about Unicode.
@see <http://www.unicode.org/versions/Unicode5.1.0/> for the official Unicode standard, version 5.1.0.
*)

(** [t] is a type representing Unicode strings, which means simply that each
character is represented as its corresponding integer representation in the
Unicode standard, without any encoding.
This is close to the concept of wide strings we can find in some platform
(except that wide strings may usually contain anything).
This is a practical internal representation when some validation needs to be
done on characters for specific uses (for instance, not all Unicode characters
can be used in any parts of a XML document, a URI, a JID, etc.). *)
type t = int list;;

(** [Invalid_unicode_character] is raised when an invalid character is met,
for instance a character from a reserved plane, or outside the range of assigned
planes.
The parameter is the invalid Unicode code. *)
exception Invalid_unicode_character of int;;

(** [Invalid_byte_sequence] is raised when an invalid byte sequence is met in
an encoded text, for instance an overlong encoding (it is mandatory in the UTF-8
encoding to encode in the shorter byte sequence possible), or simply a badly
encoded byte.
The parameter is the index where the invalid sequence begins. *)
exception Invalid_byte_sequence of int;;

module UTF8:
sig
    type conversion_state;;
    (** A type describing the current state of a conversion. This is to be used
    for instance when streaming UTF-8 data is expected or when the input data is
    cut into separate chunks. *)

    val new_conversion: unit -> conversion_state;;
    (** Create a new conversion state. *)

    (** {2 UTF-8 ENCODING} *)

    val encode: t -> string;;
    (** [encode u] transforms a Unicode string [u] into a UTF-8 byte string ([string]).
    @return the equivalent UTF-8 encoded byte string.
    @raise Invalid_unicode_character when a forbidden Unicode representation is met.
    *)

    (** {2 UTF-8 DECODING} *)

    val decode: ?cs : conversion_state -> string -> t;;
    (** [decode s] transforms a UTF-8 byte string [s] into a Unicode string.
    @param cs is an optional conversion state. If none is passed, then the
    parameter string is considered a one-time string, that is, any
    unfinished sequence is an error. Otherwise the conversion continues from the
    previous state and does not consider unfinished sequences to be an error.
    @return a unicode string.
    @raise Invalid_byte_sequence when the UTF-8 byte sequence is forbidden,
    or [if cs = None] when a character is not finished.
     *)

    (** {2 UTF-8 VALIDATION} *)

    val validate: ?cs : conversion_state -> string -> string;;
    (** [validate] validates a string [s] as being a full UTF-8 sequence.
    At the opposite to [validate_utf8_string_stream], it won't validate a string
    which ends with an incomplete sequence.
    @param cs is an optional conversion state. If none is passed, then the
    parameter string is considered a one-time string, that is, any
    unfinished sequence is an error. Otherwise the conversion continues from the
    previous state and does not consider unfinished sequences to be an error.
    @return the valid UTF-8 string (which will be the same string as the one
    entered as parameter when there is no conversatio state). It is useful when
    you need to feed incoming string in another function which will only accept
    fully valid strings (hence buffering unfinished character sequences into the
    conversion state.
    @raise Invalid_byte_sequence if an invalid UTF-8 sequence,
    or [if cs = None] when an incomplete sequence (at the end of the string),
    is encountered.
    *)

    (** {2 FINALIZATION} *)

    val finalize: conversion_state -> bool;;
    (** [finalize cs] is to be used on the conversation state after the last run of
    either [decode ~cs s] or [validate ~cs s]. Its goal is to ensure that there
    is no remaining unfinished byte at the end of an UTF-8 stream.
    It is obviously of no use after running [validate] or [decode] without
    encoding state.
    @return true if the whole stream was valid; false if it does not end.
    *)
end
