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
*******************************************************************)

type t = int list;;

exception Invalid_unicode_character of int;;
exception Invalid_byte_sequence of int;;

module UTF8 =
struct

type c_s =
    {
        mutable index: int;
        (*mutable awaiting: int*) (* TODO: say how many character we wait and
process this directly when it is received or return directly *)
        mutable unprocessed: string list (* TODO: not a list, either
concatanated string OR maybe better: the list of int from end of previous call. *)
    }
;;

(* XXX: this is a little trick to avoid having conversion_state option as
optional type (in the public API). Indeed as soon as such a state is construct,
it cannot be None.  Hence None can only be a default value. *)
type conversion_state = c_s option;;

let new_conversion () =
    Some
        {
            index = 0;
            unprocessed = []
        }
;;

let octet_size ulist =
    let rec aux current_size = function
    | [] ->
        current_size
    | u::ul ->
        if (u < 0x80)
        then
            aux (succ current_size) ul
        else if u < 0x800
        then
            aux (current_size + 2) ul
        else if u < 0x10000
        then
            aux (current_size + 3) ul
        else if u < 0x110000
        then
            aux (current_size + 4) ul
        else
            raise (Invalid_unicode_character u)
    in aux 0 ulist
;;

let encode ulist = (* {{{ *)
    let encoded = String.create (octet_size ulist) in
    let rec aux idx = function
        | [] ->
            encoded
        | code::ul ->
            if code < 0x80
            then
                begin
                    String.unsafe_set encoded idx (Char.unsafe_chr code);
                    aux (succ idx) ul
                end
            else if code < 0x800
            then
                begin
                    let weak = (code land 0x3F) lor 0x80
                    in let strong = ((code lsr 6) land 0x3F) lor 0xC0
                    (* no Endianness problem in UTF-8 *)
                    in String.unsafe_set encoded idx (Char.unsafe_chr strong);
                    String.unsafe_set encoded (succ idx) (Char.unsafe_chr weak);
                    aux (idx + 2) ul
                end
            else if code < 0x10000
            then
                begin
                    let byte3 = (code land 0x3F) lor 0x80
                    in let byte2 = ((code lsr 6) land 0x3F) lor 0x80
                    in let byte1 = ((code lsr 12) land 0xF) lor 0xE0
                    in String.unsafe_set encoded idx (Char.unsafe_chr byte1);
                    String.unsafe_set encoded (succ idx) (Char.unsafe_chr byte2);
                    String.unsafe_set encoded (idx + 2) (Char.unsafe_chr byte3);
                    aux (idx + 3) ul
                end
            else
            (* No need for the last comparison. [octet_size] took care of
            dealing with wrong encoding. *)
                begin
                    let byte4 = (code land 0x3F) lor 0x80
                    in let byte3 = ((code lsr 6) land 0x3F) lor 0x80
                    in let byte2 = ((code lsr 12) land 0x3F) lor 0x80
                    in let byte1 = ((code lsr 18) land 0x7) lor 0xF0
                    in String.unsafe_set encoded idx (Char.unsafe_chr byte1);
                    String.unsafe_set encoded (succ idx) (Char.unsafe_chr byte2);
                    String.unsafe_set encoded (idx + 2) (Char.unsafe_chr byte3);
                    String.unsafe_set encoded (idx + 3) (Char.unsafe_chr byte4);
                    aux (idx + 4) ul
                end
    in aux 0 ulist
;; (* }}} *)

let rec decode_no_cs byte_string = (* {{{ *)
    let length = String.length byte_string in
    let rec aux ulist = function
        | idx when idx = length ->
            List.rev ulist
        | idx ->
            let code = Char.code byte_string.[idx] in
            if code < 0x80
            then
                aux (code::ulist) (succ idx)
            else if code < 0xC2
            then
                (* Either an intermediary byte in the beginning or an overlong
                 2-bytes sequence. *)
                raise (Invalid_byte_sequence idx)
            else if idx = length - 1
            then
                (* The next byte is the beginning of a multi-byte sequence.
                No need to continue if there is only one byte left. *)
                raise (Invalid_byte_sequence idx)
            else if code < 0xE0
            then
                begin
                    let c1 = Char.code byte_string.[succ idx] in
                    if c1 > 0x7F && c1 < 0xC0
                    then
                        aux (((c1 land 0x3F)
                            lor ((code land 0x1F) lsl 6))::ulist) (idx + 2)
                    else
                        raise (Invalid_byte_sequence idx)
                end
            else if idx > length - 2
            then
                raise (Invalid_byte_sequence idx)
            else if code < 0xF0
            then
                begin
                    let c1 = Char.code byte_string.[succ idx]
                    and c2 = Char.code byte_string.[idx + 2] in
                    (* At least 12 bits, or else the sequence is overlong. *)
                    if code == 0xE0 && (c1 land 0x20) == 0
                    then
                        raise (Invalid_byte_sequence idx)
                    else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0
                    then
                        aux (((c2 land 0x3F) + ((c1 land 0x3F) lsl 6) +
                            ((code land 0x0F) lsl 12))::ulist) (idx + 3)
                    else
                        raise (Invalid_byte_sequence idx)
                end
            else if idx > length - 3
            then
                raise (Invalid_byte_sequence idx)
            else if code < 0xF5
            then
                begin
                    let c1 = Char.code byte_string.[succ idx]
                    and c2 = Char.code byte_string.[idx + 2]
                    and c3 = Char.code byte_string.[idx + 3] in
                    (* At least 17 bits, or else the sequence is overlong. *)
                    if code == 0xF0 && c1 land 0x30 == 0
                    then
                        raise (Invalid_byte_sequence idx)
                    else if c1 > 0x7F && c1 < 0xC0
                        && c2 > 0x7F && c2 < 0xC0
                        && c3 > 0x7F && c3 < 0xC0
                    then
                        aux (((c3 land 0x3F) + ((c2 land 0x3F) lsl 6) +
                            ((c1 land 0x3F) lsl 12)
                            + ((code land 0x07) lsl 18))::ulist) (idx + 4)
                    else
                        raise (Invalid_byte_sequence idx)
                end
            else
                (* Any bigger sequence is invalid. *)
                raise (Invalid_byte_sequence idx)
    in aux [] 0
;; (* }}} *)

let decode_cs cs input = (* {{{ *)
    let rec aux ulist byte_string length = function
        | idx when idx = length ->
            begin match cs with
                | {unprocessed = []} ->
                    List.rev ulist
                | {unprocessed = s::l} ->
                    cs.unprocessed <- l;
                    aux ulist s (String.length s) 0
            end
        | idx ->
            let c0 = Char.code byte_string.[idx] in
            if c0 < 0x80
            then
                aux (c0::ulist) byte_string length (succ idx)
            else if c0 < 0xC2
            then
                (* Either an intermediary byte in the beginning or an overlong
                 2-bytes sequence. *)
                raise (Invalid_byte_sequence idx)
            else if c0 < 0xE0
            then
                if idx = length - 1
                then begin
                    match cs with
                    | {unprocessed = []} ->
                        (* c0 is the beginning of a multi-byte sequence.
                        No need to continue if it is the last octet. *)
                        cs.unprocessed <- [byte_string];
                        cs.index <- idx;
                        List.rev ulist
                    | {unprocessed = s::l} ->
                        (* The algorithm makes sure any pending string is non
                        empty. *)
                        cs.unprocessed <- l;
                        let c1 = Char.code s.[0] in
                        if c1 > 0x7F && c1 < 0xC0
                        then
                            aux (((c1 land 0x3F)
                                lor ((c0 land 0x1F) lsl 6))::ulist)
                                s (String.length s) 1
                        else
                            raise (Invalid_byte_sequence idx)
                end
                else begin
                    let c1 = Char.code byte_string.[succ idx] in
                    aux (((c1 land 0x3F)
                        lor ((c0 land 0x1F) lsl 6))::ulist)
                        byte_string length (idx + 2)
                end
            else if c0 < 0xF0
            then
                if idx > length - 3
                then begin
                    match cs with
                    | {unprocessed = []} ->
                        cs.unprocessed <- [byte_string];
                        cs.index <- idx;
                        List.rev ulist
                    | {unprocessed = s::l} ->
                        let new_length = String.length s in
                        if new_length + length - idx < 3
                        then begin
                            (* XXX: I could try some recursivity through l, but that
                            * gets pretty complicated for a very small gain. *)
                            cs.unprocessed <- (byte_string ^ s)::l;
                            cs.index <- idx;
                            List.rev ulist
                        end
                        else
                            let c1, c2, new_idx =
                                if idx = length - 1
                                then
                                    Char.code s.[0], Char.code s.[1], 2
                                else
                                    Char.code byte_string.[succ idx], Char.code s.[0], 1
                            in
                            (* At least 12 bits, or else the sequence is overlong. *)
                            if c0 = 0xE0 && (c1 land 0x20) = 0
                            then
                                raise (Invalid_byte_sequence idx)
                            else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0
                            then begin
                                cs.unprocessed <- l;
                                aux (((c2 land 0x3F) + ((c1 land 0x3F) lsl 6) +
                                    ((c0 land 0x0F) lsl 12))::ulist)
                                    s new_length new_idx
                            end
                            else
                                raise (Invalid_byte_sequence idx)
                end
                else begin
                    let c1 =  Char.code byte_string.[succ idx]
                    and c2 = Char.code byte_string.[idx + 2]
                    in
                    (* At least 12 bits, or else the sequence is overlong. *)
                    if c0 = 0xE0 && (c1 land 0x20) = 0
                    then
                        raise (Invalid_byte_sequence idx)
                    else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0
                    then
                        aux (((c2 land 0x3F) + ((c1 land 0x3F) lsl 6) +
                            ((c0 land 0x0F) lsl 12))::ulist)
                            byte_string length (idx + 3)
                    else
                        raise (Invalid_byte_sequence idx)
                end
            else if c0 < 0xF5
            then
                if idx > length - 4
                then
                    begin match cs with
                    | {unprocessed = []} ->
                        cs.unprocessed <- [byte_string];
                        cs.index <- idx;
                        List.rev ulist
                    | {unprocessed = s::l} ->
                        let new_length = String.length s in
                        if new_length + length - idx < 4
                        then begin
                            cs.unprocessed <- (byte_string ^ s)::l;
                            cs.index <- idx;
                            List.rev ulist
                        end
                        else
                            let c1, c2, c3, new_idx =
                                if idx = length - 1
                                then
                                    Char.code s.[0], Char.code s.[1],
                                    Char.code s.[2], 3
                                else if idx = length - 2
                                then
                                    Char.code byte_string.[succ idx], Char.code s.[0],
                                    Char.code s.[1], 2
                                else
                                    Char.code byte_string.[succ idx],
                                    Char.code byte_string.[length - 1],
                                    Char.code s.[0], 1
                            in
                            if c0 = 0xF0 && c1 land 0x30 = 0
                            then
                                raise (Invalid_byte_sequence idx)
                            else if c1 > 0x7F && c1 < 0xC0
                                && c2 > 0x7F && c2 < 0xC0
                                && c3 > 0x7F && c3 < 0xC0
                            then begin
                                cs.unprocessed <- l;
                                aux (((c3 land 0x3F) + ((c2 land 0x3F) lsl 6) +
                                    ((c1 land 0x3F) lsl 12)
                                    + ((c0 land 0x07) lsl 18))::ulist)
                                    s new_length new_idx
                            end
                            else
                                raise (Invalid_byte_sequence idx)
                    end
                else begin
                    let c1 = Char.code byte_string.[succ idx]
                    and c2 = Char.code byte_string.[idx + 2]
                    and c3 = Char.code byte_string.[idx + 3] in
                    (* At least 17 bits, or else the sequence is overlong. *)
                    if c0 = 0xF0 && c1 land 0x30 = 0
                    then
                        raise (Invalid_byte_sequence idx)
                    else if c1 > 0x7F && c1 < 0xC0
                        && c2 > 0x7F && c2 < 0xC0
                        && c3 > 0x7F && c3 < 0xC0
                    then
                        aux (((c3 land 0x3F) + ((c2 land 0x3F) lsl 6) +
                            ((c1 land 0x3F) lsl 12)
                            + ((c0 land 0x07) lsl 18))::ulist)
                            byte_string length (idx + 4)
                    else
                        raise (Invalid_byte_sequence idx)
                end
            else
                (* Any bigger sequence is invalid. *)
                raise (Invalid_byte_sequence idx)
    in match cs with
    | {unprocessed = []} ->
        aux [] input (String.length input) 0
    | {unprocessed = s::l; index = i} as state ->
        state.unprocessed <- l@[input];
        aux [] s (String.length s) i
;; (* }}} *)
                
let rec decode ?(cs:conversion_state = None) input = (* {{{ *)
    match cs with
    | None ->
        decode_no_cs input
    | Some state ->
        decode_cs state input
;; (* }}} *)

let validate_cs cs input = (* {{{ *)
    let rec aux s length idx =
        match idx with
        | n when n = length ->
            begin match cs with
            | {unprocessed = []} ->
                "" (* TODO *)
            | {unprocessed = s2::l} ->
                cs.unprocessed <- l;
                aux s2 (String.length s2) 0
            end
        | n ->
            let c0 = Char.code s.[n] in
            if c0 < 0x8F
            then
                aux s length (succ n)
            else if c0 < 0xC2
            then
                raise (Invalid_byte_sequence n)
                (* Either a intermediary byte in the beginning or an overlong
                 * 2-bytes sequence. *)
            else if c0 < 0xE0
            then begin
                if n = length - 1
                then
                    begin match cs with
                    | {unprocessed = []} ->
                        cs.unprocessed <- [s];
                        cs.index <- n;
                        "" (* TODO *)
                    | {unprocessed = s2::l} ->
                        let c1 = Char.code s2.[0] in
                        if c1 > 0x7F && c1 < 0xC0
                        then begin
                            cs.unprocessed <- l;
                            aux s2 (String.length s2) 1
                        end
                        else
                            raise (Invalid_byte_sequence n)
                    end
                else
                    let c1 = Char.code s.[succ n] in
                    if c1 > 0x7F && c1 < 0xC0
                    then
                        aux s length (n + 2)
                    else
                        raise (Invalid_byte_sequence 0)
            end
		    else if c0 < 0xF0
            then begin
                if n < length - 2
                then begin
                    let	c1 = int_of_char s.[n + 1]
                    and c2 = int_of_char s.[n + 2] in
                    (* At least 12 bits, or else the bytes sequence
                     * is overlong. *)
                    if c0 == 0xE0 && c1 land 0x20 == 0
                    then
                        raise (Invalid_byte_sequence n)
                    else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0
                    then
                        aux s length (n + 3)
                    else
                        raise (Invalid_byte_sequence n)
                end
                else begin
                    match cs with
                    | {unprocessed = []} ->
                        cs.unprocessed <- [s];
                        cs.index <- n;
                        "" (* TODO *)
                    | {unprocessed = s2::l} ->
                        let new_length = String.length s2 in
                        if new_length + length - idx < 3
                        then begin
                            cs.unprocessed <- (s ^ s2)::l;
                            cs.index <- n;
                            "" (* TODO *)
                        end
                        else
                            let c1, c2, new_idx =
                                if n = length - 1
                                then
                                    Char.code s2.[0], Char.code s2.[1], 2
                                else
                                    Char.code s.[succ idx], Char.code s2.[0], 1
                            in
                            if c0 == 0xE0 && c1 land 0x20 == 0
                            then
                                raise (Invalid_byte_sequence n)
                            else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0
                            then begin
                                cs.unprocessed <- l;
                                aux s2 new_length new_idx
                            end
                            else
                                raise (Invalid_byte_sequence n)
                end
            end
            else if c0 < 0xF5
            then begin
                if n < length - 3
                then begin
                    let c1 = Char.code s.[n + 1]
                    and c2 = Char.code s.[n + 2]
                    and c3 = Char.code s.[n + 3] in
                    (* At least 17 bits, or else the bytes sequence
                     * is overlong. *)
                    if c0 == 0xF0 && c1 land 0x30 == 0
                    then
                        raise (Invalid_byte_sequence n)
                    else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0 && c3 > 0x7F && c3 < 0xC0
                    then
                        aux s length (n + 4)
                    else
                        raise (Invalid_byte_sequence n)
                end
                else begin
                    match cs with
                    | {unprocessed = []} ->
                        cs.unprocessed <- [s];
                        cs.index <- n;
                        "" (* TODO *)
                    | {unprocessed = s2::l} ->
                        let new_length = String.length s2 in
                        if new_length + length - idx < 4
                        then begin
                            cs.unprocessed <- (s ^ s2)::l;
                            cs.index <- n;
                            "" (* TODO *)
                        end
                        else
                            let c1, c2, c3, new_idx =
                                if n = length - 1
                                then
                                    Char.code s2.[0], Char.code s2.[1],
                                    Char.code s2.[2], 3
                                else if n = length - 2
                                then
                                    Char.code s.[succ idx], Char.code s2.[0],
                                    Char.code s2.[1], 2
                                else
                                    Char.code s.[succ idx], Char.code s.[length - 1],
                                    Char.code s2.[0], 1
                            in
                            if c0 == 0xE0 && c1 land 0x20 == 0
                            then
                                raise (Invalid_byte_sequence n)
                            else if c1 > 0x7F && c1 < 0xC0 && c2 > 0x7F && c2 < 0xC0
                            then
                                begin
                                    cs.unprocessed <- l;
                                    aux s2 new_length new_idx
                                end
                            else
                                raise (Invalid_byte_sequence n)
                end
            end
            else
                (* Any other sequence is invalid. *)
                raise (Invalid_byte_sequence n)
    in match cs with
    | {unprocessed = []} ->
        aux input (String.length input) 0
    | {unprocessed = s::l; index = i} as state ->
        state.unprocessed <- l@[input];
        aux s (String.length s) i
;; (* }}} *)

let validate_no_cs byte_string = (* {{{ *)
    let length = String.length byte_string in
    let rec aux = function
        | n when n == length ->
            byte_string
        | n when int_of_char byte_string.[n] < 0x8F ->
            aux (n + 1)
        | n when int_of_char byte_string.[n] < 0xC2 ->
            raise (Invalid_byte_sequence 0)
			(* Either a intermediary byte in the beginning or an overlong
			 * 2-bytes sequence. *)
        | n when length - n == 1 -> 
            raise (Invalid_byte_sequence 0)
			(* The next byte is the beginning of a multi-byte sequence. No need to continue if
            there is only one byte left. *)
        | n when int_of_char byte_string.[n] < 0xE0 ->
			let s1 = int_of_char byte_string.[n + 1] in
			if (s1 > 0x7F && s1 < 0xC0)
			then
				aux (n + 2)
			else
				raise (Invalid_byte_sequence 0)
		| n when length - n < 3 -> 
			raise (Invalid_byte_sequence 0)
		| n when int_of_char byte_string.[n] < 0xF0 ->
			let s0 = int_of_char byte_string.[n]
			and	s1 = int_of_char byte_string.[n + 1]
			and s2 = int_of_char byte_string.[n + 2] in
			(* At least 12 bits, or else the bytes sequence
			 * is overlong. *)
			if s0 == 0xE0 && (s1 land 0x20) == 0
			then
				raise (Invalid_byte_sequence 0)
			else if s1 > 0x7F && s1 < 0xC0 && s2 > 0x7F && s2 < 0xC0
			then
				aux (n + 3)
			else
				raise (Invalid_byte_sequence 0)
		| n when length - n < 4 -> 
			raise (Invalid_byte_sequence 0)
		| n when int_of_char byte_string.[n] < 0xF5 ->
			let s0 = int_of_char byte_string.[n]
			and s1 = int_of_char byte_string.[n + 1]
			and s2 = int_of_char byte_string.[n + 2]
			and s3 = int_of_char byte_string.[n + 3] in
			(* At least 17 bits, or else the bytes sequence
			 * is overlong. *)
			if s0 == 0xF0 && s1 land 0x30 == 0
			then
				raise (Invalid_byte_sequence 0)
			else if s1 > 0x7F && s1 < 0xC0 && s2 > 0x7F && s2 < 0xC0 && s3 > 0x7F && s3 < 0xC0
			then
				aux (n + 4)                                
			else
				raise (Invalid_byte_sequence 0)
		| n -> (* Any other sequence is invalid. *)
			raise (Invalid_byte_sequence 0)
	in aux 0;; (* }}} *)

let rec validate ?(cs:conversion_state = None) input = (* {{{ *)
    match cs with
    | None ->
        validate_no_cs input
    | Some state ->
        validate_cs state input
;; (* }}} *)

let finalize cs =
    match cs with
    | None ->
        (* This is supposed to be impossible to happen as it is not possible to
        * construct a None conversion state. *)
        failwith "Unicode.UTF8.finalize: internal error"
    | Some {unprocessed = []} ->
        true
    | _ ->
        false
;;

end;;
