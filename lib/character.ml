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

(** {b characters} defines legal characters as specified in the XML 1.0 
 specification, 5th edition.
 All characters are represented here by their Unicode value (ISO/IEC 10646).
 @author Jehan Hysseo
 @see <http://www.w3.org/TR/REC-xml/> The XML specification by the W3C.
 *)

let is_alphabetChar = function
	| l when l >= 97 && l <= 122 -> true
	| l when l >= 65 && l <= 90 -> true
	| _ -> false;;

let is_latinChar = function
	| l when is_alphabetChar l -> true
	| l when l >= 48 && l <= 57 -> true
	| 46 (* '.' *) | 95 (* '_' *) | 45 (* '-' *) -> true
	| _ -> false;;

let is_NameStartChar = function
    | 58 (* ':' *) -> true
    | 95 (* '_' *) -> true
    | c when is_alphabetChar c -> true
	| c when c >= 0x00C0 && c <= 0x00D6 -> true
	| c when c >= 0x00D8 && c <= 0x00F6 -> true
	| c when c >= 0x00F8 && c <= 0x02FF -> true
	| c when c >= 0x0370 && c <= 0x037D -> true
	| c when c >= 0x037F && c <= 0x1FFF -> true
	| c when c >= 0x200C && c <= 0x200D -> true
	| c when c >= 0x2070 && c <= 0x218F -> true
	| c when c >= 0x2C00 && c <= 0x2FEF -> true
	| c when c >= 0x3001 && c <= 0xD7FF -> true
	| c when c >= 0xF900 && c <= 0xFDCF -> true
	| c when c >= 0xFDF0 && c <= 0xFFFD -> true
	| c when c >= 0x10000 && c <= 0xEFFFF -> true
    | _ -> false
;;

let is_NameChar c =
	is_NameStartChar c
	|| c = 45 (* '-' *)
    || c = 46 (* '.' *)
    || c > 47 && c < 58 (* 0-9 *)
    || c = 0xB7
    || c >= 0x0300 && c <= 0x036F
    || c >= 0x203F && c <= 0x2040
;;

(** Define if the given character is a blank character.
 @see <http://www.w3.org/TR/2006/REC-xml-20060816/#sec-common-syn>
*)
let is_WhiteSpace c =
    (c = 0x20) (* ' ' *)
	|| (c = 0x9) (* '\t' *)
    || (c = 0xD) (* '\013' *)
    || (c = 0xA) (* '\n' *)
;;

let is_Char c =
	(c = 0x9) || (c = 0xA) || c = 0xD ||
	(c >= 0x20 && c <= 0xD7FF) || (c >= 0xE000 && c <= 0xFFFD) || (c >= 0x10000 && c <= 0x10FFFF)
