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

(**
{!XML} provides functions to parse XML, for the specific use of XMPP.
This implementation is based on XML 1.0 5th edition.
It provides as well a representation of XML nodes.
 
Note: this parser has been made originally for my need of parsing XMPP threads,
with all the specificities of XML within XMPP usage.
+ In particular, it takes
into account the fact that XMPP is a restricted subset of XML.
See section 11.1 and 4.9.3.18 of RFC6120.
The particular following XML features are forbidden in XMPP:
    - comments
    - Processing Instructions
    - internal or external DTD subsets
    - internal or external entity references with the exception of the
    predefined entities
Any of these kind of features will raise a {!Restricted_XML} exception.
+ Also namespaces are used and are not an option.
+ Moreover only UTF-8 is accepted. Other encoding would raise
{!Unsupported_encoding}.
+ Finally a XML stream can be resetted without the current stream being closed.
@author Jehan Hysseo
@see <http://www.w3.org/TR/REC-xml/> The XML 1.0 specification by the W3C.
@see <http://www.w3.org/TR/xml-names/> Namespaces in XML 1.0
@see <http://tools.ietf.org/html/rfc6120#section-11.1> The XML restrictions specific to XMPP.
 *)

(*******************)
(** {1 EXCEPTIONS} *)
(*******************)

exception Unsupported_encoding
(** Exception raised either if the encoding value in the XML declaration is set
to some encoding other than UTF-8 or if the stream is improperly encoded. *)

exception Restricted_XML of string
(** Exception raised when the parser meets restricted XML (in the subset of XML we are considering).
 @param a tuple with the problematic XML section, and an analysis text.
 [raise (Restricted_XML human_readable_analysis);] *)

exception Invalid_XML of string
(** Exception raised when the parser meets invalid XML.
 @param a tuple with the problematic XML section, and an analysis text.
 [raise (Invalid_XML human_readable_analysis);]*)

exception Malformed_XML of string
(** Exception raised when the parser meets non well-formed XML.
 @param a tuple with the problematic XML section, and an analysis text.
 [raise (Malformed_XML human_readable_analysis);] *)

(***********************)

(** Events which can be raised while parsing XML documents. *)
type event =
	| Start_document
    (** At start of document OR reset. Be careful as at it may happen several
    times in the life of an XMPP stream. *)
	| End_document
	| Start_element of
        (Unicode.t * Unicode.t) * ((Unicode.t * Unicode.t) * Unicode.t) list
        (* (namespace, localpart), [((attribute ns, attribute), value) ...] *)
	| End_element of (Unicode.t * Unicode.t) (* (namespace, localpart) *)
	| CharData of Unicode.t

(** The parser is a very simple class which you can pass a handler function to react upon receiving events during XML document parsing.
 @raise Unsupported_encoding when an encoding issue is encountered (non-UTF-8).
 @raise Malformed_XML when a well-formedness issue is encountered.
 @raise Invalid_XML when the XML is well formed, but invalid (for instance when using a non-declared prefix).
 @raise Restricted_XML when a restricted feature of XML in this specific subset is encountered (hence maybe there would have no error raised in a full-featured XML parser. *)
class event_parser:
    object
        method change_event_handler: (event -> unit) -> unit
        (** Set the function called when any event is encountered.
        Defaults to [fun _ -> ()]. *)

        method parse: string -> unit
        (** The parser must be fed with the XMPP stream which has been
        extracted.
        @raise Unsupported_encoding when some non-proper UTF-8 sequence is
        encountered or when an encoding other than UTF-8 is announced in the XML
        declaration. *)

        method reset: unit
        (** The parser is resetted the same as creation,
        except for the event handler. Useful in particular when the XMPP stream
        is restarted. This function should not be used elsewhere than inside an
        event handler. Using it independently may result in broken behavior. *)

		method raw_string: string
		(* Modified by Chen 20/03/2012 *)
		(* retrieve the original data *)

        method level: int
    end;;

(*****************)
(* {1 XML NODES} *)
(*****************)

type qname = string * string;;
(** A qualified name is the pair (namespace name, local name). *)

(** An attribute is a couple (qualified name, value). *)
type attributes_list = (qname * string) list;;

(** A XML node. *)
type node =
	| Empty_node (** An empty node... which is like "no node" in fact. *)
	| Character_data of string (** Just some raw data between tags. *)
	| Element of qname * attributes_list * node list;; (** XML element:
                <elt atts>sons</elt>. {i node} is the son elements' list,
                hence it can be an empty list.*)

val namespace: node -> string;;
val name: node -> string;;
val qname: node -> qname;;

(** A function to get the XML form of a node.
 Note: as this is an "intelligent" function able to add the right default namespaces, there must be no "xmlns" attribute in the nodes' attribute. The function won't make the check, so it is the developper's responsability to ensure this if you don't want to have a malformed XML string with two default namespace declaration.
 @param pretty {i optional} {b boolean} parameter (default : false) displays with return lines and indentation.
 @param xmlns {i optional} {b Unicode.t} (default: []) is the current default namespace.
 @param declared_ns {i optional} (default: []) are the namespace previously declared: list of (namespace, prefix)
 @param new_ns {i optional} (default: []) is the table with the corresponding prefix for new namespaces: list of (namespace, prefix).
 @param node the {b node} you want in XML.
 @return the XML {b string} representing the node. *)
val to_string : ?pretty:bool -> ?xmlns:string
	-> ?declared_ns:(string * string) list
    -> ?new_ns:(string * string) list -> node -> string;;

val attribute : node -> qname -> string option;;
val attributes : node -> attributes_list;;

val add : node -> int -> node -> node;;
val reverse: node -> node;;
