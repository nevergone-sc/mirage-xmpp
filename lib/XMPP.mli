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

(** {b stream} defines a generic XMPP stream without even the default stanzas
 * (messages, presence, iq) and namespaces (jabber:client/server) and their
 * logic. The RFC 3920 and 3921 let more or less opened the fact that there might be other
 * defined stanzas/namespaces, though honnestly I really don't know if there
 * exists any implementation using this fact.
 *
 * This class deals mainly with the generic aspect of a stream (XMPP Core), and
 * does not implement any advanced feature (roster, generic stanza meaning and
 * use, etc.).
 * @see <http://tools.ietf.org/html/rfc6120> RFC-6120 XMPP Core
 *)

exception Logical_error;;

(** The errors which can be sent to close a stream. *)
type stream_error = 
	| Bad_format of (string * string) option * XML.node
	| Bad_namespace_prefix of (string * string) option * XML.node
	| Conflict of (string * string) option * XML.node
	| Connection_timeout of (string * string) option * XML.node
	| Host_gone of (string * string) option * XML.node
	| Host_unknown of (string * string) option * XML.node
	| Improper_addressing of (string * string) option * XML.node
	| Internal_server_error of (string * string) option * XML.node
	| Invalid_from of (string * string) option * XML.node
	| Invalid_namespace of (string * string) option * XML.node
	| Invalid_xml of (string * string) option * XML.node
	| Not_authorized of (string * string) option * XML.node
	| Not_well_formed of (string * string) option * XML.node
	| Policy_violation of (string * string) option * XML.node
	| Remote_connection_failed of (string * string) option * XML.node
	| Reset of (string * string) option * XML.node
	| Resource_constraint of (string * string) option * XML.node
	| Restricted_xml of (string * string) option * XML.node
	| See_other_host of (string * string) option * XML.node
	| System_shutdown of (string * string) option * XML.node
	| Undefined_condition of (string * string) option * XML.node
	| Unsupported_encoding of (string * string) option * XML.node
	| Unsupported_feature of (string * string) option * XML.node
	| Unsupported_stanza_type of (string * string) option * XML.node
	| Unsupported_version of (string * string) option * XML.node
;;

(* class virtual generic: 
    ?lang: string -> ?resource: string -> recipient: string
    -> jid: string -> password: string 
    -> namespaces: string list -> stanzas: (string * string) list
    -> file_descr: Unix.file_descr -> unit ->
        object
            method on_start : (((string * string) * string) list -> unit) -> unit
            (** Event thrown when the receiving entity opens the [<stream>] tag.
             Note that this event can happen several times in the life of a single stream,
             considering some features require a stream restart (for instance TLS or
             SASL).
             *)
            method on_established: (unit -> unit) -> unit
            (** Event thrown when the stream is actually established (stanza can
             therefore be sent and received, typically true after stream
             binding). *)
            method on_close : (unit -> unit) -> unit
            (** Event thrown when the [</stream>] tag is received from the receiving entity.
            This event will happen only once. *)
            method on_stanza : (Stanza.t -> unit) -> unit
            (** Event thrown when a stanza is received. *)

            method run : unit
            (** Actually start the stream. *)
            method virtual raw_send : string -> unit
            (** Send some text over the stream. It is unadvised to use this
             function unless you know what you do. No check is run over the
             text (valid XML or XMPP check). Use [send] instead. *)
            method send : Stanza.t -> unit
            (** Send a stanza over the stream. *)
            method error : stream_error -> unit
            (** Send a stream error. *)
            method close : unit
            (** closes the given stream. Does not necessarily throw the on_close event. *)
        end;; *)

(** This is a generic XMPP client, over a TCP connection. *)
class client: ?lang:string -> ?resource:string ->
    ?server:string option -> ?port:int option ->
    string ->
    object
        method jid : string
        (** Return the bare jid before binding and the full jid after binding. *)
        method id : string option
        (** Return an id if it has been set. *)
        (*method encrypted : bool*)
        (* Return true if the connection is (TLS-)encrypted. TODO: will need to
        * make a more complete "safe level" feature. *)

        method run : SASL.credentials -> unit
        (** Start the client with the given credentials. *)
        method close : unit
        (** Closes the stream normally.
        @raise Logical_error if the stream is already closed. *)
        method error : stream_error -> unit
        (** Closes the stream with an error.
        @raise Logical_error if the stream is already closed. *)

        method send : Stanza.t -> unit
        method raw_send : string -> unit

        (*method on_test : ('a generic -> unit) -> unit*)
        method on_stanza : (client -> Stanza.t -> unit) -> unit
        method on_message : (client -> Stanza.t -> unit) -> unit
        method on_presence : (client -> Stanza.t -> unit) -> unit
        method on_iq : string -> (client -> Stanza.t -> unit) -> unit

        method on_close : (client -> unit) -> unit (* TODO: On received error, on sent
        error! *)
        (** [on_close f] set the handlers [f] which will be run once the XMPP stream is closed. The argument of [f] will be the client. *)
        method on_established : (client -> unit) -> unit
        (** [on_established f] set the handlers [f] which will be run once the XMPP stream is established (usually after binding). The argument of [f] will be the client. *)
        method on_start : (*((string * string) *
        string) list -> unit*) (client -> unit) -> unit
        (** [on_start f] set the handlers [f] which will be run once the XMPP stream opens the stream (it may happen several times, in particural when features needing a stream restart, like TLS, are negociated).  The argument of [f] will be the client. *)
    end
