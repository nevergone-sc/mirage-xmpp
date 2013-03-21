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

(*exception IO_error;;*)
(*exception Connection_error;;*)

exception Logical_error;;

(**** STREAM ERRORS {{{ *****)

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

let stream_error_to_string error =
	let text_to_string = function
		| None -> ""
		| Some (lang, message) ->
            "<text xmlns='urn:ietf:params:xml:ns:xmpp-streams' xml:lang='" ^
            lang ^ "'>" ^ message ^ "</text>"
	in let error_condition, text, application_error =
		match error with
		| Bad_format (t, ae) -> "bad-format", t, ae
		| Bad_namespace_prefix (t, ae) -> "bad-namespace-prefix", t, ae
		| Invalid_xml (t, ae) -> "invalid-xml", t, ae
		| Restricted_xml (t, ae) -> "restricted-xml", t, ae
		| Unsupported_encoding (t, ae) -> "unsupported-xml", t, ae
		| Not_well_formed (t, ae) -> "not-well-formed", t, ae
		| Conflict (t, ae) -> "conflict", t, ae
		| Connection_timeout (t, ae) -> "connection-timeout", t, ae
		| Host_gone (t, ae) -> "host-gone", t, ae
		| Host_unknown (t, ae) -> "host-unkown", t, ae
		| Improper_addressing (t, ae) -> "improper-addressing", t, ae
		| Internal_server_error (t, ae) -> "internal-server-error", t, ae
		| Invalid_from (t, ae) -> "invalid-from", t, ae 
		| Invalid_namespace (t, ae) -> "invalid-namespace", t, ae
		| Not_authorized (t, ae) -> "not-authorized", t, ae
		| Policy_violation (t, ae) -> "policy-violation", t, ae
		| Remote_connection_failed (t, ae) -> "remote-connection-failed", t, ae
		| Resource_constraint (t, ae) -> "resource-constraint", t, ae
		| See_other_host (t, ae) -> "see-other-host", t, ae
		| System_shutdown (t, ae) -> "system-shutdown", t, ae
		| Undefined_condition (t, ae) -> "undefined-condition", t, ae
		| Unsupported_stanza_type (t, ae) -> "unsupported-stanza-type", t, ae
		| Unsupported_version (t, ae) -> "unsupported-version", t, ae
		| Reset (t, ae) -> "reset", t, ae
		| Unsupported_feature (t, ae) -> "unsupported-feature", t, ae
	in "<stream:error><" ^ error_condition ^ " xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>"
	^ (text_to_string text) ^ (XML.to_string application_error)
	^ "</stream:error>"
;;

(* STREAM ERRORS }}} *)

(* Declaration of namespaces (ns), local parts (lp) and qualified names (qn)
* that will be used in the parser. *)

let xml_ns = Unicode.UTF8.decode "http://www.w3.org/XML/1998/namespace";;
let xml_lang = (xml_ns, [108; 97; 110; 103]);;

let stream_ns = Unicode.UTF8.decode "http://etherx.jabber.org/streams";;
let stream_lp = Unicode.UTF8.decode "stream";;
let stream_features_qn = (stream_ns, [102; 101; 97; 116; 117; 114; 101; 115]);;
let error_lp = [101; 114; 114; 111; 114];;
let stream_error_qn = (stream_ns, error_lp);;

let required_lp = Unicode.UTF8.decode "required";;

let sasl_ns = Unicode.UTF8.decode "urn:ietf:params:xml:ns:xmpp-sasl";;
let sasl_failure_qn = (sasl_ns, Unicode.UTF8.decode "failure");;
let sasl_success_qn = (sasl_ns, Unicode.UTF8.decode "success");;
let sasl_feature = ("urn:ietf:params:xml:ns:xmpp-sasl", "mechanisms");;

let tls_ns = Unicode.UTF8.decode "urn:ietf:params:xml:ns:xmpp-tls";;
let tls_proceed = (tls_ns, Unicode.UTF8.decode "proceed")
let tls_failure = (tls_ns, Unicode.UTF8.decode "failure");;
let tls_feature = ("urn:ietf:params:xml:ns:xmpp-tls", "starttls");;

let iq_qn = (Unicode.UTF8.decode "jabber:client", Unicode.UTF8.decode "iq");;

let bind_ns = Unicode.UTF8.decode "urn:ietf:params:xml:ns:xmpp-bind";;
let bind_qn = (bind_ns, Unicode.UTF8.decode "bind");;
let jid_qn = (bind_ns, Unicode.UTF8.decode "jid");;
let bind_feature = ("urn:ietf:params:xml:ns:xmpp-bind", "bind");;

type negotiation_state =
	| Success (** The negotiation has succeded. Hence a new features tag is expected. *)
	| Failure (** A negotiation has been taken, but did not succeed. It does not mean that the stream ends. Maybe an alternative feature is available. *)
	| Success_restart (** The negotiated succeded and requires a restart. *)
	| Negotiating of int;; (** Negotiation is in progress with state represented by a number. *)
(* XXX: as the negotiation state depends on each specific feature protocol, I cannot make a semantic while generic state system for any possible kind of feature. Hence I simply make it an integer, which each feature handler has to make a meaning from. *)

let attr_ustring_to_utf8 atts =
	let aux ((ns, name), value) =
		((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), Unicode.UTF8.encode value) in
	List.map aux atts;;

type stream_state =
    | Opened_connection
    | Started
    | Established
    | Closed;;

(* A generic client implementation. The transport protocol is unspecified. *)
class ['a] generic (*{{{*)
    ?(lang: string = "") ?(resource: string = "")
    ~(namespaces: string list) ~(stanzas: (string * string) list)
    ~(connector: 'a)
    (jid: string) =
    let regexp = Str.regexp "^\\([^@]+\\)@\\([^@]+\\)$" in
    let localpart, serverpart =
        match Str.string_match regexp jid 0 with
        | false -> failwith "wrong jid" (* TODO *)
        | true -> Str.matched_group 1 jid, Str.matched_group 2 jid
    in
    object (this)
        val run_lock = Mutex.create ()
        val mutable state = Closed
        val mutable id = None
        val mutable jid = jid
        (* For authentication. *)
        val mutable auth_client = None
        val mutable credentials = SASL.Password ""
        val node = localpart
        val domain = serverpart
        val mutable resource = resource;

        val lang = lang; (* TODO: maybe later use some gettext with this? *)
        val mutable server_lang = None
        val mutable server_xmpp_version = "0.9"
        val version = "1.0"; (* support only XMPP 1.0 right now. *) 

        val mutable stanza_num = 0;

        val mutable features = [];
        (** The second part of the tuple is the initiation function for this feature. It returns the initial state of negotiation, or None if the initialization/negotiation cannot proceed.
         * The third part is handler to use when negotiating the feature. *)
        val mutable known_features = [];

        val xml_parser = new XML.event_parser;

        val supported_stanzas = [];
        val supported_default_namespaces = ["jabber:client"];

        val mutable start_event = (fun () -> ());
            (*fun (_:(((string * string) * string) list)) -> ()*)
        val mutable close_event = (fun () -> ());
        val mutable stanza_event = (fun (_ : Stanza.t) -> ());
        val mutable established_event = (fun () -> ());
        (*val mutable test_event = (fun (cli: 'a generic) -> ());*)

        method jid = jid
        method id = id

        (*method on_test handler =
            test_event <- handler*)

        method private _on_start handler =
            start_event <- handler

        method private _on_established handler =
            established_event <- handler

        method private _on_close handler =
            close_event <- handler

        method private _on_stanza handler =
            stanza_event <- handler

        method run cred = (*{{{*)
            credentials <- cred;
            if not (Mutex.try_lock run_lock)
            then
                raise Logical_error; (* The program tried to run a stream already running. *)
            connector#connect ();
            this#start;
            while state <> Closed
            do
                try
                    xml_parser#parse (connector#read ());
                with
                (* Error texts are not destined to be shown to users. It is
                diagnostic information. Hence I leave them always English. *)
                | XML.Unsupported_encoding ->
                    this#error (Unsupported_encoding (None, XML.Empty_node))
                | End_of_file ->
                    () (* End of file in a fed stream is not an error. *)
                | XML.Restricted_XML mess ->
                    this#error (Restricted_xml ((Some ("en", mess)), XML.Empty_node))
                | XML.Invalid_XML mess ->
                    this#error (Invalid_xml ((Some ("en", mess)), XML.Empty_node))
                | XML.Malformed_XML mess ->
                    this#error (Not_well_formed ((Some ("en", mess)), XML.Empty_node))
            done;
            Mutex.unlock run_lock
            (*}}}*)

        method raw_send data =
            print_string ("\n==  C  ==: " ^ data);
            flush stdout; (* TODO: remove the two lines, for test. *)
            connector#write data;

        method send stanza =
            (* I make a digest from the full jid and the incrementing stanza count, so
            that I get a hopefully unique id numbering, hence trying to avoid clash
            with contacts' stanza numbering.
            TODO: actually I should also keep a register of used ids. *)
            let id = Digest.to_hex (Digest.string (jid ^ resource ^ string_of_int (stanza_num))) in
            stanza_num <- stanza_num + 1;
            let stanza = Stanza.set_attribute stanza ("", "id") id in
            let st = Stanza.to_string stanza in
            this#raw_send st

        method error err =
            if state = Closed
            then
                raise Logical_error;
            let error = stream_error_to_string err
            in this#raw_send error;
            this#close

        method close =
            if state = Closed
            then
                raise Logical_error;
            (* The event is run before closing the stream. This way, it is still possible to send stuff if necessary. *)
            close_event ();
            this#raw_send "</stream:stream>";
            state <- Closed;
            (* Before disconnecting the stream, I check so I don't "lose" any pending data. *)
            let remain_flag = ref true in
            while !remain_flag
            do try
                xml_parser#parse (connector#read ());
            with
            | XML.Unsupported_encoding
            | XML.Restricted_XML _
            | XML.Invalid_XML _
            | XML.Malformed_XML _
            | End_of_file ->
                (* Though only End of file is the "proper" closing, we go
                through all "possible" exceptions. *)
                remain_flag := false
            done;
            connector#disconnect ();

        method private level = 
            xml_parser#level

        method private start =(*{{{*)
            if state = Closed
            then
                state <- Opened_connection;
            xml_parser#reset;
            (* content-namespace-as-default-namespace style. *)
            let stream_start =
                "<?xml version='1.0' encoding='UTF-8'?><stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0' xmlns='"
                ^ (List.hd supported_default_namespaces) ^
                begin
                    if connector#encrypted
                    then
                        (* I set the FROM only after TLS restart. *)
                        "' from='" ^ jid ^ "' to='" ^ domain
                    else
                        "' to='" ^ domain
                end
                ^
                begin match lang with
                | "" -> "' >"
                | l -> "' xml:lang='" ^ l ^ "' >"
                end in
            this#raw_send stream_start;
            state <- Started;
            (*}}}*)

        method private start_handler = function (* {{{ *)
            | XML.CharData _ -> ()
            | XML.Start_document -> ()
            | XML.End_document ->
                this#close
            | XML.Start_element ((ns, name), att) ->
            (* XXX: I cannot check for the stream ns declaration as I have already processed and removed it. How to know if the stream declaration has been made here if it is a stream restart? Should I "clean" the attributes from the ns declarations later on? Moreover I cannot check as well for the default ns... *)
                if name <> stream_lp
                then begin
                    let message = "The expected root element is a 'stream'." in
                    this#error (Bad_format (Some ("en", message), XML.Empty_node))
                end
                else if ns <> stream_ns
                then
                    let message = "The streams namespace must be 'http://etherx.jabber.org/streams'." in
                    this#error (Invalid_namespace (Some ("en", message), XML.Empty_node))
                else begin 
                    (* TODO: check right "to" *)
                    id <-
                        begin try
                            Some (Unicode.UTF8.encode (List.assoc ([], [105; 100]) att))
                        with
                        | Not_found -> None
                        end;
                    server_lang <-
                        begin try
                            Some (Unicode.UTF8.encode (List.assoc xml_lang att))
                        with
                        | Not_found -> None
                        end;
                    server_xmpp_version <-
                        begin try
                           Unicode.UTF8.encode (List.assoc ([], [118; 101; 114; 115; 105; 111; 110]) att)
                        with
                        | Not_found -> "0.9"
                        end;
                    if id = None
                    then
                        let message = "No stream ID" in
                        this#error (Undefined_condition (Some ("en", message), XML.Empty_node))
                    else if server_lang = None
                    then
                        let message = "xml:lang MUST be set." in
                        this#error (Undefined_condition (Some ("en", message), XML.Empty_node))
                    else if version = "0.9"
                    then
                        let message = "Only version 1.0 is supported." in
                        this#error (Unsupported_version (Some ("en", message), XML.Empty_node))
                    else
                        begin
                            start_event ();
                            xml_parser#change_event_handler this#negotiation_handler
                        end
               end
            | _ ->
                failwith ("XMPP.start_handler: unexpected error.") (* }}} *)

        method private negotiation_handler = function (* {{{ *)
            | XML.Start_element (qn, att) ->
                if qn = stream_features_qn (* stream features *)
                then
                     xml_parser#change_event_handler this#features_handler
                else if qn = stream_error_qn (* stream error *)
                then begin
                    (* TODO: in fact I should wait for the end of this error element, and log it/send a signal. *)
                    this#close
                end
                else
                    this#error (Bad_format
                        (Some("en", "stream features expected."), XML.Empty_node));
            | XML.CharData _ -> ()
            (* Any character data at level 0 or 1 is ignored. *)
            (* Only the <features> tag is expected, and everything happens at level 1 here, by design. *)
            | _ ->
                failwith "XMPP.negotiation_handler: unexpected error."
                    (*}}}*)

        method private features_handler =(*{{{*)
            let required = ref false in
            let required_features = ref [] in
            let optional_features = ref []
            in function
                | XML.End_element _ when this#level = 1 ->
                    begin
                        match !required_features with
                        | [] ->
                            features <- !optional_features;
                            (* TODO: later, run handler of features: what are you interested in?! *)
                            (* XXX: this is a patch for ejabberd which does not send the mechanisms as being required! *)
                            (* A particular fate for SASL/TLS and BIND to round away currents implementation bugs in servers. *)
                            (*if List.exists (function (name, init, nego) -> name = tls_feature) stream.known_features in*)
                            if List.mem_assoc ("urn:ietf:params:xml:ns:xmpp-tls", "starttls") !optional_features
                            then begin
                                let node = (List.assoc ("urn:ietf:params:xml:ns:xmpp-tls", "starttls") !optional_features) in
                                this#feature_handler ("urn:ietf:params:xml:ns:xmpp-tls", "starttls") node
                                (* choose_sasl_mechanism stream (List.assoc sasl_feature !optional_features);*)
                            end
                            else if List.mem_assoc sasl_feature !optional_features
                            then begin
                        (*let new_handler = fun ev ->
                            match sasl_handler stream (List.assoc sasl_feature !optional_features) ev with
                            | Negotiated ->
                                    stream.xml_parser#change_event_handler (stream.negotiation_handler stream)
                            | Negotiating -> ()
                        in
                        stream.xml_parser#change_event_handler new_handler;*)
                                let node = (List.assoc sasl_feature !optional_features) in
                                this#feature_handler sasl_feature node
                            end
                            else if List.mem_assoc bind_feature !optional_features
                            then begin
                                let node = (List.assoc bind_feature !optional_features) in
                                this#feature_handler bind_feature node
                            end
                            else begin
                                xml_parser#change_event_handler this#established_handler;
                                established_event ();
                            end
                            (* XXX: end of patch. To remove when ejabberd behaves well.
                             * But it depends on a semantic clarification on features optionality. *)
                        | rf ->
                            let rec common = function
                                | [] ->	this#close; (* I know none of the required features. No error here? *)
                                | f::fs -> (* I get out at the first known required. What if I have choices? *)
                                        begin try
                                            (* I try to do required features in order. Is it right method?
                                             * I should order according to my own technics choice. *)
                                            this#feature_handler (fst f) (snd f)
                                        with
                                        | Not_found -> common fs
                                        end
                            in common rf
                    end
                | XML.Start_element ((ns, name), att) when this#level = 2 ->
                    (* There is no generic way to specify any feature being
                    mandatory-to-negotiate.
                    Hence I consider it optional by default, in order not to
                    block the negotiation if I don't know the feature. *)
                    required := false;
                    optional_features := ((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), XML.Element ((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), attr_ustring_to_utf8 att, []))::(!optional_features)
                | XML.Start_element ((ns, name), att) when this#level = 3 && name = required_lp ->
                    (* The <required/> element is not a generic logic, but a common one. *)
                    required := true;
                    required_features := (List.hd !optional_features)::(!required_features);
                    optional_features := List.tl !optional_features;
                | XML.Start_element ((ns, name), att) (* when !level > 2 *) -> 
                    begin
                        match !required with
                        | false ->
                            let feat = List.hd !optional_features in
                            optional_features := (fst feat, XML.add (XML.Element ((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), attr_ustring_to_utf8 att, [])) (this#level - 2) (snd feat))::(List.tl !optional_features)
                        | true ->
                            let feat = List.hd !required_features in
                            required_features := (fst feat, XML.add (XML.Element ((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), attr_ustring_to_utf8 att, [])) (this#level - 2) (snd feat))::(List.tl !required_features)
                    end
                | XML.End_element qn when this#level = 2 ->
                    begin
                        match !required with
                        | false ->
                            let feat = List.hd !optional_features in
                            optional_features := (fst feat, XML.reverse (snd feat))::(List.tl !optional_features)
                        | true->
                            let feat = List.hd !required_features in
                            required_features := (fst feat, XML.reverse (snd feat))::(List.tl !required_features)
                    end
                | XML.End_element qn when this#level > 2 -> ()
                | XML.CharData d when this#level > 2 ->
                    begin
                        match !required with
                        | false ->
                            let feat = List.hd !optional_features in
                            optional_features := (fst feat, XML.add (XML.Character_data (Unicode.UTF8.encode d)) (this#level - 2) (snd feat))::(List.tl !optional_features)
                        | true ->
                            let feat = List.hd !required_features in
                            required_features := (fst feat, XML.add (XML.Character_data (Unicode.UTF8.encode d)) (this#level - 2) (snd feat))::(List.tl !required_features)
                    end
                | XML.CharData _ -> ()
                | _ -> failwith "XMPP.features_handler: unexpected error."
                (*}}}*)

        method private feature_handler feature node =(*{{{*)
            let _, initialization, negotiation_h = List.find (function (name, init, nego) -> name = feature) known_features in
            match initialization node with
            | Some n ->
                let new_handler =
                    let step = ref n in
                    fun ev ->
                        match negotiation_h !step ev with
                        | Success ->
                            if feature = ("urn:ietf:params:xml:ns:xmpp-bind", "bind")
                            then
                                begin
                                    established_event ();
                                    xml_parser#change_event_handler this#established_handler
                                end
                                (** XXX: This is a dirty "hack" to work around bugs in current server implementations about non sending a new features (even if empty!) after bind.
                                 * Cf. for instance for ejabberd: https://support.process-one.net/browse/EJAB-1058 *)
                            else
                                xml_parser#change_event_handler this#negotiation_handler 
                        | Success_restart ->
                            xml_parser#change_event_handler this#start_handler;
                            this#start;
                        | Negotiating s -> step := s
                        | Failure ->
                            raise Not_found
                            (* I may have an alternative feature to negotiate the stream. *)
                in
                xml_parser#change_event_handler new_handler
            | None ->
                (** The negotiation has not begun and is cancelled (such case may happen when the details of the "feature" node are inspected and the client concludes it won't be able to proceed. For instance if it knows no provided mechanisms in the SASL feature. A feature incompletely supported is here the same as an unknown feature: Not_found is raised. *)
                raise Not_found
                    (*}}}*)

        method private established_handler = (* {{{ *)
            let current_content = ref [] in
            let attributes = ref [] in
            function
                | XML.Start_element (qn, att) when this#level = 1 ->
                    if List.mem qn supported_stanzas (* stanza *)
                    then begin
                        attributes := attr_ustring_to_utf8 att;
                        current_content := []
                    end
                    else if qn = stream_error_qn (* stream error *)
                    then begin
                        (* TODO: in fact I should wait for the end of this error element, and log it/send a signal. *)
                        (*close_event ();*)
                        this#close 
                    end
                    else if qn = stream_features_qn
                    then
                        xml_parser#change_event_handler this#features_handler 
                        (* TODO: for now when I receive an updated features while established, I don't process it. *)
                    else
                        this#error (Unsupported_stanza_type (None, XML.Empty_node))
                | XML.Start_element ((ns, name), att) when this#level = 2 ->
                    current_content := (XML.Element ((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), attr_ustring_to_utf8 att, []))::(!current_content)
                | XML.Start_element ((ns, name), att) when this#level > 1 -> 
                    current_content := (XML.add (XML.Element ((Unicode.UTF8.encode ns, Unicode.UTF8.encode name), attr_ustring_to_utf8 att, [])) (this#level - 2) (List.hd !current_content))::(List.tl !current_content)
                | XML.End_element _ when this#level = 0 ->
                    this#close;
                | XML.End_element (ns, name) when this#level = 1 ->
                    stanza_event (Stanza.create (Unicode.UTF8.encode ns, Unicode.UTF8.encode name) !attributes (List.rev !current_content));
                | XML.End_element _ when this#level = 2 ->
                    current_content := (XML.reverse (List.hd !current_content))::(List.tl !current_content)
                | XML.End_element _ -> ()
                | XML.CharData d when this#level = 2 ->
                    current_content := (XML.Character_data (Unicode.UTF8.encode d))::(!current_content)
                | XML.CharData d when this#level > 2 ->
                    current_content := (XML.add (XML.Character_data (Unicode.UTF8.encode d)) (this#level - 2) (List.hd !current_content))::(List.tl !current_content)
                | XML.CharData _ -> ()
                | _ -> failwith "XMPP.established_handler: unexpected error."
                (* Any character data at level 0 or 1 is ignored. *)
                (*}}}*)

                (************************)
                (* Feature HANDLERS {{{ *)
                (************************)
                (* Note: feature handlers can be added at stream construction.
                 * Here are the three default features: SASL, TLS and bind *)

        method private initiate_tls_negotiation _ =(*{{{*)
            this#raw_send "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>";
            Some 1 (*}}}*)

        method private tls_negotiation_handler step = function (* {{{ *)
            | XML.Start_element (qn, att) when this#level = 1 && qn = tls_proceed && step = 1 ->
                    Negotiating 10
            | XML.Start_element (qn, att) when this#level = 1 && qn = tls_failure && step = 1 ->
                    Negotiating 100
                    (* Begin success case steps...*)
            | XML.End_element _ when this#level = 1 && step = 10 ->
                    begin try
                        connector#secure ();
                    with
                    | Connector.Certificate_verification _ ->
                        (* TODO: actually do something to propose the user
                            * to accept or refuse a cert issue. *)
                        connector#validate true;
                    end;
                    Success_restart
                    (* Begin failure case steps...*) (* TODO: get the error info. *)
            | XML.End_element _ when this#level = 1 && step = 100 ->
                    Failure
            | XML.CharData _ -> Negotiating step (* TODO: As there should be no white space, I
            should remove this, no?! Cf. 6.2.1 *)
            | _ -> failwith "XMPP.tls_negotiation_handler: unexpected error."
            (*}}}*)

        method private initiate_sasl_negotiation node = (*{{{*)
            let mech_list =
                begin
                    match node with
                    | XML.Element (_, _, l) -> l
                    | _ -> failwith "XMPP.sasl_handler: unexpected error."
                end in
            let rec process_mechs proposed = function
                    | [] ->
                            let sasl_client = new Auth.client
                                ~authzid:jid ~proposed localpart
                            in
                            auth_client <- Some sasl_client;
                            (* TODO: do not ignore the load_credentials
                                * returned boolean. *)
                            ignore (sasl_client#load_credentials credentials);
                            let init_response =
                                match sasl_client#request_exchange with
                                | None -> ""
                                | Some "" ->
                                    (* As a special exception a zero-length
                                     * initial response is encoded as a
                                     * single equals sign character. *)
                                    "="
                                | Some init_response ->
                                    Base64.encode init_response
                            in
                            let message = ("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='"
                            ^ sasl_client#mechanism_name ^ "'>"
                            ^ init_response
                            ^ "</auth>") in
                            this#raw_send message;
                            Some 1;
                            (*
                            None; (* even a failure is an end of negotiation.
                            "Cancel" enables me to continue my round-check... *)
                            (*| (XML.Element (qn, _, [XML.Character_data m]))::_ when qn = mechanism_qn && m = digest_md5 ->
                                (* TODO: how to check that no white space? Change XML parser? (cf. 7.2.3 of RFC3920bis) *)
                                raw_send stream "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='DIGEST-MD5'>=</auth>";
                                let new_handler = fun ev ->
                                    sasl_digest_md5_handler handlers stream ev; (* TODO! *)
                                    stream.xml_parser#change_event_handler (handlers.established_handler handlers stream)
                                    in
                                stream.xml_parser#change_event_handler new_handler;
                            (*stream.xml_parser#change_event_handler (aux_md5 handlers stream node) *) *)
                                (* TODO: uncomment the above section to support DIGEST-MD5 once I have developped the required lib. *)
                            *)
                    | (XML.Element (qn, _, [XML.Character_data mech]))::l
                        when qn = ("urn:ietf:params:xml:ns:xmpp-sasl", "mechanism") ->
                            process_mechs (mech::proposed) l
                            (*
                            let localpart = 
                                begin try
                                    Str.string_before jid (String.index jid '@')
                                with
                                | Not_found -> ""
                                end in
                            let sasl_client = new SASL.plain_client
                                ~authzid:jid
                                localpart
                            in
                            ignore (sasl_client#load_credentials credentials);
                            let init_response =
                                match sasl_client#request_exchange with
                                | None -> failwith ""
                                | Some ir -> ir
                            in
                            let message = ("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>"
                            ^ (Base64.encode init_response)
                            (* TODO: ejabberd bug! *)
                            ^ "</auth>") in
                            this#raw_send message;
                            Some 1;*)
                    | _ -> failwith "TODO: not an element, should fail" 
            in process_mechs [] mech_list
            (* }}} *)

    method private sasl_negotiation_handler step = function (* {{{ *)
        | XML.Start_element (qn, att) when this#level = 1 && step = 1 ->
                (* step 1 = PLAIN chosen, and a 'auth' has already been sent. Expect
                 * success or failure. *)
                if qn = sasl_success_qn
                then
                    Negotiating 10
                else if qn = sasl_failure_qn
                then
                    Negotiating 100
                else begin
                    this#error (Bad_format (None, XML.Empty_node));
                    Failure
                end
                (* TODO: step 2 = DIGEST_MD5 chosen. Expect challenge, success or failure. *)
                (*| _ when level stream = 1 ->
                    error stream (Bad_format (None, XML.Empty_node));*)
                (*failwith "XMPP.sasl_negotiation_handler: unexpected error." (* TODO: no make a catchable error! Stream error.*)*)
        | XML.End_element _ when this#level = 1 && step = 10 ->
                Success_restart (* Negotiation succeeded. *)
        | XML.End_element _ when this#level = 1 && step = 100 ->
                Failure (* TODO: for failure, get the failure reason. *)
        | _ when step = 10 || step = 100 -> Negotiating step
        (* TODO: CDATA at level 1: bad format, no cdata during sasl... *)
        | _ ->
                failwith "XMPP.sasl_negotiation_handler: unexpected error."
                (*}}}*)

    method private initiate_bind_negotiation  _ =(*{{{*)
        let message =
            match resource with
            | "" ->
                    "<iq id='bind_1' type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></iq>"
                    (* TODO: id of the bind to generate! *)
            | res -> 
                    "<iq id='bind_1' type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>"
                    ^ res ^ "</resource></bind></iq>"
            in
            this#raw_send message;
            Some 1
            (*}}}*)

    method private bind_negotiation_handler step = function(*{{{*)
        | XML.Start_element (qn, att) when this#level = 1 && qn = iq_qn && step = 1 ->
                begin
                    try
                        let type_value = List.assoc ([], Unicode.UTF8.decode "type") att in
                        (*let type_value = List.assoc ("", "type") att in*)
                        if type_value = Unicode.UTF8.decode "result"
                        (*if type_value = "result"*)
                        then
                            Negotiating 10
                        else if type_value = Unicode.UTF8.decode "error"
                        (*else if type_value = "error"*)
                        then
                            Negotiating 100
                        else begin
                            this#error (Bad_format (None, XML.Empty_node));
                            Failure
                        end
                    with
                    Not_found -> 
                        this#error (Bad_format (None, XML.Empty_node));
                        Failure
                end
                (* Begin success case steps...*)
        | XML.Start_element (qn, att) when this#level = 2 && qn = bind_qn && step = 10 ->
                Negotiating 11
        | XML.Start_element (qn, att) when this#level = 3 && qn = jid_qn && step = 11 ->
                Negotiating 12
        | XML.CharData full_jid when this#level = 4 && step = 12 ->
                jid <- Unicode.UTF8.encode full_jid;
                Negotiating 13
        | XML.End_element _ when this#level = 1 && step = 13 ->
                Success
        | XML.End_element _ when step = 13 -> Negotiating 13
        (* Begin failure case steps...*) (* TODO: get the error info. *)
        | XML.End_element _ when this#level = 1 && step = 100 ->
                Failure
        | XML.CharData _ -> Negotiating step
        | _ -> failwith "XMPP.bind_negotiation_handler: unexpected error."
        (*}}}*)

(*********}}}************)
        initializer
            known_features <- [(tls_feature, this#initiate_tls_negotiation, this#tls_negotiation_handler);
            (sasl_feature, this#initiate_sasl_negotiation, this#sasl_negotiation_handler);
            (bind_feature, this#initiate_bind_negotiation, this#bind_negotiation_handler)];
            xml_parser#change_event_handler this#start_handler;
    end;;
(*}}}*)

class client ?(lang: string = "") ?(resource: string = "")
    ?(server:string option = None) ?(port:int option = None)
    jid =
        let tcp_connector = new Connector.tcp in
    object (self)
    inherit [Connector.tcp] generic ~lang ~resource jid ~namespaces:[]
        ~stanzas:[]  ~connector:(tcp_connector :> Connector.tcp)
        as super

        val mutable iq_handlers = [] (* pairs ("namespace", handler client iq) *)
        val mutable message_handlers = []
        val mutable presence_handlers = []
        val mutable iq_response_handlers = [] (* pairs (id, handler client iq) *)

        val mutable targets = []

        method private initialize_target =
            (* I query SRV records ONLY when values were not previously set. *)
            targets <-
                match (server, port) with
                | None, None ->
                    Dns_helper.srv_lookup ("_xmpp-client._tcp." ^ domain ^ ".")
                | None, Some p ->
                    List.map (fun a -> a, p)
                    (Dns_helper.address_lookup domain)
                | Some d, None ->
                    List.map (fun a -> a, 5222)
                    (Dns_helper.address_lookup d)
                | Some d, Some p ->
                    List.map (fun a -> a, p)
                    (Dns_helper.address_lookup d)

        method on_established (handler: (client -> unit)) =
            super#_on_established (function () -> handler (self :> client));

        method on_start (handler: (client -> unit)) =
            super#_on_start (function () -> handler (self :> client));

        method on_close (handler: (client -> unit)) =
            super#_on_close (function () -> handler (self :> client));

        method on_stanza (handler: (client -> Stanza.t -> unit)) =
            super#_on_stanza (function stanza -> handler (self :> client) stanza);

        method on_message (handler: (client -> Stanza.t -> unit)) =
            message_handlers <- handler::message_handlers

        method on_presence (handler: (client -> Stanza.t -> unit)) =
            presence_handlers <- handler::presence_handlers

        method on_iq (namespace: string) (handler: (client -> Stanza.t -> unit)) =
            iq_handlers <- handler::iq_handlers

        (*method send_iq stanza*)

        method run credentials =
            self#initialize_target;
            tcp_connector#set_targets targets;
            super#run credentials

    end
