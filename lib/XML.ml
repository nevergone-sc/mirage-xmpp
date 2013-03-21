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
(* line 932 modified by chen *)

type event =
	| Start_document
	| End_document
	| Start_element of
        (Unicode.t * Unicode.t) * ((Unicode.t * Unicode.t) * Unicode.t) list
        (* (namespace, localpart), [((attribute ns, attribute), value) ...] *)
	| End_element of (Unicode.t * Unicode.t) (* (namespace, localpart) *)
	| CharData of Unicode.t
;;

exception Unsupported_encoding;;
exception Restricted_XML of string;;
exception Invalid_XML of string;;
exception Malformed_XML of string;;

(* This is a private exception for internal use only. *)
exception Reset of Unicode.t;;

class event_parser = (*{{{*)
    let xml_ns = Unicode.UTF8.decode "http://www.w3.org/XML/1998/namespace" in
    let xmlns_ns = Unicode.UTF8.decode "http://www.w3.org/2000/xmlns/" in
    let default_declared_namespaces = [[
        ([120; 109; 108], xml_ns); (* xml *)
        ([120; 109; 108; 110; 115], xmlns_ns)]] (* xmlns *)
    in
	object (this)
		val mutable conversion_state = Unicode.UTF8.new_conversion ()
		val mutable unicode_string = []
        (* unparsed string, already converted to Unicode codes. *)
		val mutable raw_string = ""
		method raw_string = raw_string
		(* Modified by Chen, storage for original input string *)

        val level = ref 0
        method level = !level 
        val mutable reseted = false

        val mutable event_handler = function _ -> ()
		method change_event_handler new_event = 
			event_handler <- new_event

		(* Variables used for parsing. *)
		val mutable filters = []
		val mutable current_CharData = []
		val mutable tags = []
		val mutable default_namespace:(Unicode.t list) = []
		val mutable declared_namespaces = default_declared_namespaces

        (* Only predefined and possible Entity References in XMPP! *)
		val known_entityRefs = [
			([116; 108], 60); (* lt, < *)
			([116; 103], 62); (* gt, > *)
			([112; 109; 97], 38); (* amp, & *)
			([115; 111; 112; 97], 39); (* apos, ' *)
			([116; 111; 117; 113], 34)] (* quot *)

		method private event events remains =
            let raise_event ev =
                event_handler ev;
                if reseted
                then
                    begin
                        reseted <- false;
                        raise (Reset remains)
                    end
            in
            List.iter raise_event events;

        method reset =
            reseted <- true;
            level := 0;
            declared_namespaces <- default_declared_namespaces; 
			tags <- [];
			current_CharData <- [];
            filters <- [this#prolog_filter];
            default_namespace <- []

		method parse bstring =
            (* TODO: remove 2 lines. Test only. *)
            print_string ("\n== SRV ==: " ^ bstring); 
            flush stdout;
			raw_string <- bstring;
            try
                let ustring =
                    match bstring with
                    | "" -> []
                    | s ->
                        Unicode.UTF8.decode ~cs:conversion_state s
                in
                let rec aux feed =
                    begin
                        try match feed with
                        | [] -> []
                        | ul -> (List.hd filters) ul
                        with Reset rem -> aux rem
                    end
                in
                unicode_string <- aux (unicode_string @ ustring)
            with Unicode.Invalid_byte_sequence _ ->
                raise Unsupported_encoding

        (**** {{{ Some generic filters. ****)
    
		method private s_filter = function
			| [] -> []
			| c::l when Character.is_WhiteSpace c ->
                this#s_filter l
			| l ->
                filters <- List.tl filters;
                (List.hd filters) l

		method private eq_filter = function
			| 61::l (* '=' *) ->
                    filters <- List.tl filters;
                    (List.hd filters) l
			| s::l when Character.is_WhiteSpace s ->
                this#eq_filter l
			| [] -> []
			| _ ->
                raise (Malformed_XML "'=' was expected.")

        (**** }}} ****)

        (***** PROLOG {{{ *****)

        (* The prolog state is simplified from generic XML case:
        - there is no Processing Instructions, DTD declaration, nor comment;
        - if encoding is set, it must be 'UTF-8';
        - if standalone is set, it must be 'yes'.*)

		method private prolog_filter = function
			| 60::[] | 60::63::[] | 60::63::120::[] | 60::63::120::109::[] | 60::63::120::109::108::[] as l -> l
                (* Don't do anything for now. Let's wait more information. *)
			| 60::63::120::109::108::s::l as all when Character.is_WhiteSpace s (* "<?xml " *) -> 
                this#event [Start_document] all;
                filters <- [this#s_filter; this#versionInfo_filter []];
                this#s_filter l
			| 60::63::l (* "<?" *) -> 
                raise (Restricted_XML "Processing Instruction")
			| 60::33::45::l (* "<!-" *) -> 
                raise (Restricted_XML "Comment")
			| 60::33::l (* "<!" *) ->
                raise (Restricted_XML "DTD subset")
			| 60::l as all -> 
                this#event [Start_document] all;
                filters <- [this#stag_filter [] []];
                (List.hd filters) l
			| c::l as all when Character.is_WhiteSpace c -> 
                this#event [Start_document] all;
                filters <- [this#s_lt_filter; this#stag_filter [] []];
                this#s_lt_filter l
			| [] -> []
			| l ->
                raise (Malformed_XML "Forbidden beginning of XML document.")

		method private versionInfo_filter current_name = function
			| [] -> []
			| s::l when Character.is_WhiteSpace s ->
					if current_name = [110; 111; 105; 115; 114; 101; 118]
					(* "noisrev", opposite of "version" *)
					then
						filters <- [this#eq_filter; this#s_filter; this#versionNum_filter]
					else
						raise (Malformed_XML "The attribute \"version\" was expected.");
                    this#eq_filter l
			| e::l when e = 61 (* '=' *) ->
					if current_name = [110; 111; 105; 115; 114; 101; 118]
					(* "noisrev", opposite of "version" *)
					then
						filters <- [this#s_filter; this#versionNum_filter]
					else
						raise (Malformed_XML "The attribute \"version\" was expected.");
                    this#s_filter l
			| c::l ->
					filters <- (this#versionInfo_filter (c::current_name))::(List.tl filters);
					(List.hd filters) l 

		method private versionNum_filter = function
			| s::l when Character.is_WhiteSpace s ->
                (List.hd filters) l
			| [] -> []
			| 34::l -> filters <-
				(this#versionNum2_filter 34 [])::(List.tl filters);
				(List.hd filters) l
			| 39::l -> filters <-
				(this#versionNum2_filter 39 [])::(List.tl filters);
				(List.hd filters) l
			| _ -> raise (Malformed_XML "A single or double quote is expected around the \"version\" attribute's value.")

		method private versionNum2_filter quote current_version = function
			| [] -> []
			| q::l when q = quote ->
                if current_version = [48; 46 ;49] (* "0.1" *)
                then
                    begin
                        filters <- [this#s_filter; this#encodingDecl_filter []];
                        this#s_filter l
                    end
                else
                    raise (Restricted_XML "XML version <> 1.0.")
			| c::l ->
                    filters <- (this#versionNum2_filter quote (c::current_version))::(List.tl filters);
                    (List.hd filters) l
			
		method private encodingDecl_filter current_name = function
			| [] -> []
			| s::l when Character.is_WhiteSpace s ->
                if current_name = [103; 110; 105; 100; 111; 99; 110; 101]
                (* "gnidocne", opposite of "encoding" *)
                then
                    filters <- [this#eq_filter; this#encName_filter]
                else if current_name = [101; 110; 111; 108; 97; 100; 110; 97; 116; 115]
                (* 'enoladnats' -> standalone *)
                then
                    filters <- [this#eq_filter; this#standVal_filter]
                else
                    raise (Malformed_XML "XML declaration");
                (List.hd filters) l
			| 61::l (* '=' *) ->
                if current_name = [103; 110; 105; 100; 111; 99; 110; 101]
                then
                    filters <- [this#encName_filter]
                else if current_name = [101; 110; 111; 108; 97; 100; 110; 97; 116; 115]
                then
                    filters <- [this#standVal_filter]
                else
                    raise (Malformed_XML "XML declaration");
                (List.hd filters) l
			| 63::62::l when current_name = [] -> 
                filters <- [this#s_filter; this#root_filter];
                this#s_filter l
			| 63::[] as l -> l
			| 63::_ ->
                raise (Malformed_XML "Malformed XML declaration.")
			| c::l ->
                filters <- (this#encodingDecl_filter (c::current_name))::(List.tl filters);
                (List.hd filters) l
            
		method private encName_filter = function 
			| s::l when Character.is_WhiteSpace s ->
                this#encName_filter l
			| [] -> []
			| 34::l ->
                filters <- (this#encName2_filter 34 [])::(List.tl filters);
                (List.hd filters) l
			| 39::l ->
                filters <- (this#encName2_filter 39 [])::(List.tl filters);
                (List.hd filters) l
			| _ ->
                raise (Malformed_XML "A single or double quote is expected around the \"encoding\" attribute's value.")

		method private encName2_filter quote name = function 
			| [] -> []
			| q::l when q = quote ->
                if name = []
                then
                    raise (Malformed_XML "The encoding name is empty.")
                else if name = [56; 45; 70; 84; 85]
                    then begin
                        filters <- [this#s_filter; this#standalone_filter []];
                        (List.hd filters) l
                    end
                else
                    raise Unsupported_encoding
			| c::l when name = [] && Character.is_alphabetChar c ->
                filters <- (this#encName2_filter quote [c])::(List.tl filters);
                (List.hd filters) l
			| c::l when Character.is_latinChar c ->
                filters <- (this#encName2_filter quote (c::name))::(List.tl filters);
                (List.hd filters) l
			| _ ->
                raise (Malformed_XML "Encoding name contains only Latin characters.")

		method private standalone_filter current_name = function
			| [] -> []
			| s::l when Character.is_WhiteSpace s ->
                if current_name = [101; 110; 111; 108; 97; 100; 110; 97; 116; 115]
                (* 'enoladnats' -> standalone *)
                then
                    filters <- [this#eq_filter; this#standVal_filter]
                else
                    raise (Malformed_XML "Malformed XML declaration");
                this#eq_filter l
			| 61::l (* '=' *) ->
                if current_name = [101; 110; 111; 108; 97; 100; 110; 97; 116; 115]
                then
                    filters <- [this#standVal_filter]
                else
                    raise (Malformed_XML "Malformed XML declaration");
                this#standVal_filter l
			| 63::62::l when current_name = [] -> 
                filters <- [this#s_filter; this#root_filter];
                this#s_filter l
			| 63::[] as l -> l
			| 63::_ ->
                raise (Malformed_XML "Malformed XML declaration.")
			| c::l ->
                filters <- (this#standalone_filter (c::current_name))::(List.tl filters);
                (List.hd filters) l

		method private standVal_filter = function
			| s::l when Character.is_WhiteSpace s ->
                this#standVal_filter l
			| [] -> []
			| 34::l ->
                filters <- (this#standVal2_filter 34 [])::(List.tl filters);
                (List.hd filters) l
			| 39::l ->
                filters <- (this#standVal2_filter 39 [])::(List.tl filters);
                (List.hd filters) l
			| _ ->
                raise (Malformed_XML "A single or double quote is expected around the \"standalone\" attribute's value.")

		method private standVal2_filter quote value = function 
			| [] -> []
			| q::l when q = quote ->
                if value = []
                then
                    raise (Malformed_XML "standalone value must be 'yes' or 'no'.")
                else if value = [115; 101; 121] (* 'sey' = yes *)
                    then begin
                        filters <- [this#endXMLDecl];
                        this#endXMLDecl l
                    end
                else
                    raise (Restricted_XML "not standalone")
			| c::l ->
                filters <- (this#standVal2_filter quote (c::value))::(List.tl filters);
                (List.hd filters) l

		method private endXMLDecl = function
			| [] -> []
			| s::l when Character.is_WhiteSpace s ->
                this#endXMLDecl l
			| 63::[] as l -> l
			| 63::62::l ->
                filters <- [this#s_filter; this#root_filter];
                this#s_filter l
			| _ ->
                raise (Malformed_XML "The end of the XML declaration is expected.")

        (* }}} END OF PROLOG *)

        method private root_filter = function
            | [] -> []
            | 60::[] (* "<" *) | 60::33::[] (* "<!" *) as l -> l (* Not enough information. Exit. Maybe at next iteration, we'll have more characters. *)
            | 60::33::45::_ (* "<!-" *) -> 
                raise (Restricted_XML "Comment")
            | 60::33::_ ->
                raise (Restricted_XML "DTD subset")
            | 60::63::_ (* "<?" *) ->
                raise (Restricted_XML "Processing Instruction")
            | 60::l (* '<' *) ->
                    filters <- [this#stag_filter [] []];
                    (List.hd filters) l
            | _ ->
                raise (Malformed_XML "A tag start is expected.")

		method private content_filter = function
			| [] -> []
			| 60::[] (* "<" *) | 60::33::[] (* "<!" *) as l -> l (* Not enough information. Exit. Maybe at next iteration, we'll have more characters. *)
			| 60::33::45::l (* "<!-" *) -> 
                raise (Restricted_XML "Comment")
            | 60::33::91::l (* "<![" *) -> 
                filters <- this#cdstart_filter::filters;
                (List.hd filters) l
			| 60::63::l (* "<?" *) ->
                raise (Restricted_XML "Processing Instruction")
			| 60::47::l as all (* "</" *) ->
                if current_CharData <> []
                then
                    begin
                        this#event [CharData (List.rev current_CharData)] all;
                        current_CharData <- []
                    end;
                filters <- (this#etag_filter [] [])::(List.tl filters);
                (List.hd filters) l
			| 60::l as all (* '<' *) ->
                if current_CharData <> []
                then
                    begin
                        this#event [CharData (List.rev current_CharData)] all;
                        current_CharData <- []
                    end;
                filters <- (this#stag_filter [] [])::filters;
                (List.hd filters) l
			| 38::l (* '&' *) -> 
                filters <- this#reference_filter::filters;
                (List.hd filters) l
			| 93::[] | 93::93::[] (* "]" or "]]" *) as l -> l
                (* Not enough information. May be "]]>" which should result in an error. *)
			| 93::93::62::_ (* "]]>" *) ->
                raise (Malformed_XML "CDATA-section-close delimiter inside character data.")
			| c::l -> 
                current_CharData <- c::current_CharData;
                this#content_filter l

        (*{{{ **** CDATA sections **** *)

		method private cdstart_filter = function
			| 67::68::65::84::65::91::l (* "CDATA[" *) ->
                filters <- this#cdata_filter::List.tl filters;
                this#cdata_filter l
			| [] | 67::[] | 67::68::[] | 67::68::65::[] | 67::68::65::84::[] | 67::68::65::84::65::[] as l -> l
			| l ->
                raise (Malformed_XML "Unknown markup (it looks like a CDATA startup markup, but with errors).")

		method private cdata_filter = function
			| [] -> []
			| 93::[] | 93::93::[] as l -> l (* Maybe it will be the end markup of the CD section. Let's wait for more characters. *)
			| 93::93::62::l (* "]]>" *) ->
                filters <- (List.tl filters);
                (List.hd filters) l
			| c::l ->
                (* XXX: I don't test for is_Char though I should. *)
                current_CharData <- c::current_CharData;
                this#cdata_filter l

        (*}}} END OF CDATA *)

        (*{{{ **** REFERENCE **** *)

        method private reference_filter = function (*{{{*)
			| [] -> []
			| 35::l (* '#' *) ->
                filters <- this#charRef_filter::(List.tl filters);
                (List.hd filters) l
			| l -> filters <-
                (this#entityRef_filter [])::(List.tl filters);
                (List.hd filters) l
                (*}}}*)

		method private charRef_filter = function (*{{{*)
			| 120::l (* 'x' *) ->
                filters <- (this#hexadecimal_charRef_filter 0)::(List.tl filters);
                (List.hd filters) l
			| l -> filters <-
                (this#decimal_charRef_filter 0)::(List.tl filters);
                (List.hd filters) l
			(*}}}*)

		method private entityRef_filter representation = function (*{{{*)
			| [] -> []
			| 59::l (* ';' *) ->
                if representation = []
                then
                    raise (Malformed_XML "Null character reference.")
                else
                    begin
                        filters <- (List.tl filters);
                        try
                            current_CharData <- (List.assoc representation known_entityRefs)::current_CharData;
                            (List.hd filters) l
                        with Not_found -> raise (Malformed_XML "The named entity reference is unknown.")
                    end
			| c::l when representation = [] ->
                if Character.is_NameStartChar c
                then
                    begin
                        filters <- (this#entityRef_filter (c::representation))::(List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise (Malformed_XML "First character of reference is invalid.")
			| c::l ->
                if Character.is_NameChar c
                then
                    begin
                        filters <- (this#entityRef_filter (c::representation))::(List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise (Malformed_XML "Invalid reference character.") (* }}} *)

		method private decimal_charRef_filter representation = function (*{{{*)
			| [] -> []
			| n::l when n >= 48 && n <= 57 (* 0 to 9 *) ->
                let new_rep = representation * 10 + (n - 48) in
                filters <- (this#decimal_charRef_filter new_rep)::(List.tl filters);
                (List.hd filters) l
			| 59::l (* ';' *) ->
                if Character.is_Char representation
                then
                    begin
                        current_CharData <- representation::current_CharData;
                        filters <- (List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise (Malformed_XML "Character referred to using a character reference does not match the production for 'Char' (section 4.1 of XML specification).")
			| c::_ ->
                raise (Malformed_XML "This character is not a decimal number for a chacter reference.")
        (*}}}*)

		method private hexadecimal_charRef_filter representation = function (*{{{*)
			| [] -> []
			| n::l when n >= 48 && n <= 57 (* 0 to 9 *) -> 
                let new_rep = representation * 16 + (n - 48) in
                filters <- (this#hexadecimal_charRef_filter new_rep)::(List.tl filters);
                (List.hd filters) l
			| n::l when n >= 97 && n <= 102 (* a to f *) ->
                let new_rep = representation * 16 + (n - 87) in
                filters <- (this#hexadecimal_charRef_filter new_rep)::(List.tl filters);
                (List.hd filters) l
			| n::l when n >= 65  && n <= 70 (* A to F *) -> 
                let new_rep = representation * 16 + (n - 55) in
                filters <- (this#hexadecimal_charRef_filter new_rep)::(List.tl filters);
                (List.hd filters) l
			| 59::l (* ';' *) ->
                if Character.is_Char representation
                then
                    begin
                        current_CharData <- representation::current_CharData;
                        filters <- (List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise (Malformed_XML "Character referred to using a character reference does not match the production for 'Char' (section 4.1 of XML specification).")
			| c::_ ->
                raise (Malformed_XML "This character is not a hexadecimal number for a chacter reference.")(*}}}*)

        (* REFERENCES }}}*)

        (* {{{ **** Element tags filters **** *)

		method private stag_filter name prefix = function (*{{{*)
			| [] -> []
			| 62::l (* '>' *) ->
				if name = []
				then
					raise (Malformed_XML "The start tag is closing immediately.")
				else
					begin
						(* No attribute: the default namespace remains the same. *)
						begin
							default_namespace <- match default_namespace with
							| [] -> [[]]
							| n::_ as nl -> n::nl
						end;
						(* No attribute: the declared prefixes remain the same. *)
						declared_namespaces <- (List.hd declared_namespaces)::declared_namespaces;
						(* Check existence of the prefix. *)
						let namespace = 
							begin
								match prefix with
								| [] -> List.hd default_namespace
								| p ->
									try
										List.assoc (List.rev prefix) (List.hd declared_namespaces)
                                    with Not_found ->
                                        raise (Malformed_XML
                                            ("'" ^ Unicode.UTF8.encode prefix ^
                                            "' is not a declared prefix."))
							end in
						tags <- (namespace, (List.rev name))::tags;
						filters <- (this#content_filter)::(List.tl filters);
						this#event [Start_element ((namespace, List.rev name), [])] l;
                        incr level;
						(List.hd filters) l;
					end
                | 47::[] -> [47]
				| 47::62::l (* "/>" *) ->
					if name = []
					then
						raise (Malformed_XML "Malformed start tag")
					else
						begin
							(* Check existence of the prefix. *)
							let namespace = 
								begin
									match prefix with
									| [] ->
                                        begin try
                                            List.hd default_namespace
                                            (* same default ns than previous element as there is no attribute. *)
                                        with Failure "hd" -> []
                                        end
									| p ->
										try
											List.assoc prefix (List.hd declared_namespaces)
										with Not_found -> raise (Invalid_XML "Non-declared namespace prefix.")
								end
                            in
							filters <- (List.tl filters);
							this#event
                                [Start_element ((namespace, List.rev name), []); (* stag *)
							    End_element (namespace, List.rev name)] (* etag *)
                                l;
							if tags = []
							then
								begin
									this#event [End_document] l;
									l
								end
							else
								(List.hd filters) l;
						end
				| c::l when Character.is_WhiteSpace c ->
					if name = [] 
					then
						raise (Malformed_XML "Empty element local name.")
					else
						begin
							declared_namespaces <- (List.hd declared_namespaces)::declared_namespaces;
							filters <- (this#s_filter)::(this#attribute_filter (List.rev name) (List.rev prefix) [] [] [])::(List.tl filters);
							this#s_filter l;
						end
				| 58::l (* ':' *) ->
					if name = []
					then
						raise (Malformed_XML "Empty element prefix")
					else if prefix <> []
					then
						raise (Malformed_XML "There are two namespace prefix separators ':'.")
					else
						begin
							(* The name becomes the prefix. *)
							filters <- (this#stag_filter [] name)::(List.tl filters);
							(List.hd filters) l
						end
				| c::l when name = [] ->
					if Character.is_NameStartChar c
					then
						begin
							filters <- (this#stag_filter [c] prefix)::(List.tl filters);
							(List.hd filters) l
						end
					else
						raise
                            (Malformed_XML ("First character (code " ^
                            string_of_int c ^ ") of element name is invalid."))
				| c::l ->
                    if Character.is_NameChar c
					then
						begin
							filters <- (this#stag_filter (c::name) prefix)::(List.tl filters);
							(List.hd filters) l
						end
					else
						raise
                            (Malformed_XML ("Invalid element name's character (code "
                            ^ string_of_int c ^ ")"))
            (* }}} *)

		method private s_gt_filter = function (*{{{*)
			| [] -> []
			| c::l when Character.is_WhiteSpace c ->
                (List.hd filters) l
			| 62::_ as l (* '>' *) -> filters <- List.tl filters;
                (List.hd filters) l
			| _ -> raise (Malformed_XML "Expected '>' to close the end tag of an element.")
			(*}}}*)

		method private s_lt_filter = function
			| [] -> []
			| c::l when Character.is_WhiteSpace c ->
					this#s_lt_filter l
			| 60::l (* '<' *) ->
                filters <- List.tl filters;
                (List.hd filters) l
			| _ ->
                raise (Malformed_XML "Expected '<'.")

		method private etag_filter name prefix = function (* {{{ *)
			| [] -> []
			| 62::l ->
				if name = []
				then
					raise (Malformed_XML "End tag closing with empty name");
					let namespace = 
						begin
							match prefix with
							| [] -> List.hd default_namespace 
							| p ->
								try
									List.assoc prefix (List.hd declared_namespaces)
								with Not_found -> raise (Invalid_XML "Non-declared namespace prefix.")
						end in
					let rname = (namespace, List.rev name) in
					if rname <> List.hd tags
					then
						raise (Malformed_XML "Start and end tag are not named the same.");
					default_namespace <- List.tl default_namespace;
					declared_namespaces <- List.tl declared_namespaces;
					filters <- List.tl filters;
                    decr level;
					this#event [End_element rname] l;(*rname);*)
					tags <- List.tl tags;
					if tags = []
					then
						begin
							this#event [End_document] l;
							l
						end
					else
						(List.hd filters) l
				| c::l when Character.is_WhiteSpace c ->
					filters <- this#s_gt_filter::filters;
					(List.hd filters) l
				| 58::l (* ':' *) ->
					if name = []
					then
						raise (Malformed_XML "The namespace prefix is empty.")
					else if prefix <> []
					then
						raise (Malformed_XML "There are two namespace prefix separators ':'.")
					else
						begin
							(* The name becomes the prefix. *)
							filters <- (this#etag_filter [] (List.rev name))::(List.tl filters);
							(List.hd filters) l
						end
				| c::l when name = [] ->
					if Character.is_NameStartChar c
					then
						begin
							filters <- (this#etag_filter [c] prefix)::(List.tl filters);
							(List.hd filters) l
						end
					else
						raise (Malformed_XML "First character of element name is invalid.")
				| c::l ->
					if Character.is_NameChar c
					then
						begin
							filters <- (this#etag_filter (c::name) prefix)::(List.tl filters);
							(List.hd filters) l
						end
					else
						raise (Malformed_XML "Invalid element name's character.")
            (* }}} *)

		method private check_attribute_namespace attributes =(*{{{*)
			let rec aux checked = function
				| [] -> checked
				| (([], att), value)::l ->
                    if List.mem_assoc ([], att) checked
					then
						raise (Malformed_XML "An attribute MUST NOT appear more than once in an element.");
                    aux ((([], att), value)::checked) l
				| ((prefix, att), value)::l ->
					let namespace = 
						begin
							try
								List.assoc prefix (List.hd declared_namespaces)
							with Not_found -> raise (Invalid_XML "Non-declared namespace prefix.")
						end
					in
					if List.mem_assoc (namespace, att) checked
					then
						raise (Malformed_XML "An attribute MUST NOT appear more than once in an element.");
					aux (((namespace, att), value)::checked) l
			in let rec remove_xmlns checked = function
				| [] -> checked
				| (([], [120; 109; 108; 110; 115]), _)::l -> remove_xmlns checked l (* default namespace *)
				| (([120; 109; 108; 110; 115], _), _)::l -> remove_xmlns checked l (* namespace declaration *)
				| att::l -> remove_xmlns (att::checked) l
			(* in the end, I also remove the 'xmlns' attribute, if existing. *)
			in remove_xmlns [] (aux [] attributes)
			(*}}}*)

		method private attribute_filter element_name element_prefix attributes name prefix = function (* {{{ *)
			| [] -> []
			| 47::[] (* '/' *) -> [47]
			| 62::l (* '>' *) ->
				if name = [] && prefix = []
				then
					begin
						(* Check for any change of default namespace. *)
						begin
							default_namespace <-
								try
									(List.assoc ([], [120; 109; 108; 110; 115]) attributes)::default_namespace;
								with
								Not_found ->
									begin
										match default_namespace with
										| [] -> [[]]
										| n::_ as nl -> n::nl
									end
						end;
						let namespace = 
							begin
								match element_prefix with
								| [] ->
                                    List.hd default_namespace 
								| p ->
									try
										List.assoc p (List.hd declared_namespaces)
									with Not_found -> raise (Invalid_XML "Non-declared namespace prefix.")
							end
                        in
						filters <- (this#content_filter)::(List.tl filters);
						this#event [Start_element ((namespace, element_name), (this#check_attribute_namespace attributes))] l;
                        incr level;
						tags <- (namespace, element_name)::tags;
						this#content_filter l
					end
				else
					raise (Malformed_XML "Unfinished attribute specification.")
			| 47::62::l ->
				if name = [] && prefix = []
				then
					begin
						let namespace = 
							begin
								match element_prefix with
								| [] -> 
                                    begin try
                                        (List.assoc ([], [120; 109; 108; 110; 115]) attributes);
                                    with
                                    Not_found ->
                                        begin
                                            match default_namespace with
                                            | [] -> []
                                            | n::_ -> n
                                        end
                                    end
								| p ->
									try
										List.assoc p (List.hd declared_namespaces)
									with Not_found -> raise (Invalid_XML "Non-declared namespace prefix.")
							end in
						filters <- List.tl filters;
						declared_namespaces <- List.tl declared_namespaces;
						this#event
                            [Start_element ((namespace, element_name), this#check_attribute_namespace attributes);
						    End_element (namespace, element_name)] l;
						(List.hd filters) l
					end
				else
					raise (Malformed_XML "Unfinished attribute specification.")
			| 61::l (* '=' *) -> 
				if name = []
				then
					raise (Malformed_XML "Unexpected equal '=' character.")
				else
					begin
						filters <- (this#quote_filter element_name element_prefix attributes (prefix, List.rev name))::(List.tl filters);
						(List.hd filters) l
					end
			| b::l when Character.is_WhiteSpace b -> 
                if name <> []
                then
                    filters <- (this#attEq_filter element_name element_prefix attributes (prefix, List.rev name))::(List.tl filters);
                (List.hd filters) l
			| 58::l (* ':' *) ->
				if name = []
				then
					raise (Malformed_XML "The attribute's namespace prefix is empty.")
				else if prefix <> []
				then
					raise (Malformed_XML "There are two namespace prefix separators ':' for this attribute.")
				else
					begin
						(* The name becomes the prefix. *)
						filters <- (this#attribute_filter element_name element_prefix attributes [] (List.rev name))::(List.tl filters);
						(List.hd filters) l
					end
			| c::l when name = [] ->
				if Character.is_NameStartChar c
				then
					begin
						filters <- (this#attribute_filter element_name element_prefix attributes [c] prefix)::(List.tl filters);
						(List.hd filters) l
					end
				else
					raise (Malformed_XML "First character of attribute name is invalid.")
			| c::l ->
				if Character.is_NameChar c
				then
					begin
						filters <- (this#attribute_filter element_name element_prefix attributes (c::name) prefix)::(List.tl filters);
						(List.hd filters) l
					end
				else
					raise (Malformed_XML "Invalid attribute name's character.")
        (* }}} *)

		method private attEq_filter element_name element_prefix attributes attribute = function
			| [] -> []
			| b::l when Character.is_WhiteSpace b ->
                (List.hd filters) l
			| 61::l (* '=' *) ->
                filters <- (this#quote_filter element_name element_prefix attributes attribute)::(List.tl filters);
                (List.hd filters) l
			| c::_ ->
                raise (Malformed_XML "'=' is expected.")

		method private quote_filter element_name prefix attributes attribute = function
			| [] -> []
			| b::l when Character.is_WhiteSpace b ->
                (List.hd filters) l
			| c::l when c = 34 || c = 39 ->
                filters <- (this#attValue_filter element_name prefix attributes attribute c [])::(List.tl filters);
                (List.hd filters) l
			| _ ->
                raise (Malformed_XML "A single or double quote is expected.")

		method private attValue_filter element_name prefix attributes attribute_name quote attribute_value = function
			| [] -> []
			| 60::l (* '<' *) ->
                raise (Malformed_XML "The character '<' is forbidden inside an attribute's value.")
			| 38::l (* & *) -> 
                filters <- (this#att_reference_filter element_name prefix attributes attribute_name quote attribute_value)::(List.tl filters);
                (List.hd filters) l
			| q::l when q = quote -> 
                let att_value = List.rev attribute_value in
                if (fst attribute_name) = [120; 109; 108; 110; 115]
                then (* Attribute namespace is "xmlns", hence declaration of new ns *)
                    begin
                        let new_ns = snd attribute_name in
                        if new_ns = [120; 109; 108; 110; 115]
                        then
                            raise (Invalid_XML "'xmlns' namespace must not be declared.")
                        else if new_ns = [120; 109; 108] && att_value <> xml_ns
                        then
                            raise (Invalid_XML "'xml' must not be bound to any other namespace.")
                        (*else if att_value = xml_ns
                        then
                            raise (Invalid_XML "No prefix other than 'xml' may be bound to this namespace."))*)
                        else if att_value = xmlns_ns
                        then
                            raise (Invalid_XML "No prefix other than 'xmlns' may be bound to this namespace.")
                        else
                            declared_namespaces <- ((new_ns, att_value)::(List.hd declared_namespaces))::(List.tl declared_namespaces)
                    end;
                filters <- (this#attribute_filter element_name prefix ((attribute_name, att_value)::attributes) [] [])::(List.tl filters);
                (List.hd filters) l
			| c::l ->
                filters <- (this#attValue_filter element_name prefix attributes attribute_name quote (c::attribute_value))::(List.tl filters);
                (List.hd filters) l

        (*{{{ **** Reference Filters in attributes value. **** *)

		method private att_reference_filter element_name prefix attributes attribute_name quote attribute_value = function (*{{{*)
			| [] -> []
			| 35::l (* '#' *) -> 
                filters <- (this#att_charRef_filter element_name prefix attributes attribute_name quote attribute_value)::(List.tl filters);
                (List.hd filters) l
			| l ->
                filters <- (this#att_entityRef_filter element_name prefix attributes attribute_name quote attribute_value [])::(List.tl filters);
                (List.hd filters) l(*}}}*)

		method private att_charRef_filter element_name prefix attributes attribute_name quote attribute_value = function (*{{{*)
			| 120::l (* 'x' *) ->
                filters <- (this#att_hexadecimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value 0)::(List.tl filters);
                (List.hd filters) l
			| l ->
                filters <- (this#att_decimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value 0)::(List.tl filters);
                (List.hd filters) l
			(*}}}*)

		method private att_entityRef_filter element_name prefix attributes attribute_name quote attribute_value representation = function
			| [] -> []
			| 59::l (* ';' *) ->
                if representation = []
                then
                    raise (Malformed_XML "Null character reference.")
                else
                    begin
                        try
                            filters <- (this#attValue_filter element_name prefix attributes attribute_name quote ((List.assoc representation known_entityRefs)::attribute_value))::(List.tl filters);
                            (List.hd filters) l
                        with Not_found -> raise (Malformed_XML "The named entity reference is unknown.")
                    end
			| c::l when representation = [] ->
                if Character.is_NameStartChar c
                then
                    begin
                        filters <- (this#att_entityRef_filter element_name prefix attributes attribute_name quote attribute_value [c])::(List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise (Malformed_XML "First character of reference is invalid.")
			| c::l ->
                if Character.is_NameChar c
                then
                    begin
                        filters <- (this#att_entityRef_filter element_name prefix attributes attribute_name quote attribute_value (c::representation))::(List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise (Malformed_XML "Invalid reference character.")

		method private att_decimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value representation = function
			| [] -> []
			| n::l when n >= 48 && n <= 57 (* 0 to 9 *) ->
                let new_rep = representation * 10 + (n - 48) in
                filters <- (this#att_decimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value new_rep)::(List.tl filters);
                (List.hd filters) l
			| 59::l (* ';' *) ->
                if Character.is_Char representation
                then
                    begin
                        filters <- (this#attValue_filter element_name prefix attributes attribute_name quote (representation::attribute_value))::(List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise
                        (Malformed_XML
                        "Character referred to using a character reference does not match the production for 'Char' (section 4.1 of XML specification).")
			| c::_ ->
                raise (Malformed_XML "This character is not a decimal number for a chacter reference.")

		method private att_hexadecimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value representation = function
			| [] -> []
			| n::l when n >= 48 && n <= 57 (* 0 to 9 *) -> 
                let new_rep = representation * 16 + (n - 48) in
                filters <- (this#att_hexadecimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value new_rep)::(List.tl filters);
                (List.hd filters) l
			| n::l when n >= 97 && n <= 102 (* a to f *) ->
                let new_rep = representation * 16 + (n - 87) in
                filters <- (this#att_hexadecimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value new_rep)::(List.tl filters);
                (List.hd filters) l
			| n::l when n >= 65  && n <= 70 (* 0 to 9 *) -> 
                let new_rep = representation * 16 + (n - 55) in
                filters <- (this#att_hexadecimal_charRef_filter element_name prefix attributes attribute_name quote attribute_value new_rep)::(List.tl filters);
                (List.hd filters) l
			| 59::l (* ';' *) ->
                if Character.is_Char representation
                then
                    begin
                        filters <- (this#attValue_filter element_name prefix attributes attribute_name quote (representation::attribute_value))::(List.tl filters);
                        (List.hd filters) l
                    end
                else
                    raise
                        (Malformed_XML
                        "Character referred to using a character reference does not match the production for 'Char' (section 4.1 of XML specification).")
			| c::_ ->
                raise (Malformed_XML "This character is not a hexadecimal number for a chacter reference.")
        (*}}}*)
     (*}}}*)

    initializer
        filters <- [this#prolog_filter];
    end
(*}}}*)


(*************)
(* XML NODES *)
(* ***********)

type qname = string * string;;
(** A qualified name is the pair (namespace name, local name). *)
type attributes_list = (qname * string) list;;
(** list of (attribute qualified name, value). *)

type node =
	| Empty_node
	| Character_data of string
	| Element of qname * attributes_list * node list;;

(* Private use, not in the signature. {{{ *)
let xmlns_dot_dot = Unicode.UTF8.decode "xmlns:";;
let eq_quote = [61; 34];;
let quote_s = [34; 32];;

(* I escape '<', '&' and '"'. I won't escape ' ' ' as it is not my separator in to_string. *)
let escape_AttValue value = (*{{{*)
	let rec aux checked = function
		| "" -> checked
		| s when s.[0] = '<' -> aux (checked ^ "&lt;") (Str.string_after s 1)
		| s when s.[0] = '&' -> aux (checked ^ "&amp;") (Str.string_after s 1)
		| s when s.[0] = '"' -> aux (checked ^ "&quot;") (Str.string_after s 1)
		| s -> aux (checked ^ Str.string_before s 1) (Str.string_after s 1)
		(*| [] -> checked
		| 60::l (* '<' *) -> aux (38::108::116::59::checked) l
		| 38::l (* '&' *) -> aux (38::97::109::112::59::checked) l
		| 34::l (* '"' *) -> aux (38::113::117::111::116::59::checked) l
		| c::l -> aux (c::checked) l*)
	(*in List.rev (aux [] value) *)
	in aux "" value;;
(*}}}*)

(* I escape '<', '&' and ']]>'. Note that I don't make any character verification. I suppose it to be made before. *)
let escape_content cdata = (*{{{*)
	let rec aux checked = function
		| "" -> checked
		(*| 60::l (* '<' *) -> aux (38::108::116::59::checked) l*)
		| s when s.[0] = '<' -> aux (checked ^ "&lt;") (Str.string_after s 1)
		(*| 38::l (* '&' *) -> aux (38::97::109::112::59::checked) l *)
		| s when s.[0] = '&' -> aux (checked ^ "&amp;") (Str.string_after s 1)
		(*| 93::93::62::l (* ']]>' *) -> aux (93::93::38::103::116::59::checked) l *)
		| s when String.length s > 2 && Str.string_before s 3 = "]]>" -> aux (checked ^ "]]&gt;") (Str.string_after s 3)
		| s -> aux (checked ^ Str.string_before s 1) (Str.string_after s 1)
		(*| c::l -> aux (c::checked) l*)
		(*in List.rev (aux [] cdata);;*)
	in aux "" cdata;;
(*}}}*)
(* }}} *)

let to_string ?(pretty:bool = false) ?(xmlns:string = "")(*{{{*)
			  ?(declared_ns:(string * string) list = [])
			  ?(new_ns:(string * string) list = []) (node:node) = 
	let level = ref 0 in
	let xmlns = ref [xmlns] in
	let prefix = ref [declared_ns] in
	let attributes_to_string attributes =
		let rec aux temp = function
			| [] -> temp
			| (("", name), value)::l ->
					(*| (([], name), value)::l ->*)
					(*aux (temp ^ (Unicode.to_string name) ^ "=\"" ^ (Unicode.to_string value) ^ "\" ") l*)
				aux (temp ^ name ^ "=\"" ^ value ^ "\" ") l
			| ((ns, name), value)::l ->
				let pref =
					try
						List.assoc ns (List.hd !prefix)
					with
					| Not_found -> 
						begin
							try
								let new_pref = List.assoc ns new_ns in
								(*xmlns_dot_dot@new_pref@eq_quote@ns@quote_s@new_pref*)
								"xmlns:" ^ new_pref ^ "='" ^ ns ^ "' " ^ new_pref
							with
							| Not_found -> failwith "You must provide all prefixes for all attribute's namespace required for node's construction."
							(* XXX: maybe I should simply generate a random prefix, no? *)
						end
				in
				(*aux (temp ^ (Unicode.to_string pref) ^ ":" ^ (Unicode.to_string name) ^ "=\"" ^ (Unicode.to_string (escape_AttValue value)) ^ "\" ") l*)
				aux (temp ^ pref ^ ":" ^ name ^ "=\"" ^ (escape_AttValue value) ^ "\" ") l
		in aux " " attributes
	in let indent nb =
		match pretty with
		| false -> ""
		| true ->
			let rec aux temp = function
				| 0 -> temp
				| n -> aux (temp ^ "\t") (n - 1)
			in aux "\n" nb
	in let rec aux = function (* TODO: make it recursive terminal *)
		| Empty_node -> ""
		| Character_data s -> (indent !level) ^ (escape_content s)
		(*| Empty_element (name, attributes) -> (indent !level) ^ "<" ^ name ^
			 * attributes_to_string attributes ^ "/>"*)
		| Element ((ns, name), attributes, []) ->
				if ns <> List.hd !xmlns
				then
					(*(indent !level) ^ "<" ^ (Unicode.to_string name)*)
					(indent !level) ^ "<" ^ name
					(*^ attributes_to_string ((([], [120; 109; 108; 110; 115]), ns)::attributes) ^ "/>"*)
					^ attributes_to_string ((("", "xmlns"), ns)::attributes) ^ "/>"
				else
					(*(indent !level) ^ "<" ^ (Unicode.to_string name)*)
					(indent !level) ^ "<" ^ name
						^ attributes_to_string attributes ^ "/>"
		| Element ((ns, name), attributes, subnodes) ->
				let before =
					if ns <> List.hd !xmlns
					then
						(*(indent !level) ^ ("<" ^ (Unicode.to_string name) ^ attributes_to_string ((([], [120; 109; 108; 110; 115]), ns)::attributes) ^ ">")*)
						(indent !level) ^ ("<" ^ name ^ attributes_to_string ((("", "xmlns"), ns)::attributes) ^ ">")
					else
						(*(indent !level) ^ ("<" ^ (Unicode.to_string name) ^ attributes_to_string attributes ^ ">")*)
						(indent !level) ^ ("<" ^ name ^ attributes_to_string attributes ^ ">")
				in level := !level + 1;
				xmlns := ns::(!xmlns);
				prefix := (List.hd !prefix)::(!prefix);
				let between = (List.fold_right (^) (List.map aux subnodes) "")
				in level := !level -1;
				xmlns := List.tl (!xmlns);
				prefix := List.tl (!prefix);
				(*before ^ between ^ (indent !level) ^ ("<" ^ (Unicode.to_string name) ^ "/>")*)
				before ^ between ^ (indent !level) ^ ("<" ^ name ^ "/>")
	in aux node;;(*}}}*)

let attributes = function(*{{{*)
    | Element (_, atts, _) -> atts
	| _ -> failwith "'XML.attributes': not an Element."
;;(*}}}*)

(* {b attr node (ns, name)} returns the value of the attribute whose qualified name is (ns, name).
 * @parameter attribute ({i Unicode.t, Unicode.t})
 *)
let attribute node attribute =(*{{{*)
    match node with
    | Element (_, atts, _) ->
        begin
            try
                Some (List.assoc attribute atts)
            with Not_found -> None
        end
    | _ -> failwith ("Not an element.");;(*}}}*)


let namespace = function(*{{{*)
    | Element ((ns, _), _, _) -> ns
	| _ -> failwith "'Stanza.attributes': This exception should never happen. If so, contact the maintener of Ocaml-XMPP.";;
	(* Should not happen as the stanza has a private interface and can be constructed only by "create". *)(*}}}*)

let name = function(*{{{*)
    | Element ((_, localname), _, _) -> localname
	| _ -> failwith "'Stanza.attributes': This exception should never happen. If so, contact the maintener of Ocaml-XMPP.";;
	(* Should not happen as the stanza has a private interface and can be constructed only by "create". *)(*}}}*)

let qname = function(*{{{*)
    | Element (qn, _, _) -> qn
	| _ -> failwith "'Stanza.attributes': This exception should never happen. If so, contact the maintener of Ocaml-XMPP.";;
	(* Should not happen as the stanza has a private interface and can be constructed only by "create". *)(*}}}*)

(* Note: hum... I may see a way to make it really recursive terminale, but here, I am not sure it is worthwhile. It would make it complicated and not very elegant. *)
let rec add node level = function (* to node *)
	| Empty_node when level = 0 -> node
	| Empty_node -> failwith "bla"
	| _ when level < 1 -> failwith "Tried to add a subnode on a negative level, or at the root." (* TODO *)
	| Character_data _ -> failwith "Tried to add a subnode to character data." (* TODO *)
	| Element (name, atts, nodes) when level = 1 -> Element (name, atts, node::nodes)
	| Element (name, atts, []) -> failwith "Tried to add a subnode on an unexistant level." (* TODO *)
	| Element (name, atts, n::nodes) -> Element (name, atts, (add node (level - 1) n)::nodes);;

(* Note: with this, the node list is reverted! So I make a function to rerevert it (I don't want to make it at node's build time, because it would make many useless reverse call. *)

(* Note: once again, not recursive terminale because there is a call to aux inside aux itself! I could also reuse "add" by parsing the node from top levels and adding on a new one, which could set the right order. But not sure it would be anymore efficient. Both implementations have time complexity teta (number of nodes). But the space complexity is O (level) (if I could make any of the functions recursive terminale, the space complexity would be 0(1). But I get old...). Anyway in XMPP, stanzas are usually not that deep to be such a problem for the stack, I think. *)
let reverse node =
	let rec aux reversed = function
		| Empty_node -> Empty_node
		| Character_data _ as c -> c
		| Element (name, atts, []) -> Element (name, atts, reversed)
		| Element (name, atts, n::nodes) -> aux ((aux []  n)::reversed) (Element (name, atts, nodes))
	in aux [] node;;

