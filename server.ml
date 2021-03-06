open Lwt (* provides bind and join *)
open Lwt_io (* provides printl... *)
open Lwt_unix (* provides socket, accept, listen... *)
open XML
open Unicode
open Cryptokit
open Rosterreader

type state = Closed | Start | Negot | Connected
exception Invalid_from of string;;
exception Invalid_namespace of string;;
exception Unsupported_version of string;;
(* TODO:Define some service exceptions here!!!!!!!*)


(* create a useful local address *)
let my_name = Unix.gethostname ();;
let my_entry = Unix.gethostbyname my_name;;
let my_addr = my_entry.Unix.h_addr_list.(0);;
let global_info = Hashtbl.create 100;;
let counter = ref 0;;
Rosterreader.read_file ();;
let roster = Rosterreader.roster;;
class conn_info 
	?o_init:(o_i=Lwt_io.null) 
	?r_init:(r_i=[]) 
	?p_init:(p_i="") () =
    object
        val mutable outchan = o_i
        val mutable presence = p_i
		method get_outchan = outchan
        method change_outchan o = outchan <- o
		method get_presence = presence
		method change_presence p = presence <- p
        (*val mutable rosters = r_i
		method get_roster = rosters
		method change_roster rs = rosters <- rs*)
    end;;

class service conn_init =
	object(this) 
		val xml_lang = (UTF8.decode "http://www.w3.org/XML/1998/namespace", [108; 97; 110; 103])
		val server_id = my_name
		val mutable connection = conn_init
		val mutable outchannel = of_fd output conn_init
		val mutable client_id = ""
		val mutable server_lang = None
		val server_xmpp_version = "0.9"
		val mutable client_xmpp_version = ""
		val requirements = ["username"; "digest"] (* requirements for authtication *)
		val mutable server_req = [] (* requirements that haven't be fulfilled *)
		val mutable stream_id = ""
		val mutable username = ""
		val mutable password = ""
		val mutable resource = ""
		val mutable state = Closed
		val mutable contacts = []
		val mutable contacts_temp = []
		val mutable proceed = false
		val mutable has_data = false
		val mutable level1 = (("",""), [])
		val mutable level2 = (("",""), [])
		val mutable level3 = (("",""), [])
		val mutable chardata = []
		val mutable contents = [] (* list of ((namespace, name),(attribute list, data)) *)
		val mutable stanza_temp = ""
		val mutable element_start = ""
		(* val mutable stanza_send = []     used for packed stanza sending mechanism *)
		val xml_parser = new event_parser



		(* TODO:need further implementation*)
		method gen_id = string_of_int !counter

		method send str = 
			ignore_result (write_line outchannel str);

		method send_all strs = 
			List.iter this#send strs

(* TODO: have different responses *) 
		method send_err exc =  
			ignore_result (
			let err_namespace_str = " xmlns='urn:ietf:params:xml:ns:xmpp-streams'" in
			let err_xml_str = 
				match exc with
				 XML.Malformed_XML err_str  -> print_string err_str;
											   "not-well-formed" 
				|XML.Invalid_XML err_str    -> print_string err_str;
											   "invalid-xml"
				|XML.Restricted_XML err_str -> print_string err_str;
											   "restricted-xml"
				|XML.Unsupported_encoding   -> "unsupported-encoding"
				|Invalid_from err_str		-> print_string err_str;
											   "invalid-from"
				|Invalid_namespace err_str	-> print_string err_str;
											   "invalid-namespace"
				|Unsupported_version err_str-> print_string err_str;
											   "unspported-version"
				|_ 							-> ""
			in
			this#send ("<stream:error>
						<" ^ err_xml_str ^ err_namespace_str ^ "/>
						</stream:error>
						</stream:stream>");
			xml_parser#change_event_handler this#closing_handler;
			(* wait for 5 seconds *)
			sleep 5.0 >>= fun () ->
			this#close_conn;
			return ()
			)
			

		method send_to id str = 
			try
			let outchan_to = (Hashtbl.find global_info id)#get_outchan in
				ignore_result (write_line outchan_to str)
			with Not_found -> () (*target client not online, ignore *)

		
		method close_conn =
			if state <> Closed then begin
				counter := !counter - 1;
				Hashtbl.remove global_info client_id; 
				ignore_result (Lwt_io.close outchannel)
				end


		method tags_refresh =
			level1 <- (("",""), []);
			level2 <- (("",""), []);
			level3 <- (("",""), []);
			chardata <- [];
			contents <- []


		method change_state s = 
			if proceed then
				begin
				proceed <- false;
				this#tags_refresh;
				state <- s;
				server_req <- requirements;
				match s with
				| Closed -> xml_parser#change_event_handler this#init_handler
				| Start -> xml_parser#change_event_handler this#start_handler
				| Negot  -> xml_parser#change_event_handler this#negot_handler
				| Connected -> xml_parser#change_event_handler this#connect_handler
				end

		(* available services when server-client connection is about to close *)
		method closing_handler = function
			| End_element (ns, name) when xml_parser#level = 0 -> 
				(* when comes the stream end tag </stream> *)
				this#close_conn;
				state <- Closed
				
			| _ -> () (* TODO: other available services, like change roster, etc *)


		method init_handler = function 
			| Start_document -> proceed <- true;
								this#change_state Start
			| _				 -> ()


		method start_handler = function
			| Start_element ((ns, name), att)  -> 
				stream_id <- this#gen_id;
                server_lang <- begin try
                        Some (UTF8.encode (List.assoc xml_lang att))
                    with Not_found -> None
                    end;
                client_xmpp_version <- begin try 
                        (UTF8.encode (List.assoc ([], UTF8.decode "version") att))
                    with Not_found -> ""
                    end;
				if client_xmpp_version <> "0.9" then
					this#send_err (Unsupported_version ("version: " ^ client_xmpp_version))
				else begin
					this#send "<?xml version=\"1.0\"?>";
					this#send ("<stream:stream
 						      from='" ^ my_name ^ "'
							  id='" ^ stream_id ^ "'
 						      version='" ^ server_xmpp_version ^ "'
 						      xml:lang='en'
 						      xmlns='jabber:client'
 						      xmlns:stream='http://etherx.jabber.org/streams'>");
					proceed <- true;
					this#change_state Negot
				end
						
			| _    -> ()


		method negot_handler = function
			| Start_element ((ns, name), att) when xml_parser#level = 1 ->
				level1 <- ((UTF8.encode ns, UTF8.encode name), att)
			| End_element _ when xml_parser#level = 1 ->
				level1 <- (("", ""), []);
				this#change_state Connected
			| Start_element ((ns, name), att) when xml_parser#level = 2 ->
				level2 <- ((UTF8.encode ns, UTF8.encode name), att)
			| End_element (ns, name) when xml_parser#level = 2 ->
				let ((_, _), att) = level1 in
				let ((level2_ns, _), _) = level2 in
				let level1_tp = try (UTF8.encode (List.assoc ([], UTF8.decode "type") att)) 
								with Not_found -> ""
				and level1_id = try (UTF8.encode (List.assoc ([], UTF8.decode "id") att))
							 	with Not_found -> "" (* TODO:should raise exception here*) in
				if name = UTF8.decode "query" then 
					if level1_tp = "get" then
						if level2_ns = "jabber:iq:auth" then
							this#send ("<iq type='result' id='" ^ level1_id ^ "'>
							   <query xmlns='jabber:iq:auth'>
							   <username/>
							   <digest/>
							   <resource/>
							   </query>
							   </iq>")
						else ()
					else if level1_tp = "set" then
						if level2_ns = "jabber:iq:auth" then begin
							(* if response query contains "username" then eliminate it from server_req list *)
							begin try (
								username   <- UTF8.encode (snd (List.assoc ("", "username") contents));
								server_req <- snd (List.partition ((=) "username") server_req); 
							 	) with Not_found -> ();
							end;
							(* "password" can be optional - replaced by digest *)
							begin try (
								password   <-UTF8.encode (snd (List.assoc ("", "password") contents));
								server_req <- snd (List.partition ((=) "password") server_req);
								) with Not_found -> ();
							end;
							(* "resource" can be optional *)
							resource <- begin try (UTF8.encode (snd (List.assoc ("", "resource") contents));
										) with Not_found -> ""
							end;
							(* if "digest" is appropriate then eliminate it from server_req list *)
							begin try (
								let digest = UTF8.encode (snd (List.assoc ("", "digest") contents)) in
									(* utilities for encryption 
									let sha1 = Hash.sha1 () in
                    				let hex = Hexa.encode () in
                    				let str = transform_string hex (hash_string sha1 (stream_id ^ password)) in
									*)
									(* no password needed, allow all digest except empty *)
									if digest <> "" then
										server_req <- snd (List.partition ((=) "digest") server_req);
							) with Not_found -> ()
							end;
							(* if all requires are met (server_req is empty) then proceed *)
							if server_req = [] then
								let conn_inst = new conn_info ~o_init:(of_fd output connection) () in
									client_id <- (username ^ "@" ^ my_name ^ "/"  ^ resource);
									Hashtbl.add global_info client_id conn_inst;
									contacts <- begin try !(Hashtbl.find roster client_id)
												with Not_found -> [] (*TODO: create a new roster entry*)
												end;	
									this#send ("<iq type='result' id='" ^ level1_id ^ "' to='" ^ client_id ^ "'/>");
									proceed <- true;
							end
						else ()
					else ()
				else ();
				level2 <- (("", ""), []) 
			| Start_element ((ns, name), att) when xml_parser#level = 3 ->
				level3 <- ((UTF8.encode ns, UTF8.encode name), att);
			| End_element (ns, name) when xml_parser#level = 3 ->
				let ((_, n), _) = level3 in 
					contents <- (("", n), ([],chardata)) :: contents;
					chardata <- []
			| End_element _ when xml_parser#level > 1 ->
				chardata <- []
			| CharData s when xml_parser#level = 4 -> chardata <- s
			| _ -> ()
					

		method connect_handler = function
			| Start_element ((ns, name), att) when xml_parser#level = 1 ->
				level1 <- ((UTF8.encode ns, UTF8.encode name), att);
				let level1_from = try (UTF8.encode (List.assoc ([], UTF8.decode "from") att)) 
								  with Not_found -> "" in
				if level1_from <> "" && level1_from <> client_id then
					this#send_err (Invalid_from ("Invalid from: "^level1_from))
				
			| End_element (ns, name) when xml_parser#level = 1 ->
				let ((_, _), att) = level1 in
				let ((level2_ns, _), _) = level2 in
				let level1_tp = try (UTF8.encode (List.assoc ([], UTF8.decode "type") att)) with Not_found -> ""
				and level1_id = try (UTF8.encode (List.assoc ([], UTF8.decode "id") att)) with Not_found -> ""
				and level1_la = try (UTF8.encode (List.assoc xml_lang att)) with Not_found -> ""
				and level1_to = try (UTF8.encode (List.assoc ([], UTF8.decode "to") att)) with Not_found -> "" in
					(****** IQ ******)
					if name = UTF8.decode "iq" then
						begin
						if level1_tp = "get" && level2_ns = "jabber:iq:roster" then
							begin
							let str_iq = ref ("<iq to='"^client_id^"' type='result' id='"^level1_id^"'>
												<query xmlns='jabber:iq:roster'>") in
							(* extract information from contact and pack into a XML element *)
	      		 			let list_iter (jid, name, subs, groups) =
								(* pack groups into a XML element *)
		                    	let rec str_groups = function
		                        	| []   -> ""
		                        	| g :: gs -> ("<group>" ^ g ^ "</group>" ^ (str_groups gs))
		                    	in
		                   		str_iq := (!str_iq ^ "<item jid='" ^ jid ^ "' name='" ^ name ^
											"' subscription='" ^ subs ^ "'>" ^ str_groups groups ^ "</item>")
							in
		                	    List.iter list_iter contacts;
								this#send (!str_iq ^ "</query></iq>");
							end
						else if level1_tp = "get" && level2_ns = "http://jabber.org/protocol/disco#info" then
							this#send ("<iq type='error' from='ubuntu' to='"^client_id^"' id='info1'>
										<query xmlns='http://jabber.org/protocol/disco#info'/>
										<error type='cancel'>
										<service-unavailable xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
										</error></iq>")
						end
					(****** PRESENCE ******)
					else if name = UTF8.decode "presence" then
						(* probe presence *)
						(*TODO: may not implement server-server interaction *)
						if level1_tp = "probe" then ()
						(* unavailable presence *)
						else if level1_tp = "unavailable" then begin
							let broadcast (id, _, subs, _) =
								(*broadcast to all available contacts*)
								if (subs <> "to" ) && (Hashtbl.mem global_info id) then begin
									this#send_to id ("<presence xmlns='jabber:client' to='" ^ id ^ "' from='"                               				  ^client_id^"' type='"^level1_tp^"'>"^stanza_temp^"</presence>");
									end
							in
								List.iter broadcast contacts;
								(* send to temporary contacts *)
								List.iter broadcast contacts_temp;
								contacts_temp <- [];
								(* remove from to global_info *)
								Hashtbl.remove global_info client_id;
								counter := !counter - 1;
								state <- Closed;
								ignore_result (Lwt_unix.close connection)
							end
						(* initial, subsequent presences *)
						else
							(*TODO: send to all resources in same account *)
							if level1_to = "" then
								begin
								let broadcast (id, _, subs, _) =
									(*broadcast to all available contacts*)
									try
									let conn_inst = Hashtbl.find global_info id in
									let presence_inst = conn_inst#get_presence in
									if subs = "both" then begin
										this#send_to id ("<presence xmlns='jabber:client' to='" ^ id ^ "' from='"                               				  ^client_id^"' type='"^level1_tp^"'>"^stanza_temp^"</presence>");
										this#send ("<presence xmlns='jabber:client' from='"^id^"'>"^presence_inst^"</presence>");
										end
									(* only send presence to the contact *)
									else if subs = "from" then
										this#send_to id ("<presence xmlns='jabber:client' to='" ^ id ^ "' from='"                               				  ^client_id^"' type='"^level1_tp^"'>"^stanza_temp^"</presence>")
									(* only get presence info of the contact *)
									else if subs = "to" then
										this#send ("<presence xmlns='jabber:client' from='"^id^"'>"^presence_inst^"</presence>");
									with Not_found -> ()
								in
								List.iter broadcast contacts;
								(* update to global_info *)
								let conn_inst = Hashtbl.find global_info client_id in
									conn_inst#change_presence stanza_temp;
									stanza_temp <- ""
								end
							(* if it is a directed presence *)
							else
								try 
								this#send_to level1_to ("<presence xmlns='jabber:client' to='" ^ level1_to ^ "' from='"^client_id^"' type='"^level1_tp^"'>"^stanza_temp^"</presence>");
								stanza_temp <- "";
								contacts_temp <- (level1_to, "", "", []) :: contacts_temp
								with Not_found -> () (*TODO: raise error *)
					(****** MESSAGES ******)
					else if name = UTF8.decode "message" then
						if level1_tp = "chat" then begin
							try begin this#send_to level1_to ("<message from='"^client_id^"' id='"^level1_id^"' to='"^level1_to^"' type='chat' xml:lang='"^level1_la^"'>" ^ stanza_temp  ^ "</message>");
								contents <- [];
								stanza_temp <- "";
								end
							with Not_found -> (); (*TODO: raise exception *)
							end

			| Start_element ((ns, name), att) when xml_parser#level = 2 ->
				level2 <- ((UTF8.encode ns, UTF8.encode name), att);
				let ((_, level2_name), level2_att) = level2 in
				let level2_la = try (UTF8.encode (List.assoc xml_lang level2_att)) with Not_found -> "" in
				let ((_, level1_name), _) = level1 in
				if level1_name = "presence" then
					element_start <- xml_parser#raw_string
				else if level1_name = "message" then
					(* support multiple <body> stanza with distinct xml:lang *)
					if (List.mem_assoc (level2_la, level2_name) contents) then 
						() (*TODO: raise conflict exception *)
					else 
						element_start <- xml_parser#raw_string
				
			| End_element (ns, name) when xml_parser#level = 2 ->
				let ((_, level1_name), _) = level1 in
				let ((_, level2_name), level2_att) = level2 in
				let level2_la = try (UTF8.encode (List.assoc xml_lang level2_att)) with Not_found -> "" in
				if level1_name = "presence" then
					let element_end = xml_parser#raw_string in
					if element_start = element_end then 
						stanza_temp <- (stanza_temp ^ element_end)
					else
						stanza_temp <- (stanza_temp ^ element_start ^ element_end)
				else if level1_name = "message" then
					(* support multiple <body> stanza with distinct xml:lang *)
					if (List.mem_assoc (level2_la, level2_name) contents) then 
						() (*TODO: raise conflict exception *)
					else begin
						contents <- ((level2_la, level2_name), ([], [])) :: contents;
						let element_end = xml_parser#raw_string in
						if element_start = element_end then 
							stanza_temp <- (stanza_temp ^ element_end)
						else
							stanza_temp <- (stanza_temp ^ element_start ^ element_end)
						end;

			| CharData s when xml_parser#level = 3 -> 
				let ((_, level1_name), _) = level1 in
				if level1_name = "presence" || level1_name = "message" then 
					chardata <- s;
			| _ -> ()
		
		method stream_handler str = 
			try 
				match state with
				| Closed
				| Start
				| Negot
				| Connected -> xml_parser#parse str
			with x -> this#send_err x 

		initializer
			xml_parser#change_event_handler this#init_handler;
			server_req <- requirements;

	end;;


(* server main framework *)
let skt = socket PF_INET SOCK_STREAM 0;;
let port = int_of_string Sys.argv.(1) in
	bind skt (ADDR_INET (my_addr, port));;
let rec dispatcher () = 
	listen skt 4;
	printl "server thread established" >>= fun () -> 
	accept skt >>= fun (con, caller_addr) -> 
	let conn_thread = 
		let temp_str = ref "" in
		printlf "client thread %d established" !counter >>= fun () -> 
		let inchan = of_fd input con in
		let stream = Lwt_io.read_chars inchan in
		let service_inst = new service con in
		let char_handler c =  
            temp_str := !temp_str ^ (String.make 1 c);
            if c = '>' then begin
                service_inst#stream_handler !temp_str;
                temp_str := ""
                end;
            return ()
        in
            Lwt_stream.iter_s char_handler stream
	in
		counter := !counter + 1;
		printlf "Client number %d" !counter >>= fun () ->
		printlf "Hashtable size %d" (Hashtbl.length global_info) >>= fun () ->
		join [conn_thread; dispatcher ()]

let () = Lwt_main.run (dispatcher ())
