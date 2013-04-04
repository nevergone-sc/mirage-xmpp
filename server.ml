open Lwt (* provides bind and join *)
open Lwt_io (* provides printl *)
open Lwt_unix (* provides socket, accept, listen... *)
open XML
open Unicode
open Rosterreader

type state = Closed | Start | Negot | Connected
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
		val mutable client_id = ""
		val mutable server_lang = None
		val mutable server_xmpp_version = "1.0"
		val mutable stream_id = ""
		val mutable state = Closed
		val mutable contacts = []
		val mutable contacts_temp = []
		val mutable proceed = false
		val mutable level1 = (("",""), [])
		val mutable level2 = (("",""), [])
		val mutable level3 = (("",""), [])
		val mutable chardata = []
		val mutable contents = []
		val mutable stanza_temp = ""
		(* val mutable stanza_send = []     used for packed stanza sending mechanism *)
		val xml_parser = new event_parser

		(* TODO:need further implementation*)
		method gen_id = string_of_int !counter

		method send str = 
			let outchan = of_fd output connection in
			ignore_result (write_line outchan str)

		method send_all strs = 
			List.iter this#send strs

(* TODO: have different responses *) 
		method send_err str =  
			printl str >>= fun () ->
			this#send "<stream:error>
						<host-unknown xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>
						</stream:error>
						</stream:stream>";
			sleep 5.0 >>= fun () ->
			counter := !counter - 1;
			Hashtbl.remove global_info client_id; 
			Lwt_unix.close connection 
			

		method send_to id str = 
			try
			let outchan_to = (Hashtbl.find global_info id)#get_outchan in
				ignore_result (write_line outchan_to str)
			with Not_found -> () (*TODO: implementation*)
		
		method tags_refresh =
			level1 <- (("",""), []);
			level2 <- (("",""), []);
			level3 <- (("",""), []);
			chardata <- [];
			contents <- []

		method change_state s = 
			if proceed then
				begin
				this#tags_refresh;
				state <- s;
				end
			else () (*TODO: need proper response *)

(* TODO: add error handling in each handler!!!!! *)
		method init_handler = function 
			| Start_document -> state <- Start
			| _				 -> ()

		method start_handler = function
			| Start_element ((ns, name), att)  -> 
				stream_id <- begin try
                		UTF8.encode (List.assoc ([], [105; 100]) att)
                	with Not_found -> this#gen_id
                	end;
                server_lang <- begin try
                        Some (UTF8.encode (List.assoc xml_lang att))
                    with Not_found -> None
                    end;
                server_xmpp_version <- begin try
                       UTF8.encode (List.assoc ([], UTF8.decode "version") att)
                    with Not_found -> "0.9"
                    end;
				client_id <- begin try
                		UTF8.encode (List.assoc ([], [102; 114; 111; 109]) att)
					with Not_found -> ""
					end;
(* TODO: add conditions here!!!!!!*)
					this#send "<?xml version=\"1.0\"?>";
					this#send ("<stream:stream
 						      from='" ^ my_name ^ "'
							  id='" ^ stream_id ^ "'
 						      version='1.0'
 						      xml:lang='en'
 						      xmlns='jabber:client'
 						      xmlns:stream='http://etherx.jabber.org/streams'>");
					this#send "<stream:features>
						       </stream:features>";
					state <- Negot
						
			| _    -> ()


		(*TODO: can it be more generic???????? *)
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
				let level1_tp = try (UTF8.encode (List.assoc ([], UTF8.decode "type") att)) with Not_found -> ""
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
						if level2_ns = "jabber:iq:auth" then
							(* TODO:consider usage of <digest>, <resource> *)
							let username = try (UTF8.encode (snd (List.assoc ("", "username") contents)))
							 			   with Not_found -> "" in
							let conn_inst = new conn_info ~o_init:(of_fd output connection) () in
								client_id <- (username ^ "@" ^ my_name);
								Hashtbl.add global_info client_id conn_inst;
								this#send ("<iq type='result' id='" ^ level1_id ^ "' to='" ^ client_id ^ "'/>");
								proceed <- true;
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
				level1 <- ((UTF8.encode ns, UTF8.encode name), att)
			| Start_element ((ns, name), att) when xml_parser#level = 2 ->
				level2 <- ((UTF8.encode ns, UTF8.encode name), att)
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
							contacts <- !(Hashtbl.find roster client_id);
							let str_iq = ref ("<iq to='"^client_id^"' type='result' id='"^level1_id^"'>
												<query xmlns='jabber:iq:roster'>") in
	      		 			let list_iter (jid, name, subs, groups) =
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
						(* initial, subsequent and unavailable presences *)
						else if level1_tp = "unavailable" then begin
							let broadcast (id, _, subs, _) =
								(*broadcast to all available contacts*)
								if (subs = "both" || subs = "from") && (Hashtbl.mem global_info id) then begin
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
								ignore_result (Lwt_unix.close connection)
							end
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
										if level1_tp <> "unavailable" then this#send ("<presence xmlns='jabber:client' from='"^id^"'>"^presence_inst^"</presence>");
										end
									else if subs = "from" then
										this#send_to id ("<presence xmlns='jabber:client' to='" ^ id ^ "' from='"                               				  ^client_id^"' type='"^level1_tp^"'>"^stanza_temp^"</presence>")
									else if subs = "to" then
										if level1_tp <> "unavailable" then this#send ("<presence xmlns='jabber:client' from='"^id^"'>"^presence_inst^"</presence>");
									with Not_found -> ()
								in
								List.iter broadcast contacts;
								(* send to temporary contacts *)
								if level1_tp = "unavailable" then begin 
									List.iter broadcast contacts_temp;
									contacts_temp <- []
									end;
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
							try begin this#send_to level1_to ("<message from='"^client_id^"' id='"^level1_id ^ 
														"' to='"^level1_to^"' type='chat' xml:lang='"^level1_la ^
														"'>" ^ stanza_temp  ^ "</message>");
								contents <- [];
								stanza_temp <- "";
								end
							with Not_found -> (); (*TODO: raise exception *)
							end

			| End_element (ns, name) when xml_parser#level = 2 ->
				let ((_, level1_name), _) = level1 in
				let ((_, level2_name), level2_att) = level2 in
				let level2_la = try (UTF8.encode (List.assoc xml_lang level2_att)) with Not_found -> "" in
				if level1_name = "presence" then 
					stanza_temp <- (stanza_temp ^ xml_parser#raw_string)
				else if level1_name = "message" then
					(* support multiple <body> stanza with distinct xml:lang *)
					if (List.mem_assoc (level2_la, level2_name) contents) then 
						() (*TODO: raise conflict exception *)
					else begin
						contents <- ((level2_la, level2_name), ([], [])) :: contents;
						stanza_temp <- (stanza_temp ^ xml_parser#raw_string)
						end
			| CharData s when xml_parser#level = 2 -> 
				let ((_, level1_name), _) = level1 in
				if level1_name = "presence" then chardata <- s
			| _ -> ()
		
		method stream_handler str = 
			try 
				match state with
				| Closed -> xml_parser#change_event_handler this#init_handler;
							xml_parser#parse str;
							return ()
				| Start -> 	xml_parser#change_event_handler this#start_handler;
							xml_parser#parse str;
							return ()
				| Negot ->	xml_parser#change_event_handler this#negot_handler;
							xml_parser#parse str;
							return ()
				| Connected ->	xml_parser#change_event_handler this#connect_handler;
							xml_parser#parse str;
							return ()
			with XML.Malformed_XML err_str 
				|XML.Invalid_XML err_str
				|XML.Restricted_XML err_str -> this#send_err err_str
				|XML.Unsupported_encoding -> this#send_err "Error: Unsupport encoding."

	end;;



let skt = socket PF_INET SOCK_STREAM 0;;
let () = bind skt (ADDR_INET (my_addr, 1112));;
        let rec dispatcher () = 
			listen skt 4;
			printl "server thread established" >>= fun () -> 
			accept skt >>= fun (con, caller_addr) -> 
			let handler = 
				printlf "client thread %d established" !counter >>= fun () -> 
				let inchan = of_fd input con in
				let stream = Lwt_io.read_lines inchan in
				let service_inst = new service con in
					Lwt_stream.iter_s service_inst#stream_handler stream
			in
				counter := !counter + 1;
				printlf "Client number %d" !counter >>= fun () ->
				printlf "Hashtable size %d" (Hashtbl.length global_info) >>= fun () ->
				join [handler; dispatcher ()]

let () = Lwt_main.run (dispatcher ())
