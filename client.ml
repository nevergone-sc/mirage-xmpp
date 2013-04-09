open Lwt
open Lwt_io
open Lwt_unix
open XML
open Unicode
open Cryptokit

let password = "123";;

type state = Closed | Start | Negot | Connected
class handler init_ic init_oc =
    object(this)
		val xml_lang = (UTF8.decode "http://www.w3.org/XML/1998/namespace", [108; 97; 110; 103])
		val username = "nevergone"
		val client_id = "nevergone@ubuntu"
		val inchan = init_ic
		val outchan = init_oc
		val mutable state = Closed
		val mutable stream_id = ""
		val mutable server_lang = None
		val mutable server_xmpp_version = ""
		val mutable server_id = ""
		val xml_parser = new event_parser

		val mutable level1 = (("",""), [])
        val mutable level2 = (("",""), [])
        val mutable level3 = (("",""), [])
		val mutable stanza_temp = "" 
		val mutable chardata = []
		val mutable contents = []

		method send str =
			print_string str;
            ignore_result (write_line outchan str)

		(* TODO:need implememtation *)
		method send_err str = 
			print_string str

		method tags_refresh =
            level1 <- (("",""), []);
            level2 <- (("",""), []);
            level3 <- (("",""), []);
            chardata <- [];
            contents <- []

		method init_handler = function
            | Start_document -> state <- Start
            | _              -> ()

		method start_handler = function
            | Start_element ((ns, name), att)  ->
				if name = UTF8.decode "stream" then begin
 	               stream_id <- begin try
   						UTF8.encode (List.assoc ([], UTF8.decode "id") att)
        	            with Not_found -> ""
            	        end;
					server_lang <- begin try
       					Some (UTF8.encode (List.assoc xml_lang att))
            	        with Not_found -> None
						end;
					server_xmpp_version <- begin try
						UTF8.encode (List.assoc ([], UTF8.decode "version") att)
                    	with Not_found -> "0.9"
                    	end;
                	server_id <- begin try
                        UTF8.encode (List.assoc ([], UTF8.decode "from") att)
                    	with Not_found -> ""
                    	end;
(* TODO: add conditions here!!!!!!*)
				if stream_id <> "" then
					this#send ("<iq type='get' to='"^server_id^"' id='auth_1'>
								<query xmlns='jabber:iq:auth'>
								<username>"^username^"</username>
								</query></iq>");
					state <- Negot
				end
			| _    -> ()


		method negot_handler = function
            | Start_element ((ns, name), att) when xml_parser#level = 1 ->
                level1 <- ((UTF8.encode ns, UTF8.encode name), att)
            | Start_element ((ns, name), att) when xml_parser#level = 2 ->
                level2 <- ((UTF8.encode ns, UTF8.encode name), att)
            | End_element (ns, name) when xml_parser#level = 1 ->
                let ((_, _), att) = level1 in
                let ((level2_ns, level2_name), _) = level2 in
                let level1_tp = try (UTF8.encode (List.assoc ([], UTF8.decode "type") att)) 
								with Not_found -> ""
                and level1_id = try (UTF8.encode (List.assoc ([], UTF8.decode "id") att))
                                with Not_found -> "" (* TODO:should raise exception here*) in
                if name = UTF8.decode "iq" then
                    if level1_tp = "result" then
                        if level2_name = "query" && level2_ns = "jabber:iq:auth" then
							begin
               	            this#send ("<iq type='set' id='auth_2'>
							   	<query xmlns='jabber:iq:auth'>
								"^stanza_temp^"
                       	   	   	</query></iq>");
							stanza_temp <- "";
							level3 <- (("", ""), [])
							end
                        else if level2 = (("", ""), []) && level3 = (("", ""), []) then begin
							state <- Connected;
							(*roster query, not important*)
							this#send "<iq xmlns='jabber:client' type='get' id='aad1a'>
									   <query xmlns='jabber:iq:roster'/></iq>";
							this#send "<presence xmlns='jabber:client'>
									   <priority>5</priority>
									   <c xmlns='http://jabber.org/protocol/caps' node='http://psi-dev.googlecode.com/caps' ver='0.15' ext='ep-notify-2 html sxe whiteboard'/>
									   </presence>"
							end
                    else ()
                else ();
				this#tags_refresh
(*
			| End_element (ns, name) when xml_parser#level = 2 ->
                let ((_, _), att) = level1 in
                let ((level2_ns, _), _) = level2 in
                let level1_tp = try (UTF8.encode (List.assoc ([], UTF8.decode "type") att)) 
								with Not_found -> ""
                and level1_id = try (UTF8.encode (List.assoc ([], UTF8.decode "id") att))
                                with Not_found -> "" (* TODO:should raise exception here*) in
                if name = UTF8.decode "query" then
                    if level1_tp = "result" then
                        if level2_ns = "jabber:iq:auth" then
							if level3 = (("", ""), []) then
                                state <- Connected
                            else
                                begin
                                this#send ("<iq type='set' id='auth_2'>
                                    <query xmlns='jabber:iq:auth'>
                                    "^stanza_temp^"
                                    </query></iq>");
                                stanza_temp <- "";
                                end
                        else ()
*)
            | Start_element ((ns, name), att) when xml_parser#level = 3 ->
				let name_str = UTF8.encode name in 
                level3 <- ((UTF8.encode ns, name_str), att);
				if name_str = "username" then
					stanza_temp <- stanza_temp^"<username>"^username^"</username>\n"
				else if name_str = "resource" then
					stanza_temp <- stanza_temp^"<resource>ubuntu</resource>\n"
				else if name_str = "digest" then begin
					let sha1 = Hash.sha1 () in
					let hex = Hexa.encode () in
					let str = transform_string hex (hash_string sha1 (stream_id ^ password)) in
					stanza_temp <- stanza_temp^"<digest>" ^ str  ^ "</digest>\n"
					end
            | _ -> ()

		method connect_handler = function
			| _ -> ()

		method stream_handler str =
            try
                match state with
                | Closed -> xml_parser#change_event_handler this#init_handler;
                            xml_parser#parse str;
                | Start ->  xml_parser#change_event_handler this#start_handler;
                            xml_parser#parse str;
				| Negot ->  xml_parser#change_event_handler this#negot_handler;
							xml_parser#parse str;
                | Connected ->  xml_parser#change_event_handler this#connect_handler;
                            xml_parser#parse str;
            with XML.Malformed_XML err_str
                |XML.Invalid_XML err_str
                |XML.Restricted_XML err_str -> this#send_err err_str
                |XML.Unsupported_encoding -> this#send_err "Error: Unsupport encoding."

	end;;

let client_thread = 
	let server_addr = Unix.inet_addr_of_string Sys.argv.(1) in
	let port = int_of_string Sys.argv.(2) in
	let sktaddr = ADDR_INET (server_addr, port) in
		Lwt_io.open_connection sktaddr >>= fun (ic, oc) ->
(*
		let rec receive_thread () =
			read_line ic >>= fun input ->
			printl input >>= fun () ->
			flush Lwt_io.stdout >>= fun () ->
			join [receive_thread ()] in
*)
		let rec receive_thread () = 
			let temp_str = ref "" in
			write_line oc ("<?xml version='1.0'?>") >>= fun () ->
			write_line oc ("<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='0.9' xmlns='jabber:client' to='ubuntu' xml:lang='en' xmlns:xml='http://www.w3.org/XML/1998/namespace'>") >>= fun () ->
			let handler_inst = new handler ic oc in
			let stream = Lwt_io.read_chars ic in
			let char_handler c = 
				temp_str := !temp_str ^ (String.make 1 c);
				if c = '>' then begin
					handler_inst#stream_handler !temp_str;
					temp_str := ""
					end;
				return ()
			in
				Lwt_stream.iter_s char_handler stream
		in
		let rec send_thread () = 
			read_line Lwt_io.stdin >>= fun input ->
			Lwt_io.write_line oc input >>= fun () ->
			join [send_thread ()]
		in
			join [receive_thread (); send_thread ()]

let () = Lwt_main.run (client_thread)
