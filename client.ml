open Lwt
open Lwt_io
open Lwt_unix
open XML
open Unicode

type state = Closed | Start | Negot | Connected
class handler init_ic init_oc =
    object(this)
		val xml_lang = (UTF8.decode "http://www.w3.org/XML/1998/namespace", [108; 97; 110; 103])
		val client_id = "android@ubuntu"
		val inchan = init_ic
		val outchan = init_oc
		val mutable state = Closed
		val mutable stream_id = ""
		val mutable server_lang = None
		val mutable server_xmpp_version = ""
		val mutable server_id = ""
		val xml_parser = new event_parser

		method send str =
            ignore_result (write_line outchan str)

		(* TODO:need implememtation *)
		method send_err str = 
			printl str

		method init_handler = function
            | Start_document -> state <- Start
            | _              -> ()

		method start_handler = function
            | Start_element ((ns, name), att)  ->
                stream_id <- begin try
                        UTF8.encode (List.assoc ([], [105; 100]) att)
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
                        UTF8.encode (List.assoc ([], [102; 114; 111; 109]) att)
                    with Not_found -> ""
                    end;
(* TODO: add conditions here!!!!!!*)
                    state <- Negot;
					print_string "negot"
				| _    -> ()
(*
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
*)

		method stream_handler str =
            try
                match state with
                | Closed -> xml_parser#change_event_handler this#init_handler;
                            xml_parser#parse str;
                            return ()
                | Start ->  xml_parser#change_event_handler this#start_handler;
                            xml_parser#parse str;
                            return ()
				| Negot -> xml_parser#parse str;
							return ()
(*
                | Negot ->  xml_parser#change_event_handler this#negot_handler;
                            xml_parser#parse str;
                            return ()
                | Connected ->  xml_parser#change_event_handler this#connect_handler;
                            xml_parser#parse str;
                            return ()
*)
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
		write_line oc ("<?xml version=\"1.0\"?>") >>= fun () ->
		write_line oc ("<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\" xmlns=\"jabber:client\" to=\""^ Sys.argv.(1)  ^"\" xml:lang=\"en\" xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">") >>= fun () ->
		let receive_thread = 
			let stream = Lwt_io.read_lines ic in
			let handler_inst = new handler ic oc in
        	    Lwt_stream.iter_s handler_inst#stream_handler stream in
		let rec send_thread () = 
			read_line Lwt_io.stdin >>= fun input ->
			write_line oc input >>= fun () ->
			join [send_thread ()]
		in
			join [receive_thread; send_thread ()]

let () = Lwt_main.run (client_thread)
