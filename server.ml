open Lwt (* provides bind and join *)
open Lwt_io
open Lwt_unix
open XML
open Unicode
open Rosterreader

type state = Closed | Start | Negot | Connected
(* create a useful local address *)
let my_name = Unix.gethostname ();;
let my_entry = Unix.gethostbyname my_name;;
let my_addr = my_entry.Unix.h_addr_list.(0);;
let global_info = Hashtbl.create 100;;
let counter = ref 0;;
(*Rosterreader.read_file ();;
let roster = Rosterreader.roster;;*)
class conn_info 
	?o_init:(o_i=Lwt_io.null) 
	?r_init:(r_i=[]) 
	?p_init:(p_i="") () =
    object
        val mutable outchan = o_i
        val mutable rosters = r_i
        val mutable presence = p_i
		method get_outchan = outchan
        method change_outchan o = outchan <- o;
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
		val mutable iq_id = ""
		val mutable iq_tp = ""
		val mutable query_ns = ""
		val s1 = "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\" xmlns=\"jabber:client\" to=\"ubuntu\" xml:lang=\"en\" xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">"
		val s2 = "<?xml version=\"1.0\"?>"
		val s3 = "<stream:stream to=\"example.com\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
		val s4 = "<message
        from=\"juliet@example.com\"
        to=\"romeo@example.net\"
        xml:lang=\"en\">
        <body>Art thou not Romeo, and a Montague?</body>
    </message>"
		val mutable state = Closed
		val xml_parser = new event_parser

		(* need to further implementation*)
		method gen_id = string_of_int !counter

		method send str = 
			let outchan = of_fd output connection in
			ignore_result (write_line outchan str)

		method send_to id str = 
			let outchan_to = (Hashtbl.find global_info id)#get_outchan in
				ignore_result (write_line outchan_to str)
		
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
				let conn_inst = new conn_info ~o_init:(of_fd output connection) () in
					Hashtbl.add global_info client_id conn_inst;
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

		method negot_handler = function
			| Start_element ((ns, name), att) when xml_parser#level = 1 ->
				if name = (UTF8.decode "iq") then
					begin
					iq_tp <- UTF8.encode (List.assoc ([], UTF8.decode "type") att);
					iq_id <- UTF8.encode (List.assoc ([], UTF8.decode "id") att)
					end
				else ()
			| Start_element ((ns, name), att) when xml_parser#level = 2 ->
				if name = UTF8.decode "query" then 
					query_ns <- UTF8.encode ns
				else ()
			| End_element (ns, name) when xml_parser#level = 2 ->
				if iq_tp = "get" then
					if name = UTF8.decode "query" then 
						if query_ns = "jabber:iq:auth" then
							this#send ("<iq type='result' id='" ^ iq_id ^ "'>
							   <query xmlns='jabber:iq:auth'>
							   <username/>
							   <digest/>
							   <resource/>
							   </query>
							   </iq>")
						else ()
					else ()
				else ()
			| _ -> ()
					
		
		
		method stream_handler str = match state with
			| Closed -> xml_parser#change_event_handler this#init_handler;
						xml_parser#parse str;
						return ()
			| Start -> 	xml_parser#change_event_handler this#start_handler;
						xml_parser#parse str;
						return ()
			| Negot ->	xml_parser#change_event_handler this#negot_handler;
						xml_parser#parse str;
						return ()
			| _		->	return ()

	end;;



let skt = Lwt_unix.socket PF_INET SOCK_STREAM 0;;
let () = Lwt_unix.bind skt (ADDR_INET (my_addr, 1112));;
        let rec dispatcher () = 
			Lwt_unix.listen skt 4;
			printl "server thread established" >>= fun () -> 
			Lwt_unix.accept skt >>= fun (con, caller_addr) -> 
			let handler = 
				printlf "client thread %d established" !counter >>= fun () -> 
				let inchan = of_fd input con in
				let stream = Lwt_io.read_lines inchan in
				let service_inst = new service con in
					Lwt_stream.iter_s service_inst#stream_handler stream
			in
				counter := !counter + 1;
				printlf "Client number %d" !counter >>= fun () ->
				join [handler; dispatcher ()]

let () = Lwt_main.run (dispatcher ())
