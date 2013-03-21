open XML;;
open Unicode;;

class service =
	object(this) 
		val s1 = "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\" xmlns=\"jabber:client\" to=\"ubuntu\" xml:lang=\"en\" xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">"
		val s2 = "<?xml version=\"1.0\"?>"
		val s3 = "<stream:stream to=\"example.com\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\">"
		val s4 = "<message
        from=\"juliet@example.com\"
        to=\"romeo@example.net\"
        xml:lang=\"en\">
        <body>Art thou not Romeo, and a Montague?</body>
    </message>"
		val mutable state = 0
		val xml_parser = new event_parser

		method private start_handler e = 
			match e with
			| Start_document -> ()
			| End_document   -> ()
			| Start_element ((ns_u, lp_u), l_u)  -> 
				print_string "Starte\n";
				let	ns_s = UTF8.encode ns_u
				and lp_s = UTF8.encode lp_u in
					print_string ("namespace: " ^ ns_s ^ "\n");
					print_string ("localpart: " ^ lp_s ^ "\n");
					let print_attr ((attr_ns_u, attr_u), val_u) =
						print_string ((UTF8.encode attr_ns_u) ^ ", " ^ (UTF8.encode attr_u) ^ ", " ^ (UTF8.encode val_u) ^ "\n")
					in
						List.iter print_attr l_u
			| End_element   (ns_u, lp_u) -> 
				print_string "Ende\n";
				let	ns_s = UTF8.encode ns_u
				and lp_s = UTF8.encode lp_u in
					print_string ("namespace: " ^ ns_s ^ "\n");
					print_string ("localpart: " ^ lp_s ^ "\n")
			| CharData data_u	 -> 
				print_string "Char\n";
				let data_s = UTF8.encode data_u in
					print_string (data_s ^ "\n")
		
		
		method stream_handler str = match state with
			| 0 -> 	xml_parser#change_event_handler this#start_handler;
					xml_parser#parse str;
					state <- state + 1
			| _ ->	()

	end;;
