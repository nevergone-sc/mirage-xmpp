open Unix;;
open String;;


let parser_pointer = ref "";;
let t_l = 5;;
let roster = Hashtbl.create 10;; 

let rec str_to_list sep str = 
	try (
		let p = index str sep in
		let s = sub str 0 p in
			s :: (str_to_list sep (sub str (p+1) (String.length str-p-1)))
	) with Not_found  -> [sub str 0 (String.length str)]

let filler str = 
	let jid_s = 0 in
	let jid_e = index_from str jid_s ' ' in
	let name_s = (jid_e + 1) in
	let name_e = index_from str name_s ' ' in
	let sub_s = (name_e + 1) in
	let sub_e = index_from str sub_s ' ' in
	let str_groups = sub str (sub_e + 1) ((String.length str) - (sub_e + 1)) in
		(sub str jid_s (jid_e - jid_s),
		 sub str name_s (name_e - name_s),
		 sub str sub_s (sub_e - sub_s),
		 str_to_list ' ' str_groups) 


let roster_parser str =
	let title = String.sub str 0 t_l in
		if title = "jid =" then
			begin
			parser_pointer := String.sub str t_l ((String.length str)-t_l);
			Hashtbl.add roster !parser_pointer (ref [])
			end
		else (*title = "item=" then*)
			begin
			let content = String.sub str t_l ((String.length str) - t_l) in
			let item = Hashtbl.find roster !parser_pointer in
				item := (filler content) :: !item 
			end

let read_file () = 
	let file = openfile "roster.txt" [O_RDONLY] 0o640 in
	let inchan = in_channel_of_descr file in
	try
		while true; do
    		roster_parser (input_line inchan)
		done; 
	with End_of_file ->
		close_in inchan

let client_id = "nevergone";;
let str_iq = ref ("<iq to='" ^ client_id ^ "' type='result' id='" ^ "iq_id" ^ "'><query xmlns='jabber:iq:roster'>");; 
let test () = 
	let contacts = Hashtbl.find roster client_id in
	let list_iter (jid, name, subs, groups) = 
		let rec str_groups = function
			| []   -> ""
			| g :: gs -> ("<group>" ^ g ^ "</group>" ^ (str_groups gs))
		in
			str_iq := (!str_iq ^ "<item jid='" ^ jid ^ 
						"' name='" ^ name ^ 
						"' subscription='" ^ subs ^ "'>" ^
						str_groups groups ^ "</item>")
	in
		List.iter list_iter !contacts
