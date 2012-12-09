(***********************************************************************)
(*                                                                     *)
(*                      The Cryptokit library                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id: test.ml 60 2011-06-23 08:26:31Z stf $ *)

(* Test vectors *)

open Printf
open Cryptokit

(* Test harness *)

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d)\n" !function_tested test_number;
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end

(* Useful auxiliaries *)

let hex s = transform_string (Hexa.decode()) s
let tohex s = transform_string (Hexa.encode()) s

(* Test hex conversion first... *)
let _ =
  testing_function "Hex conversion";
  test 1 "6162636465666768696a6b6c6d6e6f70710a"
         (tohex "abcdefghijklmnopq\n");
  test 2 "abcdefghijklmnopq\n"
         (hex "616263 64656667 \n 68696a6b 6c6d6e6f\t70710a")

(* Basic ciphers and hashes *)

(* AES *)
let _ =
  testing_function "AES";
  let res = String.create 16 in
  let do_test key plain cipher testno1 testno2 =
    let c = new Block.aes_encrypt (hex key)
    and d = new Block.aes_decrypt (hex key) in
    let plain = hex plain
    and cipher = hex cipher in
    c#transform plain 0 res 0;  test testno1 res cipher;
    d#transform cipher 0 res 0; test testno2 res plain in
  do_test
    "000102030405060708090A0B0C0D0E0F"
    "00112233445566778899AABBCCDDEEFF"
    "69C4E0D86A7B0430D8CDB78070B4C55A"
    1 2;
  do_test
    "000102030405060708090A0B0C0D0E0F1011121314151617"
    "00112233445566778899AABBCCDDEEFF"
    "DDA97CA4864CDFE06EAF70A0EC0D7191"
    3 4;
  do_test
    "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F"
    "00112233445566778899AABBCCDDEEFF"
    "8EA2B7CA516745BFEAFC49904B496089"
    5 6

(* Blowfish *)

let _ =
  testing_function "Blowfish";
  let res = String.create 16 in
  let do_test key plain cipher testno =
    let c = new Block.blowfish_encrypt (hex key)
    and d = new Block.blowfish_decrypt (hex key) in
    let plain = hex plain
    and cipher = hex cipher in
    c#transform plain 0 res 0;
    d#transform cipher 0 res 8;
    test testno res (cipher ^ plain) in
  do_test "0000000000000000" "0000000000000000" "4EF997456198DD78" 1;
  do_test "FFFFFFFFFFFFFFFF" "FFFFFFFFFFFFFFFF" "51866FD5B85ECB8A" 2;
  do_test "3000000000000000" "1000000000000001" "7D856F9A613063F2" 3;
  do_test "1111111111111111" "1111111111111111" "2466DD878B963C9D" 4;
  do_test "0123456789ABCDEF" "1111111111111111" "61F9C3802281B096" 5;
  do_test "1111111111111111" "0123456789ABCDEF" "7D0CC630AFDA1EC7" 6;
  do_test "0000000000000000" "0000000000000000" "4EF997456198DD78" 7;
  do_test "FEDCBA9876543210" "0123456789ABCDEF" "0ACEAB0FC6A0A28D" 8;
  do_test "7CA110454A1A6E57" "01A1D6D039776742" "59C68245EB05282B" 9;
  do_test "0131D9619DC1376E" "5CD54CA83DEF57DA" "B1B8CC0B250F09A0" 10;
  do_test "07A1133E4A0B2686" "0248D43806F67172" "1730E5778BEA1DA4" 11;
  do_test "3849674C2602319E" "51454B582DDF440A" "A25E7856CF2651EB" 12;
  do_test "04B915BA43FEB5B6" "42FD443059577FA2" "353882B109CE8F1A" 13;
  do_test "0113B970FD34F2CE" "059B5E0851CF143A" "48F4D0884C379918" 14;
  do_test "0170F175468FB5E6" "0756D8E0774761D2" "432193B78951FC98" 15;
  do_test "43297FAD38E373FE" "762514B829BF486A" "13F04154D69D1AE5" 16;
  do_test "07A7137045DA2A16" "3BDD119049372802" "2EEDDA93FFD39C79" 17;
  do_test "04689104C2FD3B2F" "26955F6835AF609A" "D887E0393C2DA6E3" 18;
  do_test "37D06BB516CB7546" "164D5E404F275232" "5F99D04F5B163969" 19;
  do_test "1F08260D1AC2465E" "6B056E18759F5CCA" "4A057A3B24D3977B" 20;
  do_test "584023641ABA6176" "004BD6EF09176062" "452031C1E4FADA8E" 21;
  do_test "025816164629B007" "480D39006EE762F2" "7555AE39F59B87BD" 22;
  do_test "49793EBC79B3258F" "437540C8698F3CFA" "53C55F9CB49FC019" 23;
  do_test "4FB05E1515AB73A7" "072D43A077075292" "7A8E7BFA937E89A3" 24;
  do_test "49E95D6D4CA229BF" "02FE55778117F12A" "CF9C5D7A4986ADB5" 25;
  do_test "018310DC409B26D6" "1D9D5C5018F728C2" "D1ABB290658BC778" 26;
  do_test "1C587F1C13924FEF" "305532286D6F295A" "55CB3774D13EF201" 27;
  do_test "0101010101010101" "0123456789ABCDEF" "FA34EC4847B268B2" 28;
  do_test "1F1F1F1F0E0E0E0E" "0123456789ABCDEF" "A790795108EA3CAE" 29;
  do_test "E0FEE0FEF1FEF1FE" "0123456789ABCDEF" "C39E072D9FAC631D" 30;
  do_test "0000000000000000" "FFFFFFFFFFFFFFFF" "014933E0CDAFF6E4" 31;
  do_test "FFFFFFFFFFFFFFFF" "0000000000000000" "F21E9A77B71C49BC" 32;
  do_test "0123456789ABCDEF" "0000000000000000" "245946885754369A" 33;
  do_test "FEDCBA9876543210" "FFFFFFFFFFFFFFFF" "6B5C5A9C5D9E0A5A" 34

(* DES *)
let _ =
  testing_function "DES";
  let res = String.create 8 in
  let c = new Block.des_encrypt (hex "0123456789abcdef")
  and d = new Block.des_decrypt (hex "0123456789abcdef") in
  let plain = hex "0123456789abcde7"
  and cipher = hex "c95744256a5ed31d" in
  c#transform plain 0 res 0; test 1 res cipher;
  d#transform cipher 0 res 0; test 2 res plain;
  let rec iter n key input =
    if n <= 0 then key else begin
      let c = new Block.des_encrypt key in
      let t1 = String.create 8 in c#transform input 0 t1 0;
      let t2 = String.create 8 in c#transform t1 0 t2 0;
      let d = new Block.des_decrypt t2 in
      let t3 = String.create 8 in d#transform t1 0 t3 0;
      iter (n-1) t3 t1
    end in
  test 3 (iter 64 (hex "5555555555555555") (hex "ffffffffffffffff"))
         (hex "246e9db9c550381a")

(* Triple DES *)
let _ =
  testing_function "Triple DES";
  let res = String.create 8 in
  let do_test key plain cipher testno1 testno2 =
    let c = new Block.triple_des_encrypt (hex key)
    and d = new Block.triple_des_decrypt (hex key) in
    let plain = hex plain
    and cipher = hex cipher in
    c#transform plain 0 res 0; test testno1 res cipher;
    d#transform cipher 0 res 0; test testno2 res plain in
  do_test
    "0123456789abcdeffedcba9876543210"
    "0123456789abcde7"
    "7f1d0a77826b8aff"
    1 2;
  do_test
    "0123456789abcdef0123456789abcdef"
    "0123456789abcde7"
    "c95744256a5ed31d"
    3 4;
  do_test
    "0123456789abcdeffedcba987654321089abcdef01234567"
    "0123456789abcde7"
    "de0b7c06ae5e0ed5"
    5 6

(* ARCfour *)

let _ =
  testing_function "ARCfour";
  let do_test n1 n2 key input output =
    let key = hex key and input = hex input and output = hex output in
    let c = new Stream.arcfour key in
    let d = new Stream.arcfour key in
    let res = String.create (String.length input) in
    c#transform input 0 res 0 (String.length input);
    test n1 res output;
    d#transform output 0 res 0 (String.length output);
    test n2 res input in
  do_test 1 2 "0123456789abcdef" "0123456789abcdef" "75b7878099e0c596";
  do_test 3 4 "0123456789abcdef" "0000000000000000" "7494c2e7104b0879";
  do_test 5 6 "0000000000000000" "0000000000000000" "de188941a3375d3a";
  do_test 7 8 "ef012345" "00000000000000000000" "d6a141a7ec3c38dfbd61";
  let c2 = Cipher.arcfour "key" Cipher.Encrypt in
  c2#put_string (String.create 1024);
  test 9 c2#available_output 1024

(* Blowfish *)

let _ =
  testing_function "Blowfish";
  let testcnt = ref 0 in
  let res = String.create 8 in
  let do_test (key, plain, cipher) =
    let key = hex key and plain = hex plain and cipher = hex cipher in
    let c = new Block.blowfish_encrypt key
    and d = new Block.blowfish_decrypt key in
    c#transform plain 0 res 0;  incr testcnt; test !testcnt res cipher;
    d#transform cipher 0 res 0; incr testcnt; test !testcnt res plain in
  List.iter do_test [
    ("0000000000000000", "0000000000000000", "4EF997456198DD78");
    ("FFFFFFFFFFFFFFFF", "FFFFFFFFFFFFFFFF", "51866FD5B85ECB8A");
    ("3000000000000000", "1000000000000001", "7D856F9A613063F2");
    ("1111111111111111", "1111111111111111", "2466DD878B963C9D");
    ("0123456789ABCDEF", "1111111111111111", "61F9C3802281B096");
    ("1111111111111111", "0123456789ABCDEF", "7D0CC630AFDA1EC7");
    ("0000000000000000", "0000000000000000", "4EF997456198DD78");
    ("FEDCBA9876543210", "0123456789ABCDEF", "0ACEAB0FC6A0A28D");
    ("7CA110454A1A6E57", "01A1D6D039776742", "59C68245EB05282B");
    ("0131D9619DC1376E", "5CD54CA83DEF57DA", "B1B8CC0B250F09A0");
    ("07A1133E4A0B2686", "0248D43806F67172", "1730E5778BEA1DA4");
    ("3849674C2602319E", "51454B582DDF440A", "A25E7856CF2651EB");
    ("04B915BA43FEB5B6", "42FD443059577FA2", "353882B109CE8F1A");
    ("0113B970FD34F2CE", "059B5E0851CF143A", "48F4D0884C379918");
    ("0170F175468FB5E6", "0756D8E0774761D2", "432193B78951FC98");
    ("43297FAD38E373FE", "762514B829BF486A", "13F04154D69D1AE5");
    ("07A7137045DA2A16", "3BDD119049372802", "2EEDDA93FFD39C79");
    ("04689104C2FD3B2F", "26955F6835AF609A", "D887E0393C2DA6E3");
    ("37D06BB516CB7546", "164D5E404F275232", "5F99D04F5B163969");
    ("1F08260D1AC2465E", "6B056E18759F5CCA", "4A057A3B24D3977B");
    ("584023641ABA6176", "004BD6EF09176062", "452031C1E4FADA8E");
    ("025816164629B007", "480D39006EE762F2", "7555AE39F59B87BD");
    ("49793EBC79B3258F", "437540C8698F3CFA", "53C55F9CB49FC019");
    ("4FB05E1515AB73A7", "072D43A077075292", "7A8E7BFA937E89A3");
    ("49E95D6D4CA229BF", "02FE55778117F12A", "CF9C5D7A4986ADB5");
    ("018310DC409B26D6", "1D9D5C5018F728C2", "D1ABB290658BC778");
    ("1C587F1C13924FEF", "305532286D6F295A", "55CB3774D13EF201");
    ("0101010101010101", "0123456789ABCDEF", "FA34EC4847B268B2");
    ("1F1F1F1F0E0E0E0E", "0123456789ABCDEF", "A790795108EA3CAE");
    ("E0FEE0FEF1FEF1FE", "0123456789ABCDEF", "C39E072D9FAC631D");
    ("0000000000000000", "FFFFFFFFFFFFFFFF", "014933E0CDAFF6E4");
    ("FFFFFFFFFFFFFFFF", "0000000000000000", "F21E9A77B71C49BC");
    ("0123456789ABCDEF", "0000000000000000", "245946885754369A");
    ("FEDCBA9876543210", "FFFFFFFFFFFFFFFF", "6B5C5A9C5D9E0A5A")
  ]

(* SHA-1 *)
let _ =
  testing_function "SHA-1";
  let hash s = hash_string (Hash.sha1()) s in
  test 1 (hash "") (hex "da39a3ee5e6b4b0d3255bfef95601890afd80709");
  test 2 (hash "a") (hex "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8");
  test 3 (hash "abc") (hex "a9993e364706816aba3e25717850c26c9cd0d89d");
  test 4 (hash "abcdefghijklmnopqrstuvwxyz")
         (hex "32d10c7b8cf96570ca04ce37f2a19d84240d3a89");
  test 5 (hash "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
         (hex "84983E441C3BD26EBAAE4AA1F95129E5E54670F1");
  test 6 (hash (String.make 1000000 'a'))
         (hex "34AA973CD4C4DAA4F61EEB2BDBAD27316534016F")

(* SHA-256 *)
let _ =
  testing_function "SHA-256";
  let hash s = hash_string (Hash.sha256()) s in
  test 1 (hash "")
    (hex "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
  test 2 (hash "abc")
    (hex "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
  test 3 (hash "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    (hex "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1");
  test 4 (hash (String.make 1000000 'a'))
    (hex "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")

(* SHA-512 *)
let _ =
  testing_function "SHA-512";
  let hash s = hash_string (Hash.sha512()) s in
  test 1 (hash "")
    (hex "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e");
  test 2 (hash "abc")
    (hex "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f");
  test 3 (hash "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    (hex "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445");
  test 4 (hash (String.make 1000000 'a'))
    (hex "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b")

(* RIPEMD-160 *)
let _ =
  testing_function "RIPEMD-160";
  let hash s = hash_string (Hash.ripemd160()) s in
  test 1 (hash "")
    (hex "9c1185a5c5e9fc54612808977ee8f548b2258d31");
  test 2 (hash "a")
    (hex "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe");
  test 3 (hash "abc")
    (hex "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc");
  test 4 (hash "message digest")
    (hex "5d0689ef49d2fae572b881b123a85ffa21595f36");
  test 5 (hash "abcdefghijklmnopqrstuvwxyz")
    (hex "f71c27109c692c1b56bbdceb5b9d2865b3708dbc");
  test 6 (hash "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    (hex "12a053384a9c0c88e405a06c27dcf49ada62eb2b");
  test 7 (hash "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
    (hex "b0e20b6e3116640286ed3a87a5713079b21f5189");
  test 8 (hash "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
    (hex "9b752e45573d4b39f4dbd3323cab82bf63326bfb");
  test 9 (hash (String.make 1000000 'a'))
    (hex "52783243c1697bdbe16d37f97f68f08325dc1528")

(* MD5 *)
let _ =
  testing_function "MD5";
  let hash s = hash_string (Hash.md5()) s in
  test 1 (hash "") (hex "D41D8CD98F00B204E9800998ECF8427E");
  test 2 (hash "a") (hex "0CC175B9C0F1B6A831C399E269772661");
  test 3 (hash "abc") (hex "900150983CD24FB0D6963F7D28E17F72");
  test 4 (hash "message digest")
         (hex "F96B697D7CB7938D525A2F31AAF161D0")

(* Chaining modes *)

open Cipher

let some_key = hex "0123456789abcdef"

let test_enc_dec testno cipher cleartext =
  let enc = cipher some_key Encrypt and dec = cipher some_key Decrypt in
  test testno (transform_string dec (transform_string enc cleartext))
              cleartext

let _ =
  testing_function "ECB";
  test_enc_dec 1 (des ~mode:ECB) "abcdefgh";
  test_enc_dec 2 (des ~mode:ECB) "abcdefgh01234567";
  test_enc_dec 3 (des ~mode:ECB ~pad:Padding.length) "0123456789";
  test_enc_dec 4 (des ~mode:ECB ~pad:Padding.length) "abcdefghijklmnopqrstuvwxyz";
  test_enc_dec 5 (des ~mode:ECB ~pad:Padding._8000) "0123456789";
  test_enc_dec 6 (des ~mode:ECB ~pad:Padding._8000) "abcdefghijklmnopqrstuvwxyz"

let _ =
  testing_function "CBC";
  test_enc_dec 1 (des ~mode:CBC) "abcdefgh";
  test_enc_dec 2 (des ~mode:CBC) "abcdefgh01234567";
  test_enc_dec 3 (des ~mode:CBC ~pad:Padding.length) "0123456789";
  test_enc_dec 4 (des ~mode:CBC ~pad:Padding.length) "abcdefghijklmnopqrstuvwxyz";
  test_enc_dec 5 (des ~mode:CBC ~pad:Padding.length ~iv:"#@#@#@#@") "0123456789";
  test_enc_dec 6 (des ~mode:CBC ~pad:Padding.length ~iv:"ABCDEFGH") "abcdefghijklmnopqrstuvwxyz"

let _ =
  testing_function "CFB 1";
  test_enc_dec 1 (des ~mode:(CFB 1)) "ab";
  test_enc_dec 2 (des ~mode:(CFB 1)) "abcd";
  test_enc_dec 3 (des ~mode:(CFB 1)) "abcdefgh01234567";
  test_enc_dec 4 (des ~mode:(CFB 1)) "abcdefghijklmnopqrstuvwxyz";
  test_enc_dec 5 (des ~mode:(CFB 1) ~iv:"#@#@#@#@") "abcdefghijklmnopqrstuvwxyz"

let _ =
  testing_function "CFB 4";
  test_enc_dec 1 (des ~mode:(CFB 4)) "abcd";
  test_enc_dec 2 (des ~mode:(CFB 4)) "abcdefgh01234567";
  test_enc_dec 3 (des ~mode:(CFB 4) ~pad:Padding._8000) "abcdefghijklmnopqrstuvwxyz"

let _ =
  testing_function "OFB 1";
  test_enc_dec 1 (des ~mode:(OFB 1)) "ab";
  test_enc_dec 2 (des ~mode:(OFB 1)) "abcd";
  test_enc_dec 3 (des ~mode:(OFB 1)) "abcdefgh01234567";
  test_enc_dec 4 (des ~mode:(OFB 1)) "abcdefghijklmnopqrstuvwxyz";
  test_enc_dec 5 (des ~mode:(OFB 1) ~iv:"#@#@#@#@") "abcdefghijklmnopqrstuvwxyz"

let _ =
  testing_function "OFB 8";
  test_enc_dec 1 (des ~mode:(OFB 8)) "abcdefgh";
  test_enc_dec 2 (des ~mode:(OFB 8)) "abcdefgh01234567";
  test_enc_dec 3 (des ~mode:(OFB 8) ~pad:Padding._8000) "abcdefghijklmnopqrstuvwxyz"

(* HMAC-SHA256 *)

let _ =
  testing_function "HMAC-SHA256";
  List.iter
    (fun (testno, hexkey, msg, hexhash) ->
      test testno
        (hash_string (MAC.hmac_sha256 (hex hexkey)) msg)
        (hex hexhash))
[
(1,
 "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
 "abc",
 "a21b1f5d4cf4f73a4dd939750f7a066a7f98cc131cb16a6692759021cfab8181");
(2,
 "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
 "104fdc1257328f08184ba73131c53caee698e36119421149ea8c712456697d30");
(3,
 "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
 "470305fc7e40fe34d3eeb3e773d95aab73acf0fd060447a5eb4595bf33a9d1a3");
(4,
 "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b",
 "Hi There",
 "198a607eb44bfbc69903a0f1cf2bbdc5ba0aa3f3d9ae3c1c7a3b1696a0b68cf7");
(5,
 "4a656665", (* "Jefe" *)
 "what do ya want for nothing?",
 "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");
(6,
 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
 "\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd\xdd",
 "cdcb1220d1ecccea91e53aba3092f962e549fe6ce9ed7fdc43191fbde45c30b0");
(7,
 "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425",
 "\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd\xcd",
 "d4633c17f6fb8d744c66dee0f8f074556ec4af55ef07998541468eb49bd2e917");
(8,
 "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c",
 "Test With Truncation",
 "7546af01841fc09b1ab9c3749a5f1c17d4f589668a587b2700a9c97c1193cf42");
(9,
 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
 "Test Using Larger Than Block-Size Key - Hash Key First",
 "6953025ed96f0c09f80a96f78e6538dbe2e7b820e3dd970e7ddd39091b32352f");
(10,
 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
 "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data",
 "6355ac22e890d0a3c8481a5ca4825bc884d3e7a1ff98a2fc2ac7d8e064c3b2e6")
]

(* HMAC-MD5 *)

let _ =
  testing_function "HMAC-MD5";
  test 1
    (hash_string (MAC.hmac_md5 (hex "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
                 "Hi There")
    (hex "9294727a3638bb1c13f48ef8158bfc9d");
  test 2
    (hash_string (MAC.hmac_md5 "Jefe")
                 "what do ya want for nothing?")
    (hex "750c783e6ab0b503eaa86e310a5db738");
  test 3
    (hash_string (MAC.hmac_md5 (hex "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
                 (String.make 50 '\221'))
    (hex "56be34521d144c88dbb8c733f0e8b3f6")

(* RSA *)

let some_rsa_key = {
  RSA.size = 512;
  RSA.n = hex "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c0764797b8bec8972a0ed8c90a8c334dd049add0222c09d20be0a79e338910bcae422060906ae0221de3f3fc747ccf98aecc85d6edc52d93d5b7396776160525";
  RSA.e = hex "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010001";
  RSA.d = hex "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001ae36b7522f66487d9f4610d1550290ac202c929bedc7032cc3e02acf37e3ebc1f866ee7ef7a0868d23ae2b184c1abd6d4db8ea9bec046bd82803727f2888701";
  RSA.p = hex "0000000000000000000000000000000000000000000000000000000000000000df02b615fe15928f41b02b586b51c2c02260ca396818ca4cba60bb892465be35";
  RSA.q = hex "0000000000000000000000000000000000000000000000000000000000000000dceeb60d543518b4ac74834a0546c507f2e91e389a87e2f2becc6f8c67d1c931";
  RSA.dp = hex "000000000000000000000000000000000000000000000000000000000000000059487e99e375c38d732112d97d6de8687fdafc5b6b5fb16e7297d3bd1e435599";
  RSA.dq = hex "000000000000000000000000000000000000000000000000000000000000000061b550de6437774db0577718ed6c770724eee466b43114b5b69c43591d313281";
  RSA.qinv = hex "0000000000000000000000000000000000000000000000000000000000000000744c79c4b9bea97c25e563c9407a2d09b57358afe09af67d71f8198cb7c956b8"
}

let some_msg = "Supercalifragilistusexpialidolcius"

let test_same_message testno msg1 msg2 =
  test testno msg1 (String.sub msg2 (String.length msg2 - String.length msg1)
                                    (String.length msg1))

let _ =
  testing_function "RSA";
  (* Signature, no CRT *)
  test_same_message 1 some_msg
    (RSA.unwrap_signature some_rsa_key (RSA.sign some_rsa_key some_msg));
  (* Signature, CRT *)
  test_same_message 2 some_msg
    (RSA.unwrap_signature some_rsa_key (RSA.sign_CRT some_rsa_key some_msg));
  (* Encryption, no CRT *)
  test_same_message 3 some_msg
    (RSA.decrypt some_rsa_key (RSA.encrypt some_rsa_key some_msg));
  (* Encryption, CRT *)
  test_same_message 4 some_msg
    (RSA.decrypt_CRT some_rsa_key (RSA.encrypt some_rsa_key some_msg));
  (* Same, with a home-made key *)
  let prng =
    Random.pseudo_rng (hex "5b5e50dc5b6eaf5346eba8244e5666ac4dcd5409") in
  let key = RSA.new_key ~rng:prng 1024 in
  test_same_message 5 some_msg
    (RSA.unwrap_signature key (RSA.sign key some_msg));
  test_same_message 6 some_msg
    (RSA.unwrap_signature key (RSA.sign_CRT key some_msg));
  test_same_message 7 some_msg
    (RSA.decrypt key (RSA.encrypt key some_msg));
  test_same_message 8 some_msg
    (RSA.decrypt_CRT key (RSA.encrypt key some_msg));
  (* Same, with a home-made key of fixed public exponent *)
  let key = RSA.new_key ~rng:prng ~e:65537 1024 in
  test_same_message 9 some_msg
    (RSA.unwrap_signature key (RSA.sign key some_msg));
  test_same_message 10 some_msg
    (RSA.unwrap_signature key (RSA.sign_CRT key some_msg));
  test_same_message 11 some_msg
    (RSA.decrypt key (RSA.encrypt key some_msg));
  test_same_message 12 some_msg
    (RSA.decrypt_CRT key (RSA.encrypt key some_msg))

(* Diffie-Hellman *)

let _ =
  testing_function "Diffie-Hellman";
  let prng =
    Random.pseudo_rng (hex "5b5e50dc5b6eaf5346eba8244e5666ac4dcd5409") in
  let param = DH.new_parameters ~rng:prng 1024 in
  let ps1 = DH.private_secret ~rng:prng param
  and ps2 = DH.private_secret ~rng:prng param in
  let msg1 = DH.message param ps1
  and msg2 = DH.message param ps2 in
  let ss1 = DH.shared_secret param ps1 msg2
  and ss2 = DH.shared_secret param ps2 msg1 in
  test 1 ss1 ss2

(* Base64 encoding *)

let _ =
  testing_function "Base64";
  test 1
"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNr
IGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZv
eCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZy4K
" (transform_string (Base64.encode_multiline())
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
");
  test 2
"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNr
IGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZv
eCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZy4uCg==
" (transform_string (Base64.encode_multiline())
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog..
");
  test 3
"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNr
IGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZv
eCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZy4uLgo=
" (transform_string (Base64.encode_multiline())
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog...
");
  test 4
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
"
 (transform_string (Base64.decode())
"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNr
IGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZv
eCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZy4K
");
  test 5
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog..
"
 (transform_string (Base64.decode())
"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNr
IGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZv
eCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZy4uCg==
");
  test 6
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog...
"
 (transform_string (Base64.decode())
"VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNr
IGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZv
eCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4KVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZy4uLgo=
");
  let binarytext = String.create 256 in
  for i = 0 to 255 do binarytext.[i] <- Char.chr i done;
  test 7 binarytext
    (transform_string (Base64.decode())
      (transform_string (Base64.encode_compact()) binarytext))

(* Compression *)

let _ =
  testing_function "Zlib compression";
  let text =
"The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
" in
  try
    test 1 text (transform_string (Zlib.uncompress()) (transform_string (Zlib.compress()) text));
    let c = Zlib.compress() and u = Zlib.uncompress() in
    c#put_string text; c#flush; u#put_string c#get_string; u#flush;
    test 2 text u#get_string;
    c#put_string text; c#finish; u#put_string c#get_string; u#finish;
    test 3 text u#get_string
  with Error Compression_not_supported ->
    printf " (not supported)"

(* End of tests *)

let _ =
  print_newline();
  if !error_occurred then begin
    printf "********* TEST FAILED ***********\n";
    exit 2
  end else begin
    printf "All tests successful.\n";
    exit 0
  end
