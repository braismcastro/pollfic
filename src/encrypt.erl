-module(encrypt).
%%MÓDULO DE ENCRIPTACION
%Permite encryptar con claves publicas y privadas, 
%La entrada para encryptar y desencriptar debe ser un binario
%La salida de encriptar es un binario
%La salida de desencriptar es un término de erlang

-export([encrypt_with_pub/2,encrypt_with_priv/1,decrypt_with_pub/2,decrypt_with_priv/1]).

-define(KEYS_DIR, "../keys/").
-define(PRIVATE_KEY_PATH, "../keys/private_key.pem").



%Entrada:	Mensaje en binario
%			Path de la clave publica
%Salida:	Mensaje encriptado
encrypt_with_pub(Msg, KeyFile) ->
	    { ok, PemBin2 } = file:read_file(KeyFile),
		[ RSAEntry2 ] = public_key:pem_decode(PemBin2),
		PublicKey = public_key:pem_entry_decode( RSAEntry2 ),
		public_key:encrypt_public(Msg, PublicKey).

%Entrada:	Mensaje en binario
%Salida:	Mensaje encriptado
encrypt_with_priv(Msg) -> 
		{ok, PemBin } = file:read_file(?PRIVATE_KEY_PATH),
		[ RSAEntry ] = public_key:pem_decode(PemBin),
		PrivateKey = public_key:pem_entry_decode( RSAEntry ),
		public_key:encrypt_private(Msg, PrivateKey ).

%Entrada:	Mensaje encriptado
%			Path de la clave publica
%Salida:	Mensaje desencriptado como termino de erlang							
decrypt_with_pub(Msg, Keyfile) -> 
		{ ok, PemBin2 } = file:read_file(Keyfile),
		[ RSAEntry2 ] = public_key:pem_decode(PemBin2),
		PublicKey = public_key:pem_entry_decode( RSAEntry2 ),
		binary_to_term(public_key:decrypt_public(Msg, PublicKey)).

%Entrada:	Mensaje encriptado
%Salida:	Mensaje desencriptado como termino de erlang
decrypt_with_priv(Msg) -> 
		{ok, PemBin } = file:read_file(?PRIVATE_KEY_PATH),
		[ RSAEntry ] = public_key:pem_decode(PemBin),
		PrivateKey = public_key:pem_entry_decode( RSAEntry ),
		binary_to_term(public_key:decrypt_private(Msg, PrivateKey)).