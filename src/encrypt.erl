-module(encrypt).

-export([encrypt_with_pub/2,encrypt_with_priv/1,decrypt_with_pub/2,decrypt_with_priv/1]).

-define(KEYS_DIR, "../keys/").
-define(PRIVATE_KEY_PATH, "../keys/private_key.pem").

encrypt_with_pub(Msg, KeyFile) ->
	    { ok, PemBin2 } = file:read_file(KeyFile),
		[ RSAEntry2 ] = public_key:pem_decode(PemBin2),
		PublicKey = public_key:pem_entry_decode( RSAEntry2 ),
		public_key:encrypt_public(Msg, PublicKey).

encrypt_with_priv(Msg) -> 
		{ok, PemBin } = file:read_file(?PRIVATE_KEY_PATH),
		[ RSAEntry ] = public_key:pem_decode(PemBin),
		PrivateKey = public_key:pem_entry_decode( RSAEntry ),
		public_key:encrypt_private(Msg, PrivateKey ).
							
decrypt_with_pub(Msg, Keyfile) -> 
		{ ok, PemBin2 } = file:read_file(Keyfile),
		[ RSAEntry2 ] = public_key:pem_decode(PemBin2),
		PublicKey = public_key:pem_entry_decode( RSAEntry2 ),
		binary_to_term(public_key:decrypt_public(Msg, PublicKey)).


decrypt_with_priv(Msg) -> 
		{ok, PemBin } = file:read_file(?PRIVATE_KEY_PATH),
		[ RSAEntry ] = public_key:pem_decode(PemBin),
		PrivateKey = public_key:pem_entry_decode( RSAEntry ),
		binary_to_term(public_key:decrypt_private(Msg, PrivateKey)).