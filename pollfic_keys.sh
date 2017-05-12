mkdir keys/
openssl genrsa -out keys/private_key.pem 2048
openssl rsa -in keys/private_key.pem -pubout > keys/public_key.pem