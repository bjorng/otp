{ok, Bin} = file:read_file("BasicOcspResponse.data").

%% decode everything
public_key:der_decode('BasicOCSPResponse', Bin).

%% decode but not signature
'OTP-PUB-KEY':decode_BasicOCSPResponse_exclusive1(Bin).

%% decode but not certs - 'parts' crashes with function_clause: 'OTP-PUB-KEY':decode_tag_and_length({asn1,"tag failure"
'OTP-PUB-KEY':decode_BasicOCSPResponse_exclusive2(Bin).

%% decode but not certs - 'undecoded' returns error but shouldn't?
'OTP-PUB-KEY':decode_BasicOCSPResponse_exclusive3(Bin).
