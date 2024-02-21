### basic

`cabal run -v0 exe:ft_turing -- tests/resources/valid/unary_mul.json '111*11='`

### weird

`cabal run -v0 exe:ft_turing -- tests/resources/ambiguous/buggy_and.json '1001&0101='`

### universal_turing_machine.json

`cabal run -v0 exe:ft_turing -- tests/resources/valid/utm4.json 'A[;AbAb+;AcBb+;BbBb+;BdCa-;Cb.a+]!bbbcbbd'`

`cabal run -v0 exe:ft_turing -- tests/resources/valid/utm4.json 'A[;AbBb+;Aa.d+;BbAb+;Ba.c+]!bbbbb'`

`cabal run -v0 exe:ft_turing -- tests/resources/valid/utm10.json 'A[;AbBd+;AcCe+;Aa.g-;AdHb+;AeHc+;BbBb+;BcBc+;BdDd-;BeDe-;BaDa-;CbCb+;CcCc+;CdEd-;CeEe-;CaEa-;DbFd-;DcGc+;DdHb+;DeHc+;EbGb+;EcFe-;EdHb+;EeHc+;FbFb-;FcFc-;FdAd+;FeAe+;FaAa+;GdGb+;GeGc+;GaIf-;HdHb+;HeHc+;HaIg-;IbIb-;IcIc-;IdIb-;IeIc-;Ia.a-]!cbbcbbc'`

### binary_and.json same size

```shell
clear
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&111=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&111=' -q
```

### binary_and.json left bigger

```shell
clear
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '000&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '001&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '010&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '011&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '100&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '101&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '110&11=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&00=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&01=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&10=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '111&11=' -q
```

### binary_and.json right bigger

```shell
clear
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&000=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&000=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&001=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&001=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&010=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&010=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&011=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&011=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&100=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&100=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&101=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&101=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&110=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&110=' -q
echo
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '00&111=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '01&111=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '10&111=' -q
cabal run -v0 exe:ft_turing -- tests/resources/valid/binary_and.json '11&111=' -q
```
