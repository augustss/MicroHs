UNICODE_VERSION=18.0.0
.PHONY: clean

all: uni_string uni_case

uni_string:	UniParse UnicodeData.txt
	echo "unicodeVersion :: Version" > uni_string
	echo $(UNICODE_VERSION) | sed -e 's/\./,/g' -e 's/^/unicodeVersion = makeVersion [/' -e 's/$$/]/' >> uni_string
	./UniParse >> uni_string

UniParse: UniParse.hs
	mhs UniParse.hs -oUniParse

uni_case: SpecialParse SpecialCasing.txt CaseFolding.txt
	./SpecialParse > uni_case

SpecialParse: SpecialParse.hs
	mhs SpecialParse.hs -oSpecialParse

UnicodeData.txt:
	wget https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt

DerivedCoreProperties.txt:
	wget https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedCoreProperties.txt

SpecialCasing.txt:
	wget https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/SpecialCasing.txt

CaseFolding.txt:
	wget https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/CaseFolding.txt

clean:
	rm -rf UnicodeData.txt DerivedCoreProperties.txt SpecialCasing.txt CaseFolding.txt UniParse SpecialParse uni_string uni_case
