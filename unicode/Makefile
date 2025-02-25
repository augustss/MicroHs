UNICODE_VERSION=15.1.0
.PHONY: clean

all: uni_string

uni_string:	UniParse UnicodeData.txt
	./UniParse > uni_string

UniParse: UniParse.hs
	mhs UniParse.hs -oUniParse

UnicodeData.txt:
	wget https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt

DerivedCoreProperties.txt:
	wget https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedCoreProperties.txt

clean:
	rm -rf UnicodeData.txt DerivedCoreProperties.txt UniParse uni_string
