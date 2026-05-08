all: mhs-embed.js

# Generate JavaScript and Wasm for mhs
mhs-embed.js:
	mhs +RTS -H500M -RTS -temscripten_web -z -i  -i../mhs -i../src MicroHs.Main -omhs-embed.js --embed-packages base

# Run a simple we server for the mhs page
serve:
	python3 -m http.server 8000

clean:
	rm -rf mhs-embed.js
