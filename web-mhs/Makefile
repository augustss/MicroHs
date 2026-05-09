all: mhs-embed.js

# Generate JavaScript and Wasm for mhs
mhs-embed.js:
	mhs -temscripten_web -z -i  -i../mhs -i../src MicroHs.Main -omhs-embed.js --embed-packages base-32-0.15.9.0.pkg

# Run a simple we server for the mhs page
serve:
	python3 -m http.server 8000

clean:
	rm -rf mhs-embed.js
