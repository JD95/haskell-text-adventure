stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_mixer

git clone https://github.com/haskell-game/sdl2.git
git clone https://github.com/sbidin/sdl2-mixer.git
 
cd sdl2
stack build
	
cd ..

'flags: {}' >> mixer-stack.yaml
'extra-package-dbs: []' >> mixer-stack.yaml
'packages:' >> mixer-stack.yaml
"- '.'" >> mixer-stack.yaml
"- location: ../sdl2" >> mixer-stack.yaml
'extra-deps: []' >> mixer-stack.yaml
'resolver: lts-8.18' >> mixer-stack.yaml

rm sdl2-mixer/stack.yaml
Move-Item mixer-stack.yaml sdl2-mixer/stack.yaml

cd sdl2-mixer
stack build