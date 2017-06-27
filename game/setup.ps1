echo 'Installing sdl2 packages...'
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_mixer

echo 'Cloning library repos...'
git clone https://github.com/haskell-game/sdl2.git
git clone https://github.com/sbidin/sdl2-mixer.git
 
echo 'Building sdl2...'
cd sdl2
stack build
	
echo "Updating sdl2-mixer's stack.yaml to use local sdl2..."
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

echo 'Building sdl2-mixer...'
cd sdl2-mixer
stack build

echo 'Done!'