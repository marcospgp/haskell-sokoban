 Para compilar o jogo:

 ghc --make Sokoban.hs -threaded

 (Gloss programs should be compiled with -threaded, otherwise the GHC runtime will limit the frame-rate to around 20Hz.)