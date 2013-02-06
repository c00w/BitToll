APIServer: BT/*.hs
	ghc -O2 APIServer.hs -threaded -fllvm
profile: APIServer
	rm APIServer
	ghc -O2 APIServer.hs -prof -auto-all -caf-all -fllvm
clean: APIServer
	rm APIServer
